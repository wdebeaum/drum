use utf8;
package TextTagger::Units;
require Exporter;

@ISA = qw(Exporter);
@EXPORT_OK = qw(init_units_tagger tag_units);

use lib "$ENV{TRIPS_BASE}/etc/util";
require 'add_suffix.ph';

use TextTagger::Util qw(match2tag union);

use strict vars;

my $debug = 0;

# common short English words that can be constructed from multiple units and
# prefixes, e.g.:
# "the" becomes t=ton, h=Planck's constant, e=atomic charge
# "and" becomes a=are, n=nano-, d=day
# "in" could be "inches", but the preposition is waaay more common
my @stoplist = qw(
a act add age air all amp and ant as ash ask at aye
bad bag ban bar bat be bed bee beg bet bin bus but bye cab cat cut
dry due
eat egg end eye
fat fee fly fun
gas get gig gut
ham hat he hug hut
in
jar
key
lab lad leg let lid lie lot
me mid mud mug
net nut
oil
pad pal pat pet pie pig pin pit pub put
ray rib rid rod
sad sea see set she sin sue sum sun
tea the tin to toe
us use
ye yes yet
);
my $stoplist_re = '(?:' . join('|', @stoplist) . ')';
my $not_stoplist_word_re = qr/
  (?! # not
    \b $stoplist_re \b # word on the stoplist
    (?! \s* [\*×⋅\/] ) # not followed by a multiplicative operator
  )
/x;

# fundamental dimensions grounded mostly with their SI units
# - 1 is for dimensionless units
# - I'm overriding the dimensionlessness of angle measurements in SI by saying
# that their dimension is "angle"; in particular, arc degrees/minutes are
# important to us
# - macOS units file uses coulomb as fundamental and defines ampere as
# coulomb/second, but GNU units file uses ampere as fundamental and defines
# coulomb as ampere*second. We go the macOS way here.
# - any units we can't ground this way get dimension 'unknown'
my %dimensions = qw(
  1		1
  radian	angle
  meter		length
  kg		mass
  second	time
  coulomb	charge
  kelvin	temperature
  mole		amount_of_substance
  candela	luminous_intensity
  bit		information
);

# map dimensions to ONT types
# composite dimensions are of the form d1^e1*d2^e2*... where dN is a
# fundamental dimension (in alphabetical order) and eN is its exponent (which
# may be negative, or omitted to mean 1)
# (ugh. TRIPS, mass!=weight, volt!=watt, Hz!=mph !)
my %dim2ont = qw(
  unknown				MEASURE-UNIT
  1					NUMBER-UNIT
  angle					ANGLE-UNIT
  length				LENGTH-UNIT
  length^2				AREA-UNIT
  length^3				VOLUME-UNIT
  mass					WEIGHT-UNIT
  length*mass*time^-2			WEIGHT-UNIT
  time					TIME-UNIT
  information				MEMORY-UNIT
  length^2*mass*time^-2			ENERGY-UNIT
  angle^2*luminous_intensity		LUMINOSITY-UNIT
  charge^-1*length^2*mass*time^-2	POWER-UNIT
  length^2*mass*time^-3			POWER-UNIT
  time^-1				SPEED-UNIT
  length*time^-1			SPEED-UNIT
  temperature				TEMPERATURE-UNIT
);

# map each unit name to a descriptor for the unit (which is shared among
# alternative names for the same unit):
# unit_name => {
#   names => qw(...),
#   preferred => '...',
#   definition => '...',
#   dimension => { base => exp, base => exp, ... }
# }
my %units = ();

# map each prefix unit name (without the dash) to 1
my %prefixes = ();
# regexen:
# prefix unit name (without the dash)
my $prefix_re; # set at init
# factors in numerator or denominator of unit expression
my $numerator_factor_re; # set at init
my $denominator_factor_re; # set at init
# unit expression in context
my $unit_expr_re; # set at init
# unit exponents (usually 1 digit, excluding 0 and +1, maybe with negation)
my $exponent_re = qr/(?: \s* ^? \s* (?<exponent> -? [2-9] | -1 ) (?![\d\.]) )/x;
# * operator in numerator
# NOTE: juxtaposition works as * if not followed by a word on the stoplist
my $numerator_mult_re = qr/\s* (?: [\*×⋅] \s* | $not_stoplist_word_re )/x;
# * operator in denominator
my $denominator_mult_re = qr/\s* (?: [\*×⋅\/] \s* | $not_stoplist_word_re )/x;
# main / operator for unit fraction
my $per_re = qr/(?: \s* \/ \s* | \s+ per \s+ )/x;

# normalize a { base => exp } hash by deleting entries with exp=0, and deleting
# the base=1 entry if it's not the only entry
sub normalize_expr_hash {
  my $expr = shift;
  # add a unit dimension just in case
  $expr->{'1'} = 1;
  # delete any dimensions with exponent zero
  my @zero_dims = grep { $expr->{$_} == 0 } keys %$expr;
  delete @$expr{@zero_dims};
  # delete the unit dimension if there are any others
  delete $expr->{'1'} if (keys %$expr > 1);
  return $expr;
}

# given a unit name or descriptor, return a hashref mapping fundamental
# dimension names to their exponents, e.g. the dimension length/time becomes
# { length => 1, time => -1 }
sub get_dimension {
  my $unit = shift;
  unless (ref($unit)) {
    # try removing prefixes until the unit is defined
    until (defined($units{$unit})) {
      if ($unit =~ /^$prefix_re/) { # found a prefix
	$unit = $';
      } else { # unit is not defined
	return { unknown => 1 };
      }
    }
    # unit is defined now; switch to its descriptor
    $unit = $units{$unit};
  }
  unless (exists($unit->{dimension})) {
    my $defn = $unit->{definition};
    if ($defn =~ /!/) { # fundamental (but not one of ours)
      $unit->{dimension} = { unknown => 1 };
    } else { # not fundamental
      # HACK: remove & stuff in macos units defns so we at least get the right dimensions for temperature units
      $defn =~ s/\&\+?//g;
      # remove leading literal number (including sci. notation and fractions)
      $defn =~ s/^[+-]?(\d*\.\d+|\d+(\|\d+)?)([eE][+-]?\d+)?\s*//g;
      # parse unit fraction
      my ($numerator, $denominator) = split(m{\s*/\s*}, $defn, 2);
      $numerator = '1' if ($numerator eq '');
      # parse multiplication
      # FIXME? '-' might also be part of a negative exponent... but I don't see any examples of that in the unit definitions files I have
      my @numerator_terms = split(m{\s*[\*\/-]\s*}, $numerator);
      my @denominator_terms =
        (defined($denominator) ? split(m{\s*[\*\/-]\s*}, $numerator) : ());
      # parse and add exponents
      my %unit_terms = (); # map unit base to exponent
      for my $term (@numerator_terms) {
	my ($base, $exp) = split(/\^|(?=\d+$)/, $term, 2);
	$exp = 1 unless (defined($exp));
	$unit_terms{$base} += 0+$exp;
      }
      for my $term (@denominator_terms) {
	my ($base, $exp) = split(/\^|(?=\d+$)/, $term, 2);
	$exp = 1 unless (defined($exp));
	$unit_terms{$base} += 0-$exp;
      }
      # convert unit bases to their dimensions and combine
      my $dim_terms = {}; # map dimension base to exponent
      for my $unit_base (keys %unit_terms) {
	my $unit_exp = $unit_terms{$unit_base};
	my $new_dim_terms = get_dimension($unit_base);
	for my $dim_base (keys %$new_dim_terms) {
	  my $dim_exp = $new_dim_terms->{$dim_base};
	  $dim_terms->{$dim_base} += $dim_exp * $unit_exp;
	}
      }
      normalize_expr_hash($dim_terms);
      $unit->{dimension} = $dim_terms;
    }
  }
  return $unit->{dimension};
}

# given a base=>exp hashref representing a unit or dimension expression, return
# its canonical string representation
sub expr_hash_to_string {
  my $expr = shift;
  my @terms =
    map {
      my $base = $_;
      my $exp = $expr->{$base};
      ($exp == 1 ? $base : "$base^$exp")
    } sort keys %$expr;
  return join('*', @terms);
}

# return the list of prefixes followed by the root unit if the given whole unit
# can be so split; return the empty list if the unit is unknown (only used
# during init)
# (gram) millikg=>qw(milli kg) or maybe qw(milli k g) depending on defns
# (Calorie) kilocalorie=>qw(kilo calorie)
# (hectare) hectoare=>qw(hecto are)
sub parse_prefixes {
  my ($whole) = @_;
  return ($whole) if (exists($units{$whole}));
  # try longest prefix first, scan backwards for split point
  for my $prefix_len (reverse(1..(length($whole)-1))) {
    my $prefix = substr($whole, 0, $prefix_len);
    my $suffix = substr($whole, $prefix_len);
    # recurse if the prefix exists; return if rest is a known unit
    if (exists($prefixes{$prefix})) {
      my @rest = parse_prefixes($suffix);
      return ($prefix, @rest) if (@rest);
    }
  }
  # no split was successful
  return ();
}

# return a regex string that would match any of the keys of the given hashref
sub hash_keys_re {
  my $h = shift;
  return '(?:' . join('|', sort { length($b) <=> length($a) } keys %$h) . ')';
}

sub init_units_tagger {
  # locate unit definitions file
  my $units_file = `units -U 2>&1`;
  if ($units_file =~ /illegal option/) { # macOS' ancient version of units
    $units_file = '/usr/share/misc/units.lib';
  } elsif ($units_file =~ m[^/]) {
    chomp $units_file;
  } else {
    die "unable to determine units library filename; units -U said:\n$units_file";
  }
  # read definitions
  %units = ('1' => { names => ['1'], definition => '1' });
  open(U, '<', $units_file) or die "Can't open units library $units_file: $!";
  binmode U, ':utf8';
  while (<U>) {
    # treat backslash at the end of a line as a line continuation
    while (/\\\s*$/) {
      my $next_line = <U>;
      last unless ($next_line);
      $_ = $` . $next_line;
    }
    if (/^!var(?! UNITS_ENGLISH US)/) {
      # skip conditional parts of GNU units definition file, except US english
      # units
      while (<U>) {
	last if (/^!endvar/);
      }
    # unit def starts with the unit (percent sign, or word starting with a
    # letter and maybe ending with a dash), followed by whitespace, followed by
    # the definition; comments start with # and go to the end of the line
    } elsif (/^(%|\p{L}\w*-?)\s+([^#\n]+)/) {
      my ($name, $defn) = ($1,$2,$3);
      # skip dimensions (GNU units defines these with ALL_CAPS, >=4chars)
      next if ($name =~ /^[A-Z_]{4,}$/);
      # skip other specific definitions we don't like
      next if (
        ($name eq 'key' and $defn eq 'kg') or # macos is a druglord
        ($name eq 'energy' and $defn eq 'c2') or # not even right; it's mass*c2!
	($name eq 'g' and $defn eq 'g0') or # force of gravity on Earth!
	($name eq 'force' and $defn eq 'g') # ...cont'd. WTAF.
      );
      # skip names of numbers and simple fractions, but keep "partsper..."
      next if ($defn =~ /^(\d+|\d\|\d)$/ or
	       $name eq 'googol' or
	       ($name =~ /(score|illion|illiard)$/ and
	        $name !~ /^partsper\w+illion$/));
      $prefixes{$`} = 1 if ($name =~ /-$/);
      $defn =~ s/\s+$//;
      my $desc;
      if (exists($units{$name})) { # already have this unit from a prev. defn.
	$desc = $units{$name};
      } else { # unit name we've never seen before
	$desc = { names => [$name] };
      }
      if ($defn =~ /^\p{L}\w*$/i and # looks like a single unit
	  @{[parse_prefixes($defn)]} <= 1 # not prefixes+root AFAWK
	 ) {
	# therefore, $defn is another single unit, make $name a synonym
	# add a dash if we know the other unit to be a prefix
	$defn .= '-'
	  if ((not exists($units{$defn})) and exists($units{"$defn-"}));
	if (exists($units{$defn})) { # seen the other unit before
	  if (exists($units{$name})) { # also seen this unit before
	    # discard old desc for this unit and merge into desc for defn
	    print STDERR "warning: discarding old definition '$desc->{definition}' for unit '$name' in favor of equivalence to '$defn'\n" if (exists($desc->{definition}));
	    my $old_names = $desc->{names};
	    $desc = $units{$defn};
	    $desc->{names} = union($desc->{names}, $old_names);
	    for (@$old_names) { $units{$_} = $desc; }
	  } else {
	    $desc = $units{$defn};
	    push @{$desc->{names}}, $name;
	  }
	} else { # never seen the other unit before, add it too
	  $units{$defn} = $desc;
	}
      } else { # non-synonym defn.
        print STDERR "warning: discarding old definition '$desc->{definition}' for unit '$name' in favor of new definition '$defn'\n" if (exists($desc->{definition}));
        $desc->{definition} = $defn;
      }
      $units{$name} = $desc;
    } elsif (/^!include\s+([^#\n]+)/) {
      # TODO? macOS units doesn't use this, and GNU units uses it only for currency
      print STDERR "warning: ignoring '!include $1' in units file\n";
    }
  }
  close U;
  # add 'g' as a synonym of 'gram' if we're missing it because macos units
  # thinks g is the force of gravity on Earth (again, WTAF)
  if (exists($units{gram}) and not exists($units{g})) {
    push @{$units{gram}{names}}, 'g';
    $units{g} = $units{gram};
  }
  # add 'a' as a synonym of 'are' if we're missing it, so we can get ha=hectoare
  if (exists($units{are}) and not exists($units{a})) {
    push @{$units{are}{names}}, 'a';
    $units{a} = $units{are};
  }
  # add (if missing) all-caps versions of "x-per-y" unit abbreviations,
  # e.g. PPM, MPH, FPS
  for my $name (keys %units) {
    # if unit name is the lowercase version of such an abbr., and uppercase
    # version is not defined
    if ($name =~ /^([a-z])p([a-z])$/ and not exists($units{uc($name)})) {
      my ($n, $d) = ($1, $2);
      my $desc = $units{$name};
      # if the definition is a fraction with the right initials, or the
      # numerator initial is 'p' and an appropriate 'partsper...' is a synonym
      if ($desc->{definition} =~ /^$n\w*\/$d\w*$/ or
	  ($n eq 'p' and
	   grep /^partsper$d\w*$/, @{$desc->{names}})) {
	# add all-caps abbreviation as synonym
	my $cname = uc($name);
	push @{$desc->{names}}, $cname;
	$units{$cname} = $desc;
      } elsif ($debug) {
	print STDERR "note: not adding upcased synonym of '$name'\n";
	print STDERR Data::Dumper->Dump([$desc], ['*desc']);
      }
    }
  }
  # delete undefined units
  my @undef_unit_names = ();
  for my $name (keys %units) {
    push @undef_unit_names, $name unless (exists($units{$name}{definition}));
  }
  if (@undef_unit_names) {
    print STDERR "warning: deleting undefined unit names: " . join(', ', sort @undef_unit_names) . "\n";
    delete @units{@undef_unit_names};
  }
  # add versions of temperature degree units with "°" and "◦" instead of "deg"
  # (if missing)
  for my $temp_scale (qw(C F K R)) {
    my $degT = "deg$temp_scale";
    if (exists($units{$degT})) {
      my $desc = $units{$degT};
      my $symT = "°$temp_scale";
      unless (exists($units{$symT})) {
	$units{$symT} = $desc;
	push @{$desc->{names}}, $symT;
      }
      my $circT = "◦$temp_scale";
      unless (exists($units{$circT})) {
	$units{$circT} = $desc;
	push @{$desc->{names}}, $circT;
      }
    }
  }
  # count how many unit names lc the same
  my %lc_counts = ();
  for (keys %units) { $lc_counts{lc($_)}++; }
  # select a preferred name for each unit
  for my $name (keys %units) {
    next if (exists($units{$name}{preferred})); # already saw this desc
    my @names = @{$units{$name}{names}};
    # good names are all lowercase latin letters, and don't conflict with other
    # units when they are downcased
    my @good_names = grep { /^[a-z]+$/ and $lc_counts{$_} == 1 } @names;
    # even better would be to have 3 or more letters (e.g. "meter" not "m")
    my @better_names = grep { length($_) >= 3 } @good_names;
    $units{$name}{preferred} =
      (@better_names ? $better_names[0] :
		       (@good_names ? $good_names[0] : $names[0]));
  }
  # init fundamental dimensions
  for my $name (keys %dimensions) {
    my $dim = $dimensions{$name};
    if (exists($units{$name})) {
      $units{$name}{dimension} = { $dim => 1 };
    } else {
      print STDERR "warning: missing fundamental unit '$name' for dimension '$dim'\n";
    }
  }
  # special case to take care of charge/current fundamentality difference
  # between macOS units and GNU units (see comment above %dimensions)
  $units{ampere}{dimension} = { charge => 1, time => -1 };
  # regex for prefixes (used by get_dimension)
  $prefix_re = hash_keys_re(\%prefixes);
  # determine all other dimensions
  for my $name (keys %units) {
    get_dimension($name);
  }
  #print STDERR Data::Dumper->Dump([\%units],['*units']) if ($debug);
  # temporarily take out the '1' unit so it doesn't get put in regexes
  my $one_desc = $units{'1'};
  delete $units{'1'};
  # regex for singular units
  my $singular_re = hash_keys_re(\%units);
  # add plurals as additional keys of %units, but don't:
  # - pluralize prefixes
  # - pluralize the shortest name of a unit with multiple names (initialism)
  # - add any ambiguous plurals
  # - add plurals to {names}
  my %plural_units = ();
  for my $singular (keys %units) {
    next if ($singular =~ /-$/); # prefix
    next if (@{$units{$singular}{names}} > 1 and
             not grep {
	       $_ ne $singular and length($_) < length($singular)
	     } @{$units{$singular}{names}});
    my $plural = add_suffix($singular, 's');
    next if (exists($units{$plural}));
    if (exists($plural_units{$plural})) {
      $plural_units{$plural} = 0; # nevermind, this plural is ambiguous
    } else {
      $plural_units{$plural} = $units{$singular};
    }
  }
  for my $plural (keys %plural_units) {
    $units{$plural} = $plural_units{$plural} if ($plural_units{$plural});
  }
  # build up final regex:
  # unit name in numerator (can be singular or plural)
  my $numerator_unit_re = hash_keys_re(\%units);
  $units{'1'} = $one_desc; # restore '1' unit
  $numerator_factor_re = qr/$prefix_re?? $numerator_unit_re $exponent_re?/x;
  $denominator_factor_re = qr/$prefix_re?? $singular_re $exponent_re?/x;
  my $denominator_re = qr/
    (?<denominator>
      $denominator_factor_re
      (?: $denominator_mult_re $denominator_factor_re )*
    )
  /x;
  # regex for whole unit expression without context
  my $cf_unit_expr_re = qr/(?:
    (?<numerator>
      $not_stoplist_word_re
      $numerator_factor_re
      (?: $numerator_mult_re $numerator_factor_re )*
    )
    (?: $per_re $denominator_re )?
    |
    # numerator can be a single non-unit word, like "plants", if we
    # definitely have a denominator
    (?<numerator> \b \p{L}+ \b) $per_re $denominator_re
  )/x;
  # regex with context
  $unit_expr_re = qr/
    ^ \s* \K $cf_unit_expr_re (?= \s* $ ) | # the whole text is just units
    \( \s* \K $cf_unit_expr_re (?= \s* \) ) | # units in parens
    \d \s* \K $cf_unit_expr_re (?! \w ) | # units after a number
    \b [Ii]n \s+ \K $cf_unit_expr_re (?! \w ) # units after the word "in"
  /x;
}

# given a string that matched as either numerator or denominator (indicated by
# the first argument) return three values: two expr hashes using the preferred
# names of all units and prefixes (the first with separate prefixes, the second
# with attached prefixes), and a boolean indicating whether any units were
# plural
sub parse_fraction_component {
  my ($which, $component) = @_;
  my $factor_re;
  if ($which eq 'numerator') {
    $factor_re = qr/$not_stoplist_word_re $numerator_factor_re/x;
  } elsif ($which eq 'denominator') {
    $factor_re = $denominator_factor_re; # for first factor; then add stoplist
  } else {
    die "WTF";
  }
  my $expr_sp = {}; # separate prefixes
  my $expr_ap = {}; # attached prefixes
  my $is_plural = 0;
  my $prev_end = 0;
  my $process_factor = sub {
    my $unit = $&;
    print STDERR "unit=$unit\n" if ($debug);
    # get the prefix, root unit, and exponent of the factor, if they're there
    # first get the exponent and strip it off the unit
    my $exponent = 1;
    if ($unit =~ /$exponent_re$/) {
      $exponent = 0 + $+{exponent};
      $unit = $`;
    }
    print STDERR "unit=$unit; exponent=$exponent\n" if ($debug);
    # then get the prefix and strip it off, but only if we don't already know
    # the whole unit, and do know the root unit
    my $prefix = undef;
    if ((not exists($units{$unit})) and
        $unit =~ /^$prefix_re/ and
	exists($units{$'})) {
      $prefix = "$&-";
      $unit = $';
      print STDERR "prefix=$prefix; unit=$unit\n" if ($debug);
      # use preferred name of prefix
      exists($units{$prefix}) or die "bogus prefix match '$prefix'?!";
      $prefix = $units{$prefix}{preferred};
    }
    # use preferred name of root unit if it exists (might be unk. like "plants")
    # also detect plurality by whether the matched unit is in {names}
    if (exists($units{$unit})) {
      $is_plural = 1 unless (grep { $_ eq $unit } @{$units{$unit}{names}});
      $unit = $units{$unit}{preferred};
    } elsif ($unit =~ /s$/) { # detect plurality of unk. units by final "s"
      $is_plural = 1;
    }
    print STDERR "pref.prefix=$prefix; pref.unit=$unit; is_plural=$is_plural\n" if ($debug);
    # add prefix and unit to expr
    $expr_sp->{$unit} += $exponent;
    if (defined($prefix)) {
      $expr_sp->{$prefix} += $exponent;
      $prefix =~ s/-$//;
      $unit = $prefix . $unit;
    }
    $expr_ap->{$unit} += $exponent;
  };
  # scan factors of fraction component
  print STDERR "parsing $which = $component\n" if ($debug);
  while ($component =~ /(?: $factor_re \b | $factor_re )/gx) {
    my ($start, $end) = ($-[0], $+[0]);
    my $op = substr($component, $prev_end, $start - $prev_end);
    $prev_end = $end;
    if ($op =~ /\p{L}/) { # we skipped some letters
      # trigger skipped letter processing after loop
      $prev_end = 0;
      last;
    }
    &$process_factor();
    $factor_re = qr/$not_stoplist_word_re $denominator_factor_re/x
      if ($which eq 'denominator');
  }
  if (# we skipped some letters at the end
      substr($component, $prev_end) =~ /\p{L}/ or
      # we parsed multiple units run together as a plural (in the numerator)
      ($is_plural and (keys %$expr_ap > 1))
     ) {
    if ($which eq 'numerator') { # numerator actually wasn't parsed like this
      # reset and take whole word as numerator factor
      $expr_sp = {};
      $expr_ap = {};
      $is_plural = 0;
      $prev_end = 0;
      pos($component) = 0;
      die "can't parse numerator word?!"
	unless ($component =~ /^\p{L}+$/g);
      &$process_factor();
    } else { # something went very wrong with denominator
      die "can't parse denominator?!"
    }
  }
  normalize_expr_hash($expr_sp);
  normalize_expr_hash($expr_ap);
  return ($expr_sp, $expr_ap, $is_plural);
}

sub tag_units {
  my ($self, $text) = @_;
  my @tags = ();
  while ($text =~ /$unit_expr_re/g) {
    my $tag = +{ type => 'sense', match2tag() };
    my $matched_numerator = $+{numerator};
    my $matched_denominator = $+{denominator}; # maybe undef
    print STDERR "got unit expression '$tag->{lex}'; numerator = '$matched_numerator'; denominator = '$matched_denominator'\n" if ($debug);
    my ($numerator_expr_sp, $numerator_expr_ap, $is_plural) =
      parse_fraction_component('numerator', $matched_numerator);
    $tag->{'penn-pos'} = [$is_plural ? 'NNS' : 'NN'];
    # will hold combined numerator/denominator expr
    my $unit_expr_sp = {};
    my $unit_expr_ap = {};
    # copy numerator
    for my $unit (keys %$numerator_expr_sp) {
      $unit_expr_sp->{$unit} = $numerator_expr_sp->{$unit};
    }
    for my $unit (keys %$numerator_expr_ap) {
      $unit_expr_ap->{$unit} = $numerator_expr_ap->{$unit};
    }
    my $denominator_expr_sp;
    my $denominator_expr_ap;
    if (defined($matched_denominator)) {
      ($denominator_expr_sp, $denominator_expr_ap, undef) = # no plural denom.
	parse_fraction_component('denominator', $matched_denominator);
      # subtract denominator exponents
      for my $unit (keys %$denominator_expr_sp) {
	$unit_expr_sp->{$unit} -= $denominator_expr_sp->{$unit};
      }
      for my $unit (keys %$denominator_expr_ap) {
	$unit_expr_ap->{$unit} -= $denominator_expr_ap->{$unit};
      }
    }
    # make canonical string for units
    normalize_expr_hash($unit_expr_sp);
    normalize_expr_hash($unit_expr_ap);
    my $unit_str = expr_hash_to_string($unit_expr_ap);
    # get dimension by adding up dimensions of each unit
    my $dim_expr = {};
    for my $unit (keys %$unit_expr_sp) {
      my $exponent = $unit_expr_sp->{$unit};
      my $unit_dim_expr = get_dimension($unit);
      for my $dim (keys %$unit_dim_expr) {
	$dim_expr->{$dim} += $unit_dim_expr->{$dim} * $exponent;
      }
    }
    normalize_expr_hash($dim_expr);
    # use dimension to look up ONT type
    my $dim_str = expr_hash_to_string($dim_expr);
    $tag->{lftype} =
      [exists($dim2ont{$dim_str}) ? $dim2ont{$dim_str} : $dim2ont{unknown}];
    # add DSI
    $tag->{'domain-specific-info'} = {
      domain => 'cwms',
      type => 'units',
      units => $unit_str,
      dimensions => $dim_str
    };
    #print STDERR Data::Dumper->Dump([$is_plural, $numerator_expr_sp, $numerator_expr_ap, $denominator_expr_sp, $denominator_expr_ap, $unit_expr_sp, $unit_expr_ap, $unit_str, $dim_expr, $dim_str], [qw(is_plural *numerator_expr_sp *numerator_expr_ap denominator_expr_sp denominator_expr_ap *unit_expr_sp *unit_expr_ap unit_str *dim_expr dim_str)]) if ($debug);
    push @tags, $tag;
  }
  # also tag angles in degrees°(minutes′(seconds″)) format
  # this can be subtle because the symbols used overlap with temperature
  # degrees (e.g. temp. 98.6°F) as well as feet/inches (e.g. height 5'11")
  while ($text =~ /
           (?<! \d )
	   # degrees
           (?<dnum> \d{1,3} )
	   \s*
	   (?<dunit> [°◦] ) # degree symbol and small white circle
	   \s*
	   # not an explicit temperature degree
	   # (Celsius, Fahrenheit, Kelvin, Rankine)
	   (?! [CFKR] )
	   (?:
	     # minutes
	     (?<mnum> \d{1,2} )
	     \s*
	     (?<munit> [′'´] ) # prime, apostrophe, acute accent
	     \s*
	     (?:
	       # seconds (and decimal seconds)
	       (?<snum> \d{1,2} (?: \. \d* ) )
	       \s*
	       # double-prime, double-quote, or two of the minutes chars
	       (?<sunit> [″"] | ′′ | '' | ´´ )
	     )?
	   )?
         /xg) {
    # check range of numbers
    next if ($+{dnum} > 360 or $+{mnum} > 59 or $+{snum} > 59);
    # add tag for each unit
    push @tags, {
      type => 'sense',
      lex => $+{dunit},
      start => $-[2], end => $+[2],
      'penn-pos' => ['NN'],
      lftype => ['ANGLE-UNIT'],
      'domain-specific-info' => {
	domain => 'cwms',
	type => 'units',
	units => 'arcdeg',
	dimensions => 'angle'
      }
    };
    push @tags, {
      type => 'sense',
      lex => $+{munit},
      start => $-[4], end => $+[4],
      'penn-pos' => ['NN'],
      lftype => ['ANGLE-UNIT'],
      'domain-specific-info' => {
	domain => 'cwms',
	type => 'units',
	units => 'arcmin',
	dimensions => 'angle'
      }
    }
      if (exists($+{munit}));
    push @tags, {
      type => 'sense',
      lex => $+{sunit},
      start => $-[6], end => $+[6],
      'penn-pos' => ['NN'],
      lftype => ['ANGLE-UNIT'],
      'domain-specific-info' => {
	domain => 'cwms',
	type => 'units',
	units => 'arcsec',
	dimensions => 'angle'
      }
    }
      if (exists($+{sunit}));
  }
  # TODO?
  # This degrees-minutes-seconds pattern of disambiguation is actually common to a few sets of non-metric units for the same dimensions, e.g.:
  # - feet' inches"
  # - hoursH minutesM secondsS (not Planck constant, meters, seconds) or hours:minutes:seconds (which is itself ambiguous when only two are given: hours:minutes or minutes:seconds?)
  # ... but I think we're unlikely to encounter these in the current CWMS work.
  return [@tags];
}

push @TextTagger::taggers, {
  name => 'units',
  init_function => \&init_units_tagger,
  tag_function => \&tag_units,
  input_text => 1,
  output_types => ['sense']
};

1;
