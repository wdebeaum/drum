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
# Also a few longer words so constructed.
# Also a few words that are single units but are way more common in other
# senses, and have been seen in contexts where we might falsely think they're
# units (i.e. after a number), e.g. "in" as "inches", "key" as "kilogram".
my @stoplist = qw(
a act add age air all amp and ant arid as ash ask at aye
bad bag ban bar bat be bed bee beg bet bin bus but bye
cab camp camps cat cut
dry due
eat egg end epidemic eye
fat fee fly fun
gas get gig gut
ham has hat he hug hut
in
jar
key
lab lad last leg let lid lie lot
me mid mud mug
net nut
oil
pad pal pat pet pie pig pin pit pub put
ray rib rid rod
sad sale sea see set she sin sue sum sun
tea the tin to toe
us use
ye yes yet
);
my $stoplist_re = '(?:' . join('|', @stoplist) . ')';
my $stoplist_word_re = qr/
    $stoplist_re \b # word on the stoplist
    (?! \s* [\*×⋅\/] ) # not followed by a multiplicative operator
/x;
my $not_stoplist_word_re = qr/(?!$stoplist_word_re)/;

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
my $max_prefix_length; # number of characters in the longest possible prefix
# root/base unit name
my $singular_unit_re; # set at init
my $singular_or_plural_unit_re; # set at init
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
      # remove leading literal number (including sci. notation, fractions, exps)
      $defn =~ s/
        ^
	[+-]?
	( \d* \. \d+ | \d+ ( \| \d+ )? )
	( [eE^] [+-]? \d+ )?
	\s*
      //gx;
      # parse unit fraction
      my ($numerator, $denominator) = split(m{\s*/\s*}, $defn, 2);
      $numerator = '1' if ($numerator eq '');
      # parse multiplication
      # FIXME? '-' might also be part of a negative exponent... but I don't see any examples of that in the unit definitions files I have
      my @numerator_terms = split(m{\s*(?:[\*\/-]\s*|\s+)}, $numerator);
      my @denominator_terms =
        (defined($denominator) ?
	  split(m{\s*(?:[\*\/-]\s*|\s+)}, $denominator) : ());
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
  if (not defined($units_file)) {
    die "can't run 'units' command; is it installed?";
  } elsif ($units_file =~ /illegal option/) { # macOS' ancient version of units
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
      # skip names of numbers and simple fractions, but keep prefixes and
      # "partsper..."
      next if (($defn =~ /^(\d+|\d\|\d)$/ and $name !~ /-$/) or
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
      if ($defn =~ /^\p{L}\w*(?<![2-9])$/i and # looks like a single unit
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
  # find max prefix length
  $max_prefix_length = 0;
  for my $prefix (keys %prefixes) {
    my $prefix_length = length($prefix);
    $max_prefix_length = $prefix_length
      if ($max_prefix_length < $prefix_length);
  }
  # determine all other dimensions
  for my $name (keys %units) {
    get_dimension($name);
  }
  #print STDERR Data::Dumper->Dump([\%units],['*units']) if ($debug);
  # temporarily take out the '1' unit so it doesn't get put in regexes
  my $one_desc = $units{'1'};
  delete $units{'1'};
  # regex for singular units
  $singular_unit_re = hash_keys_re(\%units);
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
  # regex for singular or plural units
  $singular_or_plural_unit_re = hash_keys_re(\%units);
  $units{'1'} = $one_desc; # restore '1' unit
}

# Backtracking support
#
# usage:
#   choose([choice1, choice2, ...], sub {
#     my $chosen = shift;
#     # ... code that sets $val but might deeply call fail()...
#     return $val;
#   });
#
# This will call the sub with each choice until it doesn't fail, and then
# return its return values. If all choices fail, choose fails too, causing
# further backtracking. Other exceptions are rethrown immediately rather than
# causing backtracking.
sub fail { die '__FAIL__ at line ' . [caller]->[2]; }
sub failed { return ($@ =~ /__FAIL__/); }
my $btd = 0; # backtracking depth, used for debug indenting
sub choose {
  my ($choices, $fn) = @_;
  #print STDERR (' 'x$btd) . Data::Dumper->Dump([$choices],['*choices']) if ($debug);
  print STDERR (' 'x$btd) . "choices = ($choices->[0] .. $choices->[-1])\n" if ($debug);
  for my $chosen (@$choices) {
    print STDERR (' 'x$btd) . "trying choice $chosen...\n" if ($debug);
    my @vals;
    $btd++ if ($debug);
    my $success = eval { @vals = &$fn($chosen); 1 };
    $btd-- if ($debug);
    if ($success) {
      print STDERR (' 'x$btd) . "success!\n" if ($debug);
#      print STDERR (' 'x$btd) . Data::Dumper->Dump([\@vals],['*vals']) if ($debug);
      return @vals;
    } elsif (not failed()) {
      die $@; # re-throw exception that didn't come from fail()
    } else {
      print STDERR (' 'x$btd) . $@ . "\n" if ($debug);
    }
  }
  fail();
}

# Parsing functions
#
# In general, this:
#   parse_foo($str, args..., sub { my ($foo, $foo_len, $str) = @_; ... })
# will try to parse a foo at the start of $str, and then pass a representation
# of the foo, the number of characters it consumed, and the remainder of the
# string to the sub, and return what the sub returns. It instead calls fail()
# if a foo cannot be parsed at this location.

# parse a factor in a units expression with a prefix of a specific length
# see parse_factor() below
sub parse_factor_with_prefix_len {
  my ($str, $unit_re, $prefix_len, $then) = @_;
  my $prefix = undef;
  if ($prefix_len > 0) {
    $prefix = substr($str, 0, $prefix_len);
    print STDERR (' 'x$btd) . "prefix=$prefix\n" if ($debug);
    $prefix =~ /^$prefix_re\z/ or fail();
    $prefix .= '-';
    # use preferred name of prefix
    exists($units{$prefix}) or die "bogus prefix match '$prefix'?!";
    $prefix = $units{$prefix}{preferred};
  }
  my $after_prefix = substr($str, $prefix_len);
  # try to match the longest unit we can first, just to find out how long
  # that is (and fail early if we can't match any unit)
  $after_prefix =~ /^$unit_re/ or fail();
  my $max_unit_len = $+[0];
  # try longest matching units first, but allow backtracking to shorter ones
  print STDERR (' 'x$btd) . "choosing unit length...\n" if ($debug);
  return choose([reverse(1 .. $max_unit_len)], sub {
    my $unit_len = shift;
    my $unit = substr($after_prefix, 0, $unit_len);
    $unit =~ /^$unit_re\z/ or fail();
    print STDERR (' 'x$btd) . "unit=$unit\n" if ($debug);
    my $after_unit = substr($after_prefix, $unit_len);
    # HACK: if we're currently considering no prefix, and we found a unit with
    # the same name as a prefix, run together with following unit(s), we should
    # try to interpret it as the prefix first
    # e.g. "cm" is centi-*meter, not speedoflight*meter
    if ($prefix_len == 0 and exists($prefixes{$unit}) and
        $after_unit =~ /^\p{L}/) {
      my @vals;
      my $success = eval {
	@vals = parse_factor_with_prefix_len($str, $unit_re, $unit_len, $then);
	1
      };
      if ($success) {
	return @vals;
      } elsif (not failed()) {
	die $@; # rethrow non-fail() error
      }
      # else we failed to parse the factor while interpreting what we found as
      # a prefix; continue interpreting it as a unit instead
    }
    # use preferred name of root/base unit
    # also detect plurality by whether the matched unit is in {names}
    exists($units{$unit}) or die "bogus unit match '$unit'?!";
    my $is_plural = ((grep { $_ eq $unit } @{$units{$unit}{names}}) ? 0 : 1);
    $unit = $units{$unit}{preferred};
    # try to match an optional exponent (=1 if missing)
    my ($exp, $exp_len, $after_exp);
    if ($after_unit =~ /^$exponent_re/) {
      ($exp, $exp_len, $after_exp) = ((0 + $+{exponent}), $+[0], $');
      print STDERR (' 'x$btd) . "exp=$exp\n" if ($debug);
    } else {
      ($exp, $exp_len, $after_exp) = (1, 0, $after_unit);
    }
    my $factor = +{
      (defined($prefix) ? (prefix => $prefix) : ()),
      base => $unit,
      is_plural => $is_plural,
      exp => $exp
    };
    print STDERR (' 'x$btd) . Data::Dumper->Dump([$factor],['*factor']) if ($debug);
    my $factor_len = $prefix_len + $unit_len + $exp_len;
    my $after_factor = $after_exp;
    return &$then($factor, $factor_len, $after_factor);
  });
}

# parse a factor in a units expression, which includes exactly one unit name,
# and may have a prefix and/or an exponent. Pass an object like this:
# {
#   [prefix => "prefix",]
#   base => "unit",
#   is_plural => 1,
#   exp => #
# }
# The preferred names of the prefix (with trailing dash) and unit will be used.
# If no explicit exponent is found, the exp field will be 1.
sub parse_factor {
  my ($str, $unit_re, $then) = @_;
  # try shortest prefixes first (including no prefix)
  # (but see HACK in parse_factor_with_prefix_len())
  my $slm1 = length($str) - 1;
  ($slm1 >= 0) or fail();
  my $mpl = (($max_prefix_length < $slm1) ? $max_prefix_length : $slm1);
  print STDERR (' 'x$btd) . "choosing prefix length...\n" if ($debug);
  return choose([0 .. $mpl], sub {
    my $prefix_len = shift;
    return parse_factor_with_prefix_len($str, $unit_re, $prefix_len, $then);
  });
}

# parse one side of a units fraction
# $which = 'numerator' or 'denominator'
# $mult_op is the multiplicative operator to use, or undef if we haven't
# decided yet
# Pass an object similar to that passed by parse_cf_units_expr().
sub parse_fraction_component {
  my ($str, $which, $mult_op, $then) = @_;
  my $unit_re =
    ($which eq 'numerator' ? $singular_or_plural_unit_re : $singular_unit_re);
  # parse the first factor
  parse_factor($str, $unit_re, sub {
    my ($factor, $factor_len, $after_factor) = @_;
    # turn the first factor into the start of a component
    my $prefix_nodash = $factor->{prefix};
    $prefix_nodash =~ s/-$//;
    my $prefixed_unit = $prefix_nodash . $factor->{base};
    my $comp_head = +{
      ap => { $prefixed_unit => $factor->{exp} },
      sp => { 
	(defined($factor->{prefix}) ? ($factor->{prefix} => 1) : ()),
	$factor->{base} => $factor->{exp}
      },
      is_plural => $factor->{is_plural}
    };
    # if factor was plural, stop looking for more factors
    if ($factor->{is_plural}) {
      return &$then($comp_head, $factor_len, $after_factor);
    }
    # otherwise, look for a/the mult op and another factor
    my ($have_mult, $mult_len, $after_mult);
    if (defined($mult_op)) {
      # mult op already defined, just check for that one (but still check
      # stoplist)
      if ($after_factor =~ /^ (?! \s* $stoplist_word_re ) $mult_op /x) {
	($have_mult, $mult_len, $after_mult) = (1, $+[0], $');
      }
    } else { # mult op not yet defined, check for any mult op
      my $mult_re =
        ($which eq 'numerator' ?
	  $numerator_mult_re : $denominator_mult_re);
      if ($after_factor =~ /^$mult_re/) {
	($have_mult, $mult_op, $mult_len, $after_mult) = (1, $&, $+[0], $');
	print STDERR (' 'x$btd) . "mult_op='$mult_op'\n" if ($debug);
      }
    }
    if ($have_mult) {
      # recurse to get the rest of the component with the same mult op
      my @vals;
      my $success = eval {
	@vals = parse_fraction_component($after_mult, $which, $mult_op, sub {
	  my ($comp_tail, $comp_tail_len, $after_comp) = @_;
	  # disallow multiple factors run together with a plural (or a final
	  # "s" as "second")
	  # e.g. "plants"=pico-liter*atto-newtons (or newton*second)
	  print STDERR (' 'x$btd) . Data::Dumper->Dump([$comp_head, $comp_tail, $mult_op],[qw(*comp_head *comp_tail mult_op)]) if ($debug);
	  if (($comp_tail->{is_plural} or
	       ($comp_tail_len == 1 and 
	        (exists($comp_tail->{ap}{second}) or # GNU units
	         exists($comp_tail->{ap}{sec}) # macos units
		)
	       )
	      ) and $mult_op eq '') {
	    print STDERR (' 'x$btd) . "rejecting multiple factors run together with plural\n" if ($debug);
	    fail();
	  }
	  # HACK:
	  # disallow "drop(s)" as the last word in a units expression unless it
	  # has an explicit operator connecting it (not mere juxtaposition), or
	  # it is the whole expression
	  # This is tricky because "drop" can actually be a volume unit, but it
	  # often follows a quantity as a regular noun to indicate the
	  # direction of movement.
	  if ((($comp_tail_len == 4 and substr($after_mult, 0, 4) eq 'drop') or
	       ($comp_tail_len == 5 and substr($after_mult, 0, 5) eq 'drops')
	      ) and
	      $mult_op !~ /\S/ and # prev mult was juxtaposition
	      ($which eq 'denominator' or # this is the denominator
	       $after_comp !~ /^$per_re/ # there is no denominator
	      )
	     ) {
	    fail();
	  }
	  # add $comp_head and $comp_tail to make $comp
	  my $comp = +{
	    sp => {},
	    ap => {},
	    is_plural => $comp_tail->{is_plural}
	  };
	  for my $k (qw(ap sp)) {
	    # copy $comp_tail to $comp
	    for my $base (keys %{$comp_tail->{$k}}) {
	      $comp->{$k}{$base} = $comp_tail->{$k}{$base};
	    }
	    # add $comp_head to $comp
	    for my $base (keys %{$comp_head->{$k}}) {
	      $comp->{$k}{$base} += $comp_head->{$k}{$base};
	    }
	  }
	  my $total_len = $factor_len + $mult_len + $comp_tail_len;
	  return &$then($comp, $total_len, $after_comp);
	});
	1
      };
      if ($success) {
	return @vals;
      } elsif ($mult_op =~ /\S/) { # we saw an explicit mult op
	die $@; # rethrow
      } else { # mult was by juxtaposition, just pass first factor
        return &$then($comp_head, $factor_len, $after_factor);
      }
    } else { # no mult op
      # just pass the one factor we found
      return &$then($comp_head, $factor_len, $after_factor);
    }
  });
}

# combine representations of numerator and denominator into one object, and
# normalize
# if denominator is undef, just normalize a copy of numerator
sub make_fraction {
  my ($numerator, $denominator) = @_;
  my $fraction = {
    sp => {},
    ap => {},
    is_plural => $numerator->{is_plural}
  };
  # subtract denominator's exponents from numerator's, and normalize
  for my $k (qw(sp ap)) {
    # deep copy from numerator
    for my $base (keys %{$numerator->{$k}}) {
      $fraction->{$k}{$base} = $numerator->{$k}{$base};
    }
    # subtract denominator if we have one
    if (defined($denominator)) {
      for my $base (keys %{$denominator->{$k}}) {
	$fraction->{$k}{$base} -= $denominator->{$k}{$base};
      }
    }
    $fraction->{$k} = normalize_expr_hash($fraction->{$k});
  }
  return $fraction;
}

# call fail() if a fraction component seems like a name rather than a units
# expression
sub fail_if_seems_like_name {
  my ($str, $comp) = @_;
  if ($str =~ /^\p{Lu}\p{Ll}{4,}$/ and # capitalized word 5+ letters long
      scalar(keys %{$comp->{ap}}) > 1 # more than 1 factor
     ) {
    fail();
  }
}

# parse denominator and combine with given numerator
sub finish_parsing_fraction {
  my ($numerator, $numerator_per_len, $after_per, $then) = @_;
  return
    parse_fraction_component($after_per, 'denominator', undef, sub {
      my ($denominator, $denominator_len, $after_denominator) = @_;
      print STDERR (' 'x$btd) . Data::Dumper->Dump([$denominator],['*denominator']) if ($debug);
      fail_if_seems_like_name(substr($after_per, 0, $denominator_len), $denominator);
      my $fraction = make_fraction($numerator, $denominator);
      my $total_len = $numerator_per_len + $denominator_len;
      print STDERR (' 'x$btd) . Data::Dumper->Dump([$fraction],['*fraction']) if ($debug);
      return &$then($fraction, $total_len, $after_denominator);
    });
}

# given a string, try to parse a units expression at the start of the string,
# ignoring context after the expression, and pass an object like this:
# {
#   sp => { base => exp, base => exp, ... }, # separate prefixes
#   ap => { base => exp, base => exp, ... }  # attached prefixes
#   is_plural => bool
# }
# The base=>exp hashes in the sp and ap fields will be normalized.
sub parse_cf_units_expr {
  my ($str, $then) = @_;
  print STDERR (' 'x$btd) . "choosing known or unknown units...\n" if ($debug);
  return choose(
    [
      sub { # first try known units
	$str =~ /^$stoplist_word_re/ and fail();
	return parse_fraction_component($str, 'numerator', undef, sub {
	  my ($numerator, $numerator_len, $after_numerator) = @_;
	  print STDERR (' 'x$btd) . Data::Dumper->Dump([$numerator],['*numerator']) if ($debug);
	  fail_if_seems_like_name(substr($str, 0, $numerator_len), $numerator);
	  if ($after_numerator =~ /^$per_re/) {
	    my ($per_len, $after_per) = ($+[0], $');
	    return finish_parsing_fraction($numerator, $numerator_len + $per_len, $after_per, $then);
	  } else { # no denominator
	    my $fraction = make_fraction($numerator, undef);
	    print STDERR (' 'x$btd) . Data::Dumper->Dump([$fraction],['*fraction']) if ($debug);
	    return &$then($fraction, $numerator_len, $after_numerator);
	  }
	});
      },
      sub { # unknown unit
	# numerator can be a single non-unit word, like "plants", if we
	# definitely have a denominator
	$str =~ /^ ( \p{L}+ ) $per_re /x or fail();
	my ($numerator_str, $numerator_per_len, $after_per) = ($1, $+[0], $');
	my $numerator = +{
	  sp => { $numerator_str => 1 },
	  ap => { $numerator_str => 1 },
	  # assume numerator is plural if it ends in "s"
	  is_plural => (($numerator_str =~ /s$/i) ? 1 : 0)
	};
	print STDERR (' 'x$btd) . Data::Dumper->Dump([$numerator],['*numerator']) if ($debug);
	return finish_parsing_fraction($numerator, $numerator_per_len, $after_per, $then);
      }
    ],
    # try each of the preceding subs in turn
    sub { return &{$_[0]}(); }
  );
}

# try to parse a units expression starting at the given start offset in the
# given text, and return an object like parse_cf_units_expr() does, with
# start/end fields added, or fail. Note that this actually returns the units
# expression representation, rather than passing it to a sub given as an
# argument. But it can still fail().
sub parse_units_expr {
  my ($text, $start) = @_;
  my $after_start = substr($text, $start);
  fail() if ($after_start =~ /^\s/); # no whitespace at start of units
  my $before_start = substr($text, 0, $start);
  my $after_end_re; # substring after end must match this regex,
  # which depends on context before the units expression:
  if ($before_start =~ /^\s*$/) { # the whole text is just units
    $after_end_re = qr/^\s*$/;
  } elsif ($before_start =~ /\(\s*$/) { # units in parens
    $after_end_re = qr/^\s*\)/;
  } elsif (# units after a number (not including plural 10s)
	   $before_start =~ /\d\s+$/ or
	   $before_start =~ /\d0s\s+$/ or
	   ($before_start =~ /\d\z/ and not
	    ($before_start =~ /\d0\z/ and $after_start =~ /^s\s/)
	   ) or
	   # units after the word "in"
	   $before_start =~ / \b [Ii]n \s+ $ /x
	  ) {
    # end not in the middle of a word
    $after_end_re = qr/ ^ (?! ['-]? \w ) /x;
  } else { # no valid preceding context for units expression
    fail();
  }
  return parse_cf_units_expr($after_start, sub {
    my ($expr, $expr_len, $after_end) = @_;
    fail() unless ($after_end =~ $after_end_re);
    $expr->{start} = $start;
    $expr->{end} = $start + $expr_len;
    return $expr;
  });
}

# given the whole input text, return a list of units expression structures
sub find_and_parse_units_exprs {
  my $text = shift;
  my $search_start = 0;
  my $search_end = length($text) - 1;
  my @exprs = ();
  for(;;) {
    my $expr;
    my $start = undef;
    my $success = eval {
      print STDERR (' 'x$btd) . "choosing start...\n" if ($debug);
      ($expr) = choose([$search_start .. $search_end], sub {
	$start = shift;
	return parse_units_expr($text, $start);
      });
      1
    };
    if ($success) {
      print STDERR Data::Dumper->Dump([$expr],['*expr']) if ($debug);
      push @exprs, $expr;
      $search_start = $expr->{end};
    } elsif (failed()) { # no more matches to be found
      last;
    } else { # non-failure exception
      # re-throw with context
      die "error finding units expression starting at $start (" . substr($text, $start, 10) . "...): $@";
    }
  }
  return @exprs;
}

sub tag_units {
  my ($self, $text) = @_;
  my @tags = ();
  for my $units_expr (find_and_parse_units_exprs($text)) {
    my $lex = substr($text, $units_expr->{start}, $units_expr->{end} - $units_expr->{start});
    print STDERR "Processing units expression '$lex':\n" . Data::Dumper->Dump([$units_expr],['*units_expr']) if ($debug);
    eval { # try
    my $tag = +{
      type => 'sense',
      lex => $lex,
      start => $units_expr->{start},
      end => $units_expr->{end}
    };
    $tag->{'penn-pos'} = [$units_expr->{is_plural} ? 'NNS' : 'NN'];
    my $unit_str = expr_hash_to_string($units_expr->{ap});
    # get dimension by adding up dimensions of each unit.
    # also check that no two individual units have the same set of dimension
    # bases (ignore exponents), in order to exclude things like "Miami" as
    # "mebi-are*mile", which involves two different length-based units
    my $dim_expr = {};
    my @dim_base_strs = ();
    for my $unit (keys %{$units_expr->{sp}}) {
      my $exponent = $units_expr->{sp}{$unit};
      my $unit_dim_expr = get_dimension($unit);
      my $dim_base = {};
      for my $dim (keys %$unit_dim_expr) {
	$dim_expr->{$dim} += $unit_dim_expr->{$dim} * $exponent;
	$dim_base->{$dim} = 1;
      }
      my $dim_base_str = expr_hash_to_string($dim_base);
      return 1 # from eval, i.e. go to next match without making a tag for this
        if ($dim_base_str ne '1' and
	    grep { $_ eq $dim_base_str } @dim_base_strs);
      push @dim_base_strs, $dim_base_str;
    }
    normalize_expr_hash($dim_expr);
    # use dimension to look up ONT type
    my $dim_str = expr_hash_to_string($dim_expr);
    $tag->{lftype} =
      [exists($dim2ont{$dim_str}) ? $dim2ont{$dim_str} : $dim2ont{unknown}];
    # special case for ONT::percent
    $tag->{lftype} = ['PERCENT'] if ($tag->{lex} =~ /^(\%|percent)$/i);
    # add DSI
    $tag->{'domain-specific-info'} = {
      domain => 'cwms',
      type => 'units',
      units => $unit_str,
      dimensions => $dim_str
    };
    push @tags, $tag;
    # catch & rethrow with more context
    1} || die "error processing units expression '$lex': $@\n" . Data::Dumper->Dump([$units_expr],['*units_expr']);
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
