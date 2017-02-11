package TextTagger::Normalize;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw($dash_re $greek_re unspell_greek_letters normalize uncapitalize characterize_match score_match is_bad_match);

use Data::Dumper;

use strict vars;

my $debug = 0;

our $dash_re = qr/[\x{2010}-\x{2015}\x{2212}-]/;

$Data::Dumper::SortKeys = 1;

my %greek = qw(
  alpha		α
  beta		β
  gamma		γ
  delta		δ
  epsilon	ε
  zeta		ζ
  eta		η
  theta		θ
  iota		ι
  kappa		κ
  lambda	λ
  mu		μ
  nu		ν
  xi		ξ
  omicron	ο
  pi		π
  rho		ρ
  sigma		σ
  tau		τ
  upsilon	υ
  phi		φ
  chi		χ
  psi		ψ
  omega		ω
);
my $greek_alternation = join('|', keys %greek);
my $spelled_greek_re = qr/(?<![a-z])($greek_alternation)(?![a-z])/;

# spelled or unspelled (used in gen-ncit-terms.pl)
our $greek_re = join('|', %greek);

sub unspell_greek_letters {
  my $str = shift;
  $str =~ s/$spelled_greek_re/$greek{$&}/g;
  return $str;
}

# normalize term to be put in the first column of drum-terms.tsv
sub normalize {
  my $str = shift(@_);
  # catch stupid mistakes
  die "Attempted to normalize ref: " . Data::Dumper->Dump([$str],['*str'])
    if (ref($str));
  $str = lc($str);
  $str =~ s/$dash_re//g; # remove all kinds of dash
  $str =~ s/\s+/ /g; # turn all whitespace into ASCII space character
  return $str;
}

# downcase first letter if it's followed by two lowercase letters
sub uncapitalize {
  my $str = shift;
  $str =~ s/^[A-Z](?=[a-z]{2})/lc($&)/e;
  return $str;
}

my @word_cases = qw(sentence-cap single-cap all-caps no-caps initial-cap mixed-caps);
my %word_case_to_index = do {
  my $i = 0;
  map { ($_, $i++) } @word_cases
};

# return one of the @word_cases, characterizing the letter case pattern of the
# given word.
# Assumes the word has only letters in it.
sub characterize_word_case {
  my ($word, $bos) = @_;
       if ($bos and $word =~ /^\p{Lu}\p{Ll}*$/) {
    return 'sentence-cap';
  } elsif ($word =~ /^\p{Lu}$/) {
    # counted separately because ambiguous between all-caps and initial-cap
    return 'single-cap';
  } elsif ($word =~ /^\p{Lu}+$/) {
    return 'all-caps';
  } elsif ($word =~ /^\p{Ll}+$/) {
    return 'no-caps';
  } elsif ($word =~ /^\p{Lu}\p{Ll}+$/) {
    return 'initial-cap';
  } else {
    return 'mixed-caps';
  }
}

# return a hashref describing how strings $a and $b relate to each other.
# Assumes that normalize($a) eq normalize($b). If $bos is true, $a is at the
# beginning of a sentence.
# $b is included in the 'matched' field, so it should be the version from the
# DB, while $a should be the version from the input.
sub characterize_match {
  my ($a, $b, $bos) = @_;
  my $h = {
    type => 'match',
    matched => $b
  };
  print STDERR "characterize_match(\"$a\", \"$b\", $bos)\n" if ($debug);
  # convert all dashes to ASCII; we don't care about those differences
  $a =~ s/$dash_re/-/g;
  $b =~ s/$dash_re/-/g;
  # likewise whitespace sequences
  $a =~ s/\s+/ /g;
  $b =~ s/\s+/ /g;
  # shortcut for exact match; still count words, though
  return +{ %$h, exact => scalar(@{[$a =~ /\pL+/g]}) }
    if ($a eq $b and not $bos);
  # split $a and $b into characters, and scan through them forming words and
  # counting which dashes are deleted from each (don't count dashes they both
  # have)
  my @ac = split(//,$a);
  my @bc = split(//,$b);
  my @a_words = ('');
  my @b_words = ('');
  my $a_dashes_deleted = 0; # # of dashes in a that aren't in b
  my $b_dashes_deleted = 0; # # of dashes in b that aren't in a
  my $prev_char_was_digit = 0;
  my $a_range_dashes_deleted = 0; # # of dashes between digits in a and not b
  my $b_range_dashes_deleted = 0; # # of dashes between digits in b and not a
  while (@ac and @bc) {
    if ($ac[0] eq '-') {
      shift @ac;
      push @a_words, '';
      push @b_words, '';
      # eat following space in a if any
      shift @ac if (@ac and $ac[0] =~ /\s/);
      # eat previous space in b if any
      shift @bc if (@bc and $bc[0] =~ /\s/);
      if ($bc[0] eq '-') {
	shift @bc;
      } else {
	$a_dashes_deleted++;
	$a_range_dashes_deleted++
	  if ($prev_char_was_digit and @ac and $ac[0] =~ /\d/);
      }
      # eat following space in b if any
      shift @bc if (@bc and $bc[0] =~ /\s/);
    } elsif ($bc[0] eq '-') {
      shift @bc;
      push @a_words, '';
      push @b_words, '';
      # eat previous space in a if any
      shift @ac if (@ac and $ac[0] =~ /\s/);
      # eat following space in b if any
      shift @bc if (@bc and $bc[0] =~ /\s/);
      if ($ac[0] eq '-') { # NOTE: this can only happen if we ate a space in a
	shift @ac;
      } else {
	$b_dashes_deleted++;
	$b_range_dashes_deleted++
	  if ($prev_char_was_digit and @bc and $bc[0] =~ /\d/);
      }
      # eat following space in a if any
      shift @ac if (@ac and $ac[0] =~ /\s/);
    } elsif ($ac[0] =~ /\s/) {
      $bc[0] =~ /\s/ or die "whitespace/non-whitespace mismatch: $ac[0] ne $bc[0]";
      shift @ac;
      shift @bc;
      push @a_words, '';
      push @b_words, '';
    } elsif ($bc[0] =~ /\s/) {
      die "non-whitespace/whitespace mismatch: $ac[0] ne $bc[0]";
    } elsif ($ac[0] =~ /\PL/) {
      $ac[0] eq $bc[0] or die "non-letter, non-dash mismatch: $ac[0] ne $bc[0]";
      my $c = shift @ac;
      shift @bc;
      if ($c =~ /\s/) { # FIXME? I think this case is already handled above
	push @a_words, '';
	push @b_words, '';
      } elsif ($a_words[-1] =~ /\pL$/) {
	push @a_words, $c;
	push @b_words, $c;
      } else {
	$a_words[-1] .= $c;
	$b_words[-1] .= $c;
      }
      $prev_char_was_digit = ($c =~ /\d/);
    } else {
      my ($ac, $bc) = (shift(@ac), shift(@bc));
      lc($ac) eq lc($bc) or die "letter mismatch: $ac ne $bc (lc'd)";
      if ($a_words[-1] =~ /\PL$/) {
	push @a_words, $ac;
	push @b_words, $bc;
      } else {
	$a_words[-1] .= $ac;
	$b_words[-1] .= $bc;
      }
      $prev_char_was_digit = 0;
    }
  }
  die "junk after a: " . join('', @ac)
    if (grep { $_ ne '-' and $_ ne ' ' } @ac);
  $a_dashes_deleted += scalar(grep { $_ eq '-' } @ac);
  die "junk after b: " . join('', @bc)
    if (grep { $_ ne '-' and $_ ne ' ' } @bc);
  $b_dashes_deleted += scalar(grep { $_ eq '-' } @bc);
  print STDERR "a words = " . join(' ', @a_words) . "\n" .
               "b words = " . join(' ', @b_words) . "\n"
    if ($debug);
  # count word pairs that are exact matches, and each different pairing of word
  # cases in matrix @m
  # Note: $exact is not the same as what the sum of the diagonal of this matrix
  # would be if we weren't counting $exact separately, because words can be
  # 'mixed-caps' in different ways.
  my $exact = 0;
  my @m = map { [(0)x(scalar(@word_cases))] } @word_cases;
  my $first = 1;
  while (@a_words) { # @b_words same length by construction
    my ($aw, $bw) = (shift(@a_words), shift(@b_words));
    my $first_bos = ($first and $bos);
    $first = 0;
    next unless ($aw =~ /\pL/); # only count words composed of letters
    if ($aw eq $bw and not $first_bos) {
      $exact++;
    } else {
      my $awc = characterize_word_case($aw, $first_bos);
      my $bwc = characterize_word_case($bw, 0);
      if ($awc eq $bwc and $aw eq $bw) {
	$exact++;
      } else {
        $m[$word_case_to_index{$awc}][$word_case_to_index{$bwc}]++;
      }
    }
  }
  print STDERR "exact = $exact\nm =\n" .
  	       join("\n", map { join(',', @$_) } @m) . "\n"
    if ($debug);
  # turn the non-zero cells in @m into descriptive entries in a hashref
  for my $i (0..$#word_cases) {
    for my $j (0..$#word_cases) {
      if ($m[$i][$j] > 0) {
	my $key = $word_cases[$i] . '-' . $word_cases[$j];
	$h->{$key} = $m[$i][$j];
      }
    }
  }
  # likewise for other non-zero counts
  $h->{exact} = $exact if ($exact > 0);
  $h->{'dash-no-dash'} = $a_dashes_deleted if ($a_dashes_deleted > 0);
  $h->{'no-dash-dash'} = $b_dashes_deleted if ($b_dashes_deleted > 0);
  # note: these aren't scored, they're just used to rule out certain matches
  $h->{'range-dash-no-dash'} = $a_range_dashes_deleted
    if ($a_range_dashes_deleted > 0);
  $h->{'no-dash-range-dash'} = $b_range_dashes_deleted
    if ($b_range_dashes_deleted > 0);
  return $h;
}

# given a match structure as returned by characterize_match, return a score
# between 0 and 1, where 1 is an exact match, and 0 is the worst kind of match
sub score_match {
  my ($m) = @_;
  print STDERR "Normalize::score_match(" . Data::Dumper->Dump([$m],['*m']) . ")\n"
    if ($debug);
  
  # penalize dash mismatches
  my $dash = 2 - scalar(grep /dash/, keys %$m);
  
  # reward having any exact matches
  my $exact = (exists($m->{exact}) ? 1 : 0);
  
  my @cap_keys = grep /-cap/, keys %$m;
  
  # Award these points for these situations:
  # 2 - only exact matches or sentence-cap-no-caps
  # 1 - some all-caps-initial-cap (which is particularly common), but no other
  #     inexact matches
  # 0 - other inexact matches
  my $acic;
  if (@cap_keys == 0 or
      (@cap_keys == 1 and $cap_keys[0] eq 'sentence-cap-no-caps')) {
    $acic = 2;
  } elsif (@cap_keys == 1 and $cap_keys[0] eq 'all-caps-initial-cap') {
    $acic = 1;
  } else {
    $acic = 0;
  }
  
  # penalize mixed-caps mismatches:
  # 0 - mixed-caps-mixed-caps
  # 1 - other mixed-caps
  # 2 - no mixed-caps
  my $mixed;
  if (exists($m->{'mixed-caps-mixed-caps'})) {
    $mixed = 0;
  } elsif (grep /mixed-caps/, @cap_keys) {
    $mixed = 1;
  } else {
    $mixed = 2;
  }
  
  # penalize combinations of different case categories on each side
  my %as;
  my %bs;
  for (@cap_keys) {
    my ($a, undef, $b, undef) = split(/-/);
    $as{$a} = 1;
    $bs{$b} = 1;
  }
  my $a_combo = 4 - keys %as;
  my $b_combo = 4 - keys %bs;
  # ... but correct for single-cap matching either all-caps or initial-cap
  $a_combo++
    if (exists($as{single}) and (exists($as{all}) or exists($as{initial})));
  $b_combo++
    if (exists($bs{single}) and (exists($bs{all}) or exists($bs{initial})));
  # ... and correct for sentence-cap matching single, initial, or no-caps, or
  # no other inexact matches besides sentence-cap-no-caps at all
  $a_combo++
    if (exists($as{sentence}) and
        ((keys %as == 1 and exists($m->{'sentence-cap-no-caps'})) or
	 exists($as{single}) or exists($as{initial}) or exists($as{no})));
  # (sentence-cap can't happen for b)
  # ... and take the better of the two scores
  my $combo = ($a_combo > $b_combo ? $a_combo : $b_combo);
  
  # penalize all-caps<->no-caps for short match
  my $short_abbr = ((length($m->{matched}) <= 3 and
		     (exists($m->{'all-caps-no-caps'}) or
		      exists($m->{'no-caps-all-caps'}))
		    ) ? 0 : 1);
  
  print STDERR Data::Dumper->Dump([$short_abbr, $mixed, $exact, $acic, $a_combo, $b_combo, $combo, $dash],[qw(short_abbr mixed exact acic a_combo b_combo combo dash)])
    if ($debug);
  my $final_score = (((((((0
    # in order from most to least important (the literal number on each line
    # is the number of possible values for the variable on that line):
    ) * 2 + $short_abbr
    ) * 3 + $mixed
    ) * 2 + $exact
    ) * 3 + $acic
    ) * 5 + $combo
    ) * 3 + $dash
    ) / 539.0; # product of the coefficients -1
  print STDERR "Normalize::score_match returning $final_score\n"
    if ($debug);
  return $final_score;
};

# if we get $lex/$start/$end back from terms2.cpp when we're tagging input
# $str, should we discard it on general principle?
sub is_bad_match {
  my ($str, $lex, $start, $end) = @_;
  return (
      # don't tag a span with a space in it if either end is inside a word
      # e.g. don't tag "T cell" in "HEK293T cell"
      ($lex =~ /\s/ and (
	($start > 0 and substr($str, $start-1, 1) =~ /\w/) or
	($end < length($str) and substr($str, $end, 1) =~ /\w/)
      ))
    or
      # don't tag a span with an end that is a dash inside a word
      (($lex =~ /^$dash_re/ and
	$start > 0 and substr($str, $start-1, 1) =~ /\w|$dash_re|[\)\]\}]/) or
       ($lex =~ /$dash_re$/ and
	$end < length($str) and substr($str, $end, 1) =~ /\w|$dash_re|[\(\[\{]/
	))
  );
}

1;

