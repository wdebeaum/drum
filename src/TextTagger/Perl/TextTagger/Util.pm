#!/usr/bin/perl

package TextTagger::Util;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(match2tag lisp_intern string2w tree_subst structurally_equal remove_duplicates sets_equal union intersection set_difference is_subset asciify_string unasciify_tags word_is_in_trips_lexicon);

use charnames ':full';
use Data::Dumper;

use TextTagger::Escape qw(escape_for_pipes);

use strict vars;

sub match2tag
{
  return (lex => $&, start => $-[0], end => $+[0]);
}

sub lisp_intern
{ # turn the string $name into a lisp symbol in the given package
  my ($name, $package) = @_;
  $name =~ s/[\(\)\"]//g; # the TRIPS Facilitator doesn't like parens or quotes within pipequotes
  if ($name =~ /^[^A-Z\d_-]$/ or ($name =~ /^\d$/ and not $package)) {
    # use Unicode names for single-character symbols we'd pipequote otherwise,
    # since CCL might repeat them back with a backslash instead of pipequotes,
    # which KQML doesn't allow
    $name = charnames::viacode(ord($name));
    $name =~ s/ /_/g;
    $name = lisp_intern($name);
  } elsif ($name =~ /[^A-Z\d_-]/ or ($name =~ /^\d/ and not $package)) {
    $name = "|" . escape_for_pipes($name) . "|";
  }
  if ($package) {
    return $package . "::" . $name;
  } else {
    return $name;
  }
}

sub string2w
{ # turn $str (possibly multiple words) into a W:: symbol
  my $str = uc(shift);
  $str =~ s/\s+/_/g;
  $str =~ s/'/^/g;
  return lisp_intern($str, "W");
}

# Replace all leaves of the given tree matching the given regex with the given
# replacement tree. Inspired by Lisp's subst.
sub tree_subst {
  my ($replacement, $regex, $tree) = @_;
  if (ref($tree) eq 'ARRAY') {
    [map { tree_subst($replacement, $regex, $_) } @$tree]
  } elsif ($tree =~ $regex) {
    $replacement
  } else {
    $tree
  }
}

# Are the two arguments equal, recursively into list and hash refs? Non-refs
# are compared using eq.
sub structurally_equal {
  my ($a, $b) = @_;
  return 0 unless (ref($a) eq ref($b)); # refs of different types
  return ($a eq $b) if (ref($a) eq ''); # just strings
  return 1 if ($a == $b); # refs to the same memory location
  if (ref($a) eq 'ARRAY') { # listrefs
    return 0 unless ($#$a == $#$b); # lists of different sizes
    for my $i (0..$#$a) { # recurse on each element
      return 0 unless (structurally_equal($a->[$i], $b->[$i]));
    }
    return 1;
  } elsif (ref($a) eq 'HASH') {
    return 0 unless (structurally_equal([sort keys %$a], [sort keys %$b])); # hashes with different keys
    for my $k (keys %$a) { # recurse on each value
      return 0 unless (structurally_equal($a->{$k}, $b->{$k}));
    }
    return 1;
  } elsif (ref($a) eq 'CODE') {
    # kind of a hack since we can't really inspect the structure behind a CODE
    # ref
    return ($a == $b);
  } else { # refs, but not to lists or hashes
    die "Unhandled ref type in structurally_equal: " . ref($a);
  }
}

# return a listref containing all the unique (according to structurally_equal)
# elements of the given listref
sub remove_duplicates {
  my $l = shift;
  my @s = ();
  for my $e (@$l) {
    push @s, $e unless (grep { structurally_equal($e, $_) } @s);
  }
  return [@s];
}

# is every element of either listref argument in both of them, according to structurally_equal?
sub sets_equal {
  my ($s1, $s2) = @_;
  die "sets_equal: not an array ref: " . Data::Dumper->Dump([$s1],["*s1"]) unless (ref($s1) eq 'ARRAY');
  die "sets_equal: not an array ref: " . Data::Dumper->Dump([$s2],["*s2"]) unless (ref($s2) eq 'ARRAY');
  return ($#$s1 == $#$s2 and
          not grep { my $s1e = $_; not grep { structurally_equal($_, $s1e) } @$s2 } @$s1);
}

# return a listref containing all the items in the two listref arguments, with
# no duplicates, according to structurally_equal
sub union {
  my ($s1, $s2) = @_;
  die "union: not an array ref: " . Data::Dumper->Dump([$s1],["*s1"]) unless (ref($s1) eq 'ARRAY');
  die "union: not an array ref: " . Data::Dumper->Dump([$s2],["*s2"]) unless (ref($s2) eq 'ARRAY');
  return [
    @$s1,
    grep {
      my $s2e = $_;
      not grep {
        structurally_equal($_, $s2e)
      } @$s1
    } @$s2
  ];
}

# return a listref containing every item that is in both of the two listref
# arguments, according to structurally_equal
sub intersection {
  my ($s1, $s2) = @_;
  die "intersection: not an array ref: " . Data::Dumper->Dump([$s1],["*s1"]) unless (ref($s1) eq 'ARRAY');
  die "intersection: not an array ref: " . Data::Dumper->Dump([$s2],["*s2"]) unless (ref($s2) eq 'ARRAY');
  return [
    grep {
      my $s2e = $_;
      grep {
	structurally_equal($_, $s2e)
      } @$s1
    } @$s2
  ];
}

# return a listref containing every item that is in the first listref argument
# but not in the second, according to structurally_equal
sub set_difference {
  my ($s1, $s2) = @_;
  die "set_difference: not an array ref: " . Data::Dumper->Dump([$s1],["*s1"]) unless (ref($s1) eq 'ARRAY');
  die "set_difference: not an array ref: " . Data::Dumper->Dump([$s2],["*s2"]) unless (ref($s2) eq 'ARRAY');
  return [
    grep {
      my $s1e = $_;
      not grep {
	structurally_equal($s1e, $_)
      } @$s2
    } @$s1
  ];
}

# return true if everything in the first listref argument is also in the second
# one, according to structurally_equal
sub is_subset {
  my ($subset, $superset) = @_;
  for my $sub_elem (@$subset) {
    return 0 unless (grep { structurally_equal($sub_elem, $_) } @$superset);
  }
  return 1;
}

# Turn a UTF-8 string to a pure 7-bit ASCII one by converting other characters
# to their Unicode names with underscores, with a few special cases. Return
# both the converted string and a listref of substitution descriptions so that
# the conversion can be undone later.
sub asciify_string {
  my $str = shift;
  my $substs = [];
  $str =~ s/[^\x00-\x7f]/
    my $subst = { unicode => $&, start => $-[0] };
    push @$substs, $subst;
    my $name = charnames::viacode(ord($&));
    $subst->{ascii} = do {
      if ($name =~ m{^GREEK SMALL LETTER ([A-Z]+)$}) {
	lc($1);
      } elsif ($name =~ m{^GREEK CAPTIAL LETTER ([A-Z]+)$}) {
	$1;
      } elsif ($name =~ m{\b(?:DASH|HYPHEN|MINUS)\b}) {
	'-';
      } elsif ($name =~ m{\bSPACE$} or $name =~ m{^E[NM] QUAD$}) {
	' ';
      } elsif ($name =~ m{\bSINGLE\b.*\bQUOTATION MARK$}) {
	"'";
      } elsif ($name =~ m{\bDOUBLE\b.*\bQUOTATION MARK$}) {
	"\"";
      } else {
	$name =~ s{\s}{_}g;
	$name;
      }
    };
  /ge;
  return ($str, $substs);
}

# Undo the effect of asciify_string on a list of tags derived from the
# asciified string, given also the listref of substitution descriptions.
sub unasciify_tags {
  my ($substs, @old_tags) = @_;
  my @new_tags = @old_tags;
  for my $subst (@$substs) {
    my $ascii_end = $subst->{start} + length($subst->{ascii});
    my $offset = length($subst->{unicode}) - length($subst->{ascii});
    @old_tags = @new_tags;
    @new_tags = ();
    for my $tag (@old_tags) {
      if ($tag->{start} < $ascii_end and $tag->{end} > $subst->{start}) {
	my $substr_offset = $subst->{start} - $tag->{start};
	my $substr_length = length($subst->{ascii});
	if ($substr_offset < 0 or $tag->{end} < $ascii_end) {
	  # probably just a subword or punctuation tag picking up on underscores
	  print STDERR "Warning: dropping tag starting or ending in the middle of a Unicode/ASCII substitution\n" . Data::Dumper->Dump([$tag, $subst],[qw(tag subst)]) . "\n";
	  next;
	}
	substr($tag->{text}, $substr_offset, $substr_length, $subst->{unicode})
	  if (exists($tag->{text}));
	substr($tag->{lex}, $substr_offset, $substr_length, $subst->{unicode})
	  if (exists($tag->{lex}));
      }
      $tag->{start} += $offset if ($tag->{start} >= $ascii_end);
      $tag->{end} += $offset if ($tag->{end} >= $ascii_end);
      push @new_tags, $tag;
    }
  }
  return @new_tags;
}

# caches
my %wiitl = ();
my %wiitl_wf = ();
sub word_is_in_trips_lexicon {
  my ($self, $word, $use_wordfinder) = @_;
  $word = uc($word);
  if ($use_wordfinder and $self->{use_wordfinder}) {
    # TRIPS + WordNet (from WordFinder)
    unless (exists($wiitl_wf{$word})) {
      my $reply_content = $self->send_and_wait(
        ['request', ':receiver', 'lexiconmanager', ':content', ['is-defined-word', lisp_intern($word, 'W')]] # use current *use-wordfinder* setting
      );
      $wiitl_wf{$word} = (($reply_content =~ /(^|:)nil$/i) ? 0 : 1);
      # also cache for subset
      $wiitl{$word} = 0 unless ($wiitl_wf{$word});
    }
    return $wiitl_wf{$word};
  } else {
    # TRIPS lexicon only
    unless (exists($wiitl{$word})) {
      my $reply_content = $self->send_and_wait(
        ['request', ':receiver', 'lexiconmanager', ':content', ['is-defined-word', lisp_intern($word, 'W'), ':use-wordfinder', 'nil']]
      );
      $wiitl{$word} = (($reply_content =~ /(^|:)nil$/i) ? 0 : 1);
      # also cache for superset
      $wiitl_wf{$word} = 1 if ($wiitl{$word});
    }
    return $wiitl{$word};
  }
}

1;
