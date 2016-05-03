#!/usr/bin/perl

use List::Util qw(sum);
use strict vars;

# Return a version of $tagged_gloss with certain common transformations
# applied, followed by a list of indices into $tagged_gloss where characters
# have been deleted. These deletions apply cumulatively, so later ones refer to
# the string resulting from applying earlier ones.
sub get_deletions {
  my ($tagged_gloss, $wn_gloss) = @_;
  my @deletions = ();

  # delete certain spaces around punctuation
  while ($tagged_gloss =~ s/ (?=[\)\?])|(?<=\)) (?=\W)//) {
    push @deletions, $-[0];
  }

  # delete space before ; or , only if $wn_gloss doesn't have it
  unless ($wn_gloss =~ / [;,]/) {
    while ($tagged_gloss =~ s/ (?=[;,])//) {
      push @deletions, $-[0];
    }
  }

  # convert initial parens to colon if that's how it is in $wn_gloss
  push @deletions, 0
    if ($wn_gloss =~ /^.*?(?=:)/ and $tagged_gloss =~ s/^\((\Q$&\E)\) /$1: /);

  return $tagged_gloss, @deletions;
}

# Automatically align $tagged_gloss and $wn_gloss. Return a list of length
# length($tagged_gloss)+1 of numbers, where the i-th number is a number of
# characters to insert before the i-th character of $tagged_gloss to make it
# correspond to $wn_gloss. The list is chosen to minimize the number of
# positions at which characters are inserted. Letters are case-insensitive, and
# spaces in $tagged_gloss are allowed to match the character class [ "_`] in
# $wn_gloss.
sub get_insertions {
  my ($tagged_gloss, $wn_gloss) = @_;
  return (0)x(length($tagged_gloss)+1) if ($tagged_gloss eq $wn_gloss);
  if (length($tagged_gloss) == length($wn_gloss)) {
    my ($tg, $wg) = ($tagged_gloss, $wn_gloss);
    $tg =~ s/["_`]/ /g;
    $wg =~ s/["_`]/ /g;
    die "tagged and wn glosses are of equal length but not equal value"
      if ($tg ne $wg);
  }
  die "tagged gloss is longer than wn gloss"
    if (length($tagged_gloss) > length($wn_gloss));
  my @tagged_chars = map { lc($_) } split(//, $tagged_gloss);
  my @wn_chars = map { lc($_) } split(//, $wn_gloss);
  # A* search (roughly speaking)
  my @open_set = ([]);
  my %g = ($open_set[0] => 0);
  # h is always 1, except when we reach the end, when it's 0
  while (@open_set) {
    @open_set = sort { $g{$a} <=> $g{$b} } @open_set;
    my $current = shift(@open_set);
    my $tagged_index = scalar(@$current);
    my $wn_index = $tagged_index + sum(@$current);
    # add 0s while the strings match up
    while ($tagged_index < @tagged_chars and
           $wn_index < @wn_chars and
	   ($tagged_chars[$tagged_index] eq $wn_chars[$wn_index] or
	    # allow ' ' to match certain frequent substitutions
	    $tagged_chars[$tagged_index] eq ' ' and
	    $wn_chars[$wn_index] =~ /["_`]/
           )) {
      push @$current, 0;
      $tagged_index++;
      $wn_index++;
    }
    if ($tagged_index == @tagged_chars) { # we're at the end of the tagged gloss
      # return current, with whatever remains in WN tacked onto the end
      return (@$current, (scalar(@wn_chars) - $wn_index));
    }
    # add characters to tagged gloss until there's no room left, and whenever
    # we find a matching character in WN, add a hypothesis to the open set
    my $num_inserted = 0;
    while ($wn_index < @wn_chars and
           @tagged_chars - $tagged_index <= @wn_chars - $wn_index) {
      $wn_index++;
      $num_inserted++;
      if ($tagged_chars[$tagged_index] eq $wn_chars[$wn_index]) {
	my $hyp = [@$current, $num_inserted];
	$g{$hyp} = $g{$current} + 1;
	push @open_set, $hyp;
      }
    }
  }
  die "no match";
}

1;
