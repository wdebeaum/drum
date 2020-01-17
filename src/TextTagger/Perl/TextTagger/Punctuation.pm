#!/usr/bin/perl

package TextTagger::Punctuation;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(tag_punctuation);

use TextTagger::Util qw(match2tag);

use strict vars;

sub tag_punctuation
{
  my $self = shift;
  my $original_text = shift;
  my @words = ();
  while ($original_text =~ /[^\p{L}\d\s]/gi) # anything that isn't a number, a letter or a space must be some kind of punctuation
  { # WARNING: since this detects ' as punctuation, and Words.pm also detects ' as part of contractions, it's possible using words, punctuation, and fill-gaps will have bad results.
    push @words, { type => "punctuation", match2tag() };
  }
  return [@words];
}

push @TextTagger::taggers, {
  name => "punctuation",
  tag_function => \&tag_punctuation,
  output_types => ['punctuation'],
  input_text => 1
};

1;
