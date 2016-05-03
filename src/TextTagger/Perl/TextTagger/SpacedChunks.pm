#!/usr/bin/perl

package TextTagger::SpacedChunks;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(tag_spaced_chunks);

use TextTagger::Util qw(match2tag);

use strict vars;

# kind of a hack to make plot's select-appt dialogue work
my $space_chars = "\\s_~=-";

sub tag_spaced_chunks
{ # get chunks of text separated by multiple spaces
  my $self = shift;
  my $str = shift;
  my @chunks = ();
  while ($str =~ /[^$space_chars]+(?:[$space_chars][^$space_chars]+)*/g)
  {
    push @chunks, { type => "spaced-chunk",
                    match2tag()
                    };
  }
  return [@chunks];
}

push @TextTagger::taggers, {
  name => "spaced_chunks",
  tag_function => \&tag_spaced_chunks,
  output_types => ['spaced-chunk'],
  input_text => 1
};

1;
