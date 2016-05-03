#!/usr/bin/perl

package TextTagger::Input;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(load_input_tags tag_input);

use TextTagger::Tags2Trips qw(trips2tagNative);
use Data::Dumper;

my @next_tags = ();

sub load_input_tags {
  @next_tags = map { trips2tagNative($_) } @{$_[0]};
}

sub tag_input {
  return [@next_tags];
}

push @TextTagger::taggers, {
  name => "input",
  tag_function => \&tag_input,
  # can output any type of tag
  output_types => [@TextTagger::all_tag_types],
  input_text => 1
};

1;

