package Graphviz::Escape;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(escape_for_quotes escape_for_dot);

use strict vars;

sub escape_for_quotes { 
  my $str = shift;
  $str =~ s/\\/\\\\/g;
  $str =~ s/"/\\"/g;
  return $str;
}

my %justify2nl = ("center" => "\\n", "left" => "\\l", "right" => "\\r");

sub escape_for_dot {
  my ($str, $justify) = @_;
  return $str unless ($str =~ /\W|^\d/); # return if valid ID, no quotes needed
  return $str if ($str =~ /^-?(?:\.\d+|\d+(?:\.\d*)?)$/); # numbers are IDs too
  $justify ||= "left";
  my $nl = $justify2nl{$justify};
  $str =~ s/\\/\\\\/g;
  $str =~ s/"/\\"/g;
  $str =~ s/\r\n|\n\r|\r|\n/$nl/g;
  return "\"$str\"";
}

1;

