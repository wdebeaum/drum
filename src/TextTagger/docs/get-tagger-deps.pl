#!/usr/bin/perl
use lib "../Perl";
use lib "../../";
use TextTagger::Config;
print <<EOH;
digraph G {
  rankdir="LR"
  node [shape="none",fontsize=10]
EOH
my %types;
for (@TextTagger::taggers) {
  my $tagger=$_->{name};
  print qq(  "tagger $tagger" [fontcolor="firebrick",label="$tagger"]\n);
  if ($_->{input_text}) {
    print qq(  "input text" -> "tagger $tagger"\n);
  }
  for (@{$_->{input_types}}) {
    $types{$_} = 1;
    print qq(  "type $_" -> "tagger $tagger"\n);
  }
  for (@{$_->{output_types}}) {
    $types{$_} = 1;
    print qq(  "tagger $tagger" -> "type $_"\n);
  }
  for (@{$_->{optional_input_types}}) {
    $types{$_} = 1;
    print qq(  "type $_" -> "tagger $tagger" [color="grey60",weight=0.1]\n);
  }
}
for (keys %types) {
  print qq(  "type $_" [fontcolor="navy",label="$_"]\n);
}
print <<EOF;
  subgraph cluster_legend {
    "Legend:" [fontsize=14]
    "tag type" [fontcolor="navy"]
    indent [style="invis"]
    indent -> "tag type" [style="invis"]
    tagger [fontcolor="firebrick"]
    "required input" [fontcolor="navy"]
    "optional input" [fontcolor="navy"]
    output [fontcolor="navy"]
    "required input" -> tagger
    tagger -> output
    "optional input" -> tagger [color="grey60"]
  }
}
EOF
