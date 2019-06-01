#!/usr/bin/env perl

# input-terms-to-terms-file.pl - convert a list of native tags suitable for the :input-terms tag request argument, to a terms TSV file suitable for the -terms-file command line argument
# Each row of the output consists of a normalized term followed by
# native-format tags without :start or :end, separated by tabs.
# 2015-11-04
# William de Beaumont

use lib "../";
use lib "./Perl";
use KQML::KQML;
use TextTagger::Normalize qw(normalize);

use strict vars;

my %normalized2tags = ();

sub add_tag {
  my $tag = shift;
  my $tag_str = KQML::KQMLAsString($tag);
  my $kw_tag = KQML::KQMLKeywordify($tag);
  die "tag missing :lex: $tag_str"  unless (exists($kw_tag->{':lex'}));
  die ":lex must be a string: $tag_str" unless (KQML::KQMLAtomIsString($kw_tag->{':lex'}));
  my $lex = KQML::KQMLStringAtomAsPerlString($kw_tag->{':lex'});
  my $normalized = normalize($lex);
  die "tabs and newlines not allowed within strings in tags: $tag_str"
    if ($tag_str =~ /[\t\n]/);
  push @{$normalized2tags{$normalized}}, $tag_str;
}

my $exprno = 0;
for(;;) {
  $exprno++;
  my $kqml_text = '';
  my $kqml = KQML::KQMLRead(\*STDIN, \$kqml_text);
  if (ref($kqml)) {
    eval {
      if (ref($kqml->[0])) { # list of tags
	map { add_tag($_) } @$kqml;
      } else { # single tag
	add_tag($kqml);
      }
      1
    } || do {
      die "Error adding tag in expression number $exprno: $@";
    }
  } elsif ($kqml == 0) {
    last;
  } else {
    die "Error reading KQML at line $.: $kqml";
  }
}

for my $normalized (sort keys %normalized2tags) {
  print join("\t", $normalized, @{$normalized2tags{$normalized}}) . "\n";
}

