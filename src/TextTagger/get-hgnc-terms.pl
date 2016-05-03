#!/usr/bin/env perl

# get-hgnc-terms.pl - get a terms TSV file from HGNC data

use lib "./Perl";
use TextTagger::Normalize qw(unspell_greek_letters normalize);

use strict vars;

my %word_to_infos = ();

while (<>) {
  chomp;
  my ($id, $symbol, $name, $status, $prev_syms, $prev_names, $synonyms, undef, undef, undef) = split(/\t/);
  next unless ($status eq 'Approved');
  $prev_names =~ s/^"|"$//g;
  # get all the words that are directly in there
  my %common = (id => $id, name => $name);
  my @infos = (
    +{ %common, unnormalized => $symbol, status => 'Approved Symbol' },
    +{ %common, unnormalized => $name, status => 'Approved Name' },
    (map { +{ %common, unnormalized => $_, status => 'Synonym' } }
	 split(/, /, $synonyms)),
    (map { +{ %common, unnormalized => $_, status => 'Previous Symbol' } }
         split(/, /, $prev_syms)),
    (map { +{ %common, unnormalized => $_, status => 'Previous Name' } }
	 split(/", "/, $prev_names))
  );
  # as well as some transformations
  # get rid of stuff in parens in $name
  push @infos, +{ %common, unnormalized => $` . $',
  		  status => 'Approved Name, no parenthetical' }
    if ($name =~ / \([^\(\)]+\)/);
  # it's all greek to me
  my @greek_infos = ();
  for my $l (@infos) {
    my $g = unspell_greek_letters($l->{unnormalized});
    push @greek_infos, +{ %$l, unnormalized => $g }
      unless ($g eq $l->{unnormalized});
  }
  for my $info (@infos, @greek_infos) {
    push @{$word_to_infos{normalize($info->{unnormalized})}{$info->{unnormalized}}}, $info;
  }
}

for my $normalized (sort keys %word_to_infos) {
  next if (length($normalized) == 1); # no single letters please
  print $normalized;
  my $first = 1;
  for my $unnormalized (sort keys %{$word_to_infos{$normalized}}) {
    if ($first) {
      $first = 0;
    } else {
      print "\t,";
    }
    print "\t" . $unnormalized;
    for my $info (@{$word_to_infos{$normalized}{$unnormalized}}) {
      print "\t" . $info->{id} . "\t" . $info->{name} . "\t" . $info->{status};
    }
  }
  print "\n";
}

