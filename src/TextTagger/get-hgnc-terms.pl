#!/usr/bin/env perl

# get-hgnc-terms.pl - get a terms TSV file from HGNC data

use lib "./Perl";
use TextTagger::Normalize qw(unspell_greek_letters normalize);

use strict vars;

my %word_to_infos = ();

# HACK: We know the Makefile gives us the genes first, then the RNAs. Both have
# heading rows at the beginning that we otherwise ignore, so when we see a
# heading row ($status is 'status'), we go to the next type.
my @tps = (' bogus', ' G', ' R');

while (<>) {
  chomp;
  my ($id, $symbol, $name, undef, undef, $status, undef, undef, $alias_syms, $alias_names, $prev_syms, $prev_names) # many more unused fields
    = split(/\t/);
  shift @tps if ($status eq 'status');
  next unless ($status eq 'Approved');
  $id .= $tps[0];
  $alias_syms =~ s/^"|"$//g;
  $alias_names =~ s/^"|"$//g;
  $prev_syms =~ s/^"|"$//g;
  $prev_names =~ s/^"|"$//g;
  # get all the words that are directly in there
  my %common = (id => $id, name => $name);
  my @infos = (
    +{ %common, unnormalized => $symbol, status => 'Approved Symbol' },
    +{ %common, unnormalized => $name, status => 'Approved Name' },
    (map { +{ %common, unnormalized => $_, status => 'Alias Symbol' } }
	 split(/\s*\|\s*/, $alias_syms)),
    (map { +{ %common, unnormalized => $_, status => 'Alias Name' } }
	 split(/\s*\|\s*/, $alias_names)),
    (map { +{ %common, unnormalized => $_, status => 'Previous Symbol' } }
         split(/\s*\|\s*/, $prev_syms)),
    (map { +{ %common, unnormalized => $_, status => 'Previous Name' } }
	 split(/\s*\|\s*/, $prev_names))
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

