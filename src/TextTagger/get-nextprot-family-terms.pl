#!/usr/bin/env perl

# get-nextprot-family-terms.pl - get a terms TSV file from neXtProt cv_family.txt

use lib "./Perl";
use TextTagger::Normalize qw(normalize uncapitalize);

use strict vars;

my %ac_to_name = ();
my %term_to_acs = ();

my $current_name = undef;

$/ = "\r\n";

# discard header
while (<>) {
  chomp;
  last if (/^_+$/);
}

while (<>) {
  chomp;
  /^(..)(?: [\. ] |$)/ or die "malformed line: $_";
  my ($code, $line) = ($1, $');
  if ($code eq 'ID') {
    $current_name = uncapitalize($line);
    $current_name =~ s/\.$//;
  } elsif ($code eq 'AC') {
    die "AC without ID! $line" unless (defined($current_name));
    my $ac = $line;
    $ac =~ s/^FA-/FA:/;
    $ac_to_name{$ac} = $current_name;
    push @{$term_to_acs{normalize($current_name)}{$current_name}}, $ac;
    $current_name =~ s/ ((sub-)?sub|super)?family\.?$//g or die "name doesn't end in 'family': '$current_name'";
    push @{$term_to_acs{normalize($current_name)}{$current_name}}, $ac;
  } elsif ($code eq '//') {
    undef $current_name;
  } # ignore SY, DR, HI, for now
}

for my $normalized (sort keys %term_to_acs) {
  print $normalized;
  my $first = 1;
  for my $unnormalized (sort keys %{$term_to_acs{$normalized}}) {
    if ($first) {
      $first = 0;
    } else {
      print "\t,";
    }
    print "\t$unnormalized";
    for my $ac (@{$term_to_acs{$normalized}{$unnormalized}}) {
      print "\t$ac\t$ac_to_name{$ac}\tID";
    }
  }
  print "\n";
}

