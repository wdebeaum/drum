#!/usr/bin/env perl

# get-uniprot-subcell-terms.pl - get a terms TSV file from UniProt subcell.txt

use lib "./Perl";
use TextTagger::Normalize qw(normalize uncapitalize);

use strict vars;

my %ac_to_name = ();
my %ac_to_type = ();
my %term_to_infos = ();

my $current_ac = undef;
my $current_type = undef;
my $current_name = undef;
my @current_terms = ();

# discard header
while (<>) {
  chomp;
  last if (/^_+$/);
}

while (<>) {
  chomp;
  last if (/^-+$/); # discard footer
  /^(..)(?:   |$)/ or die "malformed line: $_";
  my ($code, $line) = ($1, $');
  if ($code =~ /^I([DTO])$/) { # start of entry, name
    $current_type = $1;
    $current_name = $line;
    $current_name =~ s/\.$//;
    push @current_terms,
         { unnormalized => uncapitalize($current_name),
	   status => 'I*' };
  } elsif ($code eq '//') { # end of entry
    if (@current_terms and defined($current_ac)) {
      for (@current_terms) {
	my $unnormalized = $_->{unnormalized};
	my $normalized = normalize($unnormalized);
	push @{$term_to_infos{$normalized}{$unnormalized}},
	     { ac => $current_ac, status => $_->{status} };
      }
      $ac_to_name{$current_ac} = $current_name if (defined($current_name));
      $ac_to_type{$current_ac} = $current_type if (defined($current_type));
    }
    $current_ac = undef;
    $current_type = undef;
    $current_name = undef;
    @current_terms = ();
  } elsif ($code eq 'AC') { # ACession number
    $current_ac = $line;
  } elsif ($code eq 'SY') { # SYnonyms
    $line =~ s/[;\.]$//;
    push @current_terms,
         map { { unnormalized => uncapitalize($_), status => 'SY' } }
	     split(/; /, $line);
  }
}

for my $normalized (sort keys %term_to_infos) {
  print $normalized;
  my $first = 1;
  for my $unnormalized (sort keys %{$term_to_infos{$normalized}}) {
    if ($first) {
      $first = 0;
    } else {
      print "\t,";
    }
    print "\t$unnormalized";
    for my $info (@{$term_to_infos{$normalized}{$unnormalized}}) {
      my $ac = $info->{ac};
      print "\tUP:$ac $ac_to_type{$ac}\t$ac_to_name{$ac}\t$info->{status}"
    }
  }
  print "\n";
}

