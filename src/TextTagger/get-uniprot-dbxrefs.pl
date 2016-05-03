#!/usr/bin/env perl

# get-uniprot-dbxrefs.pl - get a TSV file from UniProt data (.dat) for its database cross-references

# See here for input format:
# http://web.expasy.org/docs/userman.html

use strict vars;

my @name_subfields = qw(Full Short INN); # not EC, Allergen, or CD_antigen (and Biotech is a lie, it never appears)
my $accession_number = undef;
my %dbxrefs = ();

while (<>) {
  chomp;
  /^(..)(?:   |$)/ or die "malformed line: $_";
  my ($code, $line) = ($1, $');
  if ($code eq 'ID') { # start of entry ( (non-stable) "IDentification")
    $accession_number = undef;
  } elsif ($code eq 'AC') { # stable, unique ID ("ACcession number")
    unless (defined($accession_number)) {
      $line =~ s/;.*//; # remove extra ACs
      $accession_number = "UP:$line";
    }
  } elsif ($code eq 'DR') { # Database cross-Reference
    defined($accession_number) or die "no accession number for dbxref";
    $line =~ s/\.(?: \[[^\]]+\])?$//;
    my ($resource, $id, @optional_info) = split(/; /, $line);
    # only get references to certain resources
    # in particular, don't get GO because they link to excessive numbers of
    # things in there that are only tangentially related
    if ($resource eq 'HGNC') {
      push @{$dbxrefs{$id}}, $accession_number;
      push @{$dbxrefs{$accession_number}}, $id;
    } elsif ($resource eq 'Pfam') {
      $id = "XFAM:$id";
      push @{$dbxrefs{$id}}, $accession_number;
      push @{$dbxrefs{$accession_number}}, $id;
    }
  }
}

for (sort keys %dbxrefs) {
  print join("\t", $_, @{$dbxrefs{$_}}) . "\n";
}

