#!/usr/bin/env perl

# get-uniprot-dbxrefs.pl - get a TSV file from UniProt data (.dat) for its database cross-references

# See here for input format:
# http://web.expasy.org/docs/userman.html

use strict vars;
do "./uniprot-species.pl" or die;
our @species;

my @name_subfields = qw(Full Short INN); # not EC, Allergen, or CD_antigen (and Biotech is a lie, it never appears)
my $accession_number = undef;
my %dbxrefs = ();
my %ac_to_species = ();

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
  } elsif ($code =~ /^O[SH]$/) {
    defined($accession_number) or
      die "no accession number for " . ($code eq 'OH' ? 'host ' : '') .
          "species";
    unless (exists($ac_to_species{$accession_number})) {
      for my $s (@species) {
	$ac_to_species{$accession_number} = $s
	  if (exists($s->{$code}) and $line =~ $s->{$code});
      }
    } # FIXME this just takes the first species for this ID, should I do something smarter for multiple species?
  }
}

for (sort keys %dbxrefs) {
  # separate UP IDs from others
  my (@up, @not_up);
  for (@{$dbxrefs{$_}}) {
    if (/^UP:/) {
      push @up, $_;
    } else {
      push @not_up, $_;
    }
  }
  # make sure we don't use too many UP IDs
  # (see also similar code in get-uniprot-terms.pl)
  if (@up > 5) { # too many UP IDs
    my @new_up = ();
    # try getting just the species-relevant ones
    @new_up = grep { exists($ac_to_species{$_}) } @up;
    if (@new_up == 0) { # none are species we're interested in
      # just keep the first one and add 'etc' to indicate more exist
      @up = ($up[0], 'UP:etc');
    } elsif (@new_up > 5) { # still too many
      # just keep the first of each species and add 'etc' to indicate more exist
      @up = ();
      for (@new_up) {
	my $s = $ac_to_species{$_};
	push @up, $_ unless (grep { $s == $ac_to_species{$_} } @up);
      }
      push @up, 'UP:etc';
    } else { # species ones are just right
      @up = @new_up;
    }
  }
  print join("\t", $_, @up, @not_up) . "\n";
}

