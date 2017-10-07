#!/usr/bin/env perl

# get-uniprot-terms.pl - get a terms TSV file from UniProt data (.dat)

# See here for input format:
# http://web.expasy.org/docs/userman.html

use lib "./Perl";
use TextTagger::Normalize qw(unspell_greek_letters normalize uncapitalize);
do "./uniprot-species.pl" or die;
our @species;

use strict vars;

my @name_subfields = qw(Full Short INN); # not EC, Allergen, or CD_antigen (and Biotech is a lie, it never appears)
my $accession_number = undef;
my $prev_field = undef;
my %name_to_infos = ();
my %ac_to_recname = ();

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
  } elsif ($code eq 'DE') { # names ("DEscription")
    defined($accession_number) or die "no accession number for names";
    my ($prefix, $indent, $field, $suffix, $subfield, $value) = 
      ($line =~ /^
        ( (\s\s)?
	  ( \w{3} Name: |
	    \s{8} |
	    Includes: |
	    Contains: |
	    Flags: (?: \s (?: Precursor | Fragments? ) ; )+
	  )
	)
	( \s (\w+) = (.+) ; )?
      $/x)
      or die "malformed DE line: $_";
    $value =~ s/\s*{[\w:\|]+}$//; # strip curly-braced xrefs from name
    $prev_field = $field unless ($field =~ /^\s{8}$/);
    if ($suffix and (not $indent) and $field =~ /^\s{8}$|Name:$/ and
        grep { $_ eq $subfield } @name_subfields) {
      $ac_to_recname{$accession_number} = $value
        if ($field eq 'RecName:' and $subfield eq 'Full');
      $value = uncapitalize($value);
      push @{$name_to_infos{normalize($value)}{$value}},
           { ac => $accession_number, status => "$prev_field $subfield" };
      my $greek = unspell_greek_letters($value);
      push @{$name_to_infos{normalize($greek)}{$greek}},
           { ac => $accession_number, status => "$prev_field $subfield" }
        if ($greek ne $value);
    }
  } elsif ($code =~ /^O[SH]$/) {
    defined($accession_number) or
      die "no accession number for " . ($code eq 'OH' ? 'host ' : '') .
          "species";
    for my $s (@species) {
      $s->{acs}{$accession_number} = 1
        if (exists($s->{$code}) and $line =~ $s->{$code});
    }
  }
}

for my $normalized (sort keys %name_to_infos) {
  next if (length($normalized) == 1); # no single letters please
  print $normalized;
  my $first = 1;
  for my $unnormalized (sort keys %{$name_to_infos{$normalized}}) {
    my @infos = @{$name_to_infos{$normalized}{$unnormalized}};
    if (@infos > 5) { # some names are in lots of entries
      # try getting just the species-relevant genes
      my @species_infoses = ();
      my $num_species_infos = 0;
      for my $s (@species) {
	my $i = [grep { exists($s->{acs}{$_->{ac}}) } @infos];
	$num_species_infos += scalar(@$i);
	push @species_infoses, $i;
      }
      if ($num_species_infos == 0) { # none are species we're interested in
	# just keep the first one and add 'etc' to indicate more exist
	@infos = ($infos[0], { ac => 'UP:etc', status => 'fake' });
      } elsif ($num_species_infos > 5) { # still too many
	# just keep the first of each and add 'etc' to indicate more exist
	@infos = map { (@$_ > 0 ? $_->[0] : ()) } @species_infoses;
	push @infos, { ac => 'UP:etc', status => 'fake' };
      } else { # species ones are just right
	@infos = map { @$_ } @species_infoses;
      }
    }
    if ($first) {
      $first = 0;
    } else {
      print "\t,";
    }
    print "\t$unnormalized";
    for my $info (@infos) {
      print "\t$info->{ac}";
      for my $s (@species) {
        if (exists($s->{acs}{$info->{ac}})) {
	  print ' ' . $s->{tp};
	  last; # FIXME is this the right thing to do?
	}
      }
      print "\t$ac_to_recname{$info->{ac}}\t$info->{status}";
    }
  }
  print "\n";
}

