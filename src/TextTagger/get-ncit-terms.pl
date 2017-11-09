#!/usr/bin/env perl

# get-ncit-terms.pl - get a terms TSV file (and DSL hierarchy files) from NCI Thesaurus flat zipped data (.FLAT.zip)

# See here for input format (and other formats):
# http://evs.nci.nih.gov/ftp1/NCI_Thesaurus/ReadMe.txt

use lib "./Perl";
use TextTagger::Util qw(lisp_intern remove_duplicates);
use TextTagger::Normalize qw(unspell_greek_letters normalize uncapitalize $dash_re $greek_re);
use File::Path qw(make_path);

use strict vars;

my %normalized_to_unnormalized_to_entries = ();
my %id_to_name = ();
my %id_to_parents = ();
my %id_to_synonyms = ();
my %deleted_ids = ();

sub uncapitalize_each_word {
  my $str = shift;
  return join('', map { uncapitalize($_) } split(/((?:\s|$dash_re)+)/, $str));
}

sub add_entries {
  my ($term, $code, $concept_name, $status) = @_;
  $code =~ s/^C//;
  push @{$normalized_to_unnormalized_to_entries{normalize($term)}{$term}},
       [$code, $concept_name, $status];
  my $greek = unspell_greek_letters($term);
  push @{$normalized_to_unnormalized_to_entries{normalize($greek)}{$greek}},
       [$code, $concept_name, $status]
    if ($greek ne $term);
}

#
# read input
#

open(FLAT, '-|', 'unzip', '-p', $ARGV[0])
  or die "Can't unzip file '$ARGV[0]': $!";

while (my $line = <FLAT>) {
  chomp $line;
  my ($code, $concept_name, $parents, $synonyms, undef) # ignore definition
    = split(/\t/, $line);
  $concept_name =~ s/_/ /g;
  $concept_name =~ s/^\s+|\s+$//g;
  $concept_name =~ s/\s+/ /g;
  $concept_name = uncapitalize_each_word($concept_name);
  $id_to_name{$code} = $concept_name;
  if ($parents ne '') {
    my @parents = grep { $_ ne 'root_node' } split(/\|/, $parents);
    $id_to_parents{$code} = [@parents] if (@parents);
  }
  $id_to_synonyms{$code} = $synonyms;
}

close FLAT
  or die "failed to close unzip pipe: exit status = $?, C library error = $!";

#
# Remove "FOO Gene" parents of "FOO Bar Allele" from table entries
# (mostly "FOO wt Allele", but there are a few others)
# and remove the "(wt) Allele" part of the name.
#

for my $id (keys %id_to_name) {
  my $name = $id_to_name{$id};
  if ($name =~ /(?:$dash_re| )\w+ allele$/ and exists($id_to_parents{$id})) {
    my $prefix = $`;
    my @gene_parents =
      grep { $id_to_name{$_} eq $prefix . ' gene' } @{$id_to_parents{$id}};
    if (@gene_parents) {
      for (@gene_parents) { $deleted_ids{$_} = 1; }
      $id_to_name{$id} =~ s/(?: wt)? allele$//;
    }
  }
}

#
# construct table entries
#

for my $code (keys %id_to_name) {
  next if (exists($deleted_ids{$code}));
  my $concept_name = $id_to_name{$code};
  my $synonyms = $id_to_synonyms{$code};
  add_entries($concept_name, $code, $concept_name, 'name');
  my @synonyms = split(/\|/, $synonyms);
  for my $synonym (@synonyms) {
    # no plurals, please
    # FIXME: doesn't catch irregular plurals
    next if ($synonym =~ /s$/ and
	     grep { lc($_) eq lc($`) } ($concept_name, @synonyms));
    $synonym = uncapitalize_each_word($synonym);
    next if ($synonym =~ /^\Q$concept_name\E(?:(?: wt)? allele)?$/);
    add_entries($synonym, $code, $concept_name, 'synonym');
  }
}

#
# Add extra entries based on other entries.
#

# add entries first to this table, so we don't mistake entries we added earlier
# this way for entries already in the original table
my %new_n2un2e = ();

for my $normalized (keys %normalized_to_unnormalized_to_entries) {
  # Add entries for "FOO" derived from "FOO gene" or "FOO protein" where "FOO"
  # looks like an abbreviation and doesn't already have an entry.
  if ($normalized =~ / (gene|protein)$/) {
    my $unnormalized_to_entries =
      $normalized_to_unnormalized_to_entries{$normalized};
    for my $unnormalized (keys %{$unnormalized_to_entries}) {
      my $prefix = $unnormalized;
      $prefix =~ s/ (gene|protein)$//i;
      if ($unnormalized =~ / ([Gg]ene|[Pp]rotein)$/ and # no all-caps PROTEIN
	  $prefix =~ /^([0-9A-Z]|$dash_re|$greek_re)+$/) {
	my $normalized_prefix = normalize($prefix);
	unless (
	  exists($normalized_to_unnormalized_to_entries{$normalized_prefix}) and
	  exists($normalized_to_unnormalized_to_entries{$normalized_prefix}{$prefix})
	) {
	  my $entries = ($new_n2un2e{$normalized_prefix}{$prefix} ||= []);
	  my @old_ids = map { $_->[0] } @$entries;
	  my $new_entries = $unnormalized_to_entries->{$unnormalized};
	  for my $new_entry (@$new_entries) {
	    my $new_id = $new_entry->[0];
	    push @$entries, $new_entry
	      unless (grep { $_ eq $new_id } @old_ids);
	  }
	}
      }
    }
  # Add entries for "X signaling pathway" from "X pathway" and vice versa.
  } elsif ($normalized =~ / signaling pathway$/) {
    my $norm_wo_signaling = "$` pathway";
    if (exists($normalized_to_unnormalized_to_entries{$norm_wo_signaling})) {
      # find codes for entries with "signaling" suitable for adding to codes
      # without entries with "signaling"
      my @codes = ();
      my $unnormalized_to_entries =
	$normalized_to_unnormalized_to_entries{$normalized};
      for my $unnormalized (keys %{$unnormalized_to_entries}) {
	for my $entry (@{$unnormalized_to_entries->{$unnormalized}}) {
	  my ($code, $name, $status) = @$entry;
	  $code = "C$code";
	  if ($status eq 'name' and
	      # a child of "signaling pathway"
	      grep { $_ eq 'C17132' } @{$id_to_parents{$code}}
	     ) {
	    push @codes, $code;
	  }
	}
      }
      next unless (@codes);
      # do the same vice versa
      my @codes_wo_signaling = ();
      my $unnorm_to_ents_wo_signaling =
	$normalized_to_unnormalized_to_entries{$norm_wo_signaling};
      for my $unnorm_wo_signaling (keys %{$unnorm_to_ents_wo_signaling}) {
	for my $entry_wo_signaling (@{$unnorm_to_ents_wo_signaling->{$unnorm_wo_signaling}}) {
	  my ($code_wo_signaling, undef, $status_wo_signaling) =
	    @$entry_wo_signaling;
	  $code_wo_signaling = "C$code_wo_signaling";
	  if ($status_wo_signaling eq 'name' and
	      # a child of "signaling pathway"
	      grep { $_ eq 'C17132' }
		    @{$id_to_parents{$code_wo_signaling}}
	     ) {
	    push @codes_wo_signaling, $code_wo_signaling;
	  }
	}
      }
      next unless (@codes_wo_signaling);
      # remove_duplicates is just paranoia... there really should only be one
      # element per code in each of these lists, because we only take the entry
      # for the name, not synonyms
      @codes = map { s/^C//; $_ } @{remove_duplicates(\@codes)};
      @codes_wo_signaling =
        map { s/^C//; $_ } @{remove_duplicates(\@codes_wo_signaling)};
      # add synonym entries in both directions for each pair of codes
      # with/without "signaling"
      for my $code (@codes) {
	my $unnormalized = $id_to_name{"C$code"};
	for my $code_wo_signaling (@codes_wo_signaling) {
	  my $unnorm_wo_signaling = $id_to_name{"C$code_wo_signaling"};
	  push @{$new_n2un2e{$normalized}{$unnormalized}},
	       [$code_wo_signaling, $unnorm_wo_signaling, 'synonym']
	    # doesn't already have version w/o signaling as a synonym
	    unless (grep {
		      normalize($_) eq $norm_wo_signaling
		    } @{$id_to_synonyms{"C$code"}});
	  push @{$new_n2un2e{$norm_wo_signaling}{$unnorm_wo_signaling}},
	       [$code, $unnormalized, 'synonym']
	    # doesn't already have version with signaling as a synonym
	    unless (grep {
		      normalize($_) eq $normalized
		    } @{$id_to_synonyms{"C$code_wo_signaling"}});
	}
      }
    }
  }
}

# add entries from temporary table to final table
for my $n (keys %new_n2un2e) {
  my $un2e = ($normalized_to_unnormalized_to_entries{$n} ||= {});
  my $new_un2e = $new_n2un2e{$n};
  for my $un (keys %{$new_un2e}) {
    $un2e->{$un} = [@{$un2e->{$un}}, @{$new_un2e->{$un}}];
  }
}

#
# write main output
#

for my $normalized (sort keys %normalized_to_unnormalized_to_entries) {
  print $normalized;
  my $first = 1;
  for my $unnormalized (sort keys %{$normalized_to_unnormalized_to_entries{$normalized}}) {
    if ($first) {
      $first = 0;
    } else {
      print "\t,";
    }
    print join("\t", '', $unnormalized,
      map { ("NCIT:C$_->[0]", $_->[1], $_->[2]) } # flatten and add NCIT:C pref.
      sort { $a->[0] <=> $b->[0] } # sort by (numeric part of) ID
      @{$normalized_to_unnormalized_to_entries{$normalized}{$unnormalized}}
    );
  }
  print "\n";
}

#
# write DSL files
#

my $dir = 'drum-dsl/NCIT';
make_path $dir;

my @sorted_ids =
  map { "C$_" } sort { $a <=> $b } map { s/^C//; $_ } keys %id_to_name;
my $prev_prefix = undef;
my $fh = undef;
for my $id (@sorted_ids) {
  my $id_num = $id;
  $id_num =~ s/^C//;
  my $prefix = sprintf("%05d", ($id_num / 10));
  if ((not defined($prev_prefix)) or $prefix ne $prev_prefix) {
    if (defined($fh)) {
      print $fh "\n)\n";
      close $fh;
    }
    my $fn = "$dir/$prefix.lisp";
    open($fh, ">", $fn) or die "Can't open $fn for writing: $!";
    print $fh "(pos N\n\n";
  }
  $prev_prefix = $prefix;
  print $fh "(concept NCIT::$id";
  if ($id_to_name{$id} !~ /^<?https?:\/\//) { # don't bother turning bogus URL "names" into DSL senses
    print $fh "\t(sense (word ";
    my @name = map { lisp_intern(uc($_)) }
	       split(/\s+/, $id_to_name{$id});
    print $fh "(" if (@name > 1);
    print $fh join(' ', @name);
    print $fh ")" if (@name > 1);
    print $fh "))";
  }
  if (exists($id_to_parents{$id})) {
    print $fh "\n  (inherit " .
              join(' ',
	        map { "NCIT::$_" }
		@{$id_to_parents{$id}}
	      ) . ")";
  }
  print $fh ")\n";
}
if (defined($fh)) {
  print $fh "\n)\n";
  close $fh;
}

