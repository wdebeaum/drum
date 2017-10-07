#!/usr/bin/env perl

# get-obo-terms.pl - get a TSV file suitable for feeding to terms.c from some OBO files
# William de Beaumont
# 2015-10-26

# FIXME this file has a lot of historical baggage

use lib "./Perl";
use lib "../";
use TextTagger::Escape qw(escape_for_quotes);
use TextTagger::Util qw(lisp_intern);
use TextTagger::OBOParser qw(read_header read_stanza remove_trailing_modifier);
use TextTagger::ChemicalFormulae qw(is_chemical_element);
use TextTagger::Normalize qw(unspell_greek_letters normalize);
use IO::Handle;
do "../util/add_suffix.polyglot" or die;

use strict vars;

@ARGV > 0 or die "USAGE: $0 file1.obo [file2.obo...] >obo-terms.tsv";

my %normalized_word_to_obo_info = ();
my %id_to_obo_info = ();

# read OBO files
for my $obo_file_name (@ARGV) {
  my $fh;
  open $fh, "<$obo_file_name" or die "Can't open OBO file $obo_file_name: $!";
  read_header($fh); # discard
  my %stanza;
  until ($fh->eof) {
    %stanza = read_stanza($fh);
    next unless ($stanza{stanza_name} eq 'Term');
    next if (exists($stanza{is_obsolete}));
    my $info = {
      id => remove_trailing_modifier($stanza{id}[0]),
      name => remove_trailing_modifier($stanza{name}[0]),
      ( exists($stanza{subset}) ? (subsets => $stanza{subset}) : () )
    };
    $info->{id} =~ s/^CVCL_/CVCL:/; # Cellosaurus
    if ($obo_file_name =~ /efo.obo$/) {
      # EFO has lots of extra stuff in other namespaces, so be selective about
      # which ones we take
      next unless ($info->{id} =~ /^(EFO|UO|Orphanet|HP):/);
      # only take terms from the Human Phenotype Ontology when they're linked
      # to MeSH and/or DOID; they tend to be diseases (as opposed to normal
      # conditions)
      next if ($info->{id} =~ /^HP:/ and not (
               (grep /^(Me?SH|DOID):/, @{$stanza{xref}}) or
	       (grep m!^http://www\.ebi\.ac\.uk/efo/(DOID|Me?SH)_definition_citation (DOID|Me?SH):!, @{$stanza{property_value}})
	      ));
    }
    if ($obo_file_name =~ /pr\.obo$/) {
      # PR also has extra stuff in other namespaces
      next unless ($info->{id} =~ /^PR:/);
      # also check that this isn't just copied from UniProt with the namespace
      # changed
      my $up_id = "UniProtKB:$'";
      next if (exists($stanza{xref}) and
	       grep { $_ eq $up_id } @{$stanza{xref}});
      # also only get human stuff for now
      next unless ($info->{name} =~ /\(human\)$/i);
      $info->{name} =~ s/\s*\([^\(\)]*\)$//i; # remove species from name
    }
    $id_to_obo_info{$info->{id}} = $info;
    if (exists($stanza{is_a})) {
      for my $parent_mod (@{$stanza{is_a}}) {
	my $parent = remove_trailing_modifier($parent_mod);
	push @{$info->{is_a}}, $parent;
      }
#      print STDERR "parents of $info->{id} are " . join(', ', @{$info->{is_a}}) . "\n";
    }
    my @added_words = ();
    my $normalized_name = normalize($info->{name});
    my $greek_name = unspell_greek_letters($info->{name});
    my $normalized_greek_name = normalize($greek_name);
    if (exists($stanza{synonym})) {
      for my $synonym (@{$stanza{synonym}}) {
	my ($lemma, $scope, $type) = @$synonym{qw(synonym scope type)};
	# filter out certain synonym types
	next if (
	  is_chemical_element($lemma) or # let another tagger handle this
	  ($obo_file_name =~ /BrendaTissueOBO$/ and $type eq 'GE') or # German
	  ($obo_file_name =~ /chebi\.obo$/ and $type =~ /^InChI|^SMILES$/) or
	  ($obo_file_name =~ /go\.obo$/ and $scope eq 'RELATED') or
	  ($obo_file_name =~ /psi-mi25\.obo$/ and $type eq 'PSI-MI-short')
	);
	# fix some lemma bogosity
	$lemma =~ s/<new synonym>//; # argh
	$lemma =~ s/^\s+|\s+$//g; # trim space off lemma (argh)
	$lemma =~ s/`/'/g if ($obo_file_name =~ /BrendaTissueOBO$/); # ARGH
	$lemma =~ s/\s*\([^\(\)]*\)$// if ($obo_file_name =~ /pr\.obo$/); # >_<
	if ($obo_file_name =~ /cl(-basic)\.obo$/) { # *cries*
	  $lemma =~ s/[[:space:]\xa0]/ /g;
	  $lemma =~ s/[\x2013\x2014]/-/g;
	  $lemma =~ s/\x2019s /'s /g;
	}
	next if ($lemma eq '');
	# TODO dbxrefs?
	my $syn_info = +{
	  %$info,
	  'synonym-scope' => $scope,
	  ( $type ne '' ? ('synonym-type' => $type) : () ),
	  unnormalized => $lemma,
	  status => "$scope synonym"
	};
	$lemma = normalize($lemma);
	unless (grep { $_ eq $lemma } @added_words) {
	  $syn_info->{status} = 'name'
	    if ($lemma eq $normalized_name or
	        $lemma eq $normalized_greek_name);
	  push @{$normalized_word_to_obo_info{$lemma}}, $syn_info;
	  push @added_words, $lemma;
	  my $greek = unspell_greek_letters($syn_info->{unnormalized});
	  $syn_info = +{ %$syn_info, unnormalized => $greek };
	  $greek = normalize($greek);
	  unless (grep { $_ eq $greek } @added_words) {
	    $syn_info->{status} = 'name'
	      if ($greek eq $normalized_name or
		  $greek eq $normalized_greek_name);
	    push @{$normalized_word_to_obo_info{$greek}}, $syn_info;
	  }
	}
      }
    }
    unless (grep { $_ eq $normalized_name } @added_words) {
      push @{$normalized_word_to_obo_info{$normalized_name}},
           +{ %$info, unnormalized => $info->{name}, status => 'name' };
      unless (grep { $_ eq $normalized_greek_name } @added_words) {
	push @{$normalized_word_to_obo_info{$normalized_greek_name}},
	     +{ %$info, unnormalized => $greek_name, status => 'name' };
      }
    }
    # TODO relationships?
  }
  close $fh;
}

# remove regular plural "synonyms"
for my $singular (keys %normalized_word_to_obo_info) {
  my $plural = add_suffix($singular, 's');
  if (exists($normalized_word_to_obo_info{$plural})) {
    my @singular_infos = @{$normalized_word_to_obo_info{$singular}};
    @{$normalized_word_to_obo_info{$plural}} =
      grep {
	my $plural_id = $_->{id};
	not ($_->{status} =~ /synonym$/ and
	     grep { $_->{id} eq $plural_id } @singular_infos)
      } @{$normalized_word_to_obo_info{$plural}};
  }
}

# return the list of info structures reachable via repeated is_a links (not
# including the original ID's info)
sub get_ancestors {
  my ($child_id, $traversed_ids) = @_;
  push @$traversed_ids, $child_id;
  my @ret = ();
  if (exists($id_to_obo_info{$child_id}{is_a})) {
    for my $parent_id (@{$id_to_obo_info{$child_id}{is_a}}) {
      push @ret, $id_to_obo_info{$parent_id}, get_ancestors($parent_id, $traversed_ids)
        unless (grep { $_ eq $parent_id } @$traversed_ids);
    }
  }
  return @ret;
}

# add :ancestors to infos
for my $infos (values %normalized_word_to_obo_info) {
  for my $info (@$infos) {
    my @ancestors = get_ancestors($info->{id}, []);
    $info->{ancestors} = [@ancestors] if (@ancestors);
  }
}

# optionally remove "activity" suffix from words under GO's molecular_function
for my $word (keys %normalized_word_to_obo_info) {
  next unless ($word =~ /(?<=\S)(?<!channel)(?<!tion)(?<!ing)(?<!ic) activity$/);
  my $word_no_activity = $`;
  # don't add the no-activity version if we already have it
  next if (exists($normalized_word_to_obo_info{$word_no_activity}));
  for my $info (@{$normalized_word_to_obo_info{$word}}) {
    if ($info->{id} =~ /^GO:/ and
        grep { $_->{name} eq 'molecular_function' } @{$info->{ancestors}}) {
      my $new_info = { %$info };
      $new_info->{unnormalized} =~ s/ activity$//i;
      push @{$normalized_word_to_obo_info{$word_no_activity}}, $new_info;
    }
  }
}

# output
for my $word (sort keys %normalized_word_to_obo_info) {
  next if (length($word) == 1); # no single letters, please
  next if ($word =~ /\bper(?:cent)?\b/); # ignore multiword rate terms so TRIPS parser can treat them compositionally
  next unless (@{$normalized_word_to_obo_info{$word}}); # ignore words all of whose info has been removed
  print $word;
  # group by unnormalized word
  my %unnormalized_word_to_obo_info = ();
  for my $info (@{$normalized_word_to_obo_info{$word}}) {
    push @{$unnormalized_word_to_obo_info{$info->{unnormalized}}}, $info;
  }
  my $first = 1;
  for my $unnormalized (sort keys %unnormalized_word_to_obo_info) {
    if ($first) {
      $first = 0;
    } else {
      print "\t,";
    }
    print "\t" . $unnormalized;
    my %already = ();
    for my $info (@{$unnormalized_word_to_obo_info{$unnormalized}}) {
      my $str = "\t" . $info->{id} . "\t" . $info->{name} . "\t" . $info->{status};
      unless ($already{$str}) {
	print $str;
	$already{$str} = 1;
      }
    }
  }
  print "\n";
}

