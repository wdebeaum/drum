#!/usr/bin/perl

# get-wn-multiwords.pl - get list of WordNet multiwords from WordNetSQL, with tab-separated fields to put in tags
# Usage: get-wn-multiwords.pl >wn-multiwords.tsv
# Output fields:
# morphed multiword	(Penn POS	sense key)+

use lib "$ENV{TRIPS_BASE}/etc/WordNetSQL";
use WordNetSQL qw(add_suffix is_preposition db query_first_column);
# TODO reconcile add_suffixes with apply_morph

use strict vars;

my %ss_type_to_trips_pos = (n => 'N', v => 'V', a => 'ADJ', r => 'ADV', s => 'ADJ');

# map TRIPS POS to hashrefs, each of which maps from suffix to morph ID
my %trips_pos_to_morph = (
  'V' => {'' => '12S123PBASE', s => '3S', ing => 'ING', ed => 'PAST'}, # ignore pastpart for now
  'N' => {'' => 'SING', s => 'PLUR'},
  'ADJ' => {'' => 'NONE'}, #, er => 'ER', est => 'EST'},
  'ADV' => {'' => 'NONE'}, #, ly => 'LY'},
  'NAME' => {'' => 'NONE'}
);

my %trips_pos_to_morph_to_penn = (
  'V' => {'12S123PBASE' => 'VB', '3S' => 'VBZ', 'ING' => 'VBG', 'PAST' => 'VBD', 'PASTPART' => 'VBN'},
  'N' => {'SING' => 'NN', 'PLUR' => 'NNS'},
  'ADJ' => {'NONE' => 'JJ', 'ER' => 'JJR', 'EST' => 'JJS'},
  'ADV' => {'NONE' => 'RB'}, # TODO RBR and RBS?
  'NAME' => {'NONE' => 'NNP'} # TODO NNPS?
);

my %lemma_to_entries = ();

sub is_verb {
  my $word = shift;
  return scalar(query_first_column("SELECT 1 FROM senses WHERE lemma=? AND ss_type='v';", $word));
}

# given an entry containing a lemma and a trips_pos return a list of modified
# entries, containing the additional keys morphed and morph_id
sub add_suffixes {
  my %entry = %{shift(@_)};
  my @entries = ();
  my %morph = %{$trips_pos_to_morph{$entry{trips_pos}}};
  for my $suffix (keys %morph) {
    if ($entry{trips_pos} eq 'V' and $entry{lemma} =~ /[ -]/) { # multiword verb
      # for each single word in the multiword lemma that is also a verb lemma
      # and not also a preposition...
      my @words = split(/([ -])/, $entry{lemma});
      my $added_an_entry = 0;
      for my $i (0..$#words) {
	if (($words[$i] !~ /^[ -]$/ and (not is_preposition($words[$i])) and
	     is_verb($words[$i])) or
	    # if we haven't added an entry by the last word, just morph the
	    # last word
	    ($i == $#words and not $added_an_entry)
	   ) {
          # ... add an entry where the suffix is added to that word
	  my @new_words = @words;
	  $new_words[$i] = add_suffix($words[$i], $suffix);
	  my $morphed = join('', @new_words);
	  push @entries, +{ %entry,
			    morphed => $morphed,
			    morph_id => $morph{$suffix},
			    penn_pos => $trips_pos_to_morph_to_penn{$entry{trips_pos}}{$morph{$suffix}}
			  };
	  $added_an_entry = 1;
	}
      }
    } else { # non-verb or non-multiword verb
      my $morphed = add_suffix($entry{lemma}, $suffix);
      push @entries, +{ %entry,
			morphed => $morphed,
			morph_id => $morph{$suffix},
			penn_pos => $trips_pos_to_morph_to_penn{$entry{trips_pos}}{$morph{$suffix}}
		      };
    }
  }
  return @entries;
}

my $has_instance_hypernym = <<'EOSQL';
  SELECT 1 FROM pointers
  WHERE source_synset_offset=? AND source_ss_type=? AND pointer_symbol='@i';
EOSQL

print STDERR "selecting multiword senses\n";
# select relevant fields of all multiword senses
my $sth = db()->prepare(<<'EOSQL');
  SELECT synset_offset, ss_type, sense_number, sense_key, lemma
  FROM senses
  WHERE lemma LIKE '%\_%' ESCAPE '\' OR lemma LIKE '%-%';
EOSQL

$sth->execute();
# make an entry for each sense
while (my @row = $sth->fetchrow_array) {
  my ($synset_offset, $ss_type, $sense_number, $sense_key, $lemma) = @row;
  my $trips_pos = $ss_type_to_trips_pos{$ss_type};
  # nouns with instance hypernyms are proper names, not common nouns
  $trips_pos = 'NAME'
    if ($ss_type eq 'n' and
        scalar(query_first_column($has_instance_hypernym, $synset_offset, $ss_type)));
  $sense_key =~ s/^[^%]+/$lemma/; # fix capitalization in sense key
  $lemma =~ s/_/ /g;
  $sense_key =~ s/::$//; # save space in the lookup table
  push @{$lemma_to_entries{$lemma}},
       +{ lemma => $lemma,
	  trips_pos => $trips_pos,
	  sense_key => $sense_key,
	  synset_offset => $synset_offset,
	  sense_number => $sense_number
       };
}

my $get_noun_hypernyms = <<'EOSQL';
  SELECT target_synset_offset FROM pointers
  WHERE source_synset_offset=? AND source_ss_type='n' AND pointer_symbol='@';
EOSQL

my $get_member_holonyms = <<'EOSQL';
  SELECT target_synset_offset FROM pointers
  WHERE source_synset_offset=? AND source_ss_type='n' AND pointer_symbol='#m';
EOSQL

# remove taxonomic noun entries

# is the given noun synset a descendant of 'taxon'?
my %is_taxon = (
  '7992450' => 1 # 'taxon' itself
);
sub is_taxon {
  my $synset_offset = shift;
  unless (exists($is_taxon{$synset_offset})) {
    for my $hypernym (query_first_column($get_noun_hypernyms, $synset_offset)) {
      if (is_taxon($hypernym)) {
	$is_taxon{$synset_offset} = 1;
	last;
      }
    }
    $is_taxon{$synset_offset} = 0 unless (exists($is_taxon{$synset_offset}));
  }
  return $is_taxon{$synset_offset};
}

# is the given noun synset a taxon, or a member of one?
sub is_taxonomic {
  my $synset_offset = shift;
  my $ret = (is_taxon($synset_offset) or
    (0 < grep { is_taxon($_) }
	      query_first_column($get_member_holonyms, $synset_offset)));
  return $ret;
}

# remove the entries
print STDERR "removing taxonomic synsets\n";
for my $lemma (keys %lemma_to_entries) {
  $lemma_to_entries{$lemma} = [grep {
    not($_->{sense_key} =~ /%1:/ and is_taxonomic($_->{synset_offset}))
  } @{$lemma_to_entries{$lemma}}];
}


# remove named days

# is the given noun synset a descendant of day%1:28:01::?
my %is_day = (
  '15157225' => 1
);
sub is_day {
  my $synset_offset = shift;
  unless (exists($is_day{$synset_offset})) {
    for my $hypernym (query_first_column($get_noun_hypernyms, $synset_offset)) {
      if (is_day($hypernym)) {
	$is_day{$synset_offset} = 1;
	last;
      }
    }
    $is_day{$synset_offset} = 0 unless (exists($is_day{$synset_offset}));
  }
  return $is_day{$synset_offset};
}

print STDERR "removing named day synsets\n";
for my $lemma (keys %lemma_to_entries) {
  $lemma_to_entries{$lemma} = [grep {
    not($_->{sense_key} =~ /%1:/ and is_day($_->{synset_offset}))
  } @{$lemma_to_entries{$lemma}}];
}


my %morphed_to_entries = ();

# apply morphology to the multiword lemmas
print STDERR "applying morphology\n";
for my $lemma (keys %lemma_to_entries) {
  for my $entry (@{$lemma_to_entries{$lemma}}) {
    for my $morphed_entry (add_suffixes($entry)) {
      push @{$morphed_to_entries{$morphed_entry->{morphed}}}, $morphed_entry;
      # add PASTPART too when morph_id is PAST
      if ($morphed_entry->{morph_id} eq 'PAST') {
	push @{$morphed_to_entries{$morphed_entry->{morphed}}},
	     +{ %$morphed_entry,
		morph_id => 'PASTPART',
		penn_pos => $trips_pos_to_morph_to_penn{V}{PASTPART}
	      };
      }
    }
  }
}

# read stoplist/golist from WF
sub read_stoplist {
  my $filename = shift;
  open SL, "<$ENV{TRIPS_BASE}/src/WordFinder/wordnet/$filename"
    or die "Can't open $filename: $!";
  my @sl = map {
    chomp;
    s/;.*$//;
    s/^\s+|\s+$//g;
    s/::$//;
    $_
  } <SL>;
  close SL;
  return @sl;
}

my %stophash = ();
for (read_stoplist('stoplist.txt')) {
  $stophash{$_} = 1;
}
for (read_stoplist('golist.txt')) {
  delete $stophash{$_};
}

# this matches WordFinder's parts-of-speech list
my %trips_pos_rank = ('N' => 1, 'NAME' => 1, 'V' => 2, 'ADJ' => 3, 'ADV' => 4);

sub entry_is_morphed {
  my $m = $_->{morph_id};
  return (($m =~ /^(NONE|SING|12S123PBASE)$/) ? 1 : 0);
}

# sort entries in the same order as WordFinder: base forms first, then morphed
# forms; then by POS (nvar); then by sense number (i.e. decreasing frequency)
sub wf_order {
  sort {
    (entry_is_morphed($a) <=> entry_is_morphed($b)) ||
    ($trips_pos_rank{$a->{trips_pos}} <=> $trips_pos_rank{$b->{trips_pos}}) ||
    ($a->{sense_number} <=> $b->{sense_number})
  } @_;
}

# output, removing duplicate/stoplisted entries
print STDERR "printing output\n";
for my $morphed (sort keys %morphed_to_entries) {
  # skip words all of whose entries are stoplisted
  next unless (grep { not $stophash{$_->{sense_key}} } @{$morphed_to_entries{$morphed}});
  print $morphed;
  my %entries = ();
  for my $entry (wf_order(@{$morphed_to_entries{$morphed}})) {
    next if ($stophash{$entry->{sense_key}}); # skip stoplisted entries
    my $entry_text = join("\t", @{$entry}{qw(penn_pos sense_key)});
    print "\t$entry_text" unless (exists($entries{$entry_text}));
    $entries{$entry_text} = 1;
  }
  print "\n";
}

