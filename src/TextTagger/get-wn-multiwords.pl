#!/usr/bin/perl

# get-wn-multiwords.pl - get list of WordNet multiwords from data.{noun,verb,adj,adv}, with tab-separated fields to put in tags
# Usage: get-wn-multiwords.pl [wordnet home dir] >wn-multiwords.tsv
# Output fields:
# morphed multiword	(TRIPS POS	morph ID	sense key)+

use lib "$ENV{TRIPS_BASE}/etc/WordNetSQL";
use WordNetSQL qw(add_suffix is_preposition);
# TODO make this actually use SQL instead of reading WN files directly
# ... and reconcile add_suffixes with apply_morph

use strict vars;

my $wordnet_basepath = shift(@ARGV) || "/p/nl/wordnet/WordNet-3.0/dict";

my %wn_pos_to_trips_pos = (noun => 'N', verb => 'V', adj => 'ADJ', adv => 'ADV');

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
  return grep { $_->{trips_pos} eq 'V' } @{$lemma_to_entries{$word}};
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

sub sense_key {
  my ($lemma, $ss_type, $lex_filenum, $lex_id) = @_;
  $ss_type =~ tr/nvars/12345/;
  return sprintf("$lemma%%%1d:%02d:%02d", $ss_type, $lex_filenum, $lex_id);
}

my %member_meronym_to_holonyms = ();
my %noun_hyponym_to_hypernyms = ();

# get the TRIPS POS and sense key associated with each lemma
for my $pos (keys %wn_pos_to_trips_pos) {
  print STDERR "reading data.$pos\n";
  -f "$wordnet_basepath/data.$pos"
      or die "File not found: $wordnet_basepath/data.$pos\n";
  open DATA, "<$wordnet_basepath/data.$pos" 
      or die "Can't open data.$pos: $!";
  while (<DATA>) {
    next unless (/^\S/);
    s/\s+\|.*$//g; # discard glosses
    my @fields = split(/\s+/);
    my ($synset_offset, $lex_filenum, $ss_type, $w_cnt, @rest) = @fields;
    $w_cnt = hex($w_cnt);
    my @sense_words = ();
    for my $i (1..$w_cnt) {
      my $word = shift @rest;
      my $lex_id = hex(shift @rest);
      push @sense_words, [$word, sense_key($word, $ss_type, $lex_filenum, $lex_id)];
    }
    my $proper_name = 0;
    my $p_cnt = shift @rest;
    $p_cnt =~ s/^00?//; # remove leading 0s so perl doesn't think it's octal
    for my $i (1..$p_cnt) {
      my $pointer_symbol = shift @rest;
      my $target_synset_offset = shift @rest;
      my $target_pos = shift @rest;
      my $source_target = shift @rest;
      if ($pointer_symbol eq '@i') { # instance hypernym
	$proper_name = 1;
      } elsif ($pointer_symbol eq '#m') { # member holonym
        push @{$member_meronym_to_holonyms{$synset_offset}}, $target_synset_offset;
      } elsif ($pointer_symbol eq '@' and $pos eq 'noun') { # hypernym
        push @{$noun_hyponym_to_hypernyms{$synset_offset}}, $target_synset_offset;
      }
    }
    my $trips_pos = $wn_pos_to_trips_pos{$pos};
    $trips_pos = 'NAME' if ($proper_name);
    for my $word (@sense_words) {
      my ($lemma, $sense_key) = @$word;
      $lemma =~ s/_/ /g;
      $lemma =~ s/\(.*\)$//;
      push @{$lemma_to_entries{$lemma}},
	   +{ lemma => $lemma,
	      trips_pos => $trips_pos,
	      sense_key => $sense_key,
	      synset_offset => $synset_offset
	    };
    }
  }
  close DATA;
}


# remove taxonomic noun entries

# is the given noun synset a descendant of 'taxon'?
my %is_taxon = (
  '07992450' => 1 # 'taxon' itself
);
sub is_taxon {
  my $synset_offset = shift;
  unless (exists($is_taxon{$synset_offset})) {
    for my $hypernym (@{$noun_hyponym_to_hypernyms{$synset_offset}}) {
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
    (0 < grep { is_taxon($_) } @{$member_meronym_to_holonyms{$synset_offset}}));
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
    for my $hypernym (@{$noun_hyponym_to_hypernyms{$synset_offset}}) {
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
  if ($lemma =~ /[ -]/) {
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
}

# output, removing duplicate entries
print STDERR "printing output\n";
for my $morphed (sort keys %morphed_to_entries) {
  print $morphed;
  my %entries = ();
  for my $entry (@{$morphed_to_entries{$morphed}}) {
    my $entry_text = join("\t", @{$entry}{qw(penn_pos sense_key)});
    print "\t$entry_text" unless (exists($entries{$entry_text}));
    $entries{$entry_text} = 1;
  }
  print "\n";
}

