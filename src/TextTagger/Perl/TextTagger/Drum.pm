package TextTagger::Drum;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(init_drum_tagger tag_drum_terms);

use IPC::Open2;
use TextTagger::Util qw(structurally_equal remove_duplicates union lisp_intern match2tag word_is_in_trips_lexicon);
use TextTagger::Normalize qw($dash_re normalize characterize_match is_bad_match);
use TextTagger::Tags2Trips qw(sortTags);

use Data::Dumper;

use strict vars;

my $debug = 0;

my ($terms_in, $terms_out, $terms_pid);
my ($dbxrefs_in, $dbxrefs_out, $dbxrefs_pid);
my ($protmods_in, $protmods_out, $protmods_pid);
my %mirna_species = ();

sub init_drum_tagger {
  $terms_pid = open2($terms_in, $terms_out,
  		     $ENV{TRIPS_BASE} . "/bin/terms2",
		     $ENV{TRIPS_BASE} . "/etc/TextTagger/drum-terms.tsv");
  binmode $terms_in, ':utf8';
  binmode $terms_out, ':utf8';
  $dbxrefs_pid = open2($dbxrefs_in, $dbxrefs_out,
  		     $ENV{TRIPS_BASE} . "/bin/terms2",
		     $ENV{TRIPS_BASE} . "/etc/TextTagger/drum-dbxrefs.tsv");
  binmode $dbxrefs_in, ':utf8';
  binmode $dbxrefs_out, ':utf8';
  $protmods_pid = open2($protmods_in, $protmods_out,
  		     $ENV{TRIPS_BASE} . "/bin/terms2",
		     $ENV{TRIPS_BASE} . "/etc/TextTagger/go_protmods.tsv");
  binmode $protmods_in, ':utf8';
  binmode $protmods_out, ':utf8';
  open MS, "<$ENV{TRIPS_BASE}/etc/TextTagger/mirna-species.tsv"
    or die "Failed to open mirna-species.tsv: $!";
  while (<MS>) {
    chomp;
    my ($abbr, $species) = split(/\t/);
    $mirna_species{$abbr} = $species;
  }
  close MS;
}

# given an ID, look up the other IDs linked to it by database cross-references
sub get_dbxrefs {
  my $id = shift;
  # remove pipequotes and extra :
  $id =~ s/\|//g;
  $id =~ s/::/:/;
  my @dbxrefs = ();
  print $dbxrefs_out "_EXACT_\n$id\n";
  while ((my $dbxref = <$dbxrefs_in>)) {
    chomp $dbxref;
    last if ($dbxref eq '');
    push @dbxrefs, map {
      # turn ref ID into a real Lisp symbol
      /:/ or die "unpackaged id: $_";
      lisp_intern($', $`)
    } split(/\t/, $dbxref);
  }
  return @dbxrefs;
}

# Is the given span entirely composed of prefixes?
sub is_all_prefixes {
  my ($start, $end, $prefixes) = @_;
  my @first_prefixes = grep { $_->{start} == $start } @$prefixes;
  for (@first_prefixes) {
    next if ($_->{end} > $end);
    if ($_->{end} == $end or is_all_prefixes($_->{end}, $end, $prefixes)) {
      print STDERR "$start-$end is all prefixes\n" if ($debug);
      return 1;
    }
  }
  return 0;
}

# this was pulled out of the middle of tag_drum_terms so it could be reused for
# tag_protmods
sub read_from_terms2 {
  my $stream = shift;
  our ($self, $str, @prefixes, %sentence_starts, %plural_entries, %singular_entries, %listed_variant_entries);
  my @terms = ();
  while (my $term = <$stream>) {
    print STDERR "from drum/protmod terms:\n$term" if ($debug);
    chomp $term;
    last if ($term eq '');
    # if we get a subset of the input lattice back, store it and reconstruct
    # the unnormalized version of the string used to query drum-terms.tsv
    my @used_entries = ();
    my $input_variant;
    if ($term eq '_BEGIN_WORD_LATTICE_') {
      $input_variant = '';
      my $prev_end;
      my $entry;
      while (($entry = <$stream>) and $entry ne "_END_WORD_LATTICE_\n") {
	print STDERR $entry if ($debug);
	push @used_entries, $entry;
	chomp $entry;
	my ($lex, $start, $end) = split(/\t/, $entry);
	$input_variant .= ' ' if (defined($prev_end) and $prev_end < $start);
	$input_variant .= $lex;
	$prev_end = $end;
      }
      print STDERR $entry if ($debug);
      # continue with the next line after the lattice
      $term = <$stream>;
      print STDERR $term if ($debug);
      chomp $term;
    }
    # parse the main result line for this term
    my ($normalized_matched_variant, $start, $end, @rest) =
      split(/\t/, $term, -1);
    die "Bogus output from term tagger (DRUM): '$term'"
      unless ($start =~ /^\d+$/ and $end =~ /^\d+$/);
#    next if ($end - $start <= 2); # don't tag 1- or 2-letter terms
    # remake lex from original input, since the matched variant could be
    # different
    my $lex = substr($str, $start, $end - $start);
    $input_variant ||= $lex;
    next if (is_bad_match($str, $lex, $start, $end));
    # don't tag something entirely composed of more than one prefix (and
    # dashes) and joined to the following word without a dash
    next if (@used_entries > 1 and
    	     $end < length($str) and substr($str, $end, 1) =~ /\w/ and
	     is_all_prefixes($start, $end, \@prefixes));
    # compute depluralization fields for matches (which don't depend on
    # $matched_variant)
    my $num_maybe_depluralized =
      grep { exists($plural_entries{$_}) } @used_entries;
    my $num_surely_depluralized =
      grep { exists($plural_entries{$_}) and 
	     not exists($singular_entries{$_})
	   } @used_entries;
    my $min_depluralization_score = 1;
    if ($num_maybe_depluralized > 0) {
      for (@used_entries) {
	$min_depluralization_score = $plural_entries{$_}
	  if (exists($plural_entries{$_}) and
	      $plural_entries{$_} < $min_depluralization_score);
      }
    }
    while (@rest) {
      my $matched_variant = shift(@rest); # unnormalized
      my $skip = 0; # should we skip this variant?
      # require short words (usually unit abbreviations) to match exactly, and
      # not be part of a larger word (but allow numbers to run into units from
      # the left)
      if ($end - $start < 3 and
	  (not grep { $listed_variant_entries{$_} } @used_entries) and (
	    $lex ne $matched_variant or	      # FIXME? \pL vvv
	    ($start > 0 and substr($str, $start-1, 1) =~ /[a-z-]/i) or
	    ($end < length($str) and substr($str, $end, 1) =~ /[\w-]/)
	  )) {
	$skip = 1;
      }
      my $match = ($skip ? {} : characterize_match($input_variant, $matched_variant, $sentence_starts{$start}));
      # require dashes between digits to be preserved
      $skip = 1
        if ($match->{'range-dash-no-dash'} or $match->{'no-dash-range-dash'});
      if ($skip) {
	# eat @rest until next comma or EOL
	while (@rest) {
	  last if (shift(@rest) eq ',');
	  shift(@rest);
	  shift(@rest);
	}
	next;
      }
      if ($num_maybe_depluralized > 0) {
	$match->{'maybe-depluralized'} = $num_maybe_depluralized;
	$match->{'surely-depluralized'} = $num_surely_depluralized;
	$match->{'depluralization-score'} = $min_depluralization_score;
      }
      print STDERR Data::Dumper->Dump([$match],['*match']) if ($debug);
      my %mapped_id_to_matches_with_status = ();
      my @mapped_ids = (); # equivalent to "keys %mitmws" but stable across runs
      my %id2name = ();
      while (@rest) {
	my $id = shift(@rest);
	last if ($id eq ',');
	my $name = shift(@rest);
	my $status = shift(@rest);
	my $match_with_status = +{ %$match, status => $status };
	print STDERR "id=$id; status=$status\n" if ($debug);
	my $tp = undef;
	if ($id =~ /^((?:XFAM|UP):\S+) (\S+)$/) {
	  ($id, $tp) = ($1, $2);
	}
	my $lftypes = undef;
	my $species = undef;
	my @dbxrefs = ();
	$id =~ s/^CL:/CO:/; # can't use CL because that's Common Lisp
	# turn ID into a real Lisp symbol
	$id =~ /:/ or die "unpackaged id: $id";
	$id = lisp_intern($', uc($`));
	print STDERR "interned id=$id\n" if ($debug);
	if ($id =~ /^GO:/ and
	    $name =~ / activity$/ and $matched_variant !~ / activity$/i) {
	  # special case for GO molecular_function terms with "activity" removed
	  $lftypes = ['PROTEIN'];
	} elsif ($id =~ /^CVCL:/ and
		 $match->{'surely-depluralized'}
		) {
	  # skip morphed CVCL terms
	} elsif ($id =~ /^(BE|BTO|CHEBI|CO|EFO|GO|MI|NCIT|UO|SO|ORPHANET):/) { # ontologies with hierarchies
	  push @mapped_ids, $id
	    unless (exists($mapped_id_to_matches_with_status{$id}));
	  push @{$mapped_id_to_matches_with_status{$id}}, $match_with_status
	    unless (grep { structurally_equal($_, $match_with_status) }
			 @{$mapped_id_to_matches_with_status{$id}});
	  $id2name{$id} = $name;
	# for the rest, base senses only on which ontology the term came from
	} elsif ($id =~ /^HGNC:/) { # everything's a gene
	  $lftypes = ['GENE'];
	  push @dbxrefs, get_dbxrefs($id);
	} elsif ($id =~ /^UP::SL/) { # everything's a subcellular location (sorta)
	  if ($tp eq 'D') { # location
	    $lftypes = ['CELL-PART'];
	  } elsif ($tp eq 'T') { # topology
	    $lftypes = ['PROTEIN-FAMILY'];
	  } elsif ($tp eq 'O') { # orientation
	    $lftypes = ['CELL-PART']; # maybe?
	  } else {
	    print STDERR "Warning: bogus subcell.txt entry type: $tp\n";
	  }
	} elsif ($id =~ /^UP:/) { # everything's a protein
	  $lftypes = ['PROTEIN'];
	  if (defined($tp)) {
	    if ($tp eq 'H') {
	      $species = 'Homo sapiens (Human)';
	    } elsif ($tp eq 'M') {
	      $species = 'Mus musculus (Mouse)';
	    } elsif ($tp eq 'C') {
	      $species = 'Arabidopsis * (Cress)';
	    } elsif ($tp eq 'R') {
	      $species = 'Oryza * (Rice)';
	    } elsif ($tp eq 'B') {
	      $species = 'Hordeum * (Barley)';
	    } else {
	      print STDERR "Warning: unknown species identifier: $tp\n";
	    }
	  }
	  push @dbxrefs, get_dbxrefs($id);
	} elsif ($id =~ /^XFAM::\|?PF/) { # ONT type depends on $tp
	  if ($tp eq 'Family') {
	    $lftypes = ['PROTEIN-FAMILY'];
	    push @dbxrefs, get_dbxrefs($id);
	  } else { # Domain, Repeat, Motif
	    $lftypes = ['MOLECULAR-DOMAIN'];
	  }
	} elsif ($id =~ /^MESH:/) { # everything (that we get) is a drug
	  $lftypes = ['PHARMACOLOGIC-SUBSTANCE'];
	} elsif ($id =~ /^CVCL:/) { # everything's a cell line
	  $lftypes = ['CELL-LINE'];
	} elsif ($id =~ /^(FA|NEXTPROT-FAMILY):/) { # everything's a protein family
	  $lftypes = ['PROTEIN-FAMILY'];
	} elsif ($id =~ /^HP:/) { # everything (that we get) is a disease
	  # NOTE: technically we could use EFO's hierarchy for this, but that
	  # would put these under "phenotype" which maps to
	  # ONT::physical-condition, which we think is too general.
	  # (see bob:#5, #12)
	  $lftypes = ['MEDICAL-DISORDERS-AND-CONDITIONS'];
	} elsif ($id =~ /^PC:/) { # everything (that we get) is a drug
	  $lftypes = ['PHARMACOLOGIC-SUBSTANCE'];
	}
	if (defined($lftypes)) {
	  my @old_terms = grep {
	    $_->{start} == $start and
	    $_->{end} == $end and
	    $_->{'domain-specific-info'}{id} eq $id
	  } @terms;
	  if (@old_terms) { # just add the new match to the old term
	    push @{$old_terms[0]{'domain-specific-info'}{matches}},
	         $match_with_status
	      unless (grep { structurally_equal($_, $match_with_status) }
			   @{$old_terms[0]{'domain-specific-info'}{matches}}); 
	  } else { # make a new term
	    $lftypes = remove_duplicates($lftypes);
	    push @terms, +{
	      type => 'sense',
	      lex => $lex,
	      start => $start,
	      end => $end,
	      lftype => $lftypes,
	      'domain-specific-info' => +{
		domain => 'drum',
		type => 'term',
		id => $id,
		( $name ne '' ? (name => $name) : () ),
		'ont-types' => [@$lftypes], # in case the main lftype is changed
		matches => [$match_with_status],
		( defined($species) ? (species => $species) : () ),
		( @dbxrefs ? (dbxrefs => [@dbxrefs]) : () )
	      }
	    }
	  }
	}
      }
      if (%mapped_id_to_matches_with_status) {
	# FIXME push this to the end so we only do this message once per text
	# rather than once per term
	my $reply_content = KQML::KQMLKeywordify($self->send_and_wait("(request :content (get-trips-ont-mappings :concept-ids (" . join(' ', @mapped_ids) . ")))"));
	my %id2mappings = ();
	for (@{$reply_content->{':mappings'}}) {
	  my $m = KQML::KQMLKeywordify($_);
	  my $id = $m->{':from'}[1];
	  push @{$id2mappings{$id}}, $m;
	}
	for my $id (@mapped_ids) {
	  my @matches_with_status = @{$mapped_id_to_matches_with_status{$id}};
	  my @old_terms = grep {
	    $_->{start} == $start and
	    $_->{end} == $end and
	    $_->{'domain-specific-info'}{id} eq $id
	  } @terms;
	  if (@old_terms) { # just add the new matches to the old term
	    $old_terms[0]{'domain-specific-info'}{matches} =
	      union(
		$old_terms[0]{'domain-specific-info'}{matches},
	        \@matches_with_status
	      )
	  } else { # make a new term
	    my ($name, @mappings) = ($id2name{$id}, @{$id2mappings{$id}});
	    next unless (@mappings);
	    my $lftypes =
	      remove_duplicates([
		map {
		  my $lftype = $_->{':to'};
		  $lftype =~ s/^ONT:://;
		  $lftype
		} @mappings
	      ]);
	    push @terms, +{
	      type => 'sense',
	      lex => $lex,
	      start => $start,
	      end => $end,
	      lftype => $lftypes,
	      'domain-specific-info' => +{
		domain => 'drum',
		type => 'term',
		id => $id,
		( $name ne '' ? (name => $name) : () ),
		'ont-types' => [@$lftypes],
		matches => [@matches_with_status],
		mappings => [@mappings]
	      }
	    };
	  }
	}
      }
    }
  }
  return [@terms];
}

my @amino_acids; # forward declaration

sub tag_drum_terms {
  (our $self, our $str, my @input_tags) = @_;
  # get prefixes and dashes for is_all_prefixes
  our @prefixes = grep {
    $_->{type} eq 'prefix' or
    ($_->{type} eq 'punctuation' and $_->{lex} =~ $dash_re)
  } @input_tags;
  # send words, alternative spellings, and Specialist alternatives to terms2
  print $terms_out "_BEGIN_WORD_LATTICE_\n";
  print STDERR "to drum terms:\n_BEGIN_WORD_LATTICE_\n" if ($debug);
  my %entries = (); # avoid printing duplicate entries from different tags
  # keep the start offsets of sentences so we can use the fact that a match is
  # at the start of a sentence while scoring its case match
  our %sentence_starts = ();
  # map citation form, start, and end for NNS and NNPS tags from specialist
  # that we might use to get the respective NN/NNP form from drum-terms.tsv, to
  # the maximum match score found for those specialist tags
  our %plural_entries = ();
  # some entries can be both plural and singular (e.g. "AMP"), so keep a record
  # of the singular ones too
  our %singular_entries = ();
  # also record entries from VariantLists tagger to avoid throwing them out for
  # being too short and not matching exactly
  our %listed_variant_entries = ();
  for my $tag (@input_tags) {
    my $entry = undef;
    if ($tag->{source} eq 'specialist') {
      my $citation_form = $tag->{'domain-specific-info'}{'citation-form'};
      # if something looks like a pluralized acronym (e.g. GAPs), but
      # specialist downcases it (gaps), put it back to the upcased version
      if ($tag->{lex} =~ /^([A-Z0-9]+)s$/ and
	  $citation_form eq lc($1)) {
        $citation_form = $1;
      }
      $entry = "$citation_form\t$tag->{start}\t$tag->{end}\n";
      if (grep /^(?:NNP?|AITL)$/, @{$tag->{'penn-pos'}}) {
	print STDERR Data::Dumper->Dump([$tag],['*singular_tag']) if ($debug);
	$singular_entries{$entry} = 1;
      }
      if (grep /^(?:NNP?|AITL)S$/, @{$tag->{'penn-pos'}}) {
	print STDERR Data::Dumper->Dump([$tag],['*plural_tag']) if ($debug);
	my $max_match_score = ($plural_entries{$entry} || 0);
	for (@{$tag->{'domain-specific-info'}{matches}}) {
	  $max_match_score = $_->{score} if ($max_match_score < $_->{score});
	}
	print STDERR Data::Dumper->Dump([$max_match_score],['max_match_score'])
	  if ($debug);
	$plural_entries{$entry} = $max_match_score;
      } else {
	# don't bother including non-plural specialist entries that normalize
	# the same as the original string
	if (# do a quicker check first
      	    length($citation_form) == $tag->{end} - $tag->{start} and
            normalize($citation_form) eq
	      normalize(substr($str, $tag->{start},
			       $tag->{end} - $tag->{start}))
	   ) {
	  next;
	}
      }
    } elsif ($tag->{type} eq 'sentence') {
      $sentence_starts{$tag->{start}} = 1;
      next;
    } elsif (grep { $_ eq $tag->{type} } qw(word prefix ending number punctuation subword subnumber alternate-spelling)) {
      # make the word lattice entry to give to terms2
      $entry = "$tag->{lex}\t$tag->{start}\t$tag->{end}\n";
      if ($tag->{source} eq 'variant_lists') {
	$listed_variant_entries{$entry} = 1;
      }
    } else {
      next;
    }
    next if (exists($entries{$entry}));
    $entries{$entry} = 1;
    print $terms_out $entry;
    print STDERR $entry if ($debug);
  }
  print $terms_out "_END_WORD_LATTICE_\n";
  print STDERR "_END_WORD_LATTICE_\n" if ($debug);
  my @terms;
  # while terms2 does its thing, do some of our own tagging here
  push @terms, @{tag_protein_sites_and_mutations($self, @input_tags)},
  	       @{tag_mirnas($self, $str, @input_tags)},
	       @{tag_protmods(\%entries)};
  # then read tags from terms2 and assign them lftypes
  # FIXME? should merge terms with identical IDs but different matches, but
  # that's almost certainly not going to come up except within calls to
  # read_from_terms2 (not across them), where it's already handled
  push @terms, @{read_from_terms2($terms_in)};
  # remove UniProt terms for wrong species
  if (defined($self->{drum_species})) {
    @terms = grep {
      my $term = $_;
      my $keep =
	(not (exists($term->{'domain-specific-info'}{id}) and
	      $term->{'domain-specific-info'}{id} =~ /^UP::(?!SL)/ and
	      not (exists($term->{'domain-specific-info'}{species}) and
		   grep { $_ eq $term->{'domain-specific-info'}{species} }
		        @{$self->{drum_species}})));
      print STDERR "removing UniProt term for wrong species:\n" . Data::Dumper->Dump([$_], ['*tag'])
	if ($debug and not $keep);
      $keep;
    } @terms;
  }
  # remove tags on all-lowercase single words that are already in the TRIPS
  # lexicon (unless they're protmods)
  @terms = grep {
    my $keep =
      (not ($_->{lex} =~ /^\p{Ll}+$/ and
      	    (not exists($_->{'penn-pos'})) and # not protmod
	    (word_is_in_trips_lexicon($self, $_->{lex}) or
	      # check version with a dash if it could be a prefix (is followed
	      # by something other than whitespace or sentence-final
	      # punctuation)
	      ($_->{end} < length($str) and
	       substr($str, $_->{end}, 1) !~ /[\s\.\?\!]/ and
	       word_is_in_trips_lexicon($self, $_->{lex} . '-')))));
    print STDERR "removing tag on all-lowercase single word already in TRIPS lexicon:\n" . Data::Dumper->Dump([$_], ['*tag'])
      if ($debug and not $keep);
    $keep;
  } @terms;
  # remove extra tags for amino acids
  @terms = grep {
    my $lex = lc($_->{lex});
    $lex =~ s/s$// if (length($lex) > 6); # depluralize full names
    my $dsi_type = $_->{'domain-specific-info'}{type};
    my $keep =
      ($dsi_type eq 'amino-acid' or
       # only exclude 3-4-letter words if they have the right case pattern to
       # be an amino-acid abbreviation
       (length($lex) == 3 and $_->{lex} !~ /^[A-Z][a-z]+$/) or
       (length($lex) == 4 and $_->{lex} !~ /^p[A-Z][a-z]+$/) or
       not grep { lc($_) eq $lex } @amino_acids);
    print STDERR "removing extra tag for amino acid:\n" . Data::Dumper->Dump([$_], ['*tag'])
      if ($debug and not $keep);
    $keep;
  } @terms;
  # score and sort matches, and propagate max match score to DSI and tag
  for my $tag (@terms) {
    if (exists($tag->{'domain-specific-info'}{matches})) {
      $tag->{'domain-specific-info'}{id} =~ /:/ or die "unpackaged or missing id";
      my $pkg = $`;
      for my $m (@{$tag->{'domain-specific-info'}{matches}}) {
	$m->{score} = score_match($pkg, $m);
	$tag->{'domain-specific-info'}{score} = $m->{score}
	  if ($tag->{'domain-specific-info'}{score} < $m->{score});
      }
      @{$tag->{'domain-specific-info'}{matches}} =
        sort { $b->{score} <=> $a->{score} }
	     @{$tag->{'domain-specific-info'}{matches}};
      $tag->{score} = $tag->{'domain-specific-info'}{score};
    }
  }
  # add noun POSs, with plural feature depending on whether the matched variant
  # used one of the %plural_entries, and the proper feature depending on case
  for my $tag (@terms) {
    next if ($tag->{'penn-pos'}); # already have a POS
    # compute plural/singular feature
    my $plural = 0;
    my $singular = 0;
    if (exists($tag->{'domain-specific-info'}{matches})) {
      for my $m (@{$tag->{'domain-specific-info'}{matches}}) {
	if ($m->{'surely-depluralized'}) {
	  $plural = 1;
	} elsif ($m->{'maybe-depluralized'}) {
	  $plural = 1;
	  $singular = 1;
	} else {
	  $singular = 1;
	}
      }
    } else {
      $singular = 1;
    }
    $plural or $singular or die "Neither plural nor singular!";
    # compute proper/common feature
    my $proper = ($tag->{lex} =~ /[A-Z0-9]/);
    my $common = (
      (not $proper) or
      # just sentence-initial-capitalized
      ($tag->{lex} =~ /^[A-Za-z](?:[a-z\s]|$dash_re)+$/ and
       $sentence_starts{$tag->{start}})
    );
    # encode as Penn POS
    $tag->{'penn-pos'} = [
      (($common and $singular) ? ('NN')   : ()),
      (($proper and $singular) ? ('NNP')  : ()),
      (($common and $plural  ) ? ('NNS')  : ()),
      (($proper and $plural  ) ? ('NNPS') : ())
    ];
  }
  # adjust "activity" terms that GO maps via molecular_function into a tag for
  # just "activity" with the original DSI and ONT type, and remove the DSI for
  # the corresponding ONT::protein tag for the rest
  my @new_protein_terms = ();
  for my $tag (@terms) {
    if ($tag->{lex} =~ / activity$/i and
        exists($tag->{'domain-specific-info'}) and
	exists($tag->{'domain-specific-info'}{id}) and
	exists($tag->{'domain-specific-info'}{mappings}) and
	grep {
	  $_->{':through'}[1] eq 'GO::|0003674|' # molecular_function
	} @{$tag->{'domain-specific-info'}{mappings}}
        ) {
      my ($protein_start, $protein_end,    $activity_start) =
         ($tag->{start},  $tag->{end} - 9, $tag->{end} - 8);
      # adjust activity tag to just cover "activity"
      $tag->{lex} = substr($tag->{lex}, $activity_start - $tag->{start});
      $tag->{start} = $activity_start;
      # remove DSI from the corresponding protein tag
      for my $protein_tag (@terms) {
	if ($protein_tag->{start} == $protein_start and
	    $protein_tag->{end} == $protein_end and
	    exists($protein_tag->{'domain-specific-info'}) and
	    exists($protein_tag->{'domain-specific-info'}{id}) and
	    $protein_tag->{'domain-specific-info'}{id} eq
	      $tag->{'domain-specific-info'}{id}
	    ) {
	  delete $protein_tag->{'domain-specific-info'};
	}
      }
    }
  }
  return [@terms];
}

# tag protein modifications from go_protmods.obo
sub tag_protmods {
  my $entries = shift;
  our $str;
  print $protmods_out "_BEGIN_WORD_LATTICE_\n";
  print STDERR "to protmod terms:\n_BEGIN_WORD_LATTICE_\n[see above]\n" if ($debug);
  # reuse entries from main drum tagger
  print $protmods_out keys %$entries;
  # add entries for (base/derived/inflected) forms of protein modifications
  while ($str =~ /(\S+\s+)?(\S+?)at(?:es?|ed|ing|ion)\b/g) {
    my %tag = match2tag();
    my ($word1, $word2prefix) = ($1, $2);
    my $entry;
    if (defined($word1)) {
      my $word1length = length($word1);
      $word1 =~ s/\s+$/ /;
      $entry =
        "protein $word1$word2prefix" . "ation\t$tag{start}\t$tag{end}\n" .
        "protein $word2prefix" . "ation\t" . ($tag{start} + $word1length) . "\t$tag{end}\n";
    } else { # only 1 word
      $entry = "protein $word2prefix" . "ation\t$tag{start}\t$tag{end}\n";
    }
    print $protmods_out $entry;
    print STDERR $entry if ($debug);
  }
  print $protmods_out "_END_WORD_LATTICE_\n";
  print STDERR "_END_WORD_LATTICE_\n" if ($debug);
  my @terms =
    grep {
      my $tag = $_;
      # make sure the match is of the form we're looking for, and not plural
      my $keep =
	grep { $_->{matched} =~ /^protein (?:\S+\s+)?\S+ation$/ and
	       not exists($_->{'depluralization-score'})
	     } @{$tag->{'domain-specific-info'}{matches}};
      print STDERR "removing protmod tag with matches:\n" .
		   Data::Dumper->Dump([$tag->{'domain-specific-info'}{matches}],
				      ['*matches'])
	if ($debug and not $keep);
      $keep
    } @{read_from_terms2($protmods_in)};
  for my $tag (@terms) {
    # assign POS based on ending
    if ($tag->{lex} =~ /ate$/) {
      $tag->{'penn-pos'} = [qw(VB VBP)];
    } elsif ($tag->{lex} =~ /ates$/) {
      # technically could also be NNS, but it's much more likely to be VBZ here
      $tag->{'penn-pos'} = [qw(VBZ)];
    } elsif ($tag->{lex} =~ /ated$/) {
      $tag->{'penn-pos'} = [qw(VBD VBN)];
    } elsif ($tag->{lex} =~ /ating$/) {
      $tag->{'penn-pos'} = [qw(VBG)];
    } elsif ($tag->{lex} =~ /ation$/) {
      $tag->{'penn-pos'} = [qw(NN)];
    } else {
      die "WTF: $tag->{lex}";
    }
    # delete mappings to ONT::biological-process (too generic)
    $tag->{lftype} = [grep { $_ ne 'BIOLOGICAL-PROCESS' } @{$tag->{lftype}}];
    $tag->{'domain-specific-info'}{'ont-types'} =
      [grep { $_ ne 'BIOLOGICAL-PROCESS' } @{$tag->{'domain-specific-info'}{'ont-types'}}];
    $tag->{'domain-specific-info'}{mappings} =
      [grep { $_->{':to'} ne 'ONT::BIOLOGICAL-PROCESS' }
	    @{$tag->{'domain-specific-info'}{mappings}}];
  }
  return [@terms];
}

# See http://en.wikipedia.org/wiki/Amino_acid#Table_of_standard_amino_acid_abbreviations_and_properties
@amino_acids = qw(
Alanine 	Ala 	A
Arginine 	Arg 	R
Asparagine 	Asn 	N
Aspartic_acid 	Asp 	D
Cysteine 	Cys 	C
Glutamic_acid 	Glu 	E
Glutamine 	Gln 	Q
Glycine 	Gly 	G
Histidine 	His 	H
Isoleucine 	Ile 	I
Leucine 	Leu 	L
Lysine 	Lys 	K 
Methionine 	Met 	M
Phenylalanine 	Phe 	F
Proline 	Pro 	P
Serine 	Ser 	S 
Threonine 	Thr 	T
Tryptophan 	Trp 	W
Tyrosine 	Tyr 	Y
Valine 	Val 	V 	

Selenocysteine 	Sec 	U
Pyrrolysine 	Pyl 	O

Asparagine_or_aspartic_acid 	Asx 	B
Glutamine_or_glutamic_acid 	Glx 	Z
Leucine_or_Isoleucine 	Xle 	J
Unspecified_or_unknown_amino_acid 	Xaa 	X
);

my %one_letter_to_aa = ();
my %aa_to_one_letter = ();
my %three_letters_to_aa = ();
my %aa_to_aa = ();
for (my $i = 0; $i <= $#amino_acids; $i += 3) {
  my ($full, $three_letters, $one_letter) = @amino_acids[$i .. ($i+3)];
  $full =~ s/_/ /g;
  $one_letter_to_aa{$one_letter} = $full;
  $aa_to_one_letter{$full} = $one_letter;
  $three_letters_to_aa{$three_letters} = $full;
  $aa_to_aa{lc($full)} = $full;
}
my @generic_aa_words = qw(residue position site location);
for (@generic_aa_words) { $aa_to_aa{$_} = $_; }

sub amino_acid_dsi {
  my $input = shift;
  my ($name, $one_letter) = undef;
  if (length($input) == 1) {
    $name = $one_letter_to_aa{$input};
    $one_letter = $input;
  } elsif (length($input) == 3) {
    $name = $three_letters_to_aa{$input};
    $one_letter = $aa_to_one_letter{$name};
  } else {
    $name = $aa_to_aa{lc($input)};
    $one_letter = $aa_to_one_letter{$name};
  }
  return +{
    domain => 'drum',
    type => 'amino-acid',
    name => $name,
    letter => $one_letter
  }
}

sub aa_site_dsi {
  my ($aa_input, $index) = @_;
  return +{
    %{amino_acid_dsi($aa_input)},
    type => 'aa-site',
    index => $index
  }
}

# See http://www.hgmd.cf.ac.uk/docs/mut_nom.html
# for now I'm just doing the protein-level stuff, and not even all of those
sub tag_protein_sites_and_mutations {
  my ($self, @input_tags) = @_;
  @input_tags = sortTags(@input_tags);
  my @output_tags = ();
  my $prev_tag = undef;
  for my $tag (@input_tags) {
    next unless (grep { $_ eq $tag->{type} } qw(word number));
    # eliminate duplicates
    next if (defined($prev_tag) and
    	     $tag->{start} == $prev_tag->{start} and
	     $tag->{end} == $prev_tag->{end});
    $prev_tag = $tag;
    my $lftype = undef;
    my $dsi = undef;
    my $start = $tag->{start};
    my $end = $tag->{end};
    if ($tag->{lex} =~ /^([A-Z]?)([1-9]\d*)([A-Z])$/) {
      # single amino-acid substitution
      $lftype = 'MUTATION';
      $dsi = { domain => 'drum',
	       type => 'mutation',
               'sub-type' => 'substitution',
	       ( $1 ne '' ? (old => amino_acid_dsi($1)) : () ),
	       'aa-index' => $2,
	       'new' => amino_acid_dsi($3)
      };
    } elsif ($tag->{lex} =~ /^([A-Z])([1-9]\d*)del$/) {
      # single amino-acid deletion
      $lftype = 'MUTATION';
      $dsi = { domain => 'drum',
               type => 'mutation',
	       'sub-type' => 'deletion',
	       old => amino_acid_dsi($1),
	       'aa-index' => $2
      };
    } elsif ($tag->{lex} =~ /^([A-Z])([1-9]\d*)_([A-Z])([1-9]\d*)del$/ and
             $2 < $4) {
      # multiple amino-acid deletion
      $lftype = 'MUTATION';
      $dsi = { domain => 'drum',
	       type => 'mutation',
      	       'sub-type' => 'deletion',
	       lower => aa_site_dsi($1, $2),
	       upper => aa_site_dsi($3, $4)
      };
    } elsif ($tag->{lex} =~ /^([A-Z])([1-9]\d*)[_\^]([A-Z])([1-9]\d*)ins([A-Z]+)$/ and
             $2 <= $4) {
      my $upper_index = $4;
      $upper_index++ if ($upper_index == $2);
      # amino-acid insertion
      $lftype = 'MUTATION';
      $dsi = { domain => 'drum',
	       type => 'mutation',
      	       'sub-type' => 'insertion',
	       lower => aa_site_dsi($1, $2),
	       upper => aa_site_dsi($3, $upper_index),
	       'new' => [map { amino_acid_dsi($_) } split(//, $5)]
      };
    # site with single-letter amino acid abbreviation
    } elsif ($tag->{lex} =~ /^([A-Z])([1-9]\d*)$/) {
      $lftype = 'MOLECULAR-SITE';
      $dsi = aa_site_dsi($1, $2);
    # site with 3-letter amino acid abbreviation
    } elsif ($tag->{lex} =~ /^p?([A-Z][a-z][a-z])([1-9]\d*)$/ and
             exists($three_letters_to_aa{$1})) {
      $lftype = 'MOLECULAR-SITE';
      $dsi = aa_site_dsi($1, $2);
      $start++ if ($tag->{lex} =~ /^p/);
    # site with amino acid full name (and optional p prefix)
    } elsif ($tag->{lex} =~ /^p?([A-Za-z][a-z]*)([1-9]\d*)$/ and
             exists($aa_to_aa{lc($1)})) {
      $lftype = 'MOLECULAR-SITE';
      $dsi = aa_site_dsi($1, $2);
      $start++ if ($tag->{lex} =~ /^p[A-Z]/);
    # 3-letter amino acid abbreviation
    } elsif ($tag->{lex} =~ /^p?[A-Z][a-z][a-z]$/ and
             exists($three_letters_to_aa{substr($tag->{lex},-3)})) {
      $lftype = 'AMINO-ACID';
      $dsi = amino_acid_dsi(substr($tag->{lex},-3));
      $start++ if ($tag->{lex} =~ /^p/);
    # singular amino acid full name
    } elsif (exists($aa_to_aa{lc($tag->{lex})})) {
      $lftype = 'AMINO-ACID';
      $dsi = amino_acid_dsi($tag->{lex});
    # plural amino acid full name
    } elsif ($tag->{lex} =~ /s$/i and $` ne '' and
             exists($aa_to_aa{lc($`)})) {
      $lftype = 'AMINO-ACID';
      $dsi = amino_acid_dsi($`);
    # singular amino acid full name with p prefix
    } elsif ($tag->{lex} =~ /^p[A-Z]/ and
             exists($aa_to_aa{lc(substr($tag->{lex},1))})) {
      $lftype = 'AMINO-ACID';
      $dsi = amino_acid_dsi($tag->{lex});
      $start++;
    # ideally this part would be handled compositionally by the Parser, but
    # that doesn't look like it's happening soon...
    } elsif ($tag->{type} eq 'number' and # this is a number
    	     defined($output_tags[-1]) and # following...
	     $output_tags[-1]{end} + 1 == $tag->{start} and # immediately...
	     $output_tags[-1]{lftype}[0] eq 'AMINO-ACID' and # an amino-acid
	     not grep { # with only a space or dash between
	       $_->{start} == $output_tags[-1]{end} and
	       $_->{end} == $tag->{start} and
	       $_->{lex} !~ /^$dash_re$/
	     } @input_tags) {
      # add a tag for the whole thing only
      my $dsi = +{
	domain => 'drum',
	type => 'aa-site',
	'index' => $tag->{lex}
      };
      my $new_tag = +{
	type => 'sense',
	lex => $output_tags[-1]{lex} . ' ' . $tag->{lex}, # FIXME get dash
	start => $output_tags[-1]{start},
	end => $tag->{end},
	lftype => ['MOLECULAR-SITE']
      };
      # only include amino acid parts of DSI if it's a specific one
      if (grep { $_ eq lc($output_tags[-1]{lex}) } @generic_aa_words) {
	# if the word before the number is a generic AA, look for a previous
	# specific one too
	if (defined($output_tags[-2]) and
	    $output_tags[-2]{end} + 1 == $output_tags[-1]{start} and
	    $output_tags[-2]{lftype}[0] eq 'AMINO-ACID' and
	    (not grep {
	      $_->{start} == $output_tags[-2]{end} and
	      $_->{end} == $output_tags[-1]{start} and
	      $_->{lex} !~ /^$dash_re$/
	    } @input_tags) and
	    (not grep { $_ eq lc($output_tags[-2]{lex}) } @generic_aa_words)
	   ) {
	  $new_tag->{start} = $output_tags[-2]{start};
	  $new_tag->{lex} = $output_tags[-2]{lex} . ' ' . $new_tag->{lex}; # FIXME get dash
	  $dsi = +{
	    %{$output_tags[-2]{'domain-specific-info'}},
	    %$dsi
	  };
	}
      } else {
	# if the word before the number is a specific AA already, include its
	# DSI
	$dsi = +{
	  %{$output_tags[-1]{'domain-specific-info'}},
	  %$dsi
	};
      }
      $new_tag->{'domain-specific-info'} = $dsi;
      push @output_tags, $new_tag;
    }
    next unless (defined($lftype));
    push @output_tags, +{
      type => 'sense',
      lex => substr($tag->{lex}, $start - $tag->{start}), # for p prefix
      start => $start,
      end => $end,
      lftype => [$lftype],
      'domain-specific-info' => $dsi
    };
  }
  # look for substitution mutations attached to other words
  my @contiguous_sequence = ();
  for my $tag (@input_tags) {
    next unless (grep { $_ eq $tag->{type} } qw(subword subnumber));
    # clear the sequence if there's a gap
    @contiguous_sequence = ()
      if (@contiguous_sequence > 0 and 
	  $contiguous_sequence[-1]{end} != $tag->{start});
    # add the current tag to the sequence
    push @contiguous_sequence, $tag;
    # if the sequence so far ends with a number and a capital letter, tag them
    if (@contiguous_sequence >= 2 and
        $contiguous_sequence[-1]{lex} =~ /^[A-Z]$/ and
	$contiguous_sequence[-2]{lex} =~ /^[1-9]\d*/) {
      my $last_tag = $contiguous_sequence[-1];
      my $new_aa = amino_acid_dsi($contiguous_sequence[-1]{lex});
      my $aa_index = $contiguous_sequence[-2]{lex};
      my $old_aa = undef;
      my $first_tag_index = -2;
      # if there's another capital letter before, include it too
      if (@contiguous_sequence >= 3 and
	  $contiguous_sequence[-3]{lex} =~ /^[A-Z]$/) {
	# get rid of the tag we must have added for the first two parts (see below)
	pop @output_tags;
	$old_aa = amino_acid_dsi($contiguous_sequence[-3]{lex});
	$first_tag_index = -3;
      }
      my $first_tag = $contiguous_sequence[$first_tag_index];
      # concatenate the lexes of the 2-3 tags
      my $lex = join('', map { $_->{lex} } @contiguous_sequence[$first_tag_index..-1]);
      # add the tag...
      push @output_tags, +{
	type => 'sense',
	lex => $lex,
	start => $first_tag->{start},
	end => $last_tag->{end},
	lftype => ['MUTATION'],
	'domain-specific-info' => +{
	  domain => 'drum',
	  type => 'mutation',
	  'sub-type' => 'substitution',
	  ( defined($old_aa) ? (old => $old_aa) : () ),
	  'aa-index' => $aa_index,
	  'new' => $new_aa
	}
      }
	# ...unless we already tagged it as a whole word
	unless (grep { $_->{start} == $first_tag->{start} and
	               $_->{end} == $last_tag->{end}
		     } @output_tags);
    # if it ends with a capital letter and a number, tag them too
    } elsif (@contiguous_sequence >= 2 and
	     $contiguous_sequence[-2]{lex} =~ /^[A-Z]$/ and
	     $contiguous_sequence[-1]{lex} =~ /^[1-9]\d*/) {
      my $last_tag = $contiguous_sequence[-1];
      my $first_tag = $contiguous_sequence[-2];
      my $site = aa_site_dsi($first_tag->{lex}, $last_tag->{lex});
      # concatenate the lexes of the 2-3 tags
      my $lex = join('', map { $_->{lex} } @contiguous_sequence[-2..-1]);
      # add the tag...
      push @output_tags, +{
	type => 'sense',
	lex => $lex,
	start => $first_tag->{start},
	end => $last_tag->{end},
	lftype => ['MUTATION'],
	'domain-specific-info' => $site
      }
	# ...unless we already tagged it as a whole word
	unless (grep { $_->{start} == $first_tag->{start} and
	               $_->{end} == $last_tag->{end}
		     } @output_tags);
    }
  }
  # don't actually output tags for the generic amino-acid words (they're only
  # used to get generic sites)
  @output_tags = grep {
    my $lex = lc($_->{lex});
    not grep { $_ eq $lex } @generic_aa_words
  } @output_tags;
  return [@output_tags];
}

# see http://en.wikipedia.org/wiki/MicroRNA#Nomenclature
sub tag_mirnas {
  my ($self, $input_text, @input_tags) = @_;
  # scan input text for miRNAs
  my @output_tags = ();
  while ($input_text =~ /
          (?<! \pL | $dash_re )
	  (?: ( [a-z]{3,4} ) $dash_re )? # species abbrevation
	  ( mir ) # capitalization here indicates subtype
	  $dash_re?
	  ( \d+ # major ID number
	    [a-z]? # minor ID letter
	    (?: $dash_re \d+ )? # gene otherwise identical miRNAs came from
	    (?: \* | $dash_re (?: [35]p | a?s ) )? # which arm
	  )
	  (?! \pL | \d )
	/gxi) {
    my ($species_abbr, $mir, $number) = ($1, $2, $3);
#    print STDERR Data::Dumper->Dump([$species_abbr, $mir, $number],[qw(species_abbr mir number)]);
    my $species;
    if (defined($species_abbr)) {
      next unless (exists($mirna_species{$species_abbr}));
      $species = $mirna_species{$species_abbr};
      next if (defined($self->{drum_species}) and
	       not grep { my $ds = $_; $ds =~ s/ \(.*//; $ds eq $species }
	                @{$self->{drum_species}});
    }
    my $subtype;
    my $lftype;
    if ($mir eq 'miR') {
      $subtype = 'mature';
      $lftype = 'MOLECULE';
    } elsif ($mir eq 'mir') {
      $subtype = 'precursor-or-primary';
      $lftype = 'MOLECULE';
    } elsif ($mir eq 'MIR') {
      $subtype = 'gene';
      $lftype = 'GENE';
    } else {
      next;
    }
    push @output_tags, +{
      type => 'sense',
      match2tag(),
      lftype => [$lftype],
      'domain-specific-info' => +{
	domain => 'drum',
	type => 'mirna',
	'sub-type' => $subtype,
	number => $number,
	( defined($species) ? (species => $species) : () )
      }
    };
  }
  return [@output_tags];
}

# given the package of the ID symbol of a matched term, and the match structure
# returned by characterize_match and augmented with 'status' and possibly
# 'surely-depluralized' fields, return a score between 0 and 1, where 1 is an
# exact match, and 0 is the worst kind of match
sub score_match {
  my ($pkg, $m) = @_;
  print STDERR "Drum::score_match(" . Data::Dumper->Dump([$pkg, $m],[qw(pkg *m)]) . ")\n" if ($debug);
  # the part of the score due to the way the case/dash variant matched
  my $variant_score = TextTagger::Normalize::score_match($m);
  
  # penalize depluralized match; more if more than one word was depluralized
  my $depluralized = (2 - $m->{'surely-depluralized'});
  $depluralized = 0 if ($depluralized < 0);
  # ... but only if the depluralization(s) were by bad matches in Specialist
  $depluralized += $m->{'depluralization-score'}
    if ($m->{'surely-depluralized'} and exists($m->{'depluralization-score'}));

  # the part of the score due to the status (and $pkg):
  # 5 = the most official name/abbr
  # 4 = alternative name for the same exact thing
  # 3 = could be synonym or related term
  # 2 = definitely inexact synonym
  # 1 = previous name/abbr
  # 0 = fake
  my $status_score;
  if ($m->{status} =~ /^(?:NM|RecName: .*|[Nn]ame|Approved.*|I\*)$/) {
    $status_score = 5;
  } elsif ($m->{status} eq 'ID') {
    if ($pkg eq 'XFAM') {
      $status_score = 4;
    } else {
      $status_score = 5;
    }
  } elsif ($m->{status} =~ /^(?:(?:EXACT )?synonym|Alternative Name)$/) {
    $status_score = 4;
  } elsif ($m->{status} eq 'RELATED synonym') {
    if ($pkg =~ /^(?:BTO|CVCL)$/) {
      $status_score = 4;
    } else {
      $status_score = 3;
    }
  } elsif ($m->{status} =~ /^(?:NARROW|BROAD) synonym$/) {
    $status_score = 2;
  } elsif ($m->{status} =~ /^(?:SY|DE|WK|AltName: .*|Synonym)$/) {
    $status_score = 3;
  } elsif ($m->{status} =~ /^(?:PI|Previous.*)$/) {
    $status_score = 1;
  } elsif ($m->{status} eq 'fake') {
    $status_score = 0;
  } else {
    die "Unrecognized term status: $m->{status}";
  }

  print STDERR Data::Dumper->Dump([$status_score, $depluralized, $variant_score],[qw(status_score depluralized variant_score)]) if ($debug);
  my $final_score = ((((0
    ) * 6 + $status_score
    ) * 3 + $depluralized
    ) * 2 + $variant_score
    ) / 35.0; # product of the coefficients -1
  print STDERR "Drum::score_match returning $final_score\n" if ($debug);
  return $final_score;
}

push @TextTagger::taggers, {
  name => "drum",
  init_function => \&init_drum_tagger,
  tag_function => \&tag_drum_terms,
  output_types => ['sense'],
  input_text => 1,
  input_types => ['word'],
  optional_input_types => [qw(sentence prefix ending number subword subnumber punctuation pos alternate-spelling)]
};

1;

