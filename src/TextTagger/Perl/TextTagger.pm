#!/usr/bin/perl

package TextTagger;
use TripsModule::TripsModule;
@ISA = qw(TripsModule);

use TextTagger::Util qw(remove_duplicates sets_equal union intersection set_difference tree_subst lisp_intern);
use TextTagger::Escape qw(escape_for_quotes un_pipe_quote);
use TextTagger::Tags2Trips qw(tags2trips tag2tripsLattice fillGaps sortTags splitClauses splitSentences reconcileQuotationsAndSentences filter_senses_from_word_blacklist filter_sense_infos_from_penn_pos_whitelist filter_phrases_from_short_sentences);
use TextTagger::CombineTags qw(combine_tags);
use TextTagger::ExtraArgs qw(add_extra_args);
use TextTagger::Config;
use TextTagger::Gold qw(load_gold_tags);
use TextTagger::Input qw(load_input_tags);
use TextTagger::TermsInput qw(load_input_terms get_input_terms add_input_terms clear_input_terms);
use TextTagger::XMLInput qw(process_xml_tags);

use strict vars;

my $default_type = ['or', qw(affixes terms words punctuation named_entities street_addresses capitalized_names alphanumerics quotations alternate_spellings), ['and', 'stanford_pos', 'pos']];
@TextTagger::all_tag_types = qw(
  alternate-spelling
  clause
  ending
  named-entity
  number
  phrase
  pos
  prefix
  punctuation
  quotation
  sentence
  sense
  spaced-chunk
  street-address
  subnumber
  subword
  word
);
my @all_meta_map_sources = qw(
AIR ALT AOD AOT
BI
CCPSS CCS COSTAR CPM CPT CPTSP CSP CST
DDB DMDICD10 DMDUMD DSM3R DSM4 DXP
FMA
GO GS
HCDT HCPCS HCPT HHC HL7V2.5 HL7V3.0 HLREL HUGO
ICD10 ICD10AE ICD10AM ICD10AMAE ICD10DUT ICD10PCS ICD9CM ICF ICF-CY ICNP ICPC ICPC2EDUT ICPC2EENG ICPC2ICD10DUT ICPC2ICD10ENG ICPC2P ICPCBAQ ICPCDAN ICPCDUT ICPCFIN ICPCFRE ICPCGER ICPCHEB ICPCHUN ICPCITA ICPCNOR ICPCPOR ICPCSPA ICPCSWE
JABL
KCD5
LCH LNC LNC_BRADEN LNC_FLACC LNC_MDS LNC_OASIS LNC_PHQ_9 LNC_WHO
MBD MCM MDDB MDR MDRCZE MDRDUT MDRFRE MDRGER MDRITA MDRJPN MDRPOR MDRSPA MED MEDCIN MEDLINEPLUS MMSL MMX MSH MSHCZE MSHDUT MSHFIN MSHFRE MSHGER MSHITA MSHJPN MSHLAV MSHPOR MSHRUS MSHSPA MSHSWE MTH MTHCH MTHFDA MTHHH MTHHL7V2.5 MTHICD9 MTHICPC2EAE MTHICPC2ICD107B MTHICPC2ICD10AE MTHMST MTHMSTFRE MTHMSTITA MTHSPL
NAN NCBI NCI NCISEER NDDF NDFRT NEU NIC NLM-MED NOC
OMIM OMS
PCDS PDQ PNDS PPAC PSY
QMR
RAM RCD RCDAE RCDSA RCDSY RXNORM
SCTSPA SNM SNMI SNOMEDCT SPN SRC
ULT UMD USPMG UWDA
VANDF
WHO WHOFRE WHOGER WHOPOR WHOSPA);

sub get_named_taggers {
  my @tagger_names = @_;
  return map {
    my $tagger_name = lc($_);
    $tagger_name =~ s/-/_/g;
    my $tagger = (grep { $_->{name} eq $tagger_name } @TextTagger::taggers)[0];
    die "Unknown tagger: $tagger_name\nvalid taggers are: " . join(', ', map { $_->{name} } @TextTagger::taggers) . "\n"
      unless (defined($tagger));
    $tagger;
  } @tagger_names;
}

sub get_remote_meta_map {
  my $value = shift;
  if ($value =~ /^(t|true|yes)$/i) { # use default remote metamap
    { host => 'trips.ihmc.us', port => 6300 }
  } elsif ($value =~ /^(nil|false|no)$/i) { # use local metamap
    0
  } elsif ($value =~ /^([^:]+):(\d+)$/) { # HOST:PORT
    { host => $1, port => $2 }
  } else {
    die "$0: bad remote-meta-map value: $value";
  }
}

sub normalize_drum_species {
  my $species = shift;
  $species = KQML::KQMLStringAtomAsPerlString($species)
    if (KQML::KQMLAtomIsString($species));
  if ($species =~ /^(homo sapiens|human)/i) {
    return 'Homo sapiens (Human)';
  } elsif ($species =~ /^(mus musculus|mouse)/i) {
    return 'Mus musculus (Mouse)';
  } elsif ($species =~ /^(arabidopsis|cress)/i) {
    return 'Arabidopsis * (Cress)';
  } elsif ($species =~ /^(oryza|rice)/i) {
    return 'Oryza * (Rice)';
  } elsif ($species =~ /^(hordeum|barley)/i) {
    return 'Hordeum * (Barley)';
  } else {
    die "Unknown species: $species";
  }
}

sub handle_parameters {
  my $self = shift;
  my @argv = @{$self->{argv}};
  # collect init-able taggers for usage string
  my @init_taggers = ();
  for my $tagger (@TextTagger::taggers) {
    if (exists($tagger->{init_function})) {
      push @init_taggers, $tagger->{name};
    }
  }
  $self->{usage} .=
    " [-config-file filename]" .
    " [-init-taggers all|<comma-separated subset of " . join(',', sort @init_taggers) . ">]" .
    " [-dont-init-taggers <same as -init-taggers>]" . 
    " [-default-type <tag type specification>]" .
    " [-process-input-utterances <boolean>]" .
    " [-remote-meta-map boolean|HOST:PORT]" .
    " [-meta-map-sources all|<comma-separated set of UMLS source vocabulary abbreviations>]" .
    " [-drum-species all|<comma-separated set of species names>]" .
    " [-no-sense-words <comma-separated set of words>]" .
    " [-senses-only-for-penn-poss <comma-separated set of Penn POS tags>]" .
    " [-names-file <filename>]" .
    " [-terms-file <filename>]" .
    " [-xml-tags keep|remove|replace-with-spaces]" .
    " [-xml-input-rules-file <filename>]" .
    " [-parsers-must-agree <boolean>]" .
    " [-aspell-dict <filename>]";
  # defaults
  $self->{remote_meta_map} = get_remote_meta_map('t');
  $self->{meta_map_sources} = [@all_meta_map_sources];
  $self->{drum_species} = undef;
  $self->{no_sense_words} = [];
  $self->{senses_only_for_penn_poss} = 'all';
  $self->{min_sentence_length_for_phrases} = 0;
  $self->{xml_tags_mode} = 'keep';
  $self->{parsers_must_agree} = 0;
  $self->{use_wordfinder} = 1;
  $self->{allow_prefixes_for_words_only_in_wordnet} = 0;
  # process options
  eval {
  while (@argv) {
    my $opt = shift @argv;
    if ($opt eq '-config-file') {
      die "-config-file option requires an argument"
        unless (@argv > 0);
      my $filename = shift @argv;
      # get space-separated arguments from the given file, ignoring newlines
      # and comments, and allowing escaped whitespace within arguments
      open CONFIG, "<$filename" or die "Can't open config file $filename: $!";
      my @new_args = ();
      while (<CONFIG>) {
	chomp;
	s/#.*$//;
	s/(?<!\\)\$(\w+)/$ENV{$1}/ge;
	push @new_args, map { s/\\//g; $_ } split /(?<!\\)\s+/;
      }
      close CONFIG;
      unshift @argv, @new_args;
    } elsif ($opt eq '-init-taggers') {
      die "-init-taggers option requires an argument"
        unless (@argv > 0);
      my $tagger_names = lc(shift(@argv));
      if ($tagger_names eq 'all') {
	$self->{init_taggers} = [grep { exists($_->{init_function}) } @TextTagger::taggers];
      } else {
	my @tagger_names = split(',', $tagger_names);
	$self->{init_taggers} = union($self->{init_taggers} || [], [
	  grep {
	    my $tagger = $_;
	    if (exists($tagger->{init_function})) {
	      1;
	    } else {
	      print STDERR "Note: the tagger $tagger->{name} does not require initialization.\n";
	      0;
	    }
	  } get_named_taggers(@tagger_names)
	]);
      }
    } elsif ($opt eq '-dont-init-taggers') {
      die "-dont-init-taggers option requires an argument"
        unless (@argv > 0);
      my $tagger_names = lc(shift(@argv));
      if ($tagger_names eq 'all') {
	$self->{init_taggers} = set_difference($self->{init_taggers}, [grep { exists($_->{init_function}) } @TextTagger::taggers]);
      } else {
	my @tagger_names = split(',', $tagger_names);
	$self->{init_taggers} = set_difference($self->{init_taggers} || [], [
	  grep {
	    my $tagger = $_;
	    if (exists($tagger->{init_function})) {
	      1;
	    } else {
	      print STDERR "Note: the tagger $tagger->{name} does not require initialization.\n";
	      0;
	    }
	  } get_named_taggers(@tagger_names)
	]);
      }
    } elsif ($opt eq '-default-type') {
      my $default_type_string = shift(@argv);
      # while parens are unbalanced and there are more arguments
      while (($#{[$default_type_string =~ /[\(\[]/g]} !=
	      $#{[$default_type_string =~ /[\)\]]/g]}) and
	     (@argv > 0)) {
	# append the next argument to the string
	$default_type_string .= ' ' . shift(@argv);
      }
      # we also allow [] instead of () from the command line because () are
      # special to bash
      $default_type_string =~ tr/[]/()/;
      my $old_default_type = $default_type;
      $default_type = KQML::KQMLReadFromString("(tell :content $default_type_string)");
      die "Unable to parse -default-type argument: $default_type_string"
        unless (defined($default_type));
      $default_type = $default_type->[2];
      $default_type = tree_subst($old_default_type, qr/^default$/i, $default_type);
      validate_type($default_type);
    } elsif ($opt eq '-process-input-utterances') {
      $self->{process_input_utterances} = TripsModule::boolean_opt($opt, shift @argv);
    } elsif ($opt eq '-remote-meta-map') {
      my $value = shift @argv;
      $self->{remote_meta_map} = get_remote_meta_map($value);
    } elsif ($opt eq '-meta-map-sources') {
      my $sources = shift @argv;
      if (not defined($sources)) {
	die "Argument required for -meta-map-sources";
      } elsif ($sources eq 'all') {
	$self->{meta_map_sources} = [@all_meta_map_sources];
      } else {
	my @sources = split(/,/, $sources);
	for my $source (@sources) {
	  die "Unknown source vocabulary: $source"
	    unless (grep { $_ eq $source } @all_meta_map_sources);
	}
	$self->{meta_map_sources} = [@sources];
      }
    } elsif ($opt eq '-drum-species') {
      my $species = shift @argv;
      if (not defined($species)) {
	die "Argument required for -drum-species";
      } elsif ($species eq 'all') {
	$self->{drum_species} = undef;
      } else {
	my @species = split(/,/, $species);
	$self->{drum_species} = [];
	for my $species (@species) {
	  push @{$self->{drum_species}}, normalize_drum_species($species);
	}
      }
    } elsif ($opt eq '-no-sense-words') {
      my $no_sense_words = shift @argv;
      die "Argument required for -no-sense-words"
        if (not defined($no_sense_words));
      $self->{no_sense_words} = [map { lc($_) } split(/,/, $no_sense_words)];
    } elsif ($opt eq '-senses-only-for-penn-poss') {
      my $penn_poss = shift @argv;
      die "Argument required for -senses-only-for-penn-poss"
        if (not defined($penn_poss));
      $self->{senses_only_for_penn_poss} = 
	($penn_poss eq 'all' ? 'all' : [split(/,/, $penn_poss)]);
    } elsif ($opt eq '-min-sentence-length-for-phrases') {
      my $min = shift @argv;
      die "Nonnegative integer argument required for -min-sentence-length-for-phrases"
        if (not defined($min) or $min !~ /^\d+$/);
      $self->{min_sentence_length_for_phrases} = $min;
    } elsif ($opt eq '-names-file') {
      my $names_file = shift @argv;
      die "Argument required for -names-file"
        if (not defined($names_file));
      $self->{names_file} = $names_file;
    } elsif ($opt eq '-terms-file') {
      my $terms_file = shift @argv;
      die "Argument required for -terms-file"
        if (not defined($terms_file));
      $self->{terms_file} = $terms_file;
    } elsif ($opt eq '-xml-tags') {
      my $xml_tags_mode = lc(shift @argv);
      die "Argument required for -xml-tags"
        if (not defined($xml_tags_mode));
      $xml_tags_mode =~ s/_/-/g;
      die "-xml-tags argument must be one of keep, remove, or replace-with-spaces"
        unless (grep { $_ eq $xml_tags_mode }
		qw(keep remove replace-with-spaces));
      $self->{xml_tags_mode} = $xml_tags_mode;
    } elsif ($opt eq '-xml-input-rules-file') {
      my $rules_file = shift @argv;
      die "Argument required for -xml-input-rules-file"
        if (not defined($rules_file));
      $self->{xml_input_rules_file} = $rules_file;
    } elsif ($opt eq '-parsers-must-agree') {
      $self->{parsers_must_agree} = TripsModule::boolean_opt($opt, shift @argv);
    } elsif ($opt eq '-use-wordfinder') {
      $self->{use_wordfinder} = TripsModule::boolean_opt($opt, shift @argv);
    } elsif ($opt eq '-aspell-dict') {
      my $aspell_dict = shift @argv;
      die "Argument required for -aspell-dict"
        if (not defined($aspell_dict));
      $self->{aspell_dict} = $aspell_dict;
    } elsif ($opt eq '-allow-prefixes-for-words-only-in-wordnet') {
      $self->{allow_prefixes_for_words_only_in_wordnet} =
        TripsModule::boolean_opt($opt, shift @argv);
    } elsif ($opt eq '') {
      # ignore (we get this from a bug in bash 3.2)
    } else {
      die "Unknown option: '$opt'";
    }
  }
  1 } || die "$@\n$self->{usage}\n";
}

sub init
{
  my $self = shift;
  $self->{name} = "TextTagger";
  $self->SUPER::init();
  $self->handle_parameters();
  $self->{next_uttnum} = 0;
  $self->{next_paragraph_ID} = 0;
  print STDERR "\${^UNICODE}=${^UNICODE}\n\$^V=${^V}\n" if ($self->{debug});

  # initialize those taggers that need it
  for my $tagger (@{$self->{init_taggers}})
  {
    print STDERR "Initializing " . $tagger->{name} . "...\n"
      if ($self->{debug});
    &{$tagger->{init_function}}($self);
  }
  # wait for them to be ready if necessary
  for my $tagger (@{$self->{init_taggers}})
  {
    if (exists($tagger->{ready_function})) {
      print STDERR "Waiting for " . $tagger->{name} . " to be ready...\n"
	if ($self->{debug});
      &{$tagger->{ready_function}}($self);
    }
  }
  print STDERR "Done initializing taggers.\n" if ($self->{debug});
  
  $SIG{INT} = $SIG{QUIT} = sub {
    $self->fini();
    exit;
  };

  # explicit request to tag some text
  $self->send_msg("(subscribe :content (request &key :content (tag . *)))");
  if ($self->{process_input_utterances})
  {
    # implicit tagging of input tell messages
    # (don't bother subscribing to the other input tell messages, since we send
    # those when we get (tell :content (utterance . *)) )
    $self->send_msg("(subscribe :content (tell &key :content (utterance . *)))");
  }
}

# does the given string name a type of tag we know about?
sub is_tag_type
{
  my $type = lc(shift);
  if (grep { grep { $_ eq $type } @{$_->{output_types}} } @TextTagger::taggers)
  {
    return 1;
  } else
  {
    return 0;
  }
}

# does the given string name a tagger we know about?
sub is_tagger
{
  my $type = lc(shift);
  $type =~ s/-/_/g;
  if (grep { $_->{name} eq $type } @TextTagger::taggers)
  {
    return 1;
  } else
  {
    return 0;
  }
}

# die with an informative message unless the given type expression valid, given the tag types and taggers we know about
sub validate_type
{
  my $type = shift;
  if (ref($type) eq 'ARRAY')
  {
    die "Lists in type expressions must have at least two elements: " . KQML::KQMLAsString($type)
      unless (@$type >= 2);
    my $op = lc($type->[0]);
    if ($op eq 'type')
    {
      die "This type expression should have exactly two elements: " . KQML::KQMLAsString($type)
        unless (@$type == 2);
      die "$type->[1] is not a valid tag type"
        unless (is_tag_type($type->[1]));
    } elsif ($op eq 'source')
    {
      die "This type expression should have exactly two elements: " . KQML::KQMLAsString($type)
        unless (@$type == 2);
      die "$type->[1] is not a valid tag source"
        unless (is_tagger($type->[1]));
    } elsif ($op =~ /^(?:and|or)$/)
    {
      for my $t (@{$type}[1..$#$type])
      { 
	validate_type($t);
      }
    } elsif ($op eq 'not')
    {
      die "This type expression should have exactly two elements: " . KQML::KQMLAsString($type)
        unless (@$type == 2);
      validate_type($type->[1]);
    } else
    {
      die "Invalid type expression op: $op";
    }
  } else # $type is a symbol
  {
    die "'$type isn't the name of a tagger, a tag type, 'all, or 'default."
      unless ($type =~ /^(?:all|default)$/i or is_tagger($type) or is_tag_type($type));
  }
}

# FIXME type_matches_tagger and type_matches_tag are very similar; perhaps they should be one function with different behavior depending on the type of the second argument? But it's always a hashref...

# does the given type expression match the given tagger?
sub type_matches_tagger
{
  my ($type, $tagger) = @_;
  if (ref($type) eq 'ARRAY')
  {
    die "Invalid type expression: (" . join(' ', @$type) . ")"
      unless (@$type >= 2);
    my $op = lc($type->[0]);
    if ($op eq 'type')
    {
      die "Invalid type expression: (" . join(' ', @$type) . ")"
	unless (@$type == 2);
      if (grep { $_ eq lc($type->[1]) } @{$tagger->{output_types}}) {
	return 1;
      } else {
	return 0;
      }
    } elsif ($op eq 'source')
    {
      die "Invalid type expression: (" . join(' ', @$type) . ")"
	unless (@$type == 2);
      my $source = lc($type->[1]);
      $source =~ s/-/_/g;
      return ($source eq $tagger->{name});
    } elsif ($op eq 'and')
    {
      for my $arg ((@$type)[1..$#$type])
      {
	return 0 unless (type_matches_tagger($arg, $tagger));
      }
      return 1;
    } elsif ($op eq 'or')
    {
      for my $arg ((@$type)[1..$#$type])
      {
	return 1 if (type_matches_tagger($arg, $tagger));
      }
      return 0;
    } elsif ($op eq 'not')
    {
      die "Invalid type expression: (" . join(' ', @$type) . ")"
	unless (@$type == 2);
      return not type_matches_tagger($type->[1], $tagger);
    } else
    {
      die "Invalid type expression operation: $op";
    }
  } else # $type is a string/symbol
  {
    $type = lc($type);
    if ($type eq 'all')
    {
      return 1;
    } elsif ($type eq 'default')
    {
      return type_matches_tagger($default_type, $tagger);
    } else
    {
      return type_matches_tagger(['or', ['type', $type], ['source', $type]],
                                 $tagger);
    }
  }
}

# does the given type expression match the given tag?
sub type_matches_tag
{
  my ($type, $tag) = @_;
  if (ref($type) eq 'ARRAY')
  {
    die "Invalid type expression: (" . join(' ', @$type) . ")"
      unless (@$type >= 2);
    my $op = lc($type->[0]);
    if ($op eq 'type')
    {
      die "Invalid type expression: (" . join(' ', @$type) . ")"
	unless (@$type == 2);
      return (lc($type->[1]) eq $tag->{type});
    } elsif ($op eq 'source')
    {
      die "Invalid type expression: (" . join(' ', @$type) . ")"
	unless (@$type == 2);
      my $source = lc($type->[1]);
      $source =~ s/-/_/g;
      return ($source eq $tag->{source});
    } elsif ($op eq 'and')
    {
      for my $arg ((@$type)[1..$#$type])
      {
	return 0 unless (type_matches_tag($arg, $tag));
      }
      return 1;
    } elsif ($op eq 'or')
    {
      for my $arg ((@$type)[1..$#$type])
      {
	return 1 if (type_matches_tag($arg, $tag));
      }
      return 0;
    } elsif ($op eq 'not')
    {
      die "Invalid type expression: (" . join(' ', @$type) . ")"
	unless (@$type == 2);
      return not type_matches_tag($type->[1], $tag);
    } else
    {
      die "Invalid type expression operation: $op";
    }
  } else # $type is a string/symbol
  {
    $type = lc($type);
    if ($type eq 'all')
    {
      return 1;
    } elsif ($type eq 'default')
    {
      return type_matches_tag($default_type, $tag);
    } else
    {
      return type_matches_tag(['or', ['type', $type], ['source', $type]],
                              $tag);
    }
  }
}

sub tagger_must_input_type {
  my ($tagger, $type) = @_;
  exists($tagger->{input_types}) and
  grep { $_ eq $type } @{$tagger->{input_types}};
}

sub all_tagger_input_types {
  my $tagger = shift;
  return
    ( (exists($tagger->{input_types}) ? @{$tagger->{input_types}} : ()),
      (exists($tagger->{optional_input_types}) ?
        @{$tagger->{optional_input_types}} : ())
    );
}

sub tagger_may_input_type {
  my ($tagger, $type) = @_;
  grep { $_ eq $type } all_tagger_input_types($tagger);
}

sub tagger_may_output_type {
  my ($tagger, $type) = @_;
  grep { $_ eq $type } @{$tagger->{output_types}};
}

# is the tagger initilialized if it's initializable?
sub can_use_tagger {
  my ($self, $tagger) = @_;
  my $ret =
    ( (not exists($tagger->{init_function})) or
      grep { $_->{name} eq $tagger->{name} } @{$self->{init_taggers}}
    );
  print STDERR "can't use tagger " . $tagger->{name} . " because it's not initialized\n"
    if ($self->{debug} and not $ret);
  return $ret;
}

sub tag_all
{
  my ($self, $text, $type) = @_;

  validate_type($type);

  # get all the types and taggers we need
  print STDERR "Getting which taggers are needed...\n"
    if ($self->{debug});
  my @types_and_taggers_needed =
     grep {
       my $tmt = (type_matches_tagger($type, $_) and can_use_tagger($self, $_));
       print STDERR "adding tagger $_->{name} because it matches the type\n"
         if ($tmt and $self->{debug});
       $tmt
     } @TextTagger::taggers;
  my $added = 1;
  while ($added)
  {
    $added = 0;
    for my $type_or_tagger (@{[@types_and_taggers_needed]}) {
      if (ref($type_or_tagger) eq 'HASH') { # a tagger
	if (exists($type_or_tagger->{input_types})) {
	  for my $type (@{$type_or_tagger->{input_types}}) {
	    # unless we already have the type, or a tagger that outputs it
	    unless (grep {
	              my $tort = $_;
		      if (ref($tort) eq 'HASH') { # tagger
			tagger_may_output_type($tort, $type);
		      } else { # type
	                $tort eq $type;
		      }
		    } @types_and_taggers_needed) {
	      print STDERR "adding type $type because tagger $type_or_tagger->{name} needs it\n" if ($self->{debug});
	      push @types_and_taggers_needed, $type;
	      $added = 1;
	    }
	  }
	}
      } else { # a type
	for my $tagger (@TextTagger::taggers) {
	  if (tagger_may_output_type($tagger, $type_or_tagger) and
	      (not grep { $_ eq $tagger } @types_and_taggers_needed) and
	      can_use_tagger($self, $tagger)
	      )
	  {
	    print STDERR "adding tagger $tagger->{name} because it outputs tags of type $type_or_tagger\n" if ($self->{debug});
	    push @types_and_taggers_needed, $tagger;
	    $added = 1;
	  }
	}
      }
    }
  }

  # also add any relevant input types we haven't added yet
  my @more_types = ();
  my @unneeded_types = ();
  for my $tagger (@types_and_taggers_needed) {
    next unless (ref($tagger) eq 'HASH' and
		 exists($tagger->{optional_input_types}));
    for my $type (all_tagger_input_types($tagger)) {
      if (# we don't have this type already...
	  (not grep { $_ eq $type }
		    (@more_types, @unneeded_types, @types_and_taggers_needed))
         ) {
        if (# ... but some tagger we do already have provides it
            grep { my $other_tagger = $_;
                   ref($other_tagger) eq 'HASH' and
                   tagger_may_output_type($other_tagger, $type)
                 } @types_and_taggers_needed
           ) {
          print STDERR "adding type $type because tagger $tagger->{name} wants it, and we already have taggers that output it\n" if ($self->{debug});
          push @more_types, $type;
        } else {
          print STDERR "don't need type $type because it's only optional for tagger $tagger->{name}, and we don't already have taggers that output it\n" if ($self->{debug});
          push @unneeded_types, $type;
        }
      }
    }
  }
  push @types_and_taggers_needed, @more_types;

  # get the taggers in dependency order
  print STDERR "Sorting taggers...\n" if ($self->{debug});
  my @sorted_taggers = ();
  my @sorted_types = ();
  while (@types_and_taggers_needed)
  {
#    print STDERR "\@tatn = " . join(', ', map { ref($_) eq 'HASH' ? "{$_->{name}}" : $_ } @types_and_taggers_needed) . "\n\@st = " . join(', ', @sorted_types) . "\n" if ($self->{debug});
    my $type_or_tagger = shift @types_and_taggers_needed;
    if (ref($type_or_tagger) eq 'HASH') # a tagger
    {
      my @input_types_still_needed =
        grep { my $type = $_;
	       (not ref($type)) and 
	       tagger_may_input_type($type_or_tagger, $type)
	     } @types_and_taggers_needed;
      if (@input_types_still_needed)
      { # we still need some input types for this tagger,
        # so add it back to the list
	print STDERR "- tagger $type_or_tagger->{name} still needs some input types: " . join(', ', @input_types_still_needed) . "\n" if ($self->{debug});
	push @types_and_taggers_needed, $type_or_tagger;
      } else
      { # all the input types this tagger needs are accounted for,
        # so add it to the sorted list
	print STDERR "+ tagger $type_or_tagger->{name} has all its input types\n" if ($self->{debug});
        push @sorted_taggers, $type_or_tagger;
      }
    } else # a type
    {
      my $type_done = 1;
      my @loopy_taggers_to_add = ();
      for my $unsorted_tagger (@types_and_taggers_needed) {
	next unless (ref($unsorted_tagger) eq 'HASH');
	if (tagger_may_output_type($unsorted_tagger, $type_or_tagger))
	{ # we still need $unsorted_tagger to output this type
	  if ( # ... but it's a loopy tagger WRT this type
	       tagger_may_input_type($unsorted_tagger, $type_or_tagger) and
	       # ... and we already have all its non-loopy input types
	       not @{set_difference(
	         [all_tagger_input_types($unsorted_tagger)],
		 [@unneeded_types, @sorted_types,
                  @{$unsorted_tagger->{output_types}}]
	       )}
	     )
	  {
	    push @loopy_taggers_to_add, $unsorted_tagger;
	  } else { # not an addable loopy tagger
	    $type_done = 0;
	    last;
	  }
	}
      }
      if ($type_done) {
	if (@loopy_taggers_to_add) {
	  print STDERR "+ tagger(s) " . join(', ', map { $_->{name} } @loopy_taggers_to_add) . " both input(s) and output(s) type $type_or_tagger, but need(s) no more input types\n" if ($self->{debug});
	  # move addable loopy taggers from @tatn to @sorted_taggers
	  @types_and_taggers_needed =
	    @{set_difference(\@types_and_taggers_needed,
	    		     \@loopy_taggers_to_add)};
	  push @sorted_taggers, @loopy_taggers_to_add;
	}
	print STDERR "+ type $type_or_tagger has all taggers outputting it\n" if ($self->{debug});
	# mark this type as done
	push @sorted_types, $type_or_tagger;
      } else
      { # not done, put it back on the end of the queue
        print STDERR "- type $type_or_tagger still needs a tagger outputting it\n" if ($self->{debug});
	push @types_and_taggers_needed, $type_or_tagger;
      }
    }
  }

# we do this as we're adding the taggers now  
#  # remove taggers we didn't initialize
#  print STDERR "Removing uninitialized taggers...\n" if ($self->{debug});
#  @sorted_taggers = grep { my $tagger = $_;
#			   (not exists($tagger->{init_function})) or
#                           grep { $_ == $tagger } @{$self->{init_taggers}}
#			 } @sorted_taggers;

  # run all the taggers
  my @tags = ();
  for my $tagger (@sorted_taggers)
  {
    my $name = $tagger->{name};

    # run the tagger
    print STDERR "Running tagger $name\n" if ($self->{debug});
    my @new_tags =
      @{&{$tagger->{tag_function}}(
        $self, 
	( $tagger->{input_text} ? ($text) : () ),
	( ($tagger->{input_types} or $tagger->{optional_input_types}) ?
	  @tags : ()))
      };
    
    # set the source of all the new tags
    for my $tag (@new_tags)
    {
      $tag->{source} = $name;
    }

    # add the new tags
    push @tags, @new_tags;
  }

  # get only the tags we originally requested
  print STDERR "Filtering tags...\n" if ($self->{debug});
  @tags = grep { type_matches_tag($type, $_) } @tags;

  print STDERR "done tagging\n" if ($self->{debug});
  print STDERR Data::Dumper->Dump([\@tags],['*tags']) if ($self->{debug});

  # check for bogus tags
  for (@tags) {
    die "Bogus tag: " . Data::Dumper->Dump([$_],['*bogus'])
      if ($_->{start} > $_->{end});
  }

  return [@tags];
}

sub getTripsTags
{
  my ($self, $tags, $msgContent) = @_;
  my $format = ($msgContent->{':format'} || 'lattice');
  if ($self->{min_sentence_length_for_phrases}) {
    @$tags =
      filter_phrases_from_short_sentences(
        $self->{min_sentence_length_for_phrases}, @$tags
      );
  }
  @$tags = filter_senses_from_word_blacklist($self->{no_sense_words}, @$tags);
  if ($format =~ /lattice/i) {
    @$tags = combine_tags($self, @$tags);
    if (ref($self->{senses_only_for_penn_poss}) eq 'ARRAY') {
      @$tags =
        filter_sense_infos_from_penn_pos_whitelist(
	  $self->{senses_only_for_penn_poss}, @$tags
	);
    }
  }
  @$tags = fillGaps(@$tags)
    if ((!exists($msgContent->{':fill-gaps'})) ||
	$msgContent->{':fill-gaps'} =~ /t/i);
  @$tags = sortTags(@$tags);
  if (exists($msgContent->{':extra-tag-args'})) {
    return [map { [@$_, @{$msgContent->{':extra-tag-args'}}] } 
                tags2trips($tags, $format)];
  } else {
    return [tags2trips($tags, $format)];
  }
}

# Send a tell message given its :content argument.
# Also collects contents in an array instead of sending them immediately, if
# the array exists.
sub send_tell
{
  my ($self, $content) = @_;
  if (!ref($content)) {
    $content = KQML::KQMLReadFromString($content);
  }
  if (exists($self->{messages_to_send})) {
    push @{$self->{messages_to_send}}, $content;
  } else {
    $self->send_msg(['tell', ':content', $content]);
  }
}

sub tellEachTag
{
  my ($self, $chunkTag, $tripsTags) = @_;
  if (@$tripsTags) {
    for my $tripsTag (@$tripsTags)
    {
      $self->send_tell($tripsTag);
    }
  } else {
    # no tags to tell; instead just output a "word" for the chunk and let the
    # Parser figure it out (this probably happened because the tag type was
    # (not all), meaning TextTagger shouldn't do anything).
    # FIXME should we only do this if :format lattice?
    # FIXME this is missing :extra-tag-args, but the Parser ignores them anyway
    # for word messages.
    $self->send_tell(tag2tripsLattice({ %$chunkTag, type => 'word' }))
  }
}

sub imitateKeyboardManagerUtterance
{
  my ($self, $clauseTag, $tripsTags, $extraTagArgs) = @_;
  my ($text, $uttnum) = ($clauseTag->{text}, $clauseTag->{uttnum});
  $self->send_tell(['started-speaking', @$extraTagArgs, ':uttnum',$uttnum]);
  $self->send_tell(['stopped-speaking', @$extraTagArgs, ':uttnum',$uttnum]);
  $self->tellEachTag($clauseTag, $tripsTags);
  $self->send_tell(['utterance', @$extraTagArgs, ':uttnum',$uttnum, ':text','"' . escape_for_quotes($text) . '"']);
}

sub receive_request
{
  my ($self, $msg, $content) = @_;
  $content->{verb} = lc($content->{verb});
  eval {
    if ($content->{verb} eq 'tag')
    {
      my $text = $content->{':text'};
      die "'tag' request is missing a :text string"
	unless (KQML::KQMLAtomIsString($text));
      $text = KQML::KQMLStringAtomAsPerlString($text);

      load_gold_tags($text, $content->{':sense-bracketing'});
      load_input_tags($content->{':input-tags'});
      load_input_terms($content->{':input-terms'})
        if (exists($content->{':input-terms'}));
      
      my $type = 'default';
      $type = $content->{':type'}
	if (exists($content->{':type'}));
      
      # FIXME kind of a hack to get an extra argument into MetaMap
      my $old_meta_map_sources = $self->{meta_map_sources};
      if (not defined($content->{':meta-map-sources'})) {
        # do nothing
      } elsif (lc($content->{':meta-map-sources'}) eq 'all') {
        $self->{meta_map_sources} = \@all_meta_map_sources;
      } elsif (ref($content->{':meta-map-sources'}) eq 'ARRAY') {
	my @sources = map { un_pipe_quote($_) }
	                  @{$content->{':meta-map-sources'}};
	for my $source (@sources) {
	  die "Unknown source vocabulary: $source\n"
	    unless (grep { $_ eq $source } @all_meta_map_sources);
	}
	$self->{meta_map_sources} = \@sources;
      } else {
	die "Invalid value for :meta-map-sources argument: " . KQML::KQMLAsString($content->{':meta-map-sources'});
      }
      # FIXME ditto for Drum
      my $old_drum_species = $self->{drum_species};
      if (not defined($content->{':drum-species'})) {
        # do nothing
      } elsif (lc($content->{':drum-species'}) eq 'all') {
        $self->{drum_species} = undef;
      } elsif (ref($content->{':drum-species'}) eq 'ARRAY') {
	$self->{drum_species} =
	  [map { normalize_drum_species($_) } @{$content->{':drum-species'}}];
      } else {
	die "Invalid value for :drum-species argument: " . KQML::KQMLAsString($content->{':drum-species'});
      }
      # FIXME? ditto for CombineTags (this is becoming a pattern...)
      my $old_use_wordfinder = $self->{use_wordfinder};
      if (not defined($content->{':use-wordfinder'})) {
	# do nothing
      } elsif ($content->{':use-wordfinder'} =~ /t/i) {
	$self->{use_wordfinder} = 1;
      } else {
	$self->{use_wordfinder} = 0;
      }

      my $split_chunks =
	($content->{':split-clauses'} =~ /t/i or
	 $content->{':split-sentences'} =~ /t/i);

      my $paragraph = 0;
      $paragraph = 1
	if (((not exists($content->{':paragraph'})) and
	     $content->{':imitate-keyboard-manager'} =~ /t/i and
	     $split_chunks
	    ) or
	    $content->{':paragraph'} =~ /t/i
	   );

      if (exists($content->{':next-uttnum'})) {
	$self->{next_uttnum} = $content->{':next-uttnum'};
      } elsif ($paragraph) {
	$self->{next_uttnum} = 0;
      }
      
      # add dependencies
      $type = ['or', 'clauses', $type]
	if ($content->{':split-clauses'} =~ /t/i);
      # FIXME need a type intersection function to check whether we would
      # get sentence tags, rather than just listing everything that could
      # give us sentence tags
      # except that probably wouldn't work because the *Input taggers can
      # output any kind of tags, including sentence tags, but none are
      # guaranteed
      my $sentences_tagger = 
	[grep { $_->{name} eq 'sentences' } @TextTagger::taggers]->[0];
      my $tmst = type_matches_tagger($type, $sentences_tagger);
      my $ospl_tagger =
	[grep { $_->{name} eq 'one_sentence_per_line' } @TextTagger::taggers]->[0];
      my $tmosplt = type_matches_tagger($type, $ospl_tagger);
      my $cnlp_tagger =
	[grep { $_->{name} eq 'stanford_core_nlp' } @TextTagger::taggers]->[0];
      my $tmcnlpt = ( can_use_tagger($self, $cnlp_tagger) and
		      # SCNLP can output other things besides sentences, and
		      # the user might have selected only some of those things
		      # and excluded sentences, so we need to test against a
		      # fake tag rather than $cnlp_tagger itself:
      		      type_matches_tag($type, { type => 'sentence', source => 'stanford_core_nlp' }) );
#      		      type_matches_tagger($type, $cnlp_tagger) );
      print STDERR Data::Dumper->Dump(
        [$sentences_tagger,$tmst,$ospl_tagger,$tmosplt,$cnlp_tagger,$tmcnlpt],
	[qw(sentences_tagger tmst ospl_tagger tmosplt cnlp_tagger tmcnlpt)]
      ) if ($self->{debug});
      $type = ['or', 'sentences', $type]
        if (($content->{':imitate-keyboard-manager'} =~ /t/i or
	     $split_chunks) and
	    not ($tmst or $tmosplt or $tmcnlpt)
	   );
      
      # FIXME if xml_tags_mode is 'remove', this might mess up gold/input tags
      $text = process_xml_tags($self, $text)
	# if the mode isn't 'keep' or we're going to use XMLInput
        if ($self->{xml_tags_mode} ne 'keep' or
	    type_matches_tagger($type,
	        grep { $_->{name} eq 'xml_input' } @TextTagger::taggers));

      my $tags = $self->tag_all($text, $type);
      $tags = reconcileQuotationsAndSentences($tags);
      # only increment uttnum in add_extra_args if we might output multiple
      # utterance messages
      my $inc_uttnum = $split_chunks;
      my $extra_tag_args = KQML::KQMLKeywordify($content->{':extra-tag-args'});
      if (exists($extra_tag_args->{':uttnum'})) {
	die "':split-* t' and ':extra-tag-args (:uttnum *)' are incompatible"
	  if ($inc_uttnum);
	$self->{next_uttnum} = $extra_tag_args->{':uttnum'};
      }
      @$tags = sortTags(add_extra_args($self, $inc_uttnum, @$tags));
      if ($content->{':imitate-keyboard-manager'} =~ /t/i or
          $split_chunks)
      {
	push @{$content->{':extra-tag-args'}}, qw(:channel Desktop)
	  unless (exists($extra_tag_args->{':channel'}));
	push @{$content->{':extra-tag-args'}}, qw(:direction input)
	  unless (exists($extra_tag_args->{':direction'}));
	
	my $paragraph_ID = undef;
	$paragraph_ID = "paragraph" . ($self->{next_paragraph_ID}++)
	  if ($paragraph);

	my @chunks = ();
	if ($content->{':split-clauses'} =~ /t/i)
	{
	  @chunks = splitClauses($tags);
	} elsif ($content->{':split-sentences'} =~ /t/i)
	{ # like :split-clauses, but coarser
	  @chunks = splitSentences($tags);
	} else
	{ # one big sentence
	  @chunks = ([
	    { 'text' => $text,
	      'uttnum' => ($self->{next_uttnum}++)
	    },
	    $tags
	  ]);
	}

	# don't actually send individual messages unless :ikm t; rather gather
	# them into this array
	$self->{messages_to_send} = []
	  unless ($content->{':imitate-keyboard-manager'} =~ /t/i);

	$self->send_tell("(start-paragraph :id $paragraph_ID)")
	  if (defined($paragraph_ID));
	# Get all the trips-format tags for the whole paragraph first, since
	# getTripsTags can send is-defined-word requests to LXM, and those must
	# not happen at the same time as the Parser is calling functions in LXM
	# to process messages from the previous chunk. See drum:#183.
	my @trips_tagses = ();
	for my $chunk (@chunks)
	{
	  my ($chunkTag, $tagsInChunk) = @$chunk;
	  push @trips_tagses, $self->getTripsTags($tagsInChunk, $content);
	}
	for my $chunk (@chunks)
	{
	  my ($chunkTag, $tagsInChunk) = @$chunk;
	  my $tripsTags = shift @trips_tagses;
	  $self->imitateKeyboardManagerUtterance($chunkTag, $tripsTags, $content->{':extra-tag-args'});
	}
	$self->send_tell("(end-paragraph :id $paragraph_ID)")
	  if (defined($paragraph_ID));

	if ($content->{':imitate-keyboard-manager'} =~ /t/i) {
	  my @uttnums = @{remove_duplicates([map { $_->[0]{uttnum} } @chunks])};
	  $self->reply_to_msg($msg, "(reply :content (ok :uttnums (" . join(' ', @uttnums) . ")))");
	} else {
	  my $msgs = $self->{messages_to_send};
	  delete $self->{messages_to_send};
	  $self->reply_to_msg($msg, ['reply', ':content', $msgs]);
	}
      } elsif ($content->{':tell-each-tag'} =~ /t/i)
      {
	my $chunkTag = { start => 0, end => length($text), text => $text };
	my $tripsTags = $self->getTripsTags($tags, $content);
	$self->tellEachTag($chunkTag, $tripsTags);
      } else
      {
	my $tripsTags = $self->getTripsTags($tags, $content);
	$self->reply_to_msg($msg, "(reply :content " . KQML::KQMLAsString($tripsTags) . ")");
      }

      # FIXME hack (see above)
      $self->{meta_map_sources} = $old_meta_map_sources;
      $self->{drum_species} = $old_drum_species;
      $self->{use_wordfinder} = $old_use_wordfinder;
    } elsif ($content->{verb} eq 'set-parameters') {
      for my $keyword (grep /^:/, keys %$content) {
	my $value = $content->{$keyword};
	if ($keyword eq ':default-type') {
	  $value = tree_subst($default_type, qr/^default$/i, $value);
	  validate_type($value);
	  $default_type = $value;
	} elsif ($keyword eq ':process-input-utterances') {
	  $self->{process_input_utterances} = TripsModule::boolean_opt($keyword, $value);
	} elsif ($keyword eq ':remote-meta-map') {
	  $value = KQML::KQMLStringAtomAsPerlString($value)
	    if (KQML::KQMLAtomIsString($value));
	  $self->{remote_meta_map} = get_remote_meta_map($value);
	  print STDERR "Note: :remote-meta-map won't take effect until the meta-map tagger is next initialized\n";
	} elsif ($keyword eq ':meta-map-sources') {
	  $self->{meta_map_sources} =
	    (($value =~ /^all$/i) ?
	      \@all_meta_map_sources
	      : [map { uc($_) } @$value]);
	} elsif ($keyword eq ':drum-species') {
	  $self->{drum_species} =
	    (($value =~ /^all$/i) ?
	      undef
	      : [map { normalize_drum_species($_) } @$value]);
	} elsif ($keyword eq ':no-sense-words') {
	  # fix pipequoted symbols with spaces in them being split into
	  # multiple symbols by KQML.pm
	  my @fixed = ();
	  my $in_pipe_quote = 0;
	  for (@$value) {
	    if (/^\|/ and not /\|$/) {
	      $in_pipe_quote = 1;
	      push @fixed, $_;
	    } elsif ($in_pipe_quote) {
	      $fixed[-1] .= ' ' . $_;
	      $in_pipe_quote = 0 if (/\|$/);
	    } else {
	      push @fixed, $_;
	    }
	  }
	  $self->{no_sense_words} = [map { lc(un_pipe_quote($_)) } @fixed];
	} elsif ($keyword eq ':senses-only-for-penn-poss') {
	  $self->{senses_only_for_penn_poss} = $value;
	} elsif ($keyword eq ':min-sentence-length-for-phrases') {
	  $self->{min_sentence_length_for_phrases} = $value;
	} elsif ($keyword eq ':names-file') {
	  $value = KQML::KQMLStringAtomAsPerlString($value)
	    if (KQML::KQMLAtomIsString($value));
	  $self->{names_file} = $value;
	} elsif ($keyword eq ':terms-file') {
	  $value = KQML::KQMLStringAtomAsPerlString($value)
	    if (KQML::KQMLAtomIsString($value));
	  $self->{terms_file} = $value;
	} elsif ($keyword eq ':xml-tags') {
	  $value = lc($value);
	  $value =~ s/_/-/g;
	  die ":xml-tags argument must be one of keep, remove, or replace-with-spaces"
	    unless (grep { $_ eq $self->{xml_tags_mode} }
		    qw(keep remove replace-with-spaces));
	  $self->{xml_tags_mode} = $value;
	} elsif ($keyword eq ':xml-input-rules-file') {
	  $value = KQML::KQMLStringAtomAsPerlString($value)
	    if (KQML::KQMLAtomIsString($value));
	  $self->{xml_input_rules_file} = $value;
	} elsif ($keyword eq ':parsers-must-agree') {
	  $self->{parsers_must_agree} = TripsModule::boolean_opt($keyword, $value);
	} elsif ($keyword eq ':use-wordfinder') {
	  $self->{use_wordfinder} = TripsModule::boolean_opt($keyword, $value);
	} elsif ($keyword eq ':aspell-dict') {
	  if ($value =~ /^nil$/i) {
	    $value = undef;
	  } elsif (KQML::KQMLAtomIsString($value)) {
	    $value = KQML::KQMLStringAtomAsPerlString($value);
	  }
	  $self->{aspell_dict} = $value;
	} else {
	  die "Unknown parameter: $keyword";
	}
      }
      $self->reply_to_msg($msg, "(reply :content (ok))");
    } elsif ($content->{verb} eq 'get-parameters') {
      $self->reply_to_msg($msg, ['reply', ':content', ['parameters',
        ':init-taggers', [map { $_->{name} } @{$self->{init_taggers}}],
	':default-type', $default_type,
	':process-input-utterances',
	  ($self->{process_input_utterances} ? 't' : 'nil'),
	':remote-meta-map',
	  ($self->{remote_meta_map} ?
	    '"' . $self->{remote_meta_map}->{host} .
	    ':' . $self->{remote_meta_map}->{port} . '"'
	    : 'nil'),
	':meta-map-sources',
	   (sets_equal($self->{meta_map_sources}, \@all_meta_map_sources) ?
	     'all' : $self->{meta_map_sources}),
	':drum-species',
	   (defined($self->{drum_species}) ?
	     [map { '"' . escape_for_quotes($_) . '"' }
	          @{$self->{drum_species}}]
	     : 'all'
	   ),
	':no-sense-words',
	  [map { lisp_intern(uc($_)) } @{$self->{no_sense_words}}],
	':senses-only-for-penn-poss', $self->{senses_only_for_penn_poss},
	':min-sentence-length-for-phrases', $self->{min_sentence_length_for_phrases},
	':names-file',
	  ($self->{names_file} ?
	    '"' . escape_for_quotes($self->{names_file}) . '"'
	    : 'nil'),
	':terms-file',
	  ($self->{terms_file} ?
	    '"' . escape_for_quotes($self->{terms_file}) . '"'
	    : 'nil'),
	':xml-tags', $self->{xml_tags_mode},
	':xml-input-rules-file',
	  ($self->{xml_input_rules_file} ?
	    '"' . escape_for_quotes($self->{xml_input_rules_file}) . '"'
	    : 'nil'),
	':parsers-must-agree',
	  ($self->{parsers_must_agree} ? 't' : 'nil'),
	':use-wordfinder',
	  ($self->{use_wordfinder} ? 't' : 'nil'),
	':aspell-dict',
	  ($self->{aspell_dict} ?
	    '"' . escape_for_quotes($self->{aspell_dict}) . '"'
	    : 'nil')
      ]]);
    } elsif ($content->{verb} eq 'init') {
      die "'init' request is missing a :taggers list"
        unless (ref($content->{':taggers'}) eq 'ARRAY');
      # get the named taggers, and subtract the ones already initialized
      my @taggers = @{set_difference(
        [grep {
	  if (exists($_->{init_function})) {
	    1
	  } else {
	    print STDERR "Note: the tagger $_->{name} does not require initialization.\n";
	    0
	  }
	} get_named_taggers(@{$content->{':taggers'}})],
	$self->{init_taggers}
      )};
      # add them to the list of initialized taggers
      push @{$self->{init_taggers}}, @taggers;
      # initialize them
      for my $tagger (@taggers) {
	&{$tagger->{init_function}}($self);
      }
      # wait for them to be ready if necessary
      for my $tagger (@taggers) {
	&{$tagger->{ready_function}}($self)
	  if (exists($tagger->{ready_function}));
      }
      $self->reply_to_msg($msg, "(reply :content (ok))");
    } elsif ($content->{verb} eq 'fini') {
      die "'fini' request is missing a :taggers list"
        unless (ref($content->{':taggers'}) eq 'ARRAY');
      # get the named taggers, and intersect with those that are initialized
      my @taggers = @{intersection(
        [grep {
	    if (exists($_->{fini_function})) {
	      1
	    } else {
	      print STDERR "Note: the tagger $_->{name} does not require finalization.\n";
	      0
	    }
	} get_named_taggers(@{$content->{':taggers'}})],
	$self->{init_taggers}
      )};
      # remove the from the list of initialized taggers
      $self->{init_taggers} = set_difference($self->{init_taggers}, \@taggers);
      # finalize them
      for my $tagger (@taggers) {
	&{$tagger->{fini_function}}($self);
      }
      $self->reply_to_msg($msg, "(reply :content (ok))");
    } elsif ($content->{verb} eq 're-init') {
      die "'re-init' request is missing a :taggers list"
        unless (ref($content->{':taggers'}) eq 'ARRAY');
      # get the named taggers
      my @taggers = get_named_taggers(@{$content->{':taggers'}});
      # check that they are initialized
      my @non_init = @{set_difference(\@taggers, $self->{init_taggers})};
      die "Can't re-init non-initialized tagger(s) " . join(',', map { $_->{name} } @non_init)
        unless (@non_init == 0);
      # finalize, then initialize them
      for my $tagger (@taggers) {
	&{$tagger->{fini_function}}($self) if (exists($tagger->{fini_function}));
	&{$tagger->{init_function}}($self);
      }
      # wait for them to be ready if necessary
      for my $tagger (@taggers) {
	&{$tagger->{ready_function}}($self)
	  if (exists($tagger->{ready_function}));
      }
      $self->reply_to_msg($msg, "(reply :content (ok))");
    } elsif ($content->{verb} eq 'load-input-terms') {
      load_input_terms($content->{':input-terms'});
      $self->reply_to_msg($msg, "(reply :content (ok))");
    } elsif ($content->{verb} eq 'add-input-terms') {
      add_input_terms($content->{':input-terms'});
      $self->reply_to_msg($msg, "(reply :content (ok))");
    } elsif ($content->{verb} eq 'get-input-terms') {
      $self->reply_to_msg($msg, ['reply', ':content',
        get_input_terms($content->{':input-terms'})
      ]);
    } elsif ($content->{verb} eq 'clear-input-terms') {
      clear_input_terms();
      $self->reply_to_msg($msg, "(reply :content (ok))");
    } else
    {
      die "Unknown request verb " . $content->{verb};
    }
    return 1
  } || ($msg->{':sender'} eq 'TextTagger' ? die : # we did this to ourself
        $self->reply_to_msg($msg, "(reply :content (reject :reason \"" . escape_for_quotes($@) . "\"))\n"));
}

sub receive_tell
{
  my ($self, $msg, $content) = @_;
  my $sender = $msg->{':sender'};
  return undef if (lc($sender) eq lc($self->{name})); # don't talk to ourselves
  $content->{verb} = lc($content->{verb});
  eval {
    if ($content->{verb} eq 'utterance')
    {
      if ($self->{process_input_utterances} and
	  lc($content->{':direction'}) eq 'input')
      {
	# get the arguments we need
	my $text = $content->{':text'};
	die "'utterance' tell is missing a :text string"
	  unless (KQML::KQMLAtomIsString($text));
	my @extraTagArgs = ();
	for my $arg (qw(:channel :direction :mode :uttnum :genre))
	{
	  push @extraTagArgs, $arg, $content->{$arg}
	    if (exists($content->{$arg}));
	}
        # mark the start of TT's output
	$self->send_msg("(tell :content (start-new-sentence " . join(' ', @extraTagArgs) . "))");
	# act as if we received a tag request with the appropriate arguments,
	# and pass through :channel, :direction, and :mode
	$self->receive_request(
	  {':sender' => 'TextTagger'},
	  {
	    'verb' => 'tag',
	    ':text' => $text,
	    ':tell-each-tag' => 't',
	    ':extra-tag-args' => [@extraTagArgs]
	  });
	# pass through the utterance message, removing the :sender (marking the
	# end of TT's output)
	my %new_msg = %$msg;
	delete $new_msg{':sender'};
	$self->send_msg(KQML::KQMLUnkeywordify(\%new_msg));
      }
    } elsif ($content->{verb} eq 'start-conversation')
    {
      # (we get this message even though we didn't subscribe for it, because it is broadcast)
      #clear_input_terms();
    } else
    {
      die "Unknown tell verb " . $content->{verb};
    }
    return 1
  } || $self->reply_to_msg($msg, "(error :comment \"" . escape_for_quotes($@) . "\")\n");
}

sub receive_reply
{
  my ($self, $msg, $content) = @_;
  # ignore
}

sub receive_error
{
  my ($self, $msg) = @_;
  # ignore
}

sub receive_sorry
{
  my ($self, $msg) = @_;
  # ignore
}

sub run {
  my $self = shift;
  $self->SUPER::run();
  $self->fini();
}

sub fini {
  my $self = shift;

  # finalize those taggers that need it
  for my $tagger (@{$self->{init_taggers}})
  { # NOTE: only taggers with init_functions may also have fini_functions
    if (exists($tagger->{fini_function})) {
      print STDERR "Finalizing " . $tagger->{name} . "...\n"
	if ($self->{debug});
      &{$tagger->{fini_function}}($self);
    }
  }
  print STDERR "Done finalizing taggers.\n" if ($self->{debug});
}

1;
