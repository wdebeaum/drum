#!/usr/bin/perl

package TextTagger::Tags2Trips;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(tag2trips trips2tagNative tags2trips tag2tripsLattice sortTags fillGaps splitClauses splitSentences reconcileQuotationsAndSentences $min_clause_length @penn_clause_tags %penn2trips_punc stanford_word_re cj_word_re $previous_word_ended_with_period filter_senses_from_word_blacklist filter_sense_infos_from_penn_pos_whitelist);

use Data::Dumper;
use TextTagger::Util qw(lisp_intern string2w union intersection set_difference);
use TextTagger::Escape qw(escape_for_quotes unescape_backslashes un_pipe_quote);

use KQML::KQML;

use strict vars;

# The minimum length of a clause, in characters.
# NOTE: You might get a shorter clause if the entire sentence is that short.
our $min_clause_length = 30;

our @penn_clause_tags = qw(S SBAR SBARQ SINV SQ);

our %penn2trips_punc = (
  '-LRB-' => '(',
  '-RRB-' => ')',
  '-LSB-' => '[',
  '-RSB-' => ']',
  '-LCB-' => '{',
  '-RCB-' => '}',
  '``' => '"',
  "''" => '"',
  '`' => "'"
  );

# Stanford's NER and parser convert British spellings to American ones
# Most of these conversions are taken from this website:
# http://www2.gsu.edu/~wwwesl/egw/jones/differences.htm
# But Stanford doesn't do all of the conversions from that website. This list
# only has those it does do (as well as some others we've discovered
# independently).
our %penn2trips_word_re = (
  # format:
  # 'american' => qr/american|british/i,
  'favorite' => qr/favorite|favourite/i,
  'analog' => qr/analog|analogue/i,
  'analyze' => qr/analyze|analyse/i,
  'center' => qr/center|centre/i,
  'theater' => qr/theater|theatre/i,
  'encyclopedia' => qr/encyclopedia|encyclopaedia/i,
  'maneuver' => qr/maneuver|manoeuvre/i,
  'defense' => qr/defense|defence/i,
  'program' => qr/program|programme/i,
  'organization' => qr/organization|organisation/i,
  'canceled' => qr/canceled|cancelled/i,
  'canceling' => qr/canceling|cancelling/i,
  'traveled' => qr/traveled|travelled/i,
  'traveling' => qr/traveling|travelling/i,
  'labeled' => qr/labeled|labelled/i,
  'labeling' => qr/labeling|labelling/i,
  'analyzed' => qr/analyzed|analysed/i,
  'analyzing' => qr/analyzed|analysing/i,
  'anemia' => qr/anemia|anaemia/i,
  'anesthetic' => qr/anesthetic|anaesthetic/i,
  'capitalization' => qr/capitalization|capitalisation/i,
  'capitalize' => qr/capitalize|capitalise/i,
  'capitalized' => qr/capitalized|capitalised/i,
  'colored' => qr/colored|coloured/i,
  'coloring' => qr/coloring|colouring/i,
  'estrogen' => qr/estrogen|oestrogen/i,
  'favored' => qr/favored|favoured/i,
  'favoring' => qr/favoring|favouring/i,
  'fiber' => qr/fiber|fibre/i,
  'flavored' => qr/flavored|flavoured/i,
  'flavoring' => qr/flavoring|flavouring/i,
  'gray' => qr/gray|grey/i,
  'hemophilia' => qr/hemophilia|haemophilia/i,
  'hemophiliac' => qr/hemophiliac|haemophiliac/i,
  'honored' => qr/honored|honoured/i,
  'honoring' => qr/honoring|honouring/i,
  'humored' => qr/humored|humoured/i,
  'humoring' => qr/humoring|humouring/i,
  'labored' => qr/labored|laboured/i,
  'laboring' => qr/laboring|labouring/i,
  'leaned' => qr/leaned|leant/i,
  'learned' => qr/learned|learnt/i,
  'leukemia' => qr/leukemia|leukaemia/i,
  'localize' => qr/localize|localise/i,
  'localized' => qr/localized|localised/i,
  'minimize' => qr/minimize|minimise/i,
  'minimized' => qr/minimized|minimised/i,
  'minimizing' => qr/minimizing|minimising/i,
  'realize' => qr/realize|realise/i,
  'realized' => qr/realized|realised/i,
  'realizing' => qr/realizing|realising/i,
  'recognize' => qr/recognize|recognise/i,
  'recognized' => qr/recognized|recognised/i,
  'recognizing' => qr/recognizing|recognising/i,
  'rumored' => qr/rumored|rumoured/i,
  # Stanford also capitalizes days of the week
  'Monday' => qr/[Mm]onday/,
  'Tuesday' => qr/[Tt]uesday/,
  'Wednesday' => qr/[Ww]ednesday/,
  'Thursday' => qr/[Tt]hursday/,
  'Friday' => qr/[Ff]riday/,
  'Saturday' => qr/[Ss]aturday/,
  'Sunday' => qr/[Ss]unday/,
  # and months of the year, except may because that's a word too
  'January' => qr/[Jj]anuary/,
  'February' => qr/[Ff]ebruary/,
  'March' => qr/[Mm]arch/,
  'April' => qr/[Aa]pril/,
  'June' => qr/[Jj]une/,
  'July' => qr/[Jj]uly/,
  'August' => qr/[Aa]ugust/,
  'September' => qr/[Ss]eptember/,
  'October' => qr/[Oo]ctober/,
  'November' => qr/[Nn]ovember/,
  'December' => qr/[Dd]ecember/,
  # it also spells out the cent sign
  'cents' => qr/cents|\x{00a2}/
  );
# add plurals to the above
for my $key (keys %penn2trips_word_re) {
  $penn2trips_word_re{$key . 's'} = qr/(?:$penn2trips_word_re{$key})s/i;
}

# We need this for the case where Stanford inserts an extra "." when an
# abbreviation ends a sentence.
our $previous_word_ended_with_period = 0;

# Given a "word" from the output of either Stanford NER or Stanford Parser,
# return a regex that should match the word it came from in the remaining
# original text. See the "Stanford weirdness" section in README.xhtml.
sub stanford_word_re {
  my $word = shift;
  my $re;
  if ($word eq '-LRB-') {
    $re = qr/[\(\[\<]/;
  } elsif ($word eq '-RRB-') {
    $re = qr/[\)\]\>]/;
  } elsif ($word eq '...') {
    $re = qr/\.\s*\.\s*\./;
  } elsif ($word eq '--') {
    $re = qr/--|\x{2013}|\x{2014}/; # en- and em-dashes
  } elsif (exists($penn2trips_punc{$word})) {
    $re = qr/\Q$word\E|\Q$penn2trips_punc{$word}\E/;
  } elsif (exists($penn2trips_word_re{$word})) {
    $re = $penn2trips_word_re{$word};
  } elsif ($word =~ /^\w+ors?$/) {
    my $british_version = $word;
    $british_version =~ s/or(s?)$/our$1/;
    $re = qr/$word|$british_version/i;
  } elsif ($word eq '.' and $previous_word_ended_with_period) {
    $re = qr/\s(?!\s*\.)|\./; # HACK: we match the following space if this period was inserted, because we need to match something
  } else {
    $word = unescape_backslashes($word);
    $re = qr/\Q$word\E/;
  }
  $previous_word_ended_with_period = ($word =~ /[^\.]\.$/) ? 1 : 0;
  return $re;
}

# Similar to the above, but for CJParser (it does fewer weird things).
sub cj_word_re {
  my $word = shift;
  my $re;
  if (exists($penn2trips_punc{$word})) {
    $re = qr/\Q$word\E|\Q$penn2trips_punc{$word}\E/;
  } elsif ($word eq '.' and $previous_word_ended_with_period) {
    $re = qr/\s|$/; # HACK: we match the following space, or the end of the string, because we need to match something
  } else {
    $re = qr/\Q$word\E/;
  }
  $previous_word_ended_with_period = ($word =~ /[^\.]\.$/) ? 1 : 0;
  return $re;
}

my %penn2trips_pos = (
  'CC'  => [qw(CONJ)],	# Coordinating conjunction
  'CD'  => [qw(NUMBER NUMBER-UNIT)],	# Cardinal number
  'DT'  => [qw(ART QUAN PRO)],	# Determiner
  'EX'  => [qw(PRO)],	# Existential there
#  'FW'  =>	# Foreign word
  'IN'  => [qw(PREP ADV)],	# Preposition or subordinating conjunction
  'JJ'  => [qw(ADJ)],	# Adjective
  'JJR' => [qw(ADJ)],	# Adjective, comparative
  'JJS' => [qw(ADJ)],	# Adjective, superlative
#  'LS'  =>	# List item marker
  'MD'  => [qw(V)],	# Modal
  'NN'  => [qw(N)],	# Noun, singular or mass
  'NNS' => [qw(N)],	# Noun, plural
  'NNP' => [qw(NAME)],	# Proper noun, singular
  'NNPS'=> [qw(NAME)],	# Proper noun, plural
  'PDT' => [qw(QUAN)],	# Predeterminer
  'POS' => [qw(^S)],	# Possessive ending
  'PRP' => [qw(PRO)],	# Personal pronoun
  'PRP$'=> [qw(PRO)],	# Possessive pronoun
  'RB'  => [qw(ADV)],	# Adverb
  'RBR' => [qw(ADV)],	# Adverb, comparative
  'RBS' => [qw(ADV)],	# Adverb, superlative
  'RP'  => [qw(PART)],	# Particle
  'SYM' => [qw(PUNC)],	# Symbol
  'TO'  => [qw(PREP INFINITIVAL-TO)],	# to
  'UH'  => [qw(UTTWORD FP)],	# Interjection
  'VB'  => [qw(V)],	# Verb, base form
  'VBD' => [qw(V)],	# Verb, past tense
  'VBG' => [qw(V)],	# Verb, gerund or present participle
  'VBN' => [qw(V)],	# Verb, past participle
  'VBP' => [qw(V)],	# Verb, non-3rd person singular present
  'VBZ' => [qw(V)],	# Verb, 3rd person singular present
  'WDT' => [qw(ART)],	# Wh-determiner
  'WP'  => [qw(PRO)],	# Wh-pronoun
  'WP$' => [qw(PRO)],	# Possessive wh-pronoun
  'WRB' => [qw(ADV)],	# Wh-adverb  
  # various other punctuation that doesn't fall under SYM
  '.'   => [qw(PUNC)],
  ','   => [qw(PUNC)],
  ':'   => [qw(PUNC)],
  "``"	=> [qw(PUNC)],
  "''"	=> [qw(PUNC)],
  # Unicode name versions of the punctutation
  'FULL_STOP'		=> [qw(PUNC)],
  'COMMA'		=> [qw(PUNC)],
  'COLON'		=> [qw(PUNC)],
  # some more from the WordNet tagged gloss corpus
  'LEFT_PARENTHESIS'	=> [qw(PUNC)],
  'RIGHT_PARENTHESIS'	=> [qw(PUNC)]
  );

my %penn2trips_cat = (
  'ADJP'   => 'ADJP',  # Adjective Phrase.
  'ADVP'   => 'ADVBL', # Adverb Phrase.
  #'CONJP'  => '', # Conjunction Phrase.
  #'FRAG'   => '', # Fragment.
  #'INTJ'   => '', # Interjection. Corresponds approximately to the part-of-speech tag UH.
  #'LST'    => '', # List marker. Includes surrounding punctuation.
  #'NAC'    => '', # Not a Constituent; used to show the scope of certain prenominal modifiers within an NP.
  'NP'     => 'NP',    # Noun Phrase.
  'NX'     => 'N1',    # Used within certain complex NPs to mark the head of the NP. Corresponds very roughly to N-bar level but used quite differently.
  'PP'     => 'PP',    # Prepositional Phrase.
  #'PRN'    => '', # Parenthetical.
  'PRT'    => 'PART',  # Particle. Category for words that should be tagged RP.
  #'QP'     => '', # Quantifier Phrase (i.e. complex measure/amount phrase); used within NP.
  'RRC'    => 'CP',    # Reduced Relative Clause.
  'SBAR'   => 'CP',
  #'UCP'    => '', # Unlike Coordinated Phrase.
  'VP'     => 'VP',    # Verb Phrase.
  'WHADJP' => 'ADJP',  # Wh-adjective Phrase. Adjectival phrase containing a wh-adverb, as in how hot.
  'WHAVP'  => 'ADVBL', # Wh-adverb Phrase. Introduces a clause with an NP gap. May be null (containing the 0 complementizer) or lexical, containing a wh-adverb such as how or why.
  'WHNP'   => 'NP',    # Wh-noun Phrase. Introduces a clause with an NP gap. May be null (containing the 0 complementizer) or lexical, containing some wh-word, e.g. who, which book, whose daughter, none of which, or how many leopards.
  'WHPP'   => 'PP'     # Wh-prepositional Phrase. Prepositional phrase containing a wh-noun phrase (such as of which or by whose authority) that either introduces a PP gap or is contained by a WHNP.
  #'X'      => '', # Unknown, uncertain, or unbracketable. X is often used for bracketing typos and in bracketing the...the-constructions.
  );

# convert a tag hash as returned by combine_tags to a LoL ref for KQML
sub tag2tripsLattice {
  my %tag = %{shift()};
  my $trips = [$tag{type}];

  # push the lex/text
  if (exists($tag{text})) {
    push @$trips, '"' . escape_for_quotes($tag{text}) . '"';
  } elsif (exists($tag{lex})) {
    push @$trips, '"' . escape_for_quotes($tag{lex}) . '"';
  } else {
    push @$trips, 'NIL';
  }

  push @$trips, ":frame", [$tag{start}, $tag{end} - 1];

  # push optional args
  if (exists($tag{'penn-cat'})) {
    push @$trips, ":enju-cats",
	 [map { lisp_intern($_) } @{$tag{'enju-cat'}}]
      if (exists($tag{'enju-cat'}));
    push @$trips, ":penn-cats",
	 [map { lisp_intern($_) } @{$tag{'penn-cat'}}];
    my $trips_cats = [];
    for my $penn_cat (@{$tag{'penn-cat'}}) {
      $trips_cats = union($trips_cats, [$penn2trips_cat{$penn_cat}])
        if (exists($penn2trips_cat{$penn_cat}));
    }
    push @$trips, ":trips-cats",
         [map { lisp_intern($_, 'W') } @$trips_cats]
      if (@$trips_cats);
  }
  push @$trips, ':sense-info',
       [map { senseInfo2trips($_) } @{$tag{'sense-info'}}]
    if (exists($tag{'sense-info'}));
  push @$trips, ':score', $tag{score}
    if (exists($tag{score}));
  push @$trips, ':uttnum', $tag{uttnum}
    if (exists($tag{uttnum}));

  return $trips;
}

sub senseInfo2trips {
  my %info = %{shift()};
  my $trips = [];
  if (exists($info{'penn-pos'})) {
    push @$trips, ":penn-parts-of-speech",
	 [map { lisp_intern($_) } @{$info{'penn-pos'}}];
    my $trips_poss = [];
    for my $penn_pos (@{$info{'penn-pos'}}) {
      $trips_poss = union($trips_poss, $penn2trips_pos{$penn_pos})
        if (exists($penn2trips_pos{$penn_pos}));
    }
    push @$trips, ":trips-parts-of-speech",
         [map { lisp_intern($_, 'W') } @$trips_poss]
      if (@$trips_poss);
  }
  push @$trips, ":ont-types",
       [map { 
	 (ref($_) eq 'ARRAY') ? # e.g. (% W::speechact)
	   $_ : lisp_intern(uc($_), "ONT")
        } @{$info{lftype}}]
    if (exists($info{lftype}));
  push @$trips, ':wn-sense-keys',
       [map { '"' . escape_for_quotes($_) . '"' }
            @{$info{'wn-sense-keys'}}]
    if (exists($info{'wn-sense-keys'}));
  push @$trips, ":alternate-spellings",
       [map { '"' . escape_for_quotes($_) . '"' }
            @{$info{'alternate-spellings'}}]
    if (exists($info{'alternate-spellings'}));
  push @$trips, ':score', sprintf("%.5f", $info{score})
    if (exists($info{score}));
  push @$trips, ':domain-specific-info',
       domainSpecificInfo2tripsAssocList($info{'domain-specific-info'})
    if (exists($info{'domain-specific-info'}));
  
  return $trips;
}

sub tag2tripsNative {
  my $tag = shift;
  my $trips = [$tag->{type}];
  push @$trips, ':text', '"' . escape_for_quotes($tag->{text}) .'"'
    if (exists($tag->{text}));
  push @$trips, ':lex', '"' . escape_for_quotes($tag->{lex}) . '"'
    if (exists($tag->{lex}));
  push @$trips, ':start', $tag->{start}
    if (exists($tag->{start}));
  push @$trips, ':end', $tag->{end}
    if (exists($tag->{end}));
  push @$trips, ':pos', [map { lisp_intern(uc($_), "W") } @{$tag->{"pos"}}]
    if (exists($tag->{"pos"}));
  push @$trips, ':penn-pos', [map { lisp_intern(uc($_)) } @{$tag->{"penn-pos"}}]
    if (exists($tag->{"penn-pos"}));
  push @$trips, ':specialist-pos', lisp_intern(uc($tag->{"specialist-pos"}))
    if (exists($tag->{"specialist-pos"}));
  push @$trips, ':cat', [map { lisp_intern(uc($_), "W") } @{$tag->{cat}}]
    if (exists($tag->{cat}));
  push @$trips, ':penn-cat', [map { lisp_intern(uc($_)) } @{$tag->{"penn-cat"}}]
    if (exists($tag->{"penn-cat"}));
  push @$trips, ':enju-cat', [map { lisp_intern(uc($_)) } @{$tag->{"enju-cat"}}]
    if (exists($tag->{"enju-cat"}));
  push @$trips, ':lftype', (ref($tag->{lftype}) eq 'ARRAY' ?
                             $tag->{lftype} :
			     lisp_intern(uc($tag->{lftype}), "ONT"))
    if (exists($tag->{lftype}));
  push @$trips, ':stanford-ner-class', $tag->{'stanford-ner-class'}
    if (exists($tag->{'stanford-ner-class'}));
  push @$trips, ':wn-sense-keys',
       [map { '"' . escape_for_quotes($_) . '"' } @{$tag->{'wn-sense-keys'}}]
    if (exists($tag->{'wn-sense-keys'}));
  push @$trips, ':domain-specific-info', domainSpecificInfo2trips($tag->{'domain-specific-info'})
    if (exists($tag->{'domain-specific-info'}));
  push @$trips, ':score', $tag->{score}
    if (exists($tag->{score}));
  push @$trips, ':gold-sentence-id', $tag->{'gold-sentence-id'}
    if (exists($tag->{'gold-sentence-id'}));
  push @$trips, ':source', $tag->{source}
    if (exists($tag->{source}));
  return $trips;
}

# convert a listref of domain-specific-info structures into an assoc-list
# mapping their domains to lists of (converted) structures with that domain.
# Preserves order of DSIs as much as possible.
sub domainSpecificInfo2tripsAssocList {
  my $dsis = shift;
  my %domain2pair = ();
  my @domains = ();
  for my $dsi (@$dsis) {
    my $domain = (exists($dsi->{domain}) ? $dsi->{domain} : $dsi->{type}); # fall back to type for old-style dsis like UMLS
    unless (exists($domain2pair{$domain})) {
      $domain2pair{$domain} = [$domain];
      push @domains, $domain;
    }
    push @{$domain2pair{$domain}}, domainSpecificInfo2trips($dsi);
  }
  return [map { $domain2pair{$_} } @domains];
}

sub domainSpecificInfo2trips {
  my $info = shift;
  if (ref($info) eq 'ARRAY') {
    return [map { domainSpecificInfo2trips($_) } @$info];
  } elsif (ref($info) eq 'HASH') {
    my $trips = [$info->{type}];
    if ($info->{type} eq 'umls') {
      push @$trips, ':cui', lisp_intern(uc($info->{cui}));
      push @$trips, ':concept',
           '"' . escape_for_quotes($info->{concept}) . '"'
	if (exists($info->{concept}));
      push @$trips, ':preferred',
           '"' . escape_for_quotes($info->{preferred}) . '"'
	if (exists($info->{preferred}));
      push @$trips, ':semantic-types',
           [map { lisp_intern(uc($_)) } @{$info->{'semantic-types'}}]
	if (exists($info->{'semantic-types'}));
      push @$trips, ':semantic-groups',
           [map { lisp_intern(uc($_)) } @{$info->{'semantic-groups'}}]
	if (exists($info->{'semantic-groups'}));
      push @$trips, ':nci-codes',
           [map { lisp_intern(uc($_)) } @{$info->{'nci-codes'}}]
        if (exists($info->{'nci-codes'}));
      push @$trips, ':nci-isa',
           [map { lisp_intern(uc($_)) } @{$info->{'nci-isa'}}]
	if (exists($info->{'nci-isa'}));
      push @$trips, ':snomedct-codes', $info->{'snomedct-codes'}
        if (exists($info->{'snomedct-codes'}));
      push @$trips, ':snomedct-isa',
           [map { lisp_intern(uc($_)) } @{$info->{'snomedct-isa'}}]
	if (exists($info->{'snomedct-isa'}));
      push @$trips, ':ancestors',
           domainSpecificInfo2trips($info->{ancestors})
	if (exists($info->{ancestors}));
      push @$trips, ':sources',
           [map { lisp_intern(uc($_)) } @{$info->{sources}}]
	if (exists($info->{sources}));
    } elsif ($info->{type} eq 'term') {
      push @$trips, ':id', $info->{id}
	if (exists($info->{id}));
      push @$trips, ':name',
           '"' . escape_for_quotes($info->{name}) . '"'
	if (exists($info->{name}));
      push @$trips, ':score',
           sprintf("%.5f", $info->{score})
	if (exists($info->{score}));
      push @$trips, ':matches',
	   domainSpecificInfo2trips($info->{matches})
	if (exists($info->{matches}));
      push @$trips, ':species',
           '"' . escape_for_quotes($info->{species}) . '"'
	if (exists($info->{species}));
      push @$trips, ':dbxrefs', $info->{dbxrefs} if (exists($info->{dbxrefs}));
      push @$trips, ':mappings', 
      	   [map { ['map', ':through', $_->{':through'}, ':to', $_->{':to'}] }
	        @{$info->{mappings}}]
        if (exists($info->{mappings}));
      push @$trips, ':member-type', $info->{'member-type'}
        if (exists($info->{'member-type'}));
      push @$trips, ':members', $info->{members} if (exists($info->{members}));
      push @$trips, ':pmod', $info->{pmod} if (exists($info->{pmod}));
    } elsif ($info->{type} eq 'specialist') {
      push @$trips, ':eui', $info->{eui}
	if (exists($info->{eui}));
      push @$trips, ':cat', $info->{cat}
	if (exists($info->{cat}));
      push @$trips, ':citation-form',
           '"' . escape_for_quotes($info->{'citation-form'}) . '"'
	if (exists($info->{'citation-form'}));
      push @$trips, ':matches',
	   domainSpecificInfo2trips($info->{matches})
	if (exists($info->{matches}));
      push @$trips, ':complements',
           [map { '"' . escape_for_quotes($_) . '"' } @{$info->{complements}}]
	if (exists($info->{complements}));
      for my $link_type (qw(nominalization nominalization-of abbreviation-of acronym-of)) {
	push @$trips, ':' . $link_type,
	     domainSpecificInfo2trips($info->{$link_type})
	  if (exists($info->{$link_type}));
      }
    } elsif ($info->{type} eq 'match') {
      # put score up front, and don't print excessive decimal places
      push @$trips, ':score',
           sprintf("%.5f", $info->{score})
	if (exists($info->{score}));
      # the only two that are strings instead of numbers
      for my $key (qw(matched status)) {
	push @$trips, ':' . $key,
	     '"' . escape_for_quotes($info->{$key}) . '"'
	  if (exists($info->{$key}));
      }
      # put these numbers up front
      for my $key (qw(maybe-depluralized surely-depluralized depluralization-score dash-no-dash no-dash-dash exact)) {
	my $val = $info->{$key};
	$val = sprintf("%.5f", $val) if ($key eq 'depluralization-score');
	push @$trips, ':' . $key, $info->{$key}
	  if (exists($info->{$key}));
      }
      # do the rest of the numbers
      for my $key (sort keys %$info) {
	next if (grep { $_ eq $key } qw(type matched status score maybe-depluralized surely-depluralized depluralization-score dash-no-dash no-dash-dash exact));
	push @$trips, ':' . $key, $info->{$key}
	  if (exists($info->{$key}));
      }
    } elsif (grep { $_ eq $info->{type} } qw(mutation aa-site amino-acid mirna)) {
      push @$trips, ':type', $info->{'sub-type'}
        if (exists($info->{'sub-type'}));
      for my $key (qw(old name letter index aa-index lower upper new number species)) {
	next unless (exists($info->{$key}));
	my $reftype = ref($info->{$key});
	if ($reftype eq 'HASH' or $reftype eq 'ARRAY') {
	  push @$trips, ":$key", domainSpecificInfo2trips($info->{$key});
	} elsif ($key =~ /index$/) {
	  push @$trips, ":$key", $info->{$key};
	} else {
	  push @$trips, ":$key",
	    '"' . escape_for_quotes($info->{$key}) . '"';
	}
      }
    } else {
      die "Unknown type of domain-specific-info: $info->{type}\n" . Data::Dumper->Dump([$info],['*info']);
    }
    return $trips;
  } else {
    die "domain-specific-info must be array or hash ref: $info";
  }
}

# is the argument of the form ['verb', ':keyword', $value, ...] ?
sub is_performative {
  my $o = shift;
  return (ref($o) eq 'ARRAY' and
          @$o >= 3 and
	  (not ref($o->[0])) and
	  (not ref($o->[1])) and
	  $o->[1] =~ /^:/);
}

# rough inverse of tag2tripsNative, used by Asma, Minpar, MetaMap, TermsInput, XMLInput, and XML.pm
sub trips2tagNative
{
  my $trips = shift;
  my $tag = { 'type' => lc($trips->[0]) };
  for (my $i = 1; $i + 1 < @$trips; $i += 2)
  {
    my ($arg, $val) = ($trips->[$i], $trips->[$i+1]);
    die "argument name isn't a keyword: $arg\n" unless ($arg =~ /^:/);
    $arg = lc($');
    # TODO generalize all these special cases
    if ($arg eq 'sources' and ref($val) eq 'ARRAY') {
      # special case to handle pipequotes in flat :sources lists from MetaMap
      $val = [map { un_pipe_quote($_) } @$val];
    } elsif ($arg eq 'lftype' and ref($val) eq 'ARRAY') {
      # special case to handle flat :lftype lists
      $val = [map {
	# undo pipe quotes
	my $r = un_pipe_quote($_);
	# remove package
	$r = $' if ($r =~ /::/);
	# downcase
	$r = lc($r);
	$r
      } @$val];
    } elsif ($arg eq 'wn-sense-keys' and ref($val) eq 'ARRAY') {
      # special case to handle flat :wn-sense-keys lists of strings
      $val = [map {
	if (KQML::KQMLAtomIsString($_)) {
	  KQML::KQMLStringAtomAsPerlString($_);
	} else {
	  $_;
	}
      } @$val];
    } elsif ($arg eq 'id') {
      # special case to keep package on :id of drum domain-specific-info
      $val = un_pipe_quote($val);
    } elsif ($arg eq 'mappings') {
      # special case for :mappings of drum domain-specific-info
      $val = [map { KQML::KQMLKeywordify($_) } @$val];
    } elsif (grep { $_ eq $arg } qw(dbxrefs member-type members pmod)) {
      # special case for a few arguments of drum domain-specific-info
      # do nothing (even if it looks like a KQML performative, etc.)
    } elsif (grep { $_ eq $arg }
               qw(nci-isa nci-codes
	          snomedct-isa snomedct-codes
		  semantic-types) and
          ref($val) eq 'ARRAY') {
      # special case to downcase IDs in :*-isa, :*-codes, and
      # :semantic-types lists from MetaMap
      $val = [map { lc($_) } @$val];
    } elsif (ref($val) eq 'ARRAY')
    { # list
      if (is_performative($val)) {
	# a verb + keyword args list, recurse
	$val = trips2tagNative($val);
      } elsif (@$val > 0 and not grep { not is_performative($_) } @$val) {
	# a list of performatives, recurse on the elements
	$val = [map { trips2tagNative($_) } @$val];
      } else {
	# some other kind of list, do nothing
      }
    } elsif (KQML::KQMLAtomIsString($val))
    { # string
      $val = KQML::KQMLStringAtomAsPerlString($val);
    } else
    { # symbol
      # undo pipe quotes
      $val = un_pipe_quote($val);
      # remove package
      $val = $' if ($val =~ /::/);
      # downcase
      $val = lc($val);
    }
    $tag->{$arg} = $val;
  }
  return $tag;
}

sub fillGaps
{
  my @lattice = @_;
  
  # extend words where necessary: when there is a gap in the character indices between one word and the next, extend the first word to cover the gap
  # FIXME this is O(n**2), I think I could do better
  for my $before (@lattice)
  {
    my $gapStart = $before->{end};
    my $gapEnd = undef;
    for my $after (@lattice)
    {
      $gapEnd = $after->{start}
	if ($after->{start} >= $gapStart and
	    ((!defined($gapEnd)) or $after->{start} < $gapEnd));
    }
    $before->{end} = $gapEnd
      if (defined($gapEnd) and $gapStart < $gapEnd);
  }

  return @lattice;
}

sub sortTags
{
  # sort the constituents into a readable order
  return sort { my $cmp = ($a->{end} <=> $b->{end});
		($cmp == 0) ? $b->{start} <=> $a->{start}
		            : $cmp;
	      }
	      @_;
}

sub tags2trips
{ # convert a list of tags to a trips lattice
  my ($lattice, $format) = @_;
  
  # convert tags to KQML LoL refs to be passed to the TRIPS Parser
  if ($format =~ /lattice/i)
  {
    return map {tag2tripsLattice($_)} @$lattice;
  } elsif ($format =~ /native/i)
  {
    return map {tag2tripsNative($_)} @$lattice;
  } else
  {
    die "Unknown format $format";
  }
}

sub splitClauses
{ # return a list of [$clauseTag, $tagsInClause] pairs
  my $tags = shift;
  my @pairs =
  map
  {
    my $clause = $_;
    [
      $clause,
      [
        grep
	{
	  my $tag = $_;
	  $tag->{type} ne 'sentence' and
	  $tag->{type} ne 'clause' and
	  $tag->{start} >= $clause->{start} and
	  $tag->{end} <= $clause->{end}
	}
	@$tags
      ]
    ]
  }
  grep { $_->{source} eq 'clauses' or $_->{source} eq 'tags2trips' } @$tags;
  return @pairs;
}

sub splitSentences
{ # return a list of [$sentenceTag, $tagsInSentence] pairs
  my $tags = shift;
  my @pairs =
  map
  {
    my $sentence = $_;
    [
      $sentence,
      [
        grep
	{
	  my $tag = $_;
	  $tag->{type} ne 'sentence' and
	  $tag->{start} >= $sentence->{start} and
	  $tag->{end} <= $sentence->{end}
	}
	@$tags
      ]
    ]
  }
  grep { $_->{type} eq 'sentence' } @$tags;
  return @pairs;
}

sub reconcileQuotationsAndSentences
{ # make sure quoted strings fit inside sentences, and convert any sentences within the quoted strings to clauses
  my $tags = shift;
# a few cases to consider:
# John emphatically said, "Phooey on youey! Argle bargle!"
# input:
# 			  -------------------------------- quotations
# ----------------------- ----------------- -------------- clauses
# ----------------------------------------- -------------- sentences
# output:
# ----------------------- ----------------- -------------- clauses
# -------------------------------------------------------- sentence
#
# "Phooey on youey," John said, "argle bargle!"
# input:
# ------------------            --------------- quotations
# ----------------------------- --------------- clauses
# --------------------------------------------- sentence
# output:
# ------------------ ---------- --------------- clauses
# --------------------------------------------- sentence
# ...


  my @old_tags = sortTags(@$tags);
  my @new_tags = ();

  # eliminate sentence boundaries if they occur within quotations
  while (@old_tags > 0)
  {
    my $tag = shift @old_tags;
    if ($tag->{type} eq 'sentence')
    {
      if (grep { $_->{type} eq 'quotation' and
	         $tag->{start} > $_->{start} and $tag->{start} < $_->{end}
	       }
	       @$tags)
      { # the sentence starts within a quotation, remove it
        # (i.e. don't add it to @new_tags)
      } elsif (grep { $_->{type} eq 'quotation' and
	              $tag->{end} > $_->{start} and $tag->{end} < $_->{end}
	            }
	            @$tags)
      { # the sentence ends within a quotation, extend it until it doesn't
        while (@old_tags > 0 and
               grep { $_->{type} eq 'quotation' and
	              $tag->{end} > $_->{start} and $tag->{end} < $_->{end}
	            }
	            @$tags)
	{
	  my $next_tag = shift @old_tags;
	  if ($next_tag->{type} eq 'sentence')
	  {
            # fill the gap between sentences with spaces (hopefully that's right)
	    $tag->{text} .= ' 'x($next_tag->{start} - $tag->{end});
            # add $next_tag onto $tag
	    $tag->{text} .= $next_tag->{text};
	    $tag->{end} = $next_tag->{end};
	  } else
	  { # $next_tag isn't a sentence, keep it
	    push @new_tags, $next_tag;
	  }
	}
	push @new_tags, $tag;
      } else
      { # the sentence doesn't interact with a quotation, keep it
        push @new_tags, $tag;
      }
    } else
    { # not a sentence, keep it
      push @new_tags, $tag;
    }
  }

  # split clauses if they contain quotation boundaries (i.e. quote marks)
  my @quotations = grep { $_->{type} eq 'quotation' } @new_tags;
  for my $clause (grep { $_->{source} eq 'clauses' } @new_tags)
  {
    # find the boundaries made by overlapping quotations
    my @boundaries = ();
    for my $quotation (@quotations)
    {
      if ($quotation->{start} > $clause->{start} and
	  $quotation->{start} < $clause->{end})
      { # the start of the quotation is within the clause
        push @boundaries, $quotation->{start};
      }
      if ($quotation->{end} > $clause->{start} and
	  $quotation->{end} < $clause->{end})
      { # the end of the quotation is within the clause
        push @boundaries, $quotation->{end};
      }
    }
    @boundaries = sort { $a <=> $b } @boundaries;

    # split $clause on those boundaries
    my $clauseText = $clause->{text};
    my $prevClause = $clause;
    for my $boundary (@boundaries)
    {
      # don't make empty clauses
      next if ($boundary == $prevClause->{start});

      # finish the previous clause
      $prevClause->{end} = $boundary;
      $prevClause->{text} = substr($clauseText, $prevClause->{start} - $clause->{start}, $prevClause->{end} - $prevClause->{start});
      
      # trim spaces
      while (substr($clauseText, $boundary - $clause->{start}, 1) =~ /\s/)
      {
	$boundary += 1;
      }

      # start the next clause and add it
      $prevClause = { type => 'clause',
      		      source => 'tags2trips',
	              start => $boundary
                    };
      push @new_tags, $prevClause;
    }
    # finish the last clause
    $prevClause->{end} = $clause->{start} + length($clauseText);
    $prevClause->{text} = substr($clauseText, $prevClause->{start} - $clause->{start}, $prevClause->{end} - $prevClause->{start});
  }

  return [@new_tags];
}

# given a listref of words and a list of tags, filter out the sense tags for
# those words
sub filter_senses_from_word_blacklist {
  my ($words, @tags) = @_;
  return grep {
    my $tag = $_;
    my $word = lc($tag->{lex});
    not ($tag->{type} eq 'sense' and grep { $_ eq $word } @$words)
  } @tags;
}

# given a listref of Penn parts of speech and a list of combined tags, modify
# the sense-infos in the tags so that only sense-infos with one of the given
# POSs may have actual sense information (ont-types, wn-sense-keys,
# domain-specific-info)
sub filter_sense_infos_from_penn_pos_whitelist {
  my ($penn_poss, @tags) = @_;
  for my $tag (@tags) {
    if (exists($tag->{'sense-info'})) {
      my @old_sense_infos = @{$tag->{'sense-info'}};
      my @new_sense_infos = ();
      for my $sense_info (@old_sense_infos) {
	if (exists($sense_info->{'lftype'}) or exists($sense_info->{'wn-sense-keys'}) or exists($sense_info->{'domain-specific-info'})) {
	  my $pos_int = intersection($sense_info->{'penn-pos'}, $penn_poss);
	  my $pos_diff = set_difference($sense_info->{'penn-pos'}, $penn_poss);
	  # add a version of the info with just the allowed POSs if any
	  push @new_sense_infos, { %$sense_info, 'penn-pos' => $pos_int }
	    if (@$pos_int);
	  # add a version with the other POSs if any, but no actual sense info
	  if (@$pos_diff) {
	    # you're supposed to be able to delete hash slices, but it didn't
	    # work when I tried it, so I'm deleting each key instead
	    delete $sense_info->{lftype};
	    delete $sense_info->{'wn-sense-keys'};
	    delete $sense_info->{'domain-specific-info'};
	    push @new_sense_infos, { %$sense_info, 'penn-pos' => $pos_diff };
	  }
	} else { # no actual sense info, just keep it for the POSs
	  push @new_sense_infos, $sense_info;
	}
      }
      if (@new_sense_infos) {
	$tag->{'sense-info'} = [@new_sense_infos];
      } else {
	delete $tag->{'sense-info'};
      }
    }
  }
  return @tags;
}

1;
