#!/usr/bin/perl

package TextTagger::ExtraArgs;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(add_extra_args);

use TextTagger::Escape qw(escape_for_quotes);
use TextTagger::Util qw(intersection union remove_duplicates);
use TextTagger::Tags2Trips qw(sortTags);
use TextTagger::Normalize qw($dash_re);

use strict vars;

my $debug = 0;

my %lftype2args = (
  'SUBSTANCE' => {mass => 'MASS', 'penn-pos' => [qw(NN NNP)]},
  'GEOGRAPHIC-REGION' => {'penn-pos' => ['NNP']}
);

my %tag_type2args = (
  'sentence' => {
    'penn-cat' => [qw(S SBARQ SINV SQ)]
  },
  'named-entity' => {
    'penn-pos' => [qw(NNP NNPS)],
    lftype => ['REFERENTIAL-SEM']
  },
  'street-address' => {
    'penn-pos' => [qw(NNP)],
    lftype => ['LOCATION-ID']
  }
);

# add extra arguments to each tag
sub add_extra_args {
  my ($self, $inc_uttnum, @tags) = @_;
  # assign each sentence an uttnum
  my @sentences = sortTags(grep { $_->{type} eq 'sentence' } @tags);
  for (@sentences) {
    $_->{uttnum} = $self->{next_uttnum};
    $self->{next_uttnum}++ if ($inc_uttnum);
  }
 
  # FIXME? move this after any new tags are added (e.g. pos, p-)
  for my $tag (@tags) {
    # assign uttnum based on which sentence $tag is in
    for my $s (@sentences) {
      if ($tag->{start} >= $s->{start} and
	  $tag->{end} <= $s->{end}
	 ) {
	$tag->{uttnum} = $s->{uttnum};
	last;
      }
    }
    # add args based on LF type
    %$tag = (%{$lftype2args{$tag->{lftype}}}, %$tag)
      if (exists($tag->{lftype}) and exists($lftype2args{$tag->{lftype}}));
    # add args based on tag type
    %$tag = (%{$tag_type2args{$tag->{type}}}, %$tag)
      if (exists($tag_type2args{$tag->{type}}));
  }

  # get all sense tag spans that don't have penn-pos
  my @posless_sense_spans =
    grep {
      my ($start, $end) = @$_;
      not grep {
	exists($_->{'penn-pos'}) and
	$_->{start} == $start and
	$_->{end} == $end
      } @tags
    } @{remove_duplicates([
      map {
        [$_->{start}, $_->{end}]
      } grep {
        $_->{type} eq 'sense'
      } @tags
    ])};

  # add pos tags to those spans by looking at the penn-pos of the words they
  # contain
  for my $span (@posless_sense_spans) {
    my ($start, $end) = @$span;
    my $new_pos_tag = {
      type => 'pos',
      source => 'extra_args',
      start => $start,
      end => $end
    };
    push @tags, $new_pos_tag;
    # get the pos tags within $span, and at the start and end, and the phrase
    # tags for the span, if any
    my $pos_tags_within = [];
    my $starting_pos_tags = [];
    my $ending_pos_tags = [];
    my $phrase_tags = [];
    for my $tag (@tags) {
      if (exists($tag->{'penn-pos'})) {
	$pos_tags_within = union($pos_tags_within, $tag->{'penn-pos'})
	  if ($tag->{start} >= $start and $tag->{end} <= $end);
	$starting_pos_tags = union($starting_pos_tags, $tag->{'penn-pos'})
	  if ($tag->{start} == $start);
	$ending_pos_tags = union($ending_pos_tags, $tag->{'penn-pos'})
	  if ($tag->{end} == $end);
      } elsif (exists($tag->{'penn-cat'})) {
	$phrase_tags = union($phrase_tags, $tag->{'penn-cat'})
	  if ($tag->{start} == $start and $tag->{end} == $end);
      }
    }
    # If there's a single phrase tag for the span identifying it as ADJP, NP,
    # or VP, make sure the POS matches. For VP or NP, get the morph from the
    # first or last words, if they are in fact verbs or nouns, respectively. If
    # they're not the right POS, just use all morphs for the right POS (but
    # check for -ing and -s endings for VPs).
    $phrase_tags = intersection($phrase_tags, [qw(ADJP NP VP)]);
    if (@$phrase_tags == 1) {
      if ($phrase_tags->[0] eq 'ADJP') {
	$new_pos_tag->{'penn-pos'} = ['JJ'];
      } elsif ($phrase_tags->[0] eq 'NP') {
	my $final_noun_poss =
	  intersection($ending_pos_tags, [qw(NN NNS NNP NNPS)]);
	if (@$final_noun_poss) {
	  $new_pos_tag->{'penn-pos'} = $final_noun_poss;
	} else {
	  $new_pos_tag->{'penn-pos'} = [qw(NN NNS NNP NNPS)];
	}
      } else { # VP
	my $initial_verb_poss =
	  intersection($starting_pos_tags, [qw(VB VBD VBG VBN VBP VBZ)]);
	if (@$initial_verb_poss) {
	  $new_pos_tag->{'penn-pos'} = $initial_verb_poss;
	} else {
	  $new_pos_tag->{'penn-pos'} = [
	    qw(VB VBD VBN VBP),
	    ($phrase_tags->[0]{text} =~ /ing\b/ ? qw(VBG) : ()),
	    ($phrase_tags->[0]{text} =~ /s\b/ ? qw(VBZ) : ())
	  ];
	}
      }
      next;
    }
    # if we can't deduce the POS from the phrase tags, use just the POS tags
    # within the span
    if (@$pos_tags_within) {
      # if it ends with a noun, say it's a noun (with the same morph)
      my $final_noun_poss =
	intersection($ending_pos_tags, [qw(NN NNS NNP NNPS)]);
      if (@$final_noun_poss) {
	$new_pos_tag->{'penn-pos'} = $final_noun_poss;
	next;
      }
      # otherwise, if it starts with a verb, say it's a verb (with the same
      # morph)
      my $initial_verb_poss =
	intersection($starting_pos_tags, [qw(VB VBD VBG VBN VBP VBZ)]);
      if (@$initial_verb_poss) {
	$new_pos_tag->{'penn-pos'} = $initial_verb_poss;
	next;
      }
      # otherwise, if it ends with an adjective or participle, and doesn't
      # contain a preposition, say it's an adjective
      if (@{intersection($ending_pos_tags, [qw(JJ VBN VBG)])} > 0 and
	  not grep { $_ eq 'IN' or $_ eq 'TO' } @$pos_tags_within
	 ) {
	$new_pos_tag->{'penn-pos'} = ['JJ'];
	next;
      }
    } else {
      print STDERR "WARNING: Couldn't find any part of speech tags within span from $start to $end\n";
    }
    # if we failed to find a POS using the rules above, just assume it's a noun
    # of some kind, and print a warning
    print STDERR "WARNING: Couldn't assign a part of speech to span from $start to $end\n";
    print STDERR "  assuming it's a noun of some kind...\n";
    $new_pos_tag->{'penn-pos'} = [qw(NN NNS NNP NNPS)];
  }

  # FIXME this really belongs in Drum.pm, being domain-specific, but if I put
  # it there it means Drum.pm outputs prefix tags, which messes up tagger
  # sorting (I think) because it forms a cycle between Drum and Specialist:
  # Drum->prefix->Specialist->pos->Drum
  push @tags, tag_p_prefixes(@tags);

  return @tags;
}

# tag "p" as a prefix if it's its own (sub)word immediately before (or
# separated by a dash) a word tagged with one of a few phosphorylatable senses
my @phosphorylatable_lftypes = qw(
  GENE
  PROTEIN
  GENE-PROTEIN
  PROTEIN-FAMILY
  MOLECULE
  AMINO-ACID
  MOLECULAR-SITE
  MUTATION
);
sub tag_p_prefixes {
  my @input_tags = @_;
  my @output_tags = ();
  my @phosphorylatable_tags =
    grep {
      my $t = $_;
      $t->{type} eq 'sense' and
      exists($t->{lftype}) and
      (@{intersection($t->{lftype}, \@phosphorylatable_lftypes)} > 0)
    } @input_tags;
  my @dash_tags =
    grep {
      $_->{type} eq 'punctuation' and
      $_->{lex} =~ /^$dash_re$/
    } @input_tags;
  # TODO make this more efficient than this basic n^2 search?
  # for each "p"
  for my $prefix_tag (@input_tags) {
    next unless ($prefix_tag->{type} =~ /^(sub)?word$/ and
		 $prefix_tag->{lex} eq 'p');
    # for each thing we'd accept coming after a "p"
    for my $suffix_tag (@phosphorylatable_tags) {
          # if it does in fact come after
      if ($prefix_tag->{end} == $suffix_tag->{start} or
	  # ... or after a dash
	  ($prefix_tag->{end} + 1 == $suffix_tag->{start} and
	   grep {
	     $_->{start} == $prefix_tag->{end} and
	     $_->{end} == $suffix_tag->{start} 
	   } @dash_tags)
	 ) {
	# then turn the "p" into a prefix tag
        push @output_tags, +{ %$prefix_tag, type => 'prefix' };
      }
    }
  }
  return @output_tags;
}

1;
