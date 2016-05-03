#!/usr/bin/perl

=combined_tag_table

(pos/cats in [] are added in add_extra_args)

type		pos/cat		sense		score

word:
pos		penn-pos
pos		penn-pos			score

sense				lftype
sense				lftype	dsi
sense		penn-pos	wn-sense-keys

named-entity	[NNP NNPS]	lftype
street-address	[NNP]		lftype

prefer:
sentence	[S SBARQ SINV SQ]
phrase		penn-cat			score

=cut

package TextTagger::CombineTags;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(combine_tags);

use Data::Dumper;
use TextTagger::Util qw(sets_equal union intersection remove_duplicates is_subset);

use strict vars;

my $debug = 0;

# note: assumes tag list is sorted
sub remove_overlapping_but_nonnesting_phrase_tags {
  my @old_tags = @_;
  my @new_tags = @_;
  # for each tag $a at index $i
  for my $i (0..$#old_tags) {
    my $a = $old_tags[$i];
    next unless ($a->{type} eq 'phrase');
    # search backward from $a, for each tag $b at index $j
    for (my $j = $i - 1; $j >= 0; $j--) {
      my $b = $old_tags[$j];
      last if ($a->{start} >= $b->{end}); # no more overlaps to find for $a
      next unless ($b->{type} eq 'phrase');
      #   ---A---
      # ---B---
      if ($a->{start} > $b->{start} &&
	  $a->{start} < $b->{end} &&
	  $a->{end} > $b->{end}) {
	# remove both A and B without changing indices for now
	$new_tags[$i] = undef;
	$new_tags[$j] = undef;
      }
    }
  }
  return grep { defined($_) } @new_tags; # really remove stuff
}

# given a list of tags, return a list of listrefs of tags, such that the tags
# in each listref cover the same span
sub group_tags_by_span {
  my @tags = @_;
  my %frame_to_tags = ();
  for my $tag (@tags) {
    push @{$frame_to_tags{$tag->{start} . ' ' . $tag->{end}}}, $tag;
  }
  return values %frame_to_tags;
}

# Return a value that is eq for all lftypes we want to consider equivalent when
# testing whether sense tags are combinable. Right now this just means
# returning GENE-PROTEIN for each of its subtypes.
sub lftype_equivalence_class {
  my $_ = shift;
  if (/^(GENE|PROTEIN|PROTEIN-FAMILY)$/i) {
    return 'GENE-PROTEIN';
  } else {
    return $_;
  }
}

# Given a list of lftypes, return a list with any equivalent lftypes collapsed.
# Right now this just means that if we have more than one of the GENE-PROTEIN
# types, we collapse them all to GENE-PROTEIN.
sub collapse_equivalent_lftypes {
  my $old_lftypes = shift;
  my $num_gene_protein =
    grep /^(GENE|PROTEIN|PROTEIN-FAMILY|GENE-PROTEIN)$/i, @$old_lftypes;
  if ($num_gene_protein > 1) {
    return
      [ 'GENE-PROTEIN',
        grep !/^(GENE|PROTEIN|PROTEIN-FAMILY|GENE-PROTEIN)$/i, @$old_lftypes
      ];
  } else {
    return $old_lftypes;
  }
}

sub sense_tags_are_combinable {
  my ($old_tag, $new_tag) = @_;
  # we can combine $old_tag and $new_tag if...
  ( # neither has penn-pos
    ((not defined($old_tag->{'penn-pos'})) and
     (not defined($new_tag->{'penn-pos'}))
    ) or
    # they have the same penn-pos
    (defined($old_tag->{'penn-pos'}) and
     defined($new_tag->{'penn-pos'}) and
     sets_equal($old_tag->{'penn-pos'}, $new_tag->{'penn-pos'})
    )
  ) and
  ( # neither has domain-specific-info
    ((not defined($old_tag->{'domain-specific-info'})) and
     (not defined($new_tag->{'domain-specific-info'}))
    ) or
    # they have the same set of sense values
    sets_equal(
      [ (defined($old_tag->{lftype})?
          (map { lftype_equivalence_class($_) } @{$old_tag->{lftype}}) : ()),
        (defined($old_tag->{'wn-sense-keys'})?
	  @{$old_tag->{'wn-sense-keys'}} : ())
      ],
      [ (defined($new_tag->{lftype})?
          (map { lftype_equivalence_class($_) } @{$new_tag->{lftype}}) : ()),
        (defined($new_tag->{'wn-sense-keys'})?
	  @{$new_tag->{'wn-sense-keys'}} : ())
      ]
    )
  )
}

sub sense_tag_to_info {
  my $tag = shift;
  return +{
    (exists($tag->{'penn-pos'}) ?
      ('penn-pos' => $tag->{'penn-pos'}) : ()),
    (exists($tag->{'wn-sense-keys'}) ?
      ('wn-sense-keys' => $tag->{'wn-sense-keys'}) : ()),
    (exists($tag->{lftype}) ?
      (lftype => collapse_equivalent_lftypes($tag->{lftype})) : ()),
    (exists($tag->{score}) ?
      (score => $tag->{score}) : ()),
    (exists($tag->{'domain-specific-info'}) ?
      ('domain-specific-info' => [$tag->{'domain-specific-info'}]) : ())
  };
}

# combine sense tags (for the same span) with each other, returning a list of sense info lists
sub combine_sense_tags {
  my @old_tags = @_;
  my @new_infos = ();
  # first, look for a single input sense tag, and just use that if it exists
  for my $old_tag (@old_tags) {
    if ((grep { $_ eq $old_tag->{source} } qw(input xml_input terms_input terms_from_file)) and
        ((exists($old_tag->{lftype}) and @{$old_tag->{lftype}} == 1) or
	 (exists($old_tag->{'wn-sense-keys'}) and @{$old_tag->{'wn-sense-keys'}} == 1))) {
      push @new_infos, sense_tag_to_info($old_tag);
    }
  }
  if (@new_infos == 1) { # found exactly one, return it
    return @new_infos;
  } else { # otherwise start over and fall back on the old strategy
    @new_infos = ();
  }
  # stably sort unscored tags before scored, and high scores before low
  use sort 'stable';
  @old_tags = sort {
    my $cmp1 = (exists($a->{score}) <=> exists($b->{score}));
    ($cmp1 != 0 ? $cmp1 : ($b->{score} <=> $a->{score}))
  } @old_tags;
  for my $old_tag (@old_tags) {
    print STDERR "Trying to combine " . Data::Dumper->Dump([$old_tag], ["*old_tag"]) . " ...\n"
      if ($debug);
    my $added = 0;
    for my $new_info (@new_infos) {
      print STDERR "...with " . Data::Dumper->Dump([$new_info], ["*new_info"]) . "\n"
        if ($debug);
      if (sense_tags_are_combinable($old_tag, $new_info)) {
	# add stuff from $old_tag into $new_info
	$new_info->{score} = $old_tag->{score}
	  if (exists($old_tag->{score}) and
	      $old_tag->{score} > $new_info->{score}); # take max score
	$new_info->{'domain-specific-info'} =
	  union($new_info->{'domain-specific-info'} || [],
		[$old_tag->{'domain-specific-info'}])
	    if (exists($old_tag->{'domain-specific-info'}));
	$new_info->{lftype} =
	  collapse_equivalent_lftypes(
	      union($new_info->{lftype} || [], $old_tag->{lftype}))
	    if (exists($old_tag->{lftype}));
	$new_info->{'wn-sense-keys'} =
	  union($new_info->{'wn-sense-keys'} || [],
	        $old_tag->{'wn-sense-keys'})
	    if (exists($old_tag->{'wn-sense-keys'}));
	$added = 1;
	print STDERR "Success! " . Data::Dumper->Dump([$new_info], ["*new_info"]) . "\n"
	  if ($debug);
      }
    }
    push @new_infos, sense_tag_to_info($old_tag) unless ($added);
  }
  return @new_infos;
}

# combine pos tags (for the same span) with each other, including penn-pos in
# sense infos (removing them if they end up with no penn-pos)
#
# POS tags come from Stanford POS, Stanford Parser, MetaMap, Prescriptions and
# ExtraArgs. Sense tags with POS information in them come from WordNet. Also,
# ExtraArgs sometimes adds POS information to certain kinds of sense tags.
sub combine_pos_tags {
  my @old_pos_tags = @{shift(@_)};
  my @old_sense_infos = @{shift(@_)};
  # repartition the above two sets of tags into the below five
  my @unioned_tags = ();
  my @intersected_tags = ();
  my @fallback_tags = (); # only add these if there are no others
  my @intersected_infos = ();
  my @posless_infos = ();
  for my $old_pos_tag (@old_pos_tags) {
    if ($old_pos_tag->{source} eq 'specialist') {
      push @fallback_tags, $old_pos_tag;
    } elsif (grep { $_ eq $old_pos_tag->{source} }
             qw(stanford_pos stanford_parser stanford_core_nlp
	        cj_parser enju extra_args
		input xml_input terms_input terms_from_file)) {
      push @unioned_tags, $old_pos_tag;
    } else { # MetaMap, Prescriptions
      push @intersected_tags, $old_pos_tag;
    }
  }
  for my $old_sense_info (@old_sense_infos) {
    if (exists($old_sense_info->{'penn-pos'})) { # WordNet, ExtraArgs
      push @intersected_infos, $old_sense_info;
    } else { # everything else
      push @posless_infos, $old_sense_info;
    }
  }
  print STDERR Data::Dumper->Dump(
    [\@unioned_tags, \@intersected_tags, \@fallback_tags, \@intersected_infos, \@posless_infos], 
    [qw(*unioned_tags *intersected_tags *fallback_tags *intersected_infos *posless_infos)]
  ) if ($debug);
  
  my $pos_union = [];
  my @new_infos = ();
  # take the union of the penn-pos of the @unioned_tags
  for (@unioned_tags) { $pos_union = union($pos_union, $_->{'penn-pos'}); }
  if (@intersected_tags) {
    # try to intersect that union with the @intersected_tags
    my $pos_union_intersection = $pos_union;
    for (@intersected_tags) {
      $pos_union_intersection =
	intersection($pos_union_intersection, $_->{'penn-pos'});
    }
    if (@$pos_union_intersection) {
      $pos_union = $pos_union_intersection;
    } else {
      # failing that, just take the union again
      for (@intersected_tags) {
	$pos_union = union($pos_union, $_->{'penn-pos'});
      }
    }
  }
  # intersect that union with each of the penn-pos of the @intersected_infos
  for my $intersected_info (@intersected_infos) {
    my $pos_intersection =
      intersection($pos_union, $intersected_info->{'penn-pos'});
    push @new_infos, +{ %$intersected_info, 'penn-pos' => $pos_intersection }
      if (@$pos_intersection);
  }
  unless (@new_infos) {
    # We failed to add any of the @intersected_infos, so just give up on the
    # intersection.
    # add their penn-pos to the union
    for (@intersected_infos) {
      $pos_union = union($pos_union,$_->{'penn-pos'});
    }
    # and add the sense infos unchanged
    push @new_infos, @intersected_infos;
  }
  unless (@new_infos or @$pos_union) {
    # We still failed to add any POS tags, so fall back to the fallback tags
    for (@fallback_tags) {
      $pos_union = union($pos_union, $_->{'penn-pos'});
    }
    # remove Already In Trips Lexicon markers from Specialist
    @$pos_union = grep { $_ !~ /^AITLS?$/ } @$pos_union;
  }
  if (@$pos_union) {
    # add $pos_union to the @posless_infos as we add them to @new_infos
    for my $posless_info (@posless_infos) {
      push @new_infos, +{ %$posless_info, 'penn-pos' => $pos_union };
    }
    unless (@new_infos) {
      # we still failed to add any infos, so just add a generic POS info
      push @new_infos, { 'penn-pos' => $pos_union };
    }
  } else { # we have no POS information, just pass through the @posless_infos
    push @new_infos, @posless_infos;
  }
  # add in domain-specific-info from any POS tags that have it, to the new
  # infos that match them
  for my $old_pos_tag (@old_pos_tags) {
    if (exists($old_pos_tag->{'domain-specific-info'})) {
      for my $new_info (@new_infos) {
	if (is_subset($new_info->{'penn-pos'},
		      $old_pos_tag->{'penn-pos'})) {
	  push @{$new_info->{'domain-specific-info'}}, $old_pos_tag->{'domain-specific-info'};
	}
      }
    }
  }
  return @new_infos;
}

# combine phrase tags (for the same span) with each other
# the first argument is the list of sources we got any phrase tags from for the
# whole input; the second argument is a prototype for new tags, containing
# lex/text, start, and end; and the rest of the arguments are the phrase tags
# to combine
sub combine_phrase_tags {
  my $self = shift;
  my @phrase_tag_sources = @{shift(@_)};
  my %tag_prototype = %{shift(@_)};
  my @old_tags = @_;
  my %cat2score = ();
  my %score2cats = ();
  my $cats = [];
  my $enju_cats = [];
  # if -parsers-must-agree true,
  # if we got phrase tags from more than one tagger for this text, and at least
  # one of them marks this span as a phrase, report only phrases they all agree
  # on, using the score from StanfordParser if it's among them
  my %source2tags = ();
  for my $tag (@old_tags) {
    push @{$source2tags{$tag->{source}}}, $tag if ($tag->{type} eq 'phrase');
  }
  if ($self->{parsers_must_agree} and
      @phrase_tag_sources > 1 and %source2tags) {
    if (sets_equal([@phrase_tag_sources], [keys %source2tags])) {
      # for each tag from one arbitrary source
      for my $tag (@{$source2tags{(keys %source2tags)[0]}}) {
	# for each penn-cat in that tag
	CAT: for my $cat (@{$tag->{'penn-cat'}}) {
	  my $sp_tag = undef;
	  # skip it unless all sources have that penn-cat
	  for my $source (@phrase_tag_sources) {
	    my ($tag_from_source) =
	      grep { grep { $_ eq $cat } @{$_->{'penn-cat'}} }
		   @{$source2tags{$source}};
	    next CAT unless (defined($tag_from_source));
	    $sp_tag = $tag_from_source if ($source eq 'stanford_parser');
	  }
	  # get the score if it's from SP
	  if (defined($sp_tag)) {
	    $cat2score{$cat} = $sp_tag->{score};
	    $score2cats{$sp_tag->{score}} =
	      union($score2cats{$sp_tag->{score}} || [], [$cat]);
	  }
	  # add the penn-cat to the list
	  $cats = union($cats, [$cat]);
	  # add the corresponding enju-cat
	  for my $enju_tag (@{$source2tags{enju}}) {
	    $enju_cats = union($enju_cats, $enju_tag->{'enju-cat'})
	      if (grep { $_ eq $cat } @{$enju_tag->{'penn-cat'}});
	  }
	}
      }
    }
  } else { # otherwise just take the union of penn-cats from all sources
    for my $tag (@old_tags) {
      if (exists($tag->{score})) {
	for my $cat (@{$tag->{'penn-cat'}}) {
	  $cat2score{$cat} = $tag->{score};
	  $score2cats{$tag->{score}} =
	    union($score2cats{$tag->{score}} || [], [$cat]);
	}
      }
      $cats = union($cats, $tag->{'penn-cat'});
      $enju_cats = union($enju_cats, $tag->{'enju-cat'})
	if (exists($tag->{'enju-cat'}));
    }
  }
  my @new_tags = ();
  # add scored cats
  # FIXME this doesn't get corresponding enju cats... do I care? we don't
  # really use SP anymore anyway
  for my $score (keys %score2cats) {
    push @new_tags,
      +{ %tag_prototype,
         'penn-cat' => $score2cats{$score},
	 score => $score
      };
  }
  # add unscored cats (if any)
  $cats = [grep { not exists($cat2score{$_}) } @$cats];
  push @new_tags,
    +{ %tag_prototype,
       'penn-cat' => $cats,
       (@$enju_cats ? ('enju-cat' => $enju_cats) : ())
    }
      if (@$cats);

  return @new_tags;
}

# A function to be used to sort tags so that those better to use for filling
# :lex appear later in the list.
# For now this just means that meta_map and specialist tags sort before other
# tags, since MetaMap ignores certain morphology that we want to keep, and
# specialist sometimes downcases words.
sub cmp_lex {
  my ($a, $b) = @_;
  return 0 if ($a->{source} eq $b->{source});
  if (grep { $_ eq $a->{source} } qw(meta_map specialist)) {
    return -1;
  } elsif (grep { $_ eq $b->{source} } qw(meta_map specialist)) {
    return 1;
  } else {
    return 0;
  }
}

# given a list of tags covering the same span, return a (hopefully smaller)
# list of tags combining the values of some of the fields
sub combine_tags_for_span {
  my ($self, $phrase_tag_sources, @old_tags) = @_;
  return () if (@old_tags == 0);
  print STDERR "Combining tags for $old_tags[0]->{start} - $old_tags[0]->{end}\n"
    if ($debug);

  # collect information that's the same for all new tags
  my %tag_prototype = ( start => $old_tags[0]->{start},
                        end => $old_tags[0]->{end}
		      );
  $tag_prototype{uttnum} = $old_tags[0]->{uttnum}
    if (exists($old_tags[0]->{uttnum}));
  my $alternate_spellings = [];
  for my $old_tag (sort { cmp_lex($a, $b) } @old_tags) {
    if ($old_tag->{type} eq 'alternate-spelling') {
      push @$alternate_spellings, $old_tag->{lex};
    } elsif (exists($old_tag->{lex})) {
      $tag_prototype{lex} = $old_tag->{lex};
    } # not elsif, since we want both lex and text
    if (exists($old_tag->{text})) {
      $tag_prototype{text} = $old_tag->{text};
    }
  }

  # partition @old_tags into the following categories:
  my @old_pos_tags; # have only penn-pos
  my @old_sense_tags; # have lftype or wn-sense-keys (and possibly penn-pos)
  my @old_phrase_tags; # have only penn-cat
#  my @other_tags;

  # if this is true, we construct a word tag for this span even if no specific
  # tags get through all the filtering
  my $output_at_least_a_word_tag = 0;
  
  # if this is true, the word tag becomes a prefix tag instead
  my $is_prefix = 0;
  
  for my $old_tag (@old_tags) {
    if (exists($old_tag->{'penn-cat'})) { # contributes to prefer messages
      push @old_phrase_tags, $old_tag;
    } elsif (exists($old_tag->{lftype}) or
             exists($old_tag->{'wn-sense-keys'})) { # senses (word msgs)
      push @old_sense_tags, $old_tag;
      $output_at_least_a_word_tag = 1;
    } elsif (exists($old_tag->{'penn-pos'})) { # pos (word msgs)
      push @old_pos_tags, $old_tag;
      $output_at_least_a_word_tag = 1;
    } elsif (grep { $_ eq $old_tag->{type} }
		  qw(word subword ending number subnumber punctuation
		     alternate-spelling)
	    ) {
      # Tags from Words, Punctuation, and AlternateSpellings we just discard
      # because they only contribute to the common args for all tags for this
      # span.
      $output_at_least_a_word_tag = 1;
    } elsif ($old_tag->{type} eq 'prefix') {
      $output_at_least_a_word_tag = 1;
      $is_prefix = 1;
    } elsif (grep { $_ eq $old_tag->{type} } qw(phrase pos sense)) {
      # We also discard phrase, pos, and sense tags that weren't caught by the
      # other branches of this if/elsif, since we can't use them and they just
      # get in the way later.
    } else { 
      # clause, quotation, and spaced-chunk exist outside the word/prefer
      # distinction, so we just pass them through:
      # On second thought, nobody listens to anything but word/prefer in
      # lattice format, so don't bother.
      #push @other_tags, $old_tag;
    }
  }

  # get sense info
  my @sense_info = combine_sense_tags(@old_sense_tags);

  # combine pos tags with each other and with the sense info
  @sense_info = combine_pos_tags(\@old_pos_tags, \@sense_info);

  # get prefer messages/tags
  my @prefer_tags = combine_phrase_tags($self,
					$phrase_tag_sources,
                                        +{ %tag_prototype, type => 'prefer' },
  					@old_phrase_tags);

  # add alternate spellings to sense info
  if (@$alternate_spellings) {
    $alternate_spellings = remove_duplicates($alternate_spellings);
    if (@sense_info) {
      for (@sense_info) {
	$_->{'alternate-spellings'} = $alternate_spellings;
      }
    } else {
      push @sense_info, { 'alternate-spellings' => $alternate_spellings };
    }
  }

  # collect all the tags to return
  my @new_tags = (((@sense_info > 0)?
		     (+{ %tag_prototype,
		         'type' => ($is_prefix ? 'prefix' : 'word'),
		         'sense-info' => [@sense_info]
		     }) : ()),
		  @prefer_tags
		  #, @other_tags
		 );

  # add a simple word tag if we don't have any other (non-prefer) tags
  push @new_tags, +{ %tag_prototype, type => ($is_prefix ? 'prefix' : 'word') }
    if ($output_at_least_a_word_tag and
        not grep { $_->{type} ne 'prefer' } @new_tags);

  return @new_tags;
}

# remove the REFERENTIAL-SEM lftype when others are available for the same span
sub remove_redundant_names {
  map {
    my $old_tag = $_;
    if ( $old_tag->{type} eq 'word' and
         exists($old_tag->{'sense-info'}) and
         grep {
	   exists($_->{'wn-sense-keys'}) or
	   (exists($_->{lftype}) and
	    grep { $_ ne 'REFERENTIAL-SEM' } @{$_->{lftype}})
	 } @{$old_tag->{'sense-info'}}
       ) {
      # $old_tag is a word tag with at least one sense info with at least one
      # lftype not equal to REFERENTIAL-SEM, or a wn-sense-keys, so filter out
      # all the REFERENTIAL-SEMs
      +{ %$old_tag,
         'sense-info' => [
	   map {
	     if (exists($_->{lftype})) {
	       +{ %$_,
		  lftype => [grep { $_ ne 'REFERENTIAL-SEM' } @{$_->{lftype}}]
	       }
	     } else {
	       $_
	     }
	   } grep {
	     exists($_->{'wn-sense-keys'}) or
	     grep { $_ ne 'REFERENTIAL-SEM' } @{$_->{lftype}}
	   } @{$old_tag->{'sense-info'}}
	 ]
      }
    } else {
      $old_tag
    }
  } @_
}

# given a sorted list of tags, return a (hopefully smaller) list of tags, where
# some have been removed, and others combined using combine_tags_for_span
sub combine_tags {
  my $self = shift;
  my @tags = @_;
  my $phrase_tag_sources =
    remove_duplicates([map { $_->{source} }
                       grep { $_->{type} eq 'phrase' }
		       @tags
		     ]);
  @tags = remove_overlapping_but_nonnesting_phrase_tags(@tags);
  return remove_redundant_names(
           map { combine_tags_for_span($self, $phrase_tag_sources, @$_) }
	       group_tags_by_span(@tags)
	 );
}

1;
