#!/usr/bin/perl

package TextTagger::Clauses;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(tag_clauses);

use TextTagger::Tags2Trips qw(sortTags $min_clause_length);

use strict vars;

my $debug = 0;

# strings of (lower case) tokens including commas that shouldn't cause
# clause boundaries
my @comma_exceptions = (
  [qw(and at that time ,)],
  [qw(and , at that time ,)]
);

sub getClauseBoundaries
{ # return a list of positions that could be the end of a clause
  my @allTags =
    sortTags(
      grep {
	my $tag = $_;
	grep { $_ eq $tag->{type} } qw(word number punctuation)
      } @_
    );
  # get comma tags that shouldn't cause clause breaks
  my @non_clause_commas = ();
  for my $tokens (@comma_exceptions) {
    TAG: for my $i (0..(scalar(@allTags) - 1)) {
      my @commas = ();
      for my $j (0..(scalar(@$tokens) - 1)) {
	my $token = $tokens->[$j];
	my $tag = $allTags[$i+$j];
	next TAG unless ($token eq lc($tag->{lex}));
	push @commas, $tag if ($token eq ',');
      }
      push @non_clause_commas, @commas;
    }
  }
  # get positions outside brackets, after semicolons, and after commas unless
  # they're inside numbers with no spaces, or they're exceptions we found above
  my @boundaries = ();
  my $prevCommaEnd = undef;
  my $prevNumberEnd = undef;
  for my $tag (@allTags)
  {
    if ($tag->{lex} =~ /^[;\)\]\}]$/)
    {
      push @boundaries, $tag->{end};
    } elsif ($tag->{lex} =~ /^[\(\[\{]$/)
    {
      push @boundaries, $tag->{start}
        unless (grep { $_ == $tag->{start} } @boundaries);
    } elsif ($tag->{type} eq 'number')
    {
      if ($prevCommaEnd == $tag->{start})
      {
	@boundaries = grep { $_ != $prevCommaEnd } @boundaries;
      }
      $prevNumberEnd = $tag->{end};
    } elsif ($tag->{lex} eq ',' and
             not grep { $_->{start} == $tag->{start} } @non_clause_commas)
    {
      push @boundaries, $tag->{end};
      $prevCommaEnd = $tag->{end} if ($prevNumberEnd == $tag->{start});
    }
  }
  return @boundaries;
}

sub eliminateBoundariesInSentence
{ # return a list of only the boundaries we should use
  my ($boundaries, $start, $end) = @_;
  return () if (@$boundaries == 0 or $end - $start < $min_clause_length * 2);
  # Scan backwards through the boundaries, accumulating clauses until they're
  # big enough. We go backwards in order to prefer attaching small segments to
  # the end of the previous segment.
  my @ret = ($end);
  for my $boundary (reverse(@$boundaries))
  {
    unshift @ret, $boundary if ($ret[0] - $boundary >= $min_clause_length);
  }
  # don't make a short clause at the beginning
  shift @ret while ($ret[0] - $start < $min_clause_length);
  pop @ret; # get rid of $end
  return @ret;
}

sub tag_clauses
{
  my $self = shift;
  my @tags = @_;
  my @input_clauses = grep { $_->{type} eq 'clause' } @tags;
  my @output_clauses = ();
  my @allBoundaries = getClauseBoundaries(@tags);
  print STDERR "\@allBoundaries = (" . join(',', @allBoundaries) . ")\n" if ($debug);
  if (@input_clauses) {
    # get only boundaries that agree with boundaries from the input clauses
    # (within two characters)
    @allBoundaries = grep {
      my $boundary = $_;
      grep { ($boundary <= $_->{start} and $boundary >= $_->{start} - 2) or
             ($boundary >= $_->{end} and $boundary <= $_->{end} + 2)
	   } @input_clauses 
    } @allBoundaries;
    print STDERR "after filtering by input clauses, \@allBoundaries = (" . join(',', @allBoundaries) . ")\n" if ($debug);
  }
  for my $sentence (grep { $_->{type} eq 'sentence' } @tags)
  {
    my @boundaries = grep { $_ > $sentence->{start} and
                            $_ < $sentence->{end} }
		          @allBoundaries;
    @boundaries =
      eliminateBoundariesInSentence(\@boundaries,
				    $sentence->{start}, $sentence->{end});
    print STDERR "in sentence from $sentence->{start} to $sentence->{end}, \@boundaries = (" . join(',', @boundaries) . ")\n" if ($debug);
    my $prevBoundary = $sentence->{start};
    push @boundaries, $sentence->{end};
    for my $boundary (@boundaries)
    {
      # trim spaces from the start of the clause
      while (substr($sentence->{text}, $prevBoundary - $sentence->{start}, 1)
	     =~ /\s/)
      {
	$prevBoundary += 1;
      }
      # add the clause
      push @output_clauses,
           { type => "clause",
	     text => substr($sentence->{text},
		            $prevBoundary - $sentence->{start},
			    $boundary - $prevBoundary),
	     start => $prevBoundary,
	     end => $boundary
	   };
      $prevBoundary = $boundary;
    }
  }
  print STDERR "added " . scalar(@output_clauses) . " clauses total\n" if ($debug);
  return [@output_clauses];
}

push @TextTagger::taggers, {
  name => "clauses",
  tag_function => \&tag_clauses,
  output_types => ['clause'],
  input_types => [qw(word number punctuation sentence)],
  optional_input_types => ['clause']
};

1;
