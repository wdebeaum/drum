#!/usr/bin/perl

package TextTagger::Gold;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(load_gold_tags tag_gold);

use TextTagger::Util qw(match2tag);

use strict vars;

my @next_tags = ();

sub load_gold_tags
{
  my ($text, $sense_bracketing) = @_;
  @next_tags = ();
  if (ref($sense_bracketing) eq 'ARRAY')
  {
    my $remaining_text = $text;
    my $end = 0;
    for my $tree (@$sense_bracketing)
    {
      my ($new_tags, $new_remaining_text, undef, $new_end) =
	tree_to_tags($tree, $remaining_text, $end);
      push @next_tags, @$new_tags;
      ($remaining_text, $end) = ($new_remaining_text, $new_end);
    }
  }
}

# return tags that align with the original text starting at start, and the ending position of the last tag
sub tree_to_tags
{
  my ($tree, $remaining_text, $start) = @_;
  my $cat = shift @$tree;
  my @tags = ();
  my $new_start;
  my $end = $start;
  if (ref($tree->[0]) eq 'ARRAY') # a phrase node
  {
    for my $subtree (@$tree)
    {
      my ($new_tags, $new_remaining_text, $new_new_start, $new_end) =
	tree_to_tags($subtree, $remaining_text, $end);
      push @tags, @$new_tags;
      $remaining_text = $new_remaining_text;
      $new_start = $new_new_start unless (defined($new_start));
      $end = $new_end;
    }
    push @tags, { type => 'phrase',
                  cat => [$cat],
		  start => $new_start,
		  end => $end
                };
  } else # a word leaf
  {
    my $pos = $cat;
    my $word = pop @$tree;
    $word =~ s/[\-_]/[\\-_ ]/g;
    $word =~ s/\d+/join(',?', split('',$&))/ge;
    $word =~ s/\$/\\\$/g;
    my $sense = shift @$tree;
    $remaining_text =~ /$word/i
      or die "Word not found in remaining text: $word\n";
    my %match_tag = match2tag();
    $match_tag{start} += $end;
    $match_tag{end} += $end;
    $remaining_text = $';
    $new_start = $match_tag{start};
    $end = $match_tag{end};
    @tags = (
      { type => 'word',
        %match_tag
      },
      { type => 'pos',
        'pos' => [$pos],
        %match_tag
      }
    );
    push @tags, { type => 'sense',
                  lftype => [$sense],
		  %match_tag
		}
      if (defined($sense));
  }
  return (\@tags, $remaining_text, $new_start, $end);
}

sub tag_gold
{
  return [@next_tags];
}

push @TextTagger::taggers, {
  name => "gold",
  tag_function => \&tag_gold,
  output_types => [qw(phrase pos sense word)],
  input_text => 1
};

1;

