#!/usr/bin/perl

package TextTagger::Words;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(tag_words);

use TextTagger::Util qw(match2tag);

use strict vars;

# FIXME? "what's-its-name" doesn't get "s" as a word
# see also the WARNING in Punctuation.pm
my @endings = qw('s 'm 'd 'll 're 've n't);
# add copies of the endings with Unicode right single quote instead of ASCII
# apostrophe
push @endings, map { my $e = $_; $e =~ s/'/\x{2019}/; $e } @endings;
my $endings_re = '(?:' . join('|', @endings) . ')$';

sub tag_subwords
{
  my $whole_word = shift;
  my @subwords = ();
  # all lower, but not after single upper
  # (so we get "bar" from "FOObar" and "foo_bar" but not from "foObar" or "Obar")
  while ($whole_word =~ /(?<!^\p{Lu})(?<![^\p{Lu}]\p{Lu})(?<!\p{Ll})\p{Ll}+/g)
  { push @subwords, { type => "subword", match2tag() }; }
  # all upper, but not the single upper before lower
  # (so we get "A" from "A_ball" but not from "All")
  while ($whole_word =~ /\p{Lu}(?:\p{Lu}+|(?!\p{Ll})|$)/g)
  { push @subwords, { type => "subword", match2tag() }; }
  # initial upper, rest lower, but not (upper,) upper, lower s, end
  while ($whole_word =~ / (?<!\p{Lu}) \p{Lu} \p{Ll}+ |
			  \p{Lu} \p{Ll}{2,} |
			  \p{Lu} (?!s) \p{Ll}
			/xg)
  { push @subwords, { type => "subword", match2tag() }; }
  # all upper, before an initial upper as above
  # (so we get "KQML" from "KQMLString")
  while ($whole_word =~ / \p{Lu}+ (?= \p{Lu} (?: \p{Ll} \p{Ll} | (?!s) \p{Ll}))/xg)
  { push @subwords, { type => "subword", match2tag() }; }
  # all digits
  while ($whole_word =~ /\d+/g)
  { push @subwords, { type => "subnumber", match2tag() }; }
  # return only subwords not identical to the original word
  return [grep { !($_->{start} == 0 and $_->{end} == length($whole_word)) } @subwords];
}

sub tag_words
{
  my $self = shift;
  my $original_text = shift;
  my @words = ();
  while ($original_text =~ /[\w'\x{2019}]+/g)
  {
    my ($word, $start, $end) = ($&, $-[0], $+[0]);
    # don't make words that are all punctuation
    next unless ($word =~ /[^_'\x{2019}]/i);
    push @words, { type => ($word =~ /^\d+$/)? "number" : "word",
    		   lex => $word,
      		   start => $start,
    		   end => $end
    		   };
    if ($word =~ /^$endings_re/i)
    { # the word is only an ending
      $words[-1]->{type} = 'ending';
    } elsif ($word =~ /$endings_re/i)
    { # the word has an ending
      push @words, { type => "ending", 
      		     lex => $&,
		     start => $start + $-[0],
		     end => $start + $+[0]
		     };
      push @words, { type => "word",
      		     lex => $`,
      		     start => $start,
		     end => $start + $-[0]
		     };
    }
    my $base_word = $words[-1];
    for my $subword (@{tag_subwords($base_word->{lex})})
    {
      $subword->{start} += $base_word->{start};
      $subword->{end} += $base_word->{start};
      push @words, $subword;
    }
  }
  return [@words];
}

push @TextTagger::taggers, {
  name => "words",
  tag_function => \&tag_words,
  output_types => [qw(word ending number subword subnumber)],
  input_text => 1
};

1;
