#!/usr/bin/perl

package TextTagger::CapitalizedNames;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(tag_capitalized_names);

use strict vars;

sub tag_capitalized_names
{
  my $self = shift;
  my @tags = @_;
  my @names = ();
  for my $word (@tags)
  {
    if (
        # it actually is a word
        $word->{type} eq "word" and
        # the word is capitalized (not all-caps)
        $word->{lex} =~ /^[A-Z]+[a-z]/ and
	# and isn't already counted as (starting) a name
	(not grep { $_->{type} eq "named-entity" and
	            $_->{start} == $word->{start} }
		  @tags) and
        # and doesn't start a sentence
        (not grep { $_->{type} eq "sentence" and
	            $_->{start} == $word->{start} }
		  @tags)
	)
    {
      push @names, { type => "named-entity",
                     lftype => ["REFERENTIAL-SEM"],
		     lex => $word->{lex},
		     start => $word->{start},
		     end => $word->{end}
                     };
    }
  }
  return \@names;
}

push @TextTagger::taggers, {
  name => "capitalized_names",
  tag_function => \&tag_capitalized_names,
  output_types => ['named-entity'],
  input_types => [qw(word)],
  optional_input_types => [qw(sentence named-entity)]
};
