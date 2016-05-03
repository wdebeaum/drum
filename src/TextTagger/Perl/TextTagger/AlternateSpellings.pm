#!/usr/bin/perl

package TextTagger::AlternateSpellings;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(init_alternate_spellings tag_alternate_spellings);

use strict vars;

my %word2alts = ();

sub init_alternate_spellings {
  open ALTS, "<$ENV{TRIPS_BASE}/etc/TextTagger/alternate-spellings.tsv"
    or die "Can't open alternate-spellings.tsv: $!";
  while (<ALTS>) {
    chomp;
    my $alts = [split(/\t/, lc($_))];
    for my $word (@$alts) {
      # store the same list for each word (don't take out the original word),
      # to save some memory
      $word2alts{$word} = $alts;
    }
  }
  close ALTS;
}

sub tag_alternate_spellings {
  my $self = shift;
  my @tags = @_;
  my @alts = ();
  for my $word_tag (grep { $_->{type} =~ /^(?:sub)?word$/ } @tags) {
    my $word = lc($word_tag->{lex});
    if (exists($word2alts{$word})) {
      for my $alt_word (@{$word2alts{$word}}) {
	# just copy the word tag, changing its type and lex
	push @alts, { %$word_tag,
	              type => 'alternate-spelling',
		      lex => $alt_word
		    }
	  unless ($alt_word eq $word);
      }
    } elsif ($word =~ /s$/ and exists($word2alts{$`})) { # handle -s suffix
      for my $alt_word (@{$word2alts{$`}}) {
	push @alts, { %$word_tag,
	              type => 'alternate-spelling',
		      lex => $alt_word . 's'
		    }
	  unless ($alt_word eq $`);
      }
    }
  }
  return [@alts];
}

push @TextTagger::taggers, {
  name => 'alternate_spellings',
  init_function => \&init_alternate_spellings,
  tag_function => \&tag_alternate_spellings,
  output_types => ['alternate-spelling'],
  input_types => [qw(word subword)]
};

