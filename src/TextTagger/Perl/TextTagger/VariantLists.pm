package TextTagger::VariantLists;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(tag_variant_lists);

use TextTagger::Util qw(remove_duplicates);
use TextTagger::Words qw(tag_words);

use Data::Dumper;

use strict vars;

my $debug = 0;

# examples: 
# Erk1/2
# Erk1/2-dependent
# Smad1/5/8
# fooA/B
# fooβ/γ
# Note that dashes (e.g. MEK1-2) are a different phenomenon, indicating ranges
# rather than lists, and so are not covered by this tagger. And lists involving
# multiple words (e.g. "N-, K-, and H-Ras") are probably better handled using
# the Parser, so they're also not covered here.
sub tag_variant_lists {
  my ($self, $text) = @_;
  my @tags = ();
  while ($text =~ m=
      ( \w+ ) # shared prefix
      ( # variant suffix list
        # numbers with optional single letters
        (?<! \d ) \d+ \p{L}? (?: [/&] \d+ \p{L}? )+ |
	(?<! [A-Z] ) [A-Z] (?: [/&] [A-Z] )+ | # capital latin letters
	(?<! \p{greek} ) \p{greek} (?: [/&] \p{greek} )+ # greek letters
      )
      (?! \w ) # word doesn't continue
    =xg) {
    my ($prefix, $suffixes) = ($1, $2);
    my ($start, $middle) = ($-[0], $+[1]);
    # Tag words in the prefix, and get all substrings starting at a word
    # boundary and going to the end of the prefix.
    # Note that we can't just use input_types=>['word'] because we want to get
    # the tags without even the first suffix. And this is simpler anyway.
    my $prefix_tags = tag_words($self, $prefix);
    my @prefixes =
      map { substr($prefix, $_) }
          @{remove_duplicates([map { $_->{start} } @$prefix_tags])};
    my @suffixes = split(m=[/&]=, $suffixes);
    print STDERR Data::Dumper->Dump([$prefix, $suffixes, $start, $middle, \@prefixes, \@suffixes], [qw(prefix suffixes start middle *prefixes *suffixes)])
      if ($debug);
    # discard first since we should already get it as a normal word
    my $first_suffix = shift @suffixes;
    # the start of the first tag (based on the second suffix)
    my $suffix_start = $middle + length($first_suffix) + 1; # add 1 for slash
    for my $suffix (@suffixes) {
      my $suffix_end = $suffix_start + length($suffix);
      print STDERR Data::Dumper->Dump([$suffix, $suffix_start, $suffix_end], [qw(suffix suffix_start suffix_end)])
        if ($debug);
      # add a tag with this suffix for each prefix substring
      for (@prefixes) {
	push @tags, +{
	  type => 'alternate-spelling',
	  lex => $_ . $suffix,
	  start => $suffix_start,
	  end => $suffix_end
	};
      }
      $suffix_start = $suffix_end + 1; # add 1 for slash
    }
  }
  return [@tags];
}

push @TextTagger::taggers, {
  name => 'variant_lists',
  tag_function => \&tag_variant_lists,
  input_text => 1,
  output_types => ['alternate-spelling']
};

1;
