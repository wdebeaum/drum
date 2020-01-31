package TextTagger::Affixes;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(tag_affixes);

use TextTagger::Normalize qw($dash_re);

use strict vars;

# This list constructed as the union of the lists at these three websites,
# excluding anything under 3 letters:
# http://grammar.about.com/od/words/a/comprefix07.htm
# http://en.wikipedia.org/wiki/Prefix#List_of_English_derivational_prefixes
# http://en.wikipedia.org/wiki/English_prefixes#List_of_English_prefixes
# A select few 2-letter prefixes have been added to the end of the list, and
# other longer prefixes have been added as needed.
my @prefixes = qw(
acro Afro allo ambi amphi ana Anglo ante anti apo arch astro auto
bio
circum cis col com con cor contra counter cryo crypto
demi demo deuter dia dif dis down duo dys
eco electro epi Euro extra
Franco fore
geo gryo
hemi hetero hexa hind homo hydro hyper hypo
ideo idio immuno Indo infra inter intra iso
macro macr mal maxi megalo mega meta meso micro midi mid mini mis mono mon multi mult
neo non
octo omni ortho out over
paleo pan para ped penta peri per phospho photo pod poly post preter pre pros proto pro pseudo pyro
quadri quasi
retro
self semi socio step sub super supra sup sur syl sym syn
tele tetra trans tri twi
ultra under uni
vice
with
xeno
co de di re un
);
my $prefix_re = join('|',@prefixes);
$prefix_re = qr/^ (?: $prefix_re ) $/xi;

my @prefixes_by_length = ();
for (@prefixes) {
  push @{$prefixes_by_length[length($_)]}, $_;
}
my @prefixed_word_res_by_length =
  map {
    if (@$_ > 0) {
      my $prefix_bl_re = join('|', @$_);
      qr/^ ( $prefix_bl_re ) ( \p{Letter}{2,} ) $/xi;
    } else {
      undef;
    }
  } @prefixes_by_length;

sub tag_affixes {
  my ($self, $text) = @_;
  my @tags = ();
  # scan for words, including dashes if any
  while ($text =~ / \b (?: \p{Letter} | $dash_re )+ \b /xgi) {
    my $whole_word = $&;
    my ($start, $end) = ($-[0], $+[0]);
    my @dashed_chunks = split($dash_re, $whole_word, -1);
    my $last_chunk = pop @dashed_chunks;
    # check each non-last chunk for prefixhood
    my $chunk_start = $start;
    for my $chunk (@dashed_chunks) {
      my $chunk_end = $chunk_start + length($chunk);
      if ($chunk =~ $prefix_re) {
	push @tags, +{
	  type => 'prefix',
	  lex => $chunk,
	  start => $chunk_start,
	  end => $chunk_end
	};
      }
      $chunk_start = $chunk_end + 1;
    }
    # for the last chunk, check each prefix length
    for my $prefixed_word_re (@prefixed_word_res_by_length) {
      if (defined($prefixed_word_re) and
	  $last_chunk =~ $prefixed_word_re) {
	my ($prefix, $root_and_suffixes) = ($1, $2);
	my $middle = $chunk_start + $+[1];
	my $tag = +{
	  type => 'prefix',
	  lex => $prefix,
	  start => $chunk_start,
	  end => $middle,
	  # save the root tag here so we can remove it in CombineTags if needed
	  root => +{
	    type => 'word',
	    lex => $root_and_suffixes,
	    start => $middle,
	    end => $end
	  }
	};
	push @tags, $tag, $tag->{root};
      }
    }
  }
  return [@tags];
}

push @TextTagger::taggers, {
  name => "affixes",
  tag_function => \&tag_affixes,
  output_types => [qw(prefix word)],
  input_text => 1
};

1;

