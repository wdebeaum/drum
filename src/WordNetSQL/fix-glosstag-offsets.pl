#!/usr/bin/perl

# fix-glosstag-offsets.pl - adjust offsets in glosstags.psv so they line up with the original form of the WordNet glosses, with the quotes around the examples
# William de Beaumont
# 2011-03-09

use strict vars;
use Tie::File;
my $have_pb = eval "use Term::ProgressBar; 1";

my %synset2glossparts = ();

print "reading defs and exs from glosstags.psv...\n";
my $num_lines = `wc -l glosstags.psv`;
my $progress = ($have_pb ? Term::ProgressBar->new({ count => $num_lines, ETA => 'linear' }) : undef);
my $next_update = 0;
open PSV, "<glosstags.psv" or die "Can't open glosstags.psv: $!\n";
while (<PSV>) {
  $next_update = $progress->update($.)
    if ($have_pb and $. >= $next_update);
  chomp;
  my ($synset_offset, $ss_type, $start, $end, $tag_type, $pos, $sense_key) = split /\|/;
  my $synset_id = sprintf("$ss_type%08d", $synset_offset);
  my $offset = 2 * grep { $_->{type} eq 'ex' } @{$synset2glossparts{$synset_id}};
  if ($tag_type eq 'ex') {
    $offset += 1;
  } elsif ($tag_type ne 'def') {
    next;
  }
  push @{$synset2glossparts{$synset_id}}, +{
    type => $tag_type,
    old_start => $start,
    old_end => $end,
    offset => $offset
  };
}
close PSV;

print "editing offsets in glosstags.psv...\n";
$progress = ($have_pb ? Term::ProgressBar->new({ count => $num_lines, ETA => 'linear' }) : undef);
$next_update = 0;
my $line_number = 0;
my @tags;
tie @tags, 'Tie::File', "glosstags.psv" or die "Can't tie glosstags.psv: $!\n";
for my $tag (@tags) {
  $next_update = $progress->update($line_number)
    if ($have_pb and $line_number >= $next_update);
  $line_number++;
  my ($synset_offset, $ss_type, $start, $end, $tag_type, $pos, $sense_key) = split /\|/, $tag;
  my $synset_id = sprintf("$ss_type%08d", $synset_offset);
  my $offset = 0;
  for my $part (@{$synset2glossparts{$synset_id}}) {
    if ($start >= $part->{old_start} and $start < $part->{old_end}) {
      $offset = $part->{offset};
      last;
    }
  }
  $start += $offset;
  $end += $offset;
  $end -= 1 if ($tag_type =~ /^(?:def|ex)$/);
  $tag = join('|', $synset_offset, $ss_type, $start, $end, $tag_type, $pos, $sense_key);
}
untie @tags;

print "done.\n";

