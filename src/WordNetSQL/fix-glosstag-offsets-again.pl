#!/usr/bin/perl

# fix-glosstag-offsets-again.pl - adjust offsets in glosstags table so they line up with the original form of the WordNet glosses. Specifically, correct missing `' quotes.
# William de Beaumont
# 2011-03-17

use DBI;
use strict vars;
my $have_pb = eval "use Term::ProgressBar; 1";

# open the WN database
my $dbname = shift(@ARGV);
my $dbh = DBI->connect("DBI:SQLite(RaiseError=>1):dbname=$dbname", '', '');
$dbh->do("PRAGMA synchronous=OFF;");

# make the progress bar
my $num_affected_glosses = $dbh->selectall_arrayref(<<EOQ)->[0]->[0];
  SELECT COUNT(*) FROM synsets WHERE gloss LIKE '%`%''%';
EOQ
my $glosses_updated = 0;
my $pb = ($have_pb ? Term::ProgressBar->new({ count => $num_affected_glosses, ETA => 'linear' }) : undef);
my $next_pb_update = 0;

# set up queries/updates
my $start_offset_update = $dbh->prepare(<<EOU);
  UPDATE glosstags
  SET start = start + ?
  WHERE synset_offset = ? AND ss_type = ? AND
        start BETWEEN ? AND ? - 1;
EOU
my $end_offset_update = $dbh->prepare(<<EOU);
  UPDATE glosstags
  SET end = end + ?
  WHERE synset_offset = ? AND ss_type = ? AND
        end BETWEEN ? + 1 AND ?;
EOU
my $gloss_query = $dbh->prepare(<<EOQ);
  SELECT synset_offset, ss_type, gloss
  FROM synsets
  WHERE gloss LIKE '%`%''%';
EOQ

# for each affected gloss
$gloss_query->execute();
while (my @row = $gloss_query->fetchrow_array()) {
  my ($synset_offset, $ss_type, $gloss) = @row;

  # update progress bar
  $next_pb_update = $pb->update($glosses_updated)
    if ($have_pb and $glosses_updated >= $next_pb_update);
  $glosses_updated++;

  # split gloss into chunks based on `' quotes, keeping track of the offset
  # between WN and glosstag versions of the gloss
  my $chunk_start = 0;
  if ($gloss =~ /^\(.*?\) /) { # don't correct `' quotes inside initial parens
    $chunk_start = length($&);
    $gloss = $';
  }
  my $offset = 0;
  my @stack = ();
  for my $chunk (split /((?<!\w)`|'(?!\w))/, $gloss) {
    my $chunk_end = $chunk_start + length($chunk);
    if ($chunk eq "`" or $chunk eq "'") {
      $offset++;
    } elsif ($offset > 0) {
      # defer the actual updates so we can do them in reverse order
      push @stack, [$offset, $synset_offset, $ss_type, $chunk_start - $offset, $chunk_end - $offset];
    }
    $chunk_start = $chunk_end;
  }
  # do updates in reverse order so they don't step on each other
  for (reverse @stack) {
    $start_offset_update->execute(@$_);
    $end_offset_update->execute(@$_);
  }
}

# clean up
undef $start_offset_update;
undef $end_offset_update;
undef $gloss_query;
undef $dbh;

