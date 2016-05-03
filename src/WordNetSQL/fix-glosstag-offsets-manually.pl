#!/usr/bin/perl

# fix-glosstag-offsets-manually.pl - read manually-edited descriptions of how to fix certain problematic glosses, and do it

use DBI;

use strict vars;

# open the WN database
my $dbname = shift(@ARGV);
my $dbh = DBI->connect("DBI:SQLite(RaiseError=>1):dbname=$dbname", '', '');
$dbh->do("PRAGMA synchronous=OFF;");

# set up queries/updates
my $get_gloss = $dbh->prepare(<<EOSQL);
  SELECT gloss
  FROM synsets
  WHERE synset_offset=? AND ss_type=?;
EOSQL

my $set_gloss = $dbh->prepare(<<EOSQL);
  UPDATE synsets
  SET gloss=?
  WHERE synset_offset=? AND ss_type=?;
EOSQL

my $delete_tag = $dbh->prepare(<<EOSQL);
  DELETE
  FROM glosstags
  WHERE synset_offset=? AND ss_type=? AND
        start=? AND end=? AND tag_type=? AND
	pos=? AND sense_key=? AND lemma=?;
EOSQL

my $insert_tag = $dbh->prepare(<<EOSQL);
  INSERT
  INTO glosstags
  VALUES (?,?,?,?,?,?,?,?);
EOSQL

my $offset_start_tags = $dbh->prepare(<<EOSQL);
  UPDATE glosstags
  SET start = start + ?
  WHERE synset_offset=? AND ss_type=? AND
        start >= ?;
EOSQL

my $offset_end_tags = $dbh->prepare(<<EOSQL);
  UPDATE glosstags
  SET end = end + ?
  WHERE synset_offset=? AND ss_type=? AND
        end > ?;
EOSQL

# read input and use queries
my ($synset_offset, $ss_type);
while (<>) {
  chomp;
  s/\s*#.*//; # remove comments
  next unless (/\S/); # skip blank lines
  if (/^(\d{8}) ([nvars])$/) { # set current synset
    $synset_offset = $1;
    $ss_type = $2;
  } elsif (/^([+-]).*\|/) { # glosstags insertion/deletion
    my $plus_minus = $1;
    my @row = split(/\|/, substr($_, 1).' ');
    if (@row != 8) {
      print STDERR "WARNING: expected 8 cols in glosstags row, skipping: $_\n";
      next;
    }
    $row[-1] =~ s/ $//;
    if ($plus_minus eq '-') {
      $delete_tag->execute(@row);
    } else { # +
      $insert_tag->execute(@row);
    }
  } elsif (/^[+-]/) { # gloss insertion/deletion
    my ($plus_minus, $gloss) = ($&, $');
    if ($plus_minus eq '-') { # just check we have the right one
      $get_gloss->execute($synset_offset, $ss_type);
      while (my @row = $get_gloss->fetchrow_array) {
	die "Expected $ss_type$synset_offset gloss to be:\n$gloss\nbut instead got:\n$row[0]\n"
	  if ($row[0] ne $gloss);
      }
    } else { # +, replace the gloss
      $set_gloss->execute($gloss, $synset_offset, $ss_type);
    }
  } elsif (/^(\d+)([+-]\d+)$/) { # offset arithmetic
    my ($start, $offset) = ($1, $2);
    $offset_start_tags->execute($offset, $synset_offset, $ss_type, $start);
    $offset_end_tags->execute($offset, $synset_offset, $ss_type, $start);
  } else {
    print STDERR "WARNING: bogus instruction line: $_\n";
  }
}

