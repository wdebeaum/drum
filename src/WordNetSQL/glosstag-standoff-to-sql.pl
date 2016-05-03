#!/usr/bin/perl

use DBI;
require 'get-del-ins.ph';

use strict vars;
my $have_pb = eval "use Term::ProgressBar; 1";

die "USAGE: $0 /path/to/WordNet-3.0/glosstag/standoff wn.db" unless (@ARGV == 2);
my ($glosstag_standoff_dir, $dbname) = @ARGV;
my $num_chunks = 1177;

# fix non-standard sense keys in gloss corpus that use 3 instead of 5 for the
# ss_type of satellite adjectives
sub fix_sense_key {
  $_ = shift;
  s/%3/%5/ if (/^[^%]+%3:\d\d:\d\d:[^:]+:\d\d$/);
  return $_;
}

# set up database handle
my $dbh = DBI->connect("DBI:SQLite(RaiseError=>1):dbname=$dbname", '', '');
$dbh->do("PRAGMA synchronous=OFF;");

# set up statements
my $fix_satellites = $dbh->prepare(<<EOS);
  UPDATE structs
  SET ss_type='s'
  WHERE
      ss_type='a' AND    
      EXISTS(
	  SELECT *
	  FROM synsets
	  WHERE synsets.synset_offset=structs.synset_offset AND
		synsets.ss_type='s'
      )
  ;   
EOS
my $get_struct_synsets = $dbh->prepare(
  "SELECT DISTINCT synset_offset, ss_type FROM structs;"
);
my $get_gloss = $dbh->prepare(
  "SELECT gloss FROM synsets WHERE synset_offset=? AND ss_type=?;"
);
my $get_range = $dbh->prepare(
  "SELECT min(start), max(end) FROM structs WHERE synset_offset=? AND ss_type=?;"
);
my $get_structs = $dbh->prepare(
  "SELECT id, type, start, end FROM structs WHERE synset_offset=? AND ss_type=?;"
);
my $drop_feats_index = $dbh->prepare("DROP INDEX IF EXISTS x_feats;");
my $create_feats_index = $dbh->prepare("CREATE INDEX x_feats ON feats(id);");
my $get_feats = $dbh->prepare(
  "SELECT DISTINCT name, value FROM feats WHERE id=?;"
);
my $insert_tag = $dbh->prepare(
  "INSERT INTO glosstags VALUES (?,?,?,?,?,?,?,?);"
);

# iterate over glosstag corpus chunks
my $progress = ($have_pb ? Term::ProgressBar->new({ count => $num_chunks, ETA => 'linear' }) : undef);
my $next_update = 0;
for (my $i = 0; $i < $num_chunks; $i++) {
  $next_update = $progress->update($i)
    if ($have_pb and $i >= $next_update);
  
  my $chunk =
    sprintf("$glosstag_standoff_dir/%02d/%03d/wsd-%04d00",
            $i / 100, $i / 10, $i);

  # get only the files that actually exist
  my $xml_files =
    join(' ',
    grep { -e $_ }
    map { "$chunk-wn$_.xml" }
    qw(gloss dc coll word ann)
    );
  
  # get structs and feats tables for this chunk
  $drop_feats_index->execute();
  system(<<EOS);
  ( echo "BEGIN TRANSACTION; DELETE FROM structs; DELETE FROM feats;" ; \
    xsltproc glosstag-standoff-to-sql.xsl $xml_files ; \
    echo "END TRANSACTION;" \
  ) | sqlite3 $dbname
EOS
  $create_feats_index->execute();
  $fix_satellites->execute();

  my $chunk_text = `iconv -f UTF-16 -t UTF-8 $chunk.txt`;

  # coordinate structs, feats, synsets.gloss, and $chunk_text to make glosstags
  # table
  $get_struct_synsets->execute();
  my $synsets = $get_struct_synsets->fetchall_arrayref();
  for my $ss (@$synsets) {
    # get both versions of the gloss
    $get_gloss->execute(@$ss);
    my $row = $get_gloss->fetchrow_arrayref();
    unless (defined($row)) {
      print "Warning: failed to get wn gloss; skipping synset.\n  ss = $ss->[0] $ss->[1]\n\n";
      next;
    }
    my $wn_gloss = $row->[0];
    $get_range->execute(@$ss);
    my ($min, $max) = $get_range->fetchrow_array();
    my $tagged_gloss = substr($chunk_text, $min, $max - $min);
    
    # remove some junk at the end
    $wn_gloss =~ s/\s*;?\s*$//;
    $tagged_gloss =~ s/\s*;?\s*$//;
    
    # find a few common deletions and apply them to $tagged_gloss so the
    # alignment works
    my @deletions;
    ($tagged_gloss, @deletions) = get_deletions($tagged_gloss, $wn_gloss);
    
    # align the two versions of the gloss, assuming only insertions are
    # required to go from tagged to wn
    my @insertions = ();
    my $aligned = 0;
    eval {
      @insertions = get_insertions($tagged_gloss, $wn_gloss);
      $aligned = 1;
    };
    unless ($aligned) {
      print "Warning: failed to align using insertions: $@  ss = $ss->[0] $ss->[1]\n  tagged_gloss = '$tagged_gloss'\n      wn_gloss = '$wn_gloss'\n\n";
      @insertions = (0)x(length($tagged_gloss)+1);
    };
    
    # apply @deletions to @insertions by splicing in -1s
    for (reverse @deletions) {
      splice(@insertions, $_, 0, -1);
    }
    
    # convert @insertions to an array mapping tagged starts to wn starts
    # ends are off by one
    my $so_far = -1;
    my @tagged_to_wn_start = map { $so_far += $_ + 1 } @insertions;
    
    # convert structs/feats to glosstags, while applying insertions
    $get_structs->execute(@$ss);
    my $structs = $get_structs->fetchall_arrayref({});
    my %ids = ();
    for my $struct (@$structs) {
      # figure out the real start/end indices relative to $wn_gloss
      my ($tagged_start, $tagged_end) = ($struct->{start} - $min, $struct->{end} - $min);
      
      # skip the final ; we deleted earlier
      next if ($tagged_start >= length($tagged_gloss));
      # clamp the end to the actual end of the gloss
      $tagged_end = length($tagged_gloss) if ($tagged_end > length($tagged_gloss));
      
      my ($wn_start, $wn_end) =
        ($tagged_to_wn_start[$tagged_start],
	 $tagged_to_wn_start[$tagged_end-1]+1);
      
      unless (defined($wn_start) and defined($wn_end)) {
	print "Warning: bogus start/end offset; skipping struct.\n  ss = $ss->[0] $ss->[1]\n  tagged_gloss = '$tagged_gloss'\n  length(tagged_gloss) = " . length($tagged_gloss) . "\n  tagged_start,end = $tagged_start,$tagged_end\n      wn_gloss = '$wn_gloss'\n  length(wn_gloss) = " . length($wn_gloss) . "\n  wn_start,end = $wn_start,$wn_end\n\n";
	next;
      }
      
      if ($aligned) {
	# clamp wn_start/end to the bounds of $wn_gloss
	$wn_start = 0 if ($wn_start < 0);
	$wn_start = length($wn_gloss) if ($wn_start > length($wn_gloss));
	$wn_end = 0 if ($wn_end < 0);
	$wn_end = length($wn_gloss) if ($wn_end > length($wn_gloss));

	# if this is a def, get rid of the final ; for consistency
	$wn_end--
	  if ($struct->{type} eq 'def' and
	      substr($wn_gloss, $wn_end - 1, 1) eq ';');
	
        # similarly for ex and ";
	$wn_end -= 2
	  if ($struct->{type} eq 'ex' and
	      substr($wn_gloss, $wn_end - 2, 2) eq '";');
	
	# if the whole struct was deleted, skip it
	next if ($wn_start >= $wn_end);
      }

      # insert tags
      if ($struct->{type} =~ /^(def|ex)$/) {
	$insert_tag->execute(@$ss, $wn_start, $wn_end, $struct->{type}, '', '', '');
      } elsif (not exists($ids{$struct->{id}})) {
	$ids{$struct->{id}} = 1;
	$get_feats->execute($struct->{id});
	my $feats = $get_feats->fetchall_arrayref({});
	for my $feat (@$feats) {
	  if ($feat->{name} eq 'pos') {
	    $insert_tag->execute(@$ss, $wn_start, $wn_end, 'pos', $feat->{value}, '', '');
	  } elsif ($feat->{name} eq 'wnsk') {
	    $insert_tag->execute(@$ss, $wn_start, $wn_end, 'sns', '', fix_sense_key($feat->{value}), '');
	  } elsif ($feat->{name} eq 'lemma') {
	    for (split(/\|/, $feat->{value})) {
	      $insert_tag->execute(@$ss, $wn_start, $wn_end, 'lem', '', '', $_);
	    }
	  }
	}
      }
    }
  }
}

# drop temporary-ish tables
$dbh->do("DROP TABLE structs;");
$drop_feats_index->execute();
$dbh->do("DROP TABLE feats;");

# make indexes
$dbh->do("CREATE INDEX x_glosstags_synset ON glosstags(synset_offset, ss_type);");
$dbh->do("CREATE INDEX x_glosstags_sense_key ON glosstags(sense_key);");

# finish progress bar
$next_update = $progress->update($num_chunks)
  if ($have_pb and $num_chunks >= $next_update);

# cleanup
undef $fix_satellites;
undef $get_struct_synsets;
undef $get_gloss;
undef $get_range;
undef $get_structs;
undef $drop_feats_index;
undef $create_feats_index;
undef $get_feats;
undef $insert_tag;
undef $dbh;
