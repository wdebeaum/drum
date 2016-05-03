#!/usr/bin/perl

# make-wordnet-sql-db.pl - Make an SQL version of the WordNet (3.0) database
# William de Beaumont
# $Date: 2014/07/15 17:42:39 $

# USAGE: make-wordnet-sql-db.pl path/to/WordNet dbname

use DBI;
use strict vars;
my $have_pb = eval "use Term::ProgressBar; 1";

my ($wn_home, $dbname) = @ARGV;

my $dbh = DBI->connect("DBI:SQLite(RaiseError=>1):dbname=$dbname", '', '');
$dbh->do("PRAGMA synchronous=OFF;");
$dbh->sqlite_create_function('regexp', 2, sub { $_[0] =~ /$_[1]/; });

$dbh->do("DROP TABLE IF EXISTS senses;");
$dbh->do(<<EOS);
CREATE TABLE senses (
  synset_offset INT(8) NOT NULL,
  ss_type CHAR(1) NOT NULL,
  sense_number INT,
  word_number INT,
  tag_cnt INT,
  sense_key VARCHAR(82) NOT NULL PRIMARY KEY,
  lemma VARCHAR(72) NOT NULL,
  lex_filenum INT(2) NOT NULL,
  lex_id INT(2) NOT NULL
);
EOS

# Hack to deal with the fact that index.sense is lower case while data.* is
# case-sensitive, meaning we can get more than one "sense" for each sense key,
# which differ only in capitalization and word number (which matters for
# pointers). The senses table contains only the first word with the same
# case-insensitive spelling, while this table contains all case variants (but
# only for words affected by this problem).
$dbh->do("DROP TABLE IF EXISTS capitalization;");
$dbh->do(<<EOS);
CREATE TABLE capitalization (
  synset_offset INT(8) NOT NULL,
  ss_type CHAR(1) NOT NULL,
  word_number INT,
  lemma VARCHAR(72) NOT NULL
);
EOS

$dbh->do("DROP TABLE IF EXISTS synsets;");
$dbh->do(<<EOS);
CREATE TABLE synsets (
  synset_offset INT(8) NOT NULL,
  lex_filenum INT(2) NOT NULL,
  ss_type CHAR(1) NOT NULL,
  gloss VARCHAR(512) NOT NULL,
  CONSTRAINT offset UNIQUE (synset_offset, ss_type)
);
EOS

$dbh->do("DROP TABLE IF EXISTS frames;");
$dbh->do(<<EOS);
CREATE TABLE frames (
  synset_offset INT(8) NOT NULL,
  ss_type CHAR(1) NOT NULL,
  word_number INT(3),
  frame_number INT(2) NOT NULL
);
EOS

$dbh->do("DROP TABLE IF EXISTS frames_text;");
$dbh->do(<<EOS);
CREATE TABLE frames_text (
  frame_number INT(2) NOT NULL PRIMARY KEY,
  frame_text VARCHAR(45) NOT NULL
);
EOS

$dbh->do("DROP TABLE IF EXISTS examples;");
$dbh->do(<<EOS);
CREATE TABLE examples (
  sense_key VARCHAR(82) NOT NULL,
  example_number INT(3) NOT NULL
);
EOS

$dbh->do("DROP TABLE IF EXISTS examples_text;");
$dbh->do(<<EOS);
CREATE TABLE examples_text (
  example_number INT(3) NOT NULL PRIMARY KEY,
  example_text VARCHAR(55) NOT NULL
);
EOS

$dbh->do("DROP TABLE IF EXISTS pointers;");
$dbh->do(<<EOS);
CREATE TABLE pointers (
  source_synset_offset INT(8) NOT NULL,
  source_ss_type CHAR(1) NOT NULL,
  source_word_number INT(3),
  pointer_symbol VARCHAR(2) NOT NULL,
  target_synset_offset INT(8) NOT NULL,
  target_ss_type CHAR(1) NOT NULL,
  target_word_number INT(3)
);
EOS

$dbh->do("DROP TABLE IF EXISTS pointer_symbols;");
$dbh->do(<<EOS);
CREATE TABLE pointer_symbols (
  pointer_symbol VARCHAR(2) NOT NULL PRIMARY KEY,
  pointer_name VARCHAR(32) NOT NULL
);
EOS

$dbh->do("DROP TABLE IF EXISTS pointer_inverses;");
$dbh->do(<<EOS);
CREATE TABLE pointer_inverses (
  pointer_symbol VARCHAR(2) NOT NULL PRIMARY KEY,
  inverse VARCHAR(2)
);
EOS

$dbh->do("DROP TABLE IF EXISTS exceptions;");
$dbh->do(<<EOS);
CREATE TABLE exceptions (
  ss_type CHAR(1) NOT NULL,
  inflected VARCHAR(75) NOT NULL,
  lemma VARCHAR(72) NOT NULL,
  pos VARCHAR(4)
);
EOS

my $insert_sense = $dbh->prepare("INSERT INTO senses VALUES (?,?,?,?,?,?,?,?,?);");
my $insert_capitalization = $dbh->prepare("INSERT INTO capitalization VALUES (?,?,?,?);");
my $insert_synset = $dbh->prepare("INSERT INTO synsets VALUES (?,?,?,?);");
my $update_word_number = $dbh->prepare("UPDATE senses SET word_number=? WHERE synset_offset=? AND ss_type=? AND lemma=?;");
my $insert_frame = $dbh->prepare("INSERT INTO frames VALUES (?,?,?,?);");
my $insert_frame_text = $dbh->prepare("INSERT INTO frames_text VALUES (?,?);");
my $insert_example = $dbh->prepare("INSERT INTO examples VALUES (?,?);");
my $insert_example_text = $dbh->prepare("INSERT INTO examples_text VALUES (?,?);");
my $insert_pointer = $dbh->prepare("INSERT INTO pointers VALUES (?,?,?,?,?,?,?);");
my $insert_pointer_symbol = $dbh->prepare("INSERT INTO pointer_symbols VALUES (?,?);");
my $insert_pointer_inverse = $dbh->prepare("INSERT INTO pointer_inverses VALUES (?,?);");
my $insert_exception = $dbh->prepare("INSERT INTO exceptions VALUES (?,?,?,?);");
my $get_verbs_like = $dbh->prepare("SELECT DISTINCT lemma FROM senses WHERE ss_type='v' AND lemma LIKE ?;");

# from wninput(5WN)
my @pointer_symbols = (
  ['!',  'Antonym'],
  ['@', 'Hypernym'],
  ['@i', 'Instance hypernym'],
  ['~',  'Hyponym'],
  ['~i', 'Instance hyponym'],
  ['#m', 'Member holonym'],
  ['#s', 'Substance holonym'],
  ['#p', 'Part holonym'],
  ['%m', 'Member meronym'],
  ['%s', 'Substance meronym'],
  ['%p', 'Part meronym'],
  ['=',  'Attribute'],
  ['+',  'Derivationally related form'], 
  [';c', 'Domain of synset - TOPIC'],
  ['-c', 'Member of this domain - TOPIC'],
  [';r', 'Domain of synset - REGION'],
  ['-r', 'Member of this domain - REGION'],
  [';u', 'Domain of synset - USAGE'],
  ['-u', 'Member of this domain - USAGE'],
  ['*',  'Entailment'],
  ['>',  'Cause'],
  ['^',  'Also see'],
  ['$',  'Verb group'],
  ['&',  'Similar to'],
  ['<',  'Participle of verb'],
  ['\\', 'Pertains to noun / Derived from adjective']
);
for (@pointer_symbols) { $insert_pointer_symbol->execute(@$_) }

my @pointer_inverses = (
  ['!',  '!'],  # Antonym			Antonym
  ['~',  '@'],  # Hyponym			Hypernym
  ['@',  '~'],  # Hypernym			Hyponym
  ['~i', '@i'], # Instance Hyponym		Instance Hypernym
  ['@i', '~i'], # Instance Hypernym		Instance Hyponym
  ['#m', '%m'], # Holonym			Meronym
  ['#s', '%s'],
  ['#p', '%p'],
  ['%m', '#m'], # Meronym			Holonym
  ['%s', '#s'],
  ['%p', '#p'],
  ['&',  '&'],  # Similar to			Similar to
  ['=',  '='],  # Attribute			Attribute
  ['$',  '$'],  # Verb Group			Verb Group
  ['+',  '+'],  # Derivationally Related	Derivationally Related
  [';c', '-c'], # Domain of synset		Member of Domain
  [';r', '-r'],
  [';u', '-u'],
  ['-c', ';c'],
  ['-r', ';r'],
  ['-u', ';u'],
  # not inverted:
  ['*',  undef], # Entailment
  ['>',  undef], # Cause
  ['^',  undef], # Also see
  ['<',  undef], # Participle of verb
  ['\\', undef] # Pertains to noun / Derived from adjective
);
for (@pointer_inverses) { $insert_pointer_inverse->execute(@$_) }

sub for_each_line_in_file {
  my ($filename, $fn) = @_;
  print "$filename\n";
  my $num_lines = `wc -l $filename`;
  my $progress = ($have_pb ? Term::ProgressBar->new({ count => $num_lines, ETA => 'linear' }) : undef);
  my $next_update = 0;
  open DATA, "<$filename" or die "Can't open $filename: $!";
  while (<DATA>) {
    $next_update = $progress->update($.)
      if ($have_pb and $. >= $next_update);
    chomp;
    &$fn;
  }
  close DATA;
}

for_each_line_in_file("$wn_home/dict/verb.Framestext", sub {
  next unless (/^(\d+)\s+/);
  my ($frame_number, $frame_text) = ($1, $');
  $insert_frame_text->execute($frame_number, $frame_text);
});

for_each_line_in_file("$wn_home/dict/sents.vrb", sub {
  next unless (/^(\d+)\s+/);
  my ($example_number, $example_text) = ($1, $');
  $insert_example_text->execute($example_number, $example_text);
});

for_each_line_in_file("$wn_home/dict/sentidx.vrb", sub {
  my ($sense_key, @example_numbers) = split(/[\s,]/);
  for my $example_number (@example_numbers) {
    $insert_example->execute($sense_key, $example_number);
  }
});

for_each_line_in_file("$wn_home/dict/index.sense", sub {
  my ($sense_key, $synset_offset, $sense_number, $tag_cnt) = split(/\s+/);
  $sense_key =~ /^ ([^%]+) %
                   (\d) :
		   (\d\d) :
		   (\d\d) :
		   ( ([^:]+) : (\d\d) |
		     :
		   )
		 $/x
    or die "Malformed sense key: $sense_key";
  my ($lemma, $ss_type, $lex_filenum, $lex_id, $head_word, $head_id) =
    ($1, $2, $3, $4, $5, $6);
  $ss_type =~ tr/12345/nvars/;
  $insert_sense->execute(0+$synset_offset, $ss_type, $sense_number, 
  			 0, # placeholder word number
			 0+$tag_cnt, $sense_key, $lemma, $lex_filenum, $lex_id);
});

print "Create sense index to make setting word numbers go faster\n";
$dbh->do("CREATE INDEX x_senses_synset_lemma on senses(synset_offset, ss_type, lemma);");

for my $pos (qw(noun verb adj adv)) {
  for_each_line_in_file("$wn_home/dict/data.$pos", sub {
    next unless (/^(\d{8} .*) \| (.*)/);
    $_ = $1;
    my $gloss = $2;
    my ($synset_offset, $lex_filenum, $ss_type, $w_cnt, @rest) = split(/\s+/);
    $insert_synset->execute(0+$synset_offset, 0+$lex_filenum, $ss_type, $gloss);
    $w_cnt = hex($w_cnt);
    # get all the words first so we can find case-insensitive duplicates
    my @words = ();
    for my $w_num (1..$w_cnt) {
      my ($word, $lex_id) = splice(@rest, 0, 2);
      $lex_id = hex($lex_id); # do nothing with this info, since we get it from index.sense
      $word =~ s/\(i?[ap]\)$//;
      push @words, [$word, $w_num];
    }
    # put all dupes in capitalization table, and update the word numbers using
    # only the first dupe (and non-dupes)
    for my $word_and_num (@words) {
      my $lc_word = lc($word_and_num->[0]);
      my @case_dupes = grep { lc($_->[0]) eq $lc_word } @words;
      if (1 < @case_dupes) {
	if ($word_and_num->[1] == $case_dupes[0][1]) { # first version
	  $update_word_number->execute($word_and_num->[1], 0+$synset_offset, $ss_type, $lc_word);
	}
	$insert_capitalization->execute(0+$synset_offset, $ss_type, $word_and_num->[1], $word_and_num->[0]);
      } else { # no case-insensitive duplicates
	$update_word_number->execute($word_and_num->[1], 0+$synset_offset, $ss_type, $lc_word);
      }
    }
    for (my $p_cnt = shift @rest; $p_cnt > 0; $p_cnt--) {
      my ($pointer_symbol, $target_synset_offset, $target_pos, $source_target) =
        splice(@rest, 0, 4);
      my $target_ss_type = $target_pos;
      $target_ss_type = 's'
        if ($pointer_symbol eq '&' and $ss_type eq 'a' and $target_pos eq 'a');
      my ($source_word_number, $target_word_number) =
        (hex(substr($source_target,0,2)) || undef,
	 hex(substr($source_target,2,2)) || undef);
      $insert_pointer->execute(0+$synset_offset, $ss_type, $source_word_number,
                               $pointer_symbol,
			       0+$target_synset_offset, $target_ss_type,
			       $target_word_number);
    }
    if ($pos eq 'verb') {
      my $f_cnt = shift @rest;
      die "Not enough fields left for $f_cnt frames: " . join(' ', @rest)
        unless ($f_cnt * 3 <= @rest);
      for my $i (1..$f_cnt) {
	my ($plus, $f_num, $w_num) = splice(@rest, 0, 3);
	die "Expected + at start of frame record, but got $plus"
	  unless ($plus eq '+');
	$w_num = (hex($w_num) || undef);
	$insert_frame->execute(0+$synset_offset, $ss_type, $w_num, 0+$f_num);
      }
    }
    die "Extra stuff between pointers and gloss: " . join(' ', @rest)
      unless (@rest == 0);
  });

  my $ss_type = {noun=>'n', verb=>'v', adj=>'a', adv=>'r'}->{$pos};
  for_each_line_in_file("$wn_home/dict/$pos.exc", sub {
    my ($inflected, @bases) = split;
    # guess at which inflectional suffix was applied
    my @poses;
    if ($pos eq 'noun') {
      @poses = qw(NNS);
    } elsif ($pos eq 'verb') {
      if ($inflected =~ /(did|went)\b/) {
	@poses = qw(VBD);
      } elsif ($inflected =~ /[dg]one\b/) {
        @poses = qw(VBN);
      } elsif ($inflected =~ /ing\b/) {
	@poses = qw(VBG);
      } elsif ($inflected =~ /ed\b/) {
	@poses = qw(VBD VBN);
      } elsif ($inflected =~ /s\b/) {
	@poses = qw(VBZ);
      } elsif ($inflected =~ /[erw]n\b/) {
	@poses = qw(VBN);
      } else {
	@poses = qw(VBD VBN);
      }
    } elsif ($pos eq 'adj') {
      if ($inflected =~ /st\b/) {
	@poses = qw(JJS);
      } else {
	@poses = qw(JJR);
      }
    } elsif ($pos eq 'adv') {
      if ($inflected =~ /st\b/) {
	@poses = qw(RBS);
      } else {
	@poses = qw(RBR);
      }
    }
    for my $lemma (@bases) {
      # don't add it if it's just telling us this word isn't inflected, since
      # then we don't know the actual POS/ss_type (e.g. "vest" appears in
      # adj.exc because it looks like a superlative adjective, but is actually
      # a noun and a verb)
      next if ($lemma eq $inflected and (
        ($ss_type eq 'n' and $lemma =~ /s$/) or
	($ss_type eq 'v' and $lemma =~ /(s|ed|ing)$/) or
	($ss_type eq 'a' and $lemma =~ /(er|est)$/)
	# no adverb cases
      ));
      if ($lemma eq 'be') { # 'be' is special
	$insert_exception->execute($ss_type, $inflected, $lemma, undef);
      } else {
	for my $pos (@poses) {
	  $insert_exception->execute($ss_type, $inflected, $lemma, $pos);
	}
      }
    }
  });
}

print "Add do/go-compound VBZ exceptions\n"; # strangely missing
$dbh->do(<<EOS);
INSERT INTO exceptions
SELECT 'v', lemma || 'es', lemma, 'VBZ'
FROM exceptions
WHERE pos='VBN'
  AND ((lemma LIKE '%do' AND inflected LIKE '%done')
    OR (lemma LIKE '%go' AND inflected LIKE '%gone'));
EOS

print "Translate NNS exceptions to VBZ\n";
$dbh->do(<<EOS);
INSERT INTO exceptions
SELECT 'v', e1.inflected, e1.lemma, 'VBZ'
FROM exceptions AS e1
WHERE e1.pos='NNS'
  AND EXISTS(
    SELECT * FROM senses
    WHERE senses.ss_type='v' AND senses.lemma=e1.lemma
  )
  AND NOT EXISTS(
    SELECT * FROM exceptions AS e2
    WHERE e2.pos='VBZ' AND e2.lemma=e1.lemma
  );
EOS

print "Cull past participle exceptions\n";
# if there's an -en past participle, don't use the other past inflection for
# the past participle
# (DELETE doesn't support aliasing a table with AS, so I have to make a
# temporary table first)
$dbh->do(<<EOS);
CREATE TEMPORARY TABLE enpastpart AS
  SELECT lemma FROM exceptions
  WHERE pos='VBN' AND regexp(inflected, '[erw]n\\b')
EOS
$dbh->do(<<EOS);
DELETE FROM exceptions
WHERE pos = 'VBN'
  AND (regexp(inflected, 'ed\\b') OR NOT regexp(inflected, '[erw]n\\b'))
  AND EXISTS(
    SELECT * FROM enpastpart WHERE enpastpart.lemma = exceptions.lemma
  );
EOS

print "Add base past exceptions\n";
# WordNet apparently doesn't include these
for my $lemma (
  # These lists are from Wikipedia:
  #   http://en.wikipedia.org/wiki/List_of_English_irregular_verbs#Verbs_with_preterite_identical_to_present
  # these always have past/past part identical to present
  qw(beat bet burst cast cost cut hit hurt let put quit read set shed shut slit split spread),
  # these optionally do
  qw(bid fit knit rid shit spit sweat thrust wed wet),
  # some manually prefixed versions of the above
  qw(overfit
     lipread lip-read misread proofread reread sightread sight-read speech-read
     beset inset offset reset typeset upset)
  ) {
  # add not only these verbs, but also verbs that end with them
  my @prefixed = ($lemma);
  unless (grep { $_ eq $lemma} qw(fit let quit read set)) { # these are problematic
    $get_verbs_like->execute('%' . $lemma);
    my @prefixed = map { $_->[0] } @{$get_verbs_like->fetchall_arrayref()};
  }
  for my $p (@prefixed) {
    $insert_exception->execute('v', $p, $p, 'VBD');
    $insert_exception->execute('v', $p, $p, 'VBN');
  }
}


print "Create sense indices\n";
$dbh->do("CREATE INDEX x_senses_synset ON senses(synset_offset, ss_type);");
$dbh->do("CREATE INDEX x_senses_sense_number ON senses(ss_type, lemma, sense_number);");
$dbh->do("CREATE INDEX x_senses_lemma on senses(ss_type, lemma);");

print "Create frame index\n";
$dbh->do("CREATE INDEX x_frames ON frames(synset_offset,ss_type);");

print "Create example index\n";
$dbh->do("CREATE INDEX x_examples ON examples(sense_key);");

print "Create pointer index\n";
$dbh->do("CREATE INDEX x_pointers_source ON pointers(source_synset_offset,source_ss_type);");

print "Create exception indices\n";
$dbh->do("CREATE INDEX x_exception_stemming on exceptions(inflected);");
$dbh->do("CREATE INDEX x_exception_listing on exceptions(ss_type, lemma);");
$dbh->do("CREATE INDEX x_exception_generation on exceptions(lemma, pos);");

print "Cleanup\n";
undef $insert_sense;
undef $insert_synset;
undef $update_word_number;
undef $insert_frame;
undef $insert_frame_text;
undef $insert_example;
undef $insert_example_text;
undef $insert_pointer;
undef $insert_pointer_symbol;
undef $insert_pointer_inverse;
undef $insert_exception;
undef $get_verbs_like;
undef $dbh;

print "Done!\n";

