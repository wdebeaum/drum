#!/usr/bin/perl

use DBI;
use Data::Dumper;

require 'add_suffix.ph';

use strict vars;
use sort 'stable';

my $dbh;
my $smh;

sub connect_to_db {
  my $dbname = 'wn.db';
  $dbname = shift if (@_);
  $dbh = DBI->connect("DBI:SQLite(RaiseError=>1):dbname=$dbname", '', '');
  if ($dbname eq 'eswn.db' and -e 'sense-map.db') {
    $smh = DBI->connect("DBI:SQLite(RaiseError=>1):dbname=sense-map.db", '', '');
  }
  return $dbh;
}

sub disconnect_from_db {
  $dbh->disconnect();
  $smh->disconnect() if (defined($smh));
}

sub xml_escape {
  my $str = shift;
  $str =~ s/\&/&amp;/g;
  $str =~ s/</&lt;/g;
  $str =~ s/>/&gt;/g;
  return $str;
}

sub print_pointers_to_english {
  my ($synset_offset_16, $ss_type_16, $indent) = @_;
  my $query = $smh->prepare_cached(<<EOQ);
    SELECT DISTINCT d.new_synset_offset AS synset_offset, d.ss_type AS ss_type
    FROM sense_map AS a
    JOIN sense_map AS b
      ON a.new_synset_offset=b.old_synset_offset AND a.ss_type=b.ss_type
    JOIN sense_map AS c
      ON b.new_synset_offset=c.old_synset_offset AND b.ss_type=c.ss_type
    JOIN sense_map AS d
      ON c.new_synset_offset=d.old_synset_offset AND c.ss_type=d.ss_type
    WHERE a.old_version='1.6' AND b.old_version='1.7.1'
      AND c.old_version='2.0' AND d.old_version='2.1'
      AND a.old_synset_offset=? AND a.ss_type=?;
EOQ
  $query->execute($synset_offset_16, $ss_type_16);
  my $english_synsets = $query->fetchall_arrayref({});
  if (@$english_synsets) {
    print qq($indent <pointers type="English">\n);
    for my $ss (@$english_synsets) {
      print qq($indent  <synset synset_offset="$ss->{synset_offset}" ss_type="$ss->{ss_type}" lang="en"/>\n);
    }
    print "$indent </pointers>\n";
  }
}

# Return a modified list of hypernym or hyponym pointers such that only synsets
# satisfying &$include_synset are pointer targets.
sub snap_pointers {
  my ($include_synset, @pointers) = @_;
  my $query = $dbh->prepare_cached(<<EOSQL);
    SELECT pointer_symbol, pointer_name, target_synset_offset, target_ss_type
    FROM pointers NATURAL JOIN pointer_symbols
    WHERE source_synset_offset=? AND source_ss_type=? AND pointer_symbol=?;
EOSQL
  my @snapped = map {
    my $pointer = $_;
    if (&$include_synset($pointer->{target_synset_offset}, $pointer->{target_ss_type})) {
      $pointer;
    } else {
      $query->execute($pointer->{target_synset_offset}, $pointer->{target_ss_type}, $pointer->{pointer_symbol});
      my $target_pointers = $query->fetchall_arrayref({});
      $query->finish();
      snap_pointers($include_synset, @{$target_pointers});
    }
  } @pointers;

  return @snapped;
}

sub yes { return 1 }

# Print to STDOUT XML describing the synset specified by $synset_offset and
# $ss_type, with the specified $indent spacing. Only include senses and synsets
# such that &$include_sense($sense_key) and &$include_synset($synset_offset,
# $ss_type) return true. If these aren't specified, it's assumed they always
# return true.
sub print_synset_xml {
  my ($indent, $synset_offset, $ss_type, $lang, $include_sense, $include_synset) = @_;
  $include_sense ||= \&yes;
  $include_synset ||= \&yes;

  print qq($indent<synset synset_offset="$synset_offset" ss_type="$ss_type" lang="$lang">\n);

  my $query = $dbh->prepare_cached("SELECT lemma,sense_key FROM senses WHERE synset_offset=? AND ss_type=? ORDER BY word_number;");
  $query->execute($synset_offset, $ss_type);
  while (my $row = $query->fetchrow_hashref()) {
    print qq($indent <sense lemma="$row->{lemma}" sense_key="$row->{sense_key}" />\n)
      if (&$include_sense($row->{sense_key}));
  }
  $query->finish();

  # get the text of the gloss (definitions and examples)
  $query = $dbh->prepare_cached("SELECT gloss FROM synsets WHERE synset_offset=? AND ss_type=?;");
  $query->execute($synset_offset, $ss_type);
  my ($gloss) = $query->fetchrow_array();
  $query->finish();

  # get the tags for the gloss
  my @tags = @{$dbh->selectall_arrayref("SELECT * FROM glosstags WHERE synset_offset=? AND ss_type=? AND tag_type IN ('def','ex','sns','pos') AND sense_key<>'purposefully_ignored%0:00:00::';", { Slice => {} }, $synset_offset, $ss_type)};
  # sort by start/end offsets and tag types, so that we go left to right, outer
  # to inner
  my %type2rank = (def => 0, ex => 0, sns => 1, 'pos' => 2);
  @tags = sort { $a->{start} <=> $b->{start} }
          sort { $b->{end} <=> $a->{end} }
	  sort { $type2rank{$a->{tag_type}} <=> $type2rank{$b->{tag_type}} }
	  @tags;
  pop @tags if (@tags != 0 and $tags[-1]{'pos'} eq ':'); # ignore trailing ';'
  # make sure we start at the beginning, with a definition
  if (@tags == 0) {
    unshift @tags, +{
      tag_type => 'def',
      start => 0,
      end => length($gloss)
    };
  } elsif ($tags[0]->{tag_type} eq 'def') {
    $tags[0]->{start} = 0;
  } elsif ($tags[0]->{start} > 0) {
    unshift @tags, +{
      tag_type => 'def',
      start => 0,
      end => $tags[0]->{start} - 1
    };
  }
  # write XML tags corresponding to the gloss tags, filling them with text from $gloss
  my $char_index = 0; # index into $gloss of what we've written so far
  my @stack = (); # XML tags we still have to close, with the indices to close them at
  while (@tags or @stack) {
    while (@stack and
           (@tags == 0 or $stack[-1][0] <= $tags[0]{start})
	  ) {
      my ($new_char_index, $end_tag) = @{pop @stack};
      die "invalid gloss end index: $new_char_index (end tag: $end_tag)\n"
        unless ($new_char_index <= length($gloss));
      print xml_escape(substr($gloss, $char_index, $new_char_index - $char_index));
      $char_index = $new_char_index;
      print $end_tag;
    }
    next unless (@tags);
    my %tag = %{shift @tags};
    die "invalid gloss start index: $tag{start}\n"
      unless ($tag{start} <= length($gloss));
    print xml_escape(substr($gloss, $char_index, $tag{start} - $char_index)) if (@stack);
    $char_index = $tag{start};
    my ($start_tag, $end_tag);
    if ($tag{tag_type} eq 'def') {
      $start_tag=qq($indent <definition>);
      $end_tag=qq(</definition>\n);
    } elsif ($tag{tag_type} eq 'ex') {
      $start_tag=qq($indent <example>);
      $end_tag=qq(</example>\n);
    } elsif ($tag{tag_type} eq 'sns') {
      my $lemma = $tag{sense_key};
      $lemma =~ s/%.*//;
      $start_tag=qq(<sense lemma="$lemma" sense_key="$tag{sense_key}">);
      $end_tag=qq(</sense>);
    } elsif ($tag{tag_type} eq 'pos') {
      $start_tag=qq(<pos pos="$tag{pos}">);
      $end_tag=qq(</pos>);
    } else {
      die "invalid tag_type: $tag{tag_type}\n";
    }
    if (@stack or $tag{tag_type} eq 'def' or $tag{tag_type} eq 'ex') {
      print $start_tag;
      push @stack, [$tag{end}, $end_tag];
    }
  }

  $query = $dbh->prepare_cached(<<EOSQL);
    SELECT pointer_symbol, pointer_name, target_synset_offset, target_ss_type
    FROM pointers NATURAL JOIN pointer_symbols
    WHERE source_synset_offset=? AND source_ss_type=?;
EOSQL
  $query->execute($synset_offset, $ss_type);
  my %pointers = ();
  while (my $row = $query->fetchrow_hashref()) {
    if ($row->{pointer_symbol} eq '\\') {
      if ($ss_type eq 'r') {
	$row->{pointer_name} = "Derived from adjective";
      } else { # 'a' or 's'
	$row->{pointer_name} = "Pertainym";
      }
    }
    push @{$pointers{$row->{pointer_name}}}, $row;
  }
  $query->finish();
  for my $pointer_name (sort keys %pointers) {
    @{$pointers{$pointer_name}} =
      snap_pointers($include_synset, @{$pointers{$pointer_name}})
       if ($pointer_name eq 'Hypernym' or $pointer_name eq 'Hyponym');
    print qq($indent <pointers type="$pointer_name">\n);
    for my $ss (@{$pointers{$pointer_name}}) {
      print qq($indent  <synset synset_offset="$ss->{target_synset_offset}" ss_type="$ss->{target_ss_type}" lang="$lang"/>\n);
    }
    print "$indent </pointers>\n";
  }

  if ($ss_type eq 'v') {
    $query = $dbh->prepare_cached(<<EOSQL);
      SELECT word_number, frame_number, frame_text
      FROM frames NATURAL JOIN frames_text
      WHERE synset_offset=? AND ss_type=?;
EOSQL
    $query->execute($synset_offset, $ss_type);
    my $frames = $query->fetchall_arrayref({});
    $query->finish();
    for my $frame (@$frames) {
      my $text = $frame->{frame_text};
      if ($frame->{word_number} > 0) {
	$query = $dbh->prepare_cached(<<EOSQL);
	  SELECT lemma, sense_key
	  FROM senses
	  WHERE synset_offset=? AND ss_type=? AND word_number=?;
EOSQL
        $query->execute($synset_offset, $ss_type, $frame->{word_number});
	my $sense = $query->fetchrow_hashref();
	$query->finish();
	$text =~ s!----(s|ing)!qq(<sense lemma="$sense->{lemma}" sense_key="$sense->{sense_key}">) . add_suffix($sense->{lemma}, $1) . qq(</sense>)!e;
      }
      print qq($indent <frame frame_number="$frame->{frame_number}">$text</frame>\n);
    }
  }

  print_pointers_to_english($synset_offset, $ss_type, $indent) if (defined($smh));

  print "$indent</synset>\n";
}

my %ss_type2pos = (qw(n noun v verb a adjective r adverb s adjective));
sub print_word_xml {
  my ($indent, $lemma, $selected_sense_key, $show_sense_keys, $lang) = @_;
  print qq($indent<word lemma="$lemma" lang="$lang");
  print qq( selected_sense_key="$selected_sense_key")
    if (defined($selected_sense_key));
  print qq( show_sense_keys="1")
    if ($show_sense_keys);
  print qq(>\n);
  my %pos2rows = ();
  my $query = $dbh->prepare_cached("SELECT synset_offset,ss_type FROM senses WHERE lemma=? ORDER BY tag_cnt DESC, sense_number ASC;");
  $query->execute($lemma);
  while (my $row = $query->fetchrow_hashref()) {
    push @{$pos2rows{$ss_type2pos{$row->{ss_type}}}}, $row;
  }
  $query->finish();
  for my $pos (qw(noun verb adjective adverb)) {
    if (exists($pos2rows{$pos})) {
      print qq($indent <$pos>\n);
      for my $ss (@{$pos2rows{$pos}}) {
	print_synset_xml("$indent  ", $ss->{synset_offset}, $ss->{ss_type}, $lang);
      }
      print qq($indent </$pos>\n);
    }
  }
  print qq($indent</word>\n);
}

1;

