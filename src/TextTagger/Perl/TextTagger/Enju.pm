package TextTagger::Enju;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(init_enju ready_enju run_enju);

use Encode qw(encode);
use IPC::Open2;
use Data::Dumper;
use TextTagger::Util qw(intersection);
use TextTagger::Tags2Trips qw($min_clause_length @penn_clause_tags);
use TextTagger::XMLInput qw(parse_xml translate_xml_entity);

my $debug = 0;

# From:
# http://www.nactem.ac.uk/enju/enju-manual/enju-output-spec.html#correspondence
# Note they say this is only a "rough sketch", but it's all we have.
my %enju_to_penn_cats = (
  ADJP		=> [qw(ADJP QP)], # , (number expression)
  ADJP_REL	=> ['WHADJP'], # (relativizer)
  ADJP_FREL	=> ['WHADJP'], # (free relative)
  ADJP_WH	=> ['WHADJP'], # (wh-phrase)
  ADVP		=> ['ADVP'],
  ADVP_REL	=> ['WHADVP'], # (relativizer)
  ADVP_FREL	=> ['WHADVP'], # (free relative)
  ADVP_WH	=> ['WHADVP'], # (wh-phrase)
  CONJP		=> ['CONJP'],
  CP		=> ['SBAR'], # (complementizer phrase)
  # This seems like it would hurt more than it helps
  #DP		=> [qw(NP QP)], # (possessive), (quantifier)
  NP		=> ['NP'],
  NX		=> [qw(NX NAC)],
  NP_REL	=> ['WHNP'], # (relativizer)
  NP_FREL	=> ['WHNP'], # (free relative)
  NP_WH		=> ['WHNP'], # (wh-phrase)
  PP		=> ['PP'],
  PP_REL	=> ['WHPP'], # (relativizer)
  PP_WH		=> ['WHPP'], # (wh-phrase)
  PRT		=> ['PRT'],
  S		=> ['S'],
  S_INV		=> ['SINV'],
  S_Q		=> ['SQ'],
  S_REL		=> ['SBAR'], # (relative clause)
  S_FREL	=> ['SBAR'], # (free relative clause)
  S_WH		=> ['SBARQ'],
  SCP		=> ['SBAR'], # (subordinate clause)
  # This is wrong: the PTB spec explicitly says RRC is only for non-VP clauses
  #VP		=> [qw(VP RRC)]
  VP		=> ['VP']
); # no Enju equivalents: PRN, INTJ, LST, X

my ($enju_in, $enju_out, $enju_pid, $enju_offset);
sub init_enju {
  die "Enju not configured. Please install Enju and re-run the configure script. See the TextTagger README for details on how to install Enju."
    if ($TextTagger::Config::ENJU eq '');
  $enju_pid = open2($enju_in, $enju_out, $TextTagger::Config::ENJU, qw(-genia -so));
  binmode $enju_in, ':utf8';
  binmode $enju_out, ':utf8';
  $enju_offset = 0;
}

sub ready_enju {
  print $enju_out "ready\n";
  # read until blank line
  while (<$enju_in>) {
    last if (/^\s*$/);
  }
  $enju_offset += 6;
  return 1;
}

sub fini_enju {
  close $enju_out;
  close $enju_in;
  waitpid $enju_pid, 0;
}

# return an arrayref of byte positions of non-first bytes in multibyte
# characters in the given string, suitable for passing to byte_to_char_offset
sub get_multibyte_offsets {
  my $str = shift;
  my @chars = split('', $str);
  my $multibyte_offsets = [];
  my $byte_offset = 0;
  for (@chars) {
    my $byte_length = length(encode('UTF-8', $_));
    die "WTF" if ($byte_length < 1);
    $byte_length--;
    $byte_offset++;
    while ($byte_length > 0) {
      push @$multibyte_offsets, $byte_offset;
      $byte_length--;
      $byte_offset++;
    }
  }
  return $multibyte_offsets;
}

# given a byte offset into a string and the result of calling
# get_multibyte_offsets on that string, return the corresponding character
# offset in that string (or the nearest previous one if the offset is in the
# middle of a character)
sub byte_to_char_offset {
  my ($byte_offset, $multibyte_offsets) = @_;
  my $char_offset = $byte_offset;
  for (@$multibyte_offsets) {
    last if ($_ >= $byte_offset);
    $char_offset--;
  }
  return $char_offset;
}

sub run_enju {
  my ($self, @input_tags) = @_;
  my @tags = ();
  for my $sentence (@input_tags) {
    next unless ($sentence->{type} eq 'sentence');
    my $text = $sentence->{text};
    $text =~ s/[[:space:]\000]/ /g;
    my $multibyte_offsets = get_multibyte_offsets($text);
    print $enju_out $text . "\n";
    while (<$enju_in>) {
      last if (/^\s*$/);
      chomp;
      my ($start, $end, $xml_tag) = split(/\t/);
      $start -= $enju_offset;
      $end -= $enju_offset;
      $start = byte_to_char_offset($start, $multibyte_offsets);
      $end = byte_to_char_offset($end, $multibyte_offsets);
      my $tag_text = substr($sentence->{text}, $start, $end-$start);
      $start += $sentence->{start};
      $end += $sentence->{start};
      my ($length, $xml_tags, $unparsed_suffix) = parse_xml("<$xml_tag />");
      die "failed to parse XML tag from Enju: <$xml_tag />"
        unless ($unparsed_suffix eq '' and length(@$xml_tags) == 1);
      my $tag_name = $xml_tags->[0]{tag_name};
      my $attrs = $xml_tags->[0]{attributes};
      # flatten list of hashrefs $attrs to single hash %attrs (we know there
      # are no duplicate attribute names in this case, and the order doesn't
      # matter)
      my %attrs = map { ($_->{name} => $_->{val}) } @$attrs;
      # translate XML entities in attribute values
      s/&#?[\w-]+;/translate_xml_entity($&)/eg for (values %attrs);
      # extend top-level S phrases to cover final punctuation
      if ($tag_name eq 'cons' and $start == $sentence->{start} and
	  $attrs{cat} eq 'S' and $attrs{xcat} eq '' and
	  substr($sentence->{text}, $end-$start) !~ /\w/) {
	$end = $sentence->{end};
	$tag_text = $sentence->{text};
      }
      print STDERR Data::Dumper->Dump([$start, $end, $tag_name, \%attrs], [qw(start end tag_name *attrs)]) if ($debug);
      if ($tag_name eq 'sentence') {
	print STDERR "Enju did not succeed: $attrs{parse_status}\n"
	  unless ($attrs{parse_status} eq 'success');
	# TODO do something with fom score?
      } elsif ($tag_name eq 'tok') {
	push @tags, +{
	  type => 'pos',
	  lex => $tag_text,
	  start => $start,
	  end => $end,
	  'penn-pos' => [split(/\|/, $attrs{pos})], # occasionally Enju will output pipe-separated POS options (undocumented)
	  # TODO other info in DSI (except it's not domain-specific)?
	  # cat is redundant, but base, lexentry, etc. might be useful
	};
      } elsif ($tag_name eq 'cons') {
	my $xcat = $attrs{xcat};
	if ($xcat ne '') {
	  # spec says it's a space-separated list, though I've never seen more
	  # than one
	  $xcat =~ s/\s+/_/g;
	  $xcat = '_' . $xcat;
	}
        my $enju_cat = $attrs{cat} . $xcat;
	if (exists($enju_to_penn_cats{$enju_cat})) {
	  my @penn_cats = @{$enju_to_penn_cats{$enju_cat}};
	  my %common = (
	    text => $tag_text,
	    start => $start,
	    end => $end,
	  );
	  # add a clause tag if it's long enough and a clause-ish cat
	  push @tags, +{ type => 'clause', %common }
	    if ($end - $start >= $min_clause_length and
	        @{intersection(\@penn_cats, \@penn_clause_tags)});
	  push @tags, +{
	    type => 'phrase',
	    'enju-cat' => [$enju_cat],
	    'penn-cat' => [@penn_cats],
	    # TODO other info in DSI (except it's not domain-specific)?
	    %common
	  };
	}
      } else {
	print STDERR "Ignoring tag from Enju: $xml_tag\n";
      }
    }
    $enju_offset += length(encode('UTF-8', $text)) + 1;
  }
  return [@tags];
}

push @TextTagger::taggers, {
  name => 'enju',
  init_function => \&init_enju,
  fini_function => \&fini_enju,
  ready_function => \&ready_enju,
  tag_function => \&run_enju,
  output_types => [qw(pos phrase clause)],
  input_types => ['sentence']
};

1;
