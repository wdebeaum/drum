package TextTagger::StanfordCoreNLP;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(init_stanford_core_nlp ready_stanford_core_nlp run_stanford_core_nlp fini_stanford_core_nlp $debug);

use IPC::Open2;
use Data::Dumper;
use TextTagger::Tags2Trips qw(sortTags $min_clause_length @penn_clause_tags);
use TextTagger::XML qw(read_xml process_xml);
use TextTagger::Util qw(asciify_string unasciify_tags);

use strict vars;

my $debug = 0;

my %NEclass2lftype = (
  'organization' => 'ORGANIZATION',
  'person' => 'PERSON',
  'location' => 'GEOGRAPHIC-REGION',
  'misc' => 'REFERENTIAL-SEM'
# numerical NER classes (not used yet)
#,  'date' => ???,
#  'time' => ???,
#  'money' => ???,
#  'number' => ???
# RegexNER classes (not used yet)
#,  'ideology' => ???,
#  'nationality' => ???,
#  'religion' => ???,
#  'title' => ???
);

my ($stanford_in, $stanford_out, $stanford_pid);
sub init_stanford_core_nlp {
  # TODO wait for this PID on exit?
  $stanford_pid = open2($stanford_in, $stanford_out, $ENV{TRIPS_BASE} . "/bin/CoreNLPFilter", '-annotators', 'tokenize,ssplit,pos,lemma,ner,parse'); # no dcoref
  binmode $stanford_in, ':utf8';
  binmode $stanford_out, ':utf8';
}

sub fini_stanford_core_nlp {
  close($stanford_in);
  close($stanford_out);
  waitpid $stanford_pid, 0;
}

sub ready_stanford_core_nlp {
  print $stanford_out "ready\n";
  read_xml($stanford_in);
  return 1;
}

sub parse_parse_tree {
  my $tree_string = shift;
  my $tree = undef;
  my @stack = ([]);
  my $word = '';
  for my $char (split('', $tree_string)) {
    if ($char eq '(') {
      push @{$stack[-1]}, [];
      push @stack, $stack[-1][-1];
    } elsif ($char eq ')') {
      if ($word ne '') {
        push @{$stack[-1]}, $word;
	$word = '';
      }
      $tree = pop @stack;
    } elsif ($char eq ' ') {
      if ($word ne '') {
        push @{$stack[-1]}, $word;
	$word = '';
      }
    } else {
      $word .= $char;
    }
  }
  return $tree;
}

# return phrase tags aligned with the tokens at the leaves of the tree (removing the tokens from the list), and the end of the last token used
sub tree_to_tags_rec {
  my ($tree, $text, $remaining_tokens) = @_;
  my $cat = shift @$tree;
  my $is_phrase = 0;
  die "Out of tokens to align tree with!" unless (@$remaining_tokens);
  my $start = $remaining_tokens->[0]->{start};
  my $end = $start;
  my @tags = ();
  for my $subtree (@$tree) {
    if (ref($subtree) eq 'ARRAY') { # a subtree, recurse
      my ($new_tags, $new_end) = tree_to_tags_rec($subtree, $text, $remaining_tokens);
      push @tags, @$new_tags;
      $end = $new_end;
      $is_phrase = 1;
    } else { # a token, align
      die "Out of tokens to align tree with!" unless (@$remaining_tokens);
      print STDERR "Got token " . Data::Dumper->Dump([$remaining_tokens->[0]],['*token'])
        if ($debug);
      $end = (shift @$remaining_tokens)->{end};
    }
  }
  if ($is_phrase) {
    my $phrase_text = substr($text, $start, $end - $start);
    push @tags, { type => 'clause',
                  text => $phrase_text,
		  start => $start,
		  end => $end
                }
      if ($end - $start >= $min_clause_length and
	  grep { $_ eq $cat } @penn_clause_tags);
    push @tags, { type => 'phrase',
		  text => $phrase_text,
		  'penn-cat' => [$cat],
		  start => $start,
		  end => $end
		};
  }
  return ([@tags], $end);
}

sub tree_to_tags {
  my ($tree_string, $text, @token_tags) = @_;
  my ($tags, $end) = tree_to_tags_rec(parse_parse_tree($tree_string), $text, [sortTags(@token_tags)]);
  my $top = pop @$tags; # discard ROOT
  return @$tags;
}

sub run_stanford_core_nlp_on_text {
  my ($self, $text) = @_;
  
  # output to CoreNLP
  $text =~ s/[[:space:]]/ /g;
  # CoreNLP is supposed to be able to handle multibyte UTF-8 characters, but in
  # my experience it doesn't. So convert everything to 7-bit ASCII and be done
  # with it.
  my $substs;
  ($text, $substs) = asciify_string($text);
  print $stanford_out $text . "\n";

  # input from CoreNLP via XSLT, sorting tags by type
  my %type2tags = ();
  for my $tag (process_xml("$ENV{TRIPS_BASE}/etc/TextTagger/CoreNLP-to-TextTagger.xsl", read_xml($stanford_in))) {
    # correct text/lex
    $tag->{text} = substr($text, $tag->{start}, $tag->{end} - $tag->{start})
      if (exists($tag->{text}));
    $tag->{lex} = substr($text, $tag->{start}, $tag->{end} - $tag->{start})
      if (exists($tag->{lex}));

    push @{$type2tags{$tag->{type}}}, $tag;
  }

  if ($debug) {
    print STDERR "Received:\n";
    for my $type (sort keys %type2tags) {
      print STDERR "  " . scalar(@{$type2tags{$type}}) . " $type tags\n";
    }
  }

  # map NER classes to lftypes
  @{$type2tags{'named-entity'}} = map {
    exists($NEclass2lftype{$_->{'stanford-ner-class'}}) ?
      (+{ %$_, lftype => [$NEclass2lftype{$_->{'stanford-ner-class'}}] })
      :
      ()
  } @{$type2tags{'named-entity'}};

  # convert parse tags to phrase tags
  for my $parse_tag (@{$type2tags{parse}}) {
    my @token_tags = grep { $_->{start} >= $parse_tag->{start} and
                            $_->{end} <= $parse_tag->{end}
			  } map { @$_ } @type2tags{qw(word punctuation)};
    print STDERR "Converting tree " . Data::Dumper->Dump([$parse_tag->{'penn-parse-tree'}], ['*tree']) if ($debug);
    for (tree_to_tags($parse_tag->{'penn-parse-tree'}, $text, @token_tags)) {
      push @{$type2tags{$_->{type}}}, $_;
    }
  }
  print STDERR "Made " . scalar(@{$type2tags{phrase}}) . " phrase tags\n"
    if ($debug);

  my @tags = map { @$_ } @type2tags{qw(word punctuation pos phrase named-entity clause sentence)};
  return [unasciify_tags($substs, @tags)];
}

sub run_stanford_core_nlp_on_tags {
  my ($self, @tags) = @_;
  my @new_tags = ();
  for my $chunk (@tags) {
    if ($chunk->{type} eq 'sentence') {
      push @new_tags, map {
	my $tag = $_;
	$tag->{start} += $chunk->{start};
	$tag->{end} += $chunk->{start};
	$tag
      } grep {
	# don't provide potentially conflicting sentence tags if we already
	# have them in the input
	$_->{type} ne 'sentence'
      } @{run_stanford_core_nlp_on_text($self, $chunk->{text})};
    }
  }
  return [@new_tags];
}

sub run_stanford_core_nlp {
  my ($self, $text, @tags) = @_;
  # use sentence tags if we have them, otherwise use the whole text and let
  # CoreNLP's sentence splitter deal with it
  if (grep { $_->{type} eq 'sentence' } @tags) {
    print STDERR "Sending one sentence at a time through CoreNLP\n" if ($debug);
    return run_stanford_core_nlp_on_tags($self, @tags);
  } else {
    print STDERR "Sending whole text at once through CoreNLP\n" if ($debug);
    my $ret = run_stanford_core_nlp_on_text($self, $text);
    # if we got no sentence tags, add one for the whole input
    # (this is so we don't say (ok :uttnums ()) on a blank input)
    unless (grep { $_->{type} eq 'sentence' } @$ret) {
      push @$ret, +{
	type => 'sentence',
	text => $text,
	start => 0,
	end => length($text)
      };
    }
    return $ret;
  }
}

push @TextTagger::taggers, {
  name => "stanford_core_nlp",
  init_function => \&init_stanford_core_nlp,
  ready_function => \&ready_stanford_core_nlp,
  tag_function => \&run_stanford_core_nlp,
  fini_function => \&fini_stanford_core_nlp,
  output_types => [qw(word punctuation pos phrase named-entity clause sentence)],
  input_text => 1,
  optional_input_types => [qw(sentence)]
};

1;
