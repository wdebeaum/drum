package TextTagger::XMLInput;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(init_xml_input_tagger process_xml_tags tag_xml_input parse_xml translate_xml_entity);

use KQML::KQML;
use TextTagger::Tags2Trips qw(trips2tagNative sortTags);

use strict vars;

my $debug = 0;

my $qname_re = qr/ [\w-]+ (?: : [\w-]+ )? /x;

sub convert_single_rhs_tag {
  my $rhs_kqml = shift;
  my $rhs = trips2tagNative($rhs_kqml);
  # add default arguments bound to implicit variables
  unless (exists($rhs->{lex}) or exists($rhs->{text})) {
    push @$rhs_kqml, ':lex', '"?lex"';
    push @$rhs_kqml, ':text', '"?text"';;
  }
  push @$rhs_kqml, ':start', '?start' unless (exists($rhs->{start}));
  push @$rhs_kqml, ':end', '?end' unless (exists($rhs->{end}));
  print STDERR Data::Dumper->Dump([$rhs_kqml],['*rhs_kqml']) if ($debug);
  return $rhs_kqml;
}

sub read_rule_from_string {
  my $str = shift;
  # TODO allow regexps in // on either side of the LHS, meaning that the rule only matches if those regexps match before/after the XML element, and the resulting tag extends over those matches
  $str =~ /
    ^ < ( \? [\w-]+ | $qname_re )
    ( (?: \s+ $qname_re (?: =" [^"]+ " )? )* )
    \s* > \s* ->
  /x
    or die "malformed XML input rule: $str";
  my ($tag_name, $attr_str, $rhs_str) = ($1, $2, $');
  print STDERR Data::Dumper->Dump([$tag_name, $attr_str, $rhs_str], [qw(tag_name attr_str rhs_str)]) if ($debug);
  my $attrs = [];
  while ($attr_str =~ / ^ \s* ( $qname_re ) (?: =" ( [^"]+ ) " )? /x) {
    my ($attr_name, $attr_val) = ($1, $2);
    $attr_str = $';
    print STDERR "  attr_name=$attr_name; attr_val=$attr_val\n" if ($debug);
    unless (defined($attr_val)) {
      my $var = $attr_name;
      $var =~ s/^[\w-]+://; # remove XML namespace
      $var = "?$var";
      $attr_val = $var;
    }
    push @$attrs, +{ name => $attr_name, val => $attr_val };
  }
  die "failed to parse all XML attributes: '$attr_str'"
    unless ($attr_str eq '');
  my $rhs;
  if ($rhs_str =~ /\s*erase\s*/i) {
    $rhs = 'erase';
    print STDERR "Warning: 'erase' rules are not yet implemented\n";
  } else {
    $rhs = KQML::KQMLReadFromString($rhs_str);
    if (ref($rhs->[0]) eq 'ARRAY') { # list of tags
      $rhs = [map { convert_single_rhs_tag($_) } @$rhs];
    } else { # single tag
      $rhs = convert_single_rhs_tag($rhs);
    }
  }
  # TODO check that variables used on RHS are also used on LHS, or are among
  # the implicitly defined variables
  return +{
    lhs => +{ tag_name => $tag_name, attributes => $attrs },
    rhs => $rhs
  };
}

# load rules from $self->{xml_input_rules_file} into $self->{xml_input_rules}
sub init_xml_input_tagger {
  my $self = shift;
  $self->{xml_input_rules} = [];
  if ($self->{xml_input_rules_file} eq '') {
    print STDERR "Warning: initialized XMLInput tagger with no rules file\n";
    return;
  }
  open RULES, "<$self->{xml_input_rules_file}" or die "Can't open XML input rules file '$self->{xml_input_rules_file}': $!";
  my $current_rule = undef;
  while (<RULES>) {
    chomp;
    s/[;#].*//; # remove comments
    next unless (/\S/); # ignore blank lines
    if (/^</) { # new rule
      push @{$self->{xml_input_rules}}, read_rule_from_string($current_rule)
        if (defined($current_rule));
      $current_rule = $_;
    } elsif (defined($current_rule)) {
      $current_rule .= " $_";
    } else {
      die "on line $. of XML input rules file '$self->{xml_input_rules_file}': expected '<' at start of rule but got '$_'";
    }
  }
  push @{$self->{xml_input_rules}}, read_rule_from_string($current_rule)
    if (defined($current_rule));
  print STDERR Data::Dumper->Dump([$self->{xml_input_rules}], ['*xml_input_rules']) if ($debug);
  close RULES;
}

sub offset_xml_tag {
  my ($tag, $offset) = @_;
  $tag->{start} += $offset;
  $tag->{end} += $offset;
  $tag->{content_start} += $offset;
  $tag->{content_end} += $offset;
  return $tag;
}

# given a string, parse a prefix of it as XML, and return three values:
#  * the length of the prefix parsed
#  * a listref of 'xml' TextTagger tags for the elements in the prefix
#  * the unparsed suffix string
sub parse_xml {
  my $prefix_length = 0;
  my $tags = [];
  my $remaining = shift;
  print STDERR "parse_xml($remaining)\n" if ($debug);
  if ($remaining eq '') {
    return (0, [], '');
  } elsif ($remaining =~ /^[^<]+/) {
    ($prefix_length, $remaining) = (length($&), $');
  } elsif ($remaining =~ /
	    ^ < ( $qname_re ) # tag name
	        # attributes
		( (?: \s+ $qname_re =
		      (?: " [^"]* " |
		          ' [^']* ' |
			  # technically not valid XML, but people do it anyway
			  (?: [^"'] [^>=]*? )? (?= > | \s+ $qname_re = )
		      )
		  )*
		)
	      \s* (\/?) >
	   /x) {
    # start tag
    my ($tag_name, $attr_str, $is_empty) = ($1, $2, $3);
    ($prefix_length, $remaining) = (length($&), $');
    print STDERR "got start tag for $tag_name\n" if ($debug);
    my $tag = +{
      type => 'xml',
      start => 0,
      tag_name => $tag_name,
      attributes => [],
      content_start => $prefix_length,
      content_end => undef,
      end => undef
    };
    while ($attr_str =~ /
            ^ \s+ ( $qname_re ) =
	      (?: " ( [^"]* ) " |
		  ' ( [^']* ) ' |
		  # technically not valid XML, but people do it anyway
		  ( (?: [^"'] [^>=]*? )? ) (?= > | \s+ $qname_re = )
	      )
	    /x) {
      push @{$tag->{attributes}}, +{ name => $1, val => ($2 or $3 or $4) };
      $attr_str = $';
    }
    die "failed to parse all XML attributes: '$attr_str'"
      unless ($attr_str eq '');
    my ($parsed_length, $parsed_tags);
    if ($is_empty eq '/') { # empty tag, no end tag expected
      $tag->{content_end} = $tag->{content_start};
      $tag->{end} = $tag->{content_start};
    } else { # not empty
      print STDERR "parsing children of $tag_name\n" if ($debug);
      # parse children until we get to the corresponding end tag
      until ($remaining =~ /^<\/$tag_name>/ or $remaining eq '') {
	($parsed_length, $parsed_tags, $remaining) = parse_xml($remaining);
	push @$tags, map { offset_xml_tag($_, $prefix_length) } @$parsed_tags;
	$prefix_length += $parsed_length;
      }
      $tag->{content_end} = $prefix_length;
      if ($remaining =~ /^<\/$tag_name>/) { # found the end tag
	print STDERR "got end tag for $tag_name\n" if ($debug);
	$prefix_length += length($&);
	$remaining = $';
	$tag->{end} = $prefix_length;
	print STDERR "after end tag, remaining=$remaining\n" if ($debug);
      } else { # found the end of the string
	print STDERR "Warning: missing end tag '</$tag_name>'\n";
	$tag->{end} = $tag->{content_end};
      }
    }
    push @$tags, $tag;
  } elsif ($remaining =~ /^ < \/ ( [\w-]+ (?: : [\w-]+ ) ) >/x) {
    # end tag
    print STDERR "Warning: skipping extra end tag '$&'\n";
    ($prefix_length, $remaining) = (length($&), $');
  } else { # got a < but failed to parse a tag
    print STDERR "Warning: malformed XML, skipping a '<'\n";
    ($prefix_length, $remaining) = (1, substr($remaining, 1));
  }
  print STDERR "parse_xml returning prefix_length=$prefix_length; remaining=$remaining\n" if ($debug);
  return ($prefix_length, $tags, $remaining);
}

# just a few common ones
my %xml_entities = (qw(
  amp	&
  lt	<
  gt	>
  quot	"
  ),
  'nbsp' => ' '
);

# given an XML entity &...;, return the string (usually single character) it
# represents
sub translate_xml_entity {
  my $entity = shift;
  if ($entity =~ /^&#x([0-9a-f]+);$/i) { # hexadecimal character reference
    chr(hex($1));
  } elsif ($entity =~ /^&#(\d+);$/) { # decimal character reference
    chr($1);
  } elsif ($entity =~ /^&([\w-]+);$/) { # named character (entity) reference
    my $entity_name = $1;
    if (exists($xml_entities{$entity_name})) {
      $xml_entities{$entity_name};
    } else {
      print STDERR "Warning: ignoring unknown XML entity reference '$entity'\n";
      $entity;
    }
  }
}

# given some text with XML entities (but no tags) in it, process it according
# to $self->{xml_tags_mode}:
#  * keep - keep entities as is
#  * remove - translate entities with no spaces added
#  * replace-with-spaces - translate entities and add enough spaces to preserve
#  character offsets in the rest of the string
sub process_xml_entities {
  my ($self, $text, $start) = @_;
  if ($self->{xml_tags_mode} eq 'keep') {
    # do nothing
  } elsif ($self->{xml_tags_mode} eq 'remove') {
    $text =~ s/&#?[\w-]+;/translate_xml_entity($&)/eg;
  } elsif ($self->{xml_tags_mode} eq 'replace-with-spaces') {
    $text =~ s/&#?[\w-]+;/
      my $translation = translate_xml_entity($&);
      die "entity translation '$translation' bigger than its reference '$&'!"
        if (length($translation) > length($&));
      $translation .= ' 'x(length($&) - length($translation));
      push @{$self->{next_xml_input_spaces}}, +{
	start => $start + $-[0],
	end => $start + $+[0]
      };
      $translation
    /eg;
  } else {
    die "bad xml_tags_mode: $self->{xml_tags_mode}";
  }
  return $text;
}

# given some text with XML tags in it, process it according to
# $self->{xml_tags_mode}, store the XML tags in $self->{next_xml_input_tags},
# store any spaces used to replace tags and entities in
# $self->{next_xml_input_spaces}, and return the processed string.
sub process_xml_tags {
  my ($self, $text) = @_;
  my $tags = [];
  $self->{next_xml_input_spaces} = [];
  my $remaining = $text;
  my $prefix_length = 0;
  my ($parsed_length, $new_tags);
  until ($remaining eq '') {
    print STDERR "remaining=$remaining\n" if ($debug);
    ($parsed_length, $new_tags, $remaining) = parse_xml($remaining);
    push @$tags, map { offset_xml_tag($_, $prefix_length) } @$new_tags;
    $prefix_length += $parsed_length;
  }
  print STDERR Data::Dumper->Dump([$tags],['*(xml)tags']) if ($debug);
  my $new_text;
  if ($self->{xml_tags_mode} eq 'keep') {
    for (@$tags) {
      $_->{text} = substr($text, $_->{start}, $_->{end} - $_->{start});
    }
    $new_text = $text;
  } elsif ($self->{xml_tags_mode} eq 'remove') {
    $new_text = '';
    # get just the spans of the XML tags themselves (not their contents)
    my @xml_tag_spans =
      sort { $a->{start} <=> $b->{start} }
      map {
	({ start => $_->{start}, end => $_->{content_start} },
	 ($_->{content_end} != $_->{end} ? 
	   { start => $_->{content_end}, end => $_->{end} } : ())
	)
      } @$tags;
    print STDERR Data::Dumper->Dump([\@xml_tag_spans],['*xml_tag_spans']) if ($debug);
    # copy in everything between those spans (processing entities too)
    my $prev_end = 0;
    my %old_to_new_offsets = (0 => 0);
    for (@xml_tag_spans) {
      my $chunk =
        process_xml_entities($self, substr($text, $prev_end, $_->{start} - $prev_end), $prev_end);
      print STDERR "adding chunk '$chunk' after $prev_end and before $_->{start}...$_->{end}\n" if ($debug);
      $new_text .= $chunk;
      $old_to_new_offsets{$_->{start}} = length($new_text);
      $old_to_new_offsets{$_->{end}} = length($new_text);
      $prev_end = $_->{end};
    }
    # adjust tag offsets and set text
    for my $tag (@$tags) {
      for my $offset_name (qw(start content_start content_end end)) {
	$tag->{$offset_name} = $old_to_new_offsets{$tag->{$offset_name}};
      }
      $tag->{text} =
        substr($new_text, $tag->{content_start},
	       $tag->{content_end} - $tag->{content_start});
    }
  } elsif ($self->{xml_tags_mode} eq 'replace-with-spaces') {
    # start out with copy of old text
    $new_text = $text;
    # replace start/end tags with spaces
    for my $tag (@$tags) {
      print STDERR "replacing start tag from " . $tag->{start} . " to " . $tag->{content_start} . " with spaces\n" if ($debug);
      my $start_tag_length = $tag->{content_start} - $tag->{start};
      substr($new_text, $tag->{start}, $start_tag_length,
	     ' 'x$start_tag_length);
      push @{$self->{next_xml_input_spaces}}, +{
	start => $tag->{start},
	end => $tag->{content_start}
      };
      my $end_tag_length = $tag->{end} - $tag->{content_end};
      if ($end_tag_length > 0) {
	print STDERR "replacing end tag from " . $tag->{content_end} . " to " . $tag->{end} . " with spaces\n" if ($debug);
	substr($new_text, $tag->{content_end}, $end_tag_length,
	       ' 'x$end_tag_length);
	push @{$self->{next_xml_input_spaces}}, +{
	  start => $tag->{content_end},
	  end => $tag->{end}
	};
      }
    }
    $new_text = process_xml_entities($self, $new_text, 0);
    # sort spaces since we added them out of order
    @{$self->{next_xml_input_spaces}} = sortTags(@{$self->{next_xml_input_spaces}});
    print STDERR Data::Dumper->Dump([$self->{next_xml_input_spaces}],['*next_xml_input_spaces']) if ($debug);
    # set tag text
    for (@$tags) {
      $_->{text} = substr($new_text, $_->{start}, $_->{end} - $_->{start});
    }
  } else {
    die "bad xml_tags_mode: $self->{xml_tags_mode}";
  }
  $self->{next_xml_input_tags} = $tags;
  print STDERR "after applying xml_tags_mode=$self->{xml_tags_mode}, new_text is:\n$new_text\n" if ($debug);
  return $new_text;
}

# if $lhs matches $xml_tag, return a hashref of variable bindings
sub lhs_matches_xml_tag {
  my ($lhs, $xml_tag, $text) = @_;
  # check whether they match, making variable bindings where necessary
  my $bindings = {};
  if ($lhs->{tag_name} =~ /^\?/) {
    $bindings->{$lhs->{tag_name}} = $xml_tag->{tag_name};
  } elsif ($lhs->{tag_name} ne $xml_tag->{tag_name}) {
    return 0;
  }
  # FIXME we generally assume there are no repeat attribute names, but I think
  # XML allows them
  for my $lhs_attr (@{$lhs->{attributes}}) {
    my @tag_attrs =
      grep { $_->{name} eq $lhs_attr->{name} } @{$xml_tag->{attributes}};
    return 0 unless (@tag_attrs);
    my $tag_attr_val = $tag_attrs[0]{val};
    if ($lhs_attr->{val} =~ /^\?/) { # variable
      if (exists($bindings->{$lhs_attr->{val}})) { # pre-existing binding
	return 0 unless ($bindings->{$lhs_attr->{val}} eq $tag_attr_val);
      } else { # new binding
        $bindings->{$lhs_attr->{val}} = $tag_attr_val;
      }
    } else { # constant
      return 0 unless ($lhs_attr->{val} eq $tag_attr_val);
    }
  }
  # now we know the lhs matches
  # add some implicit variable bindings
  my ($start, $end) = ($xml_tag->{content_start}, $xml_tag->{content_end});
  # get just the content of the tag
  # FIXME this happens too early: if the RHS of a rule overrides start/end,
  # we'll get the wrong lex
  my $lex = substr($text, $start, $end - $start);
  # trim spaces from the content, in case we're in replace-with-spaces
  # mode, and there are nested XML tags
  $lex =~ s/^(\s*)(.*?)(\s*)$/$2/;
  $start += length($1);
  $end -= length($3);
  $bindings = +{
    '?lex' => $lex, # do both lex and text, just in case
    '?text' => $lex,
    '?start' => $start,
    '?end' => $end,
    '?tag-start' => $xml_tag->{start},
    '?tag-end' => $xml_tag->{end},
    '?content-start' => $xml_tag->{content_start},
    '?content-end' => $xml_tag->{content_end},
    %$bindings
  };
  return $bindings;
}

# return a copy of $rhs with the keys of the hashref $bindings substituted with
# their values
sub substitute_variables {
  my ($rhs, $bindings) = @_;
  my $new_rhs;
  if (ref($rhs) eq 'ARRAY') {
    $new_rhs = [ map { substitute_variables($_, $bindings) } @$rhs ];
  } elsif (ref($rhs) eq 'HASH') {
    $new_rhs = {};
    for (keys %$rhs) {
      $new_rhs->{$_} = substitute_variables($rhs->{$_}, $bindings);
    }
  } elsif ($rhs =~ /^\?/) { # variable
    die "Unbound variable $rhs on RHS of rule"
      unless (exists($bindings->{$rhs}));
    $new_rhs = $bindings->{$rhs};
  } elsif ($rhs =~ /^"(\?[\w-]+)"$/) { # quoted variable
    my $var = $1;
    die "Unbound variable $var on RHS of rule"
      unless (exists($bindings->{$var}));
    $new_rhs = '"' . $bindings->{$var} . '"';
  } else { # constant scalar
    $new_rhs = $rhs;
  }
  return $new_rhs;
}

# evaluate the RHS of a rule after its variables have been substituted; that
# is, substitute case expressions with the value from the matching branch
sub evaluate_rhs {
  my $rhs = shift;
  my $new_rhs;
  if (ref($rhs) eq 'ARRAY') {
    if (@$rhs >= 3 and $rhs->[0] eq 'case') { # case expression
      my $key = $rhs->[1];
      die "malformed case expression key"
        if (ref($key));
      for my $branch (@{$rhs}[2,$#$rhs]) {
	die "malformed case expression branch"
	  unless (ref($branch) eq 'ARRAY' and @$branch == 2);
	if ($branch->[0] eq $key) {
	  $new_rhs = $branch->[1];
	  last;
	}
      }
      die "key '$key' fell through all branches of case expression"
        unless (defined($new_rhs));
    } else { # literal array
      $new_rhs = [ map { evaluate_rhs($_) } @$rhs ];
    }
  } elsif (ref($rhs) eq 'HASH') {
    $new_rhs = {};
    for (keys %$rhs) {
      $new_rhs->{$_} = evaluate_rhs($rhs->{$_});
    }
  } else { # scalar
    $new_rhs = $rhs;
  }
  return $new_rhs;
}

# return the list of tags resulting from evaluating $rule against $xml_tag (if
# any)
sub apply_rule {
  my ($rule, $xml_tag, $text) = @_;
  print STDERR "apply_rule(\n" . Data::Dumper->Dump([$rule, $xml_tag, $text], [qw(*rule *xml_tag *text)]) . ")\n" if ($debug);
  return () unless (ref($rule->{rhs}));
  print STDERR "rule rhs is a ref!\n" if ($debug);
  my $bindings = lhs_matches_xml_tag($rule->{lhs}, $xml_tag, $text);
  return () unless ($bindings);
  print STDERR "rule applies!\n" . Data::Dumper->Dump([$bindings], ['*bindings']) if ($debug);
  my $rhs = substitute_variables($rule->{rhs}, $bindings);
  print STDERR "substituted " . Data::Dumper->Dump([$rhs],['*rhs']) if ($debug);
  $rhs = evaluate_rhs($rhs);
  print STDERR "evaluated " . Data::Dumper->Dump([$rhs],['*rhs']) if ($debug);
  if (ref($rhs->[0]) eq 'ARRAY') {
    return map { trips2tagNative($_) } @$rhs;
  } else {
    return trips2tagNative($rhs);
  }
}

# apply rules from $self->{xml_input_rules} to $self->{next_xml_input_tags} and
# return the results of those rule applications (not the raw xml tags)
sub tag_xml_input {
  my ($self, $text) = @_;
  my $tags = [];
  for my $xml_tag (@{$self->{next_xml_input_tags}}) {
    push @$tags,
         map { apply_rule($_, $xml_tag, $text) }
	     @{$self->{xml_input_rules}};
  }
  return $tags;
}

push @TextTagger::taggers, {
  name => "xml_input",
  init_function => \&init_xml_input_tagger,
  tag_function => \&tag_xml_input,
  # can output any type of tag
  output_types => [@TextTagger::all_tag_types],
  input_text => 1
};

1;

