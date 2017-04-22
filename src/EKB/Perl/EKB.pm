# EKB.pm
#
# Time-stamp: <Fri Apr 21 23:22:07 CDT 2017 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>,  3 May 2016
# $Id: EKB.pm,v 1.26 2017/04/22 04:23:53 lgalescu Exp $
#

#----------------------------------------------------------------
# Description:
# Package for manipulating EKBs.

#----------------------------------------------------------------
# History
# 2016/05/03 v1.0	lgalescu
# - Created, based on prior scripts.
# 2016/05/22 v1.1	lgalescu
# - Basic OO interface.
# 2016/05/26 v1.2	lgalescu
# - OO interface good enough to support a number of EKB scripts.
# 2016/06/07 v1.3	lgalescu
# - Various fixes.
# - Clearer distinction between OO interface and LibXML extensions.
# - Added more functionality.
# - Added option to mark up inputs with section types for EKBs derived
#   from publications.
# 2016/06/13 v1.4	lgalescu
# - Several additions to the interface for ease of use.
# - Various fixes.
# 2016/08/01 v1.4.1	lgalescu
# - Minor fixes.
# 2016/09/22 v1.4.2	lgalescu
# - Fixed ONT type for CAUSE (used to be ONT::CAUSE-EFFECT, now just
#   ONT::CAUSE)
# 2016/10/01 v1.4.3	lgalescu
# - Code cleanup.
# 2016/10/03 v1.5	lgalescu
# - Added method for cropping an EKB to a single sentence.
#   Note: this only works for text inputs!
# 2016/10/05 v1.5.1	lgalescu
# - Improved cropping to handle differences in spacing between paragraph and
#   sentence.
# 2017/02/03 v1.5.2	lgalescu
# - adapted to work as part of EKB TRIPS component
# 2017/02/14 v1.5.2	lgalescu
# - New methods: set_file, get_file, save, is_article.
# - Added handling for new attribute describing input (doc) type.
#   - TODO: eventually the "pmc" option should be removed; keeping it around
#     for now so old files can still be handled properly.
# - Some code cleanup.
# 2017/02/20 v1.6	lgalescu
# - Started adding pod documentation.
# - Improved constructor: now can create an empty EKB.
# 2017/03/02 v1.6.1	lgalescu
# - More documentation.
# - Some minor code cleanup and additions.
# 2017/03/04 v1.6.2	lgalescu
# - More documentation.
# - Some refactoring.
# - Implemented a bunch of new utility functions.
# 2017/03/07 v1.6.3	lgalescu
# - More documentation.
# - Some more refactoring.
# 2017/03/07 v1.6.4	lgalescu
# - Added toString method.
# 2017/03/11 v1.7.0	lgalescu
# - Improved pod.
# - Reimplemented &add_X_r methods.
# 2017/03/19 v1.8.0	lgalescu
# - Changed some utility functions to methods.
# - Improved normalization.
# 2017/03/21 v1.8.1	lgalescu
# - More small improvements in normalization.
# - Changed add_feature to allow adding empty nodes (HACK!).
# 2017/03/21 v1.9.0	lgalescu
# - Added method for making terms for complexes.
# 2017/03/23 v1.10.0	lgalescu
# - Added method for making terms for complexes.
# 2017/04/01 v1.11.0	lgalescu
# - Added filter() method; updated crop().
# 2017/04/01 v1.11.1	lgalescu
# - Remove XML declaration from result of &toString.
# 2017/04/18 v1.11.2	lgalescu
# - Fixed &normalize so it works on EKBs that are not the result of reading
#   (no sentences).
# 2017/04/21 v1.12.0	lgalescu
# - &make_assertion now creates an id, if not given
# - new utility function for parsing attribute/properties arguments
# - improved &add_complex_r method
# 2017/04/21 v1.13.0	lgalescu
# - some cleanup
# - added XPath filter to &assertion_args. made &get_children_by_name_regex and
#   &get_child_node_regex_attr obsolete.


# TODO:
# - maybe split off XML Node extensions into a separate package (EKB::EKBNode?)
# - add DTD validation

package EKB;

$VERSION = '1.12.0';

=head1 NAME

EKB - Perl module for handling EKBs

=head1 SYNOPSIS

OO interface:

  use EKB;
  $ekb = EKB->new(); # creates new, empty EKB
  $ekb = EKB->new($filepath); # creates new EKB, read from file

a. EKB operations

  $ekb->print($filepath); # writes EKB to file
  $ekb->normalize(); # normalizes EKB
  $ekb->crop($uttnum); # crops EKB to content derived from a given utterance
  $ekb->save(); # saves EKB (writes it to the save file it was read from)

b. Search

  @list = $ekb->get_assertions(); # returns all assertions
  @list = $ekb->get_assertions('TERM'); # returns all assertions of type TERM
  $a = $ekb->get_assertion('V123', 'EVENT'); # returns assertion of type EVENT with id V123
  ...

c. Editing

  $a = $ekb->make_assertion($atype, $attributes, @properties);
  $ekb->add_assertion($a);
  $ekb->remove_assertion($a);
  $ekb->clean_assertion($a);
  $a1 = $ekb->clone_assertion($a);
  $a1 = $ekb->derive_assertion($a, $atype);
  $ekb->modify_assertion($a, $attributes, @properties);
  $ekb->add_arg($a, $role => $id);
  $ekb->add_feature($a, $feature => $value);
  $ekb->add_notfeature($a, $feature => $value);
  $ekb->make_complex_term($ids, $attributes, @properties);
  $ekb->make_aggregate_term($ids, $operator, $attributes, @properties);


d. Other

  $boolean = $ekb->is_relation($a);


Non-OO utility functions:

  $t = get_assertion_type($a);
  set_slot_value($n, $name, $value);
  $v = get_slot_value($n, $name);  
  $p = make_slot_node($pname => $pvalue);
  @plist = make_slot_nodes( $n1 => $v1, $n2 => $v2, ...);


  ...

=head1 DESCRIPTION

An EKB is an XML-based representation of events and concepts. EKBs are used 
in the TRIPS system to represent knowledge, typically extracted from text.

This module offers an OO Perl representation of an EKB, with methods for 
manipulating it. Most methods operate on the XML document itself, and are 
based on L<XML::LibXML>.

In short, an EKB is a set of assertions about the existence of certain
entities and relations. The type of each assertion is represented via the tag
of the XML element. Entities have the type C<TERM>. Relations can represent
events (type  C<EVENT>), causal connections (type C<CC>), and two kinds of
modality operators (types C<MODALITY> and C<EPI>) over relations. 

Every assertion has a type, a set of attributes and a set of properties.
Although logically there is no deep distinction between attributes and
properties, by convention we represent as attributes those features of the
assertion that don't have semantic content, and as properties those features
that do have semantic content. Attributes are represented as XML attributes,
and properties are represented as children elements. Properties themselves can
have attributes.

NOTE: "slot" is another name for "property"

Typically, these assertions are extracted from text, in which case: 

=over 10

=item - the EKB includes a representation of the input text in two forms: 
as a collection of paragraphs, and as a collection of sentences. 

=item - the assertions include attributes pointing at the sentence, 
paragraph, and frames in the paragraph from which the assertion was derived.

=back

This package includes a number of helper functions that are not part of the
OO representation. Most can be thought of as extensions of L<XML::LibXML> 
for manipulating nodes with a certain kind of structure that are used often
in EKBs.

=cut

use 5.012;
use strict 'vars';
use warnings;
use open qw(:utf8 :std);

use Data::Dumper;
use File::Basename;
use List::Util qw(any first max);
use XML::LibXML;

use EKB::Match;

use util::Log;

# FIXME: do we really need to export symbols? maybe i should move non-OO
# functions to a new package (EKB::Util)?!?
# TODO: find the minimum set of functions we need to export

require Exporter;

our @ISA = qw(Exporter);

our @EXPORT = qw(
		  get_child_node
		  get_children_by_name_regex
		  get_child_node_regex_attr
		  get_child_node_attr
		  get_slot_value set_slot_value

		  remove_elements
		  clean_empty_leafs

		  make_node clone_node
		  make_slot_node make_slot_nodes
		  make_predicate
		  make_arg

		  set_attribute
		  append_to_attribute

		  check_protein_equivs
               );



my @entityTypes = ('TERM');
my @relationTypes = ('EVENT', 'MODALITY', 'EPI', 'CC' );
my @extractionTypes = ( @entityTypes, @relationTypes );


=head1 CONSTRUCTOR

=head2 new( $filepath, @options )

Creates a new EKB. All arguments are optional.

With no arguments, returns an empty EKB object.

If $filepath is given, it reads the EKB from the respective file; 
on success, it returns a new EKB object backed by the document read, 
and on failure it returns C<undef>.

A second argument may be provided, which can set various options. 

TODO: describe options.

=cut

# options:
# pub => 0|1,
# xml => <XML::LibXML->load_xml options hash> 
sub new {
  my $class = shift;
  my $self = {
	      file => undef,
	      options => undef,
	      document  => undef,
	      # gensym counters
	      symbol_table => {},
	     };
  if (@_) {
    $self->{file} = shift;    
    $self->{options} = shift;
  }
  bless $self, $class;
  if ($self->{file}) {
    $self->read()
      or return undef;
  } else {
    $self->init();
  }
  return $self;
}

=head1 GETTERS AND SETTERS

=head2 get_file( )

Returns the file associated with this EKB.

=cut

sub get_file {
  my $self = shift;
  return $self->{file};
}

=head2 set_file( $filepath )

Sets the file associated with this EKB.

This file must be set for L<read()|/read(_)> and L<save()|/save(_)> to work.

=cut

sub set_file {
  my ($self, $file) = @_;
  $self->{file} = $file;
}

sub get_option {
  my $self = shift;
  my $opt = shift;
  return undef unless ($self->{options} &&
		       exists $self->{options}{$opt});
  return $self->{options}{$opt};
}

=head2 get_document( )

Returns the XML document backing up this EKB.

=cut

sub get_document {
  my $self = shift;
  return $self->{document};
}

=head1 METHODS

=cut

# 3. Whole EKB operations

=head2 init( )

Sets the EKB document to a new, empty EKB (one that contains only the root 
element, C<E<lt>ekb/E<gt>>).

=cut

## Create a new EKB document
sub init {
  my $self = shift;
  my $doc = XML::LibXML->createDocument("1.0", "UTF-8");
  my $root = $doc->createElement("ekb");
  $doc->setDocumentElement($root);
  $self->{document} = $doc;
  $self->set_attr("timestamp", _timestamp());
  1;
}

=head2 read( )

Sets the document to one read from the EKB file.

As a side effect, if the EKB represents knowledge extracted from an article,
as evidenced by the input document type, it tags all input paragraphs with
the appropriate section type. NOTE: this feature should eventually be removed;
instead, section types should be annotated during the creation of the source 
EKB.

=cut

# also handles XML::LibXML::Parser options, passed to load_xml
sub read {
  my $self = shift;
  my $file = $self->{file};

  DEBUG 2, "Initializing EKB from $file with options:\n%s",
    Dumper($self->{options});

  unless (-f $file) {
    ERROR "$file: $!";
    return;
  }

  my $xml_options = $self->get_option('xml') // {};
  # remove blank nodes, unless specified otherwise by the caller
  unless (exists $xml_options->{no_blanks}) {
    $xml_options->{no_blanks} = 1;
  }
  DEBUG 2, "Reading EKB from $file with options:\n%s", Dumper($xml_options);

  eval {
    $self->{document} = XML::LibXML->load_xml(location => $file,
					      %$xml_options);
  } || do
  {
    ERROR "Error reading EKB from $file: %s", $@;
    return;
  };
    
  # if we have PMC (article) input, mark it up
  if ($self->is_article()) {
    $self->tag_pub_sections();
  }
  
  1;
}

=head2 toString( )

Returns a serialization of the EKB document root element as a string.

This is just a wrapper around the C<toString> function in
L<XML::LibXML::Node>.

Caveat emptor: This method produces a serialization of the "ekb" root node,
not of the document itself, therefore there will be no XML declaration.

=cut

sub toString {
  my $self = shift;

  $self->get_document()->documentElement()->toString(1);
}

=head2 print( $filepath )

Prints the EKB to a file with the given file name. The EKB is formated and 
indented using xmllint. Generates a fatal error in case of failure.

If the argument is missing, the output goes to STDOUT.

=cut

sub print {
  my $self = shift;
  my $file = shift;
  
  my $out;
  if ($file) {
    open $out, "|xmllint --format - > $file"
      or FATAL "Cannot write to pipe or file: $!";
  } else {
    $out = *STDOUT;
  }
  binmode $out;			# do not apply any encoding layers!
  $self->get_document()->toFH($out, 1);
  close $out if $file;
}

=head2 save( )

Saves the EKB to the EKB file. Generates an error if the EKB file is not set.

=cut

## Save EKB
sub save {
  my $self = shift;

  unless ($self->{file}) {
    ERROR "Filename missing";
    return;
  }
  
  $self->print($self->{file});
}

=head2 normalize( )

Normalizes the EKB. Specifically, it modifies the EKB so that:

=over 10

=item - uttnums start at 1;

=item - lisp code is removed;

=item - duplicative information is removed from event arguments.

=back

=cut

sub normalize {
  my $self = shift;

  DEBUG 2, "Normalizing...";
  
  # fix sentence ids
  my @sentences = $self->get_sentences();
  my @assertions = $self->get_assertions();
  if (@sentences) {
    my $first_uid = $sentences[0]->getAttribute('id');
    if ($first_uid != 1) {
      foreach my $s (@sentences) {
	my $uid = $s->getAttribute('id');
	set_attribute($s, 'id', 1 + $uid - $first_uid); 
      }
      # fix uttnums (sentence id refs)
      foreach my $a (@assertions) {
	my $uttnum = $a->getAttribute('uttnum');
	set_attribute($a, 'uttnum', 1 + $uttnum - $first_uid);
      }
    }
  }
  # cleanup assertions
  map { $self->clean_assertion($_) } @assertions;
}

# check if EKB input document is "article"
## NB: newer EKBs use a type attribute for the input element, which can
## be set to "article" to signal that the EKB represents all the information
## extracted from an article. older ones don't have the type attribute; for
## them, the EKB must be created using the pub option. An alternative would
## be to add a utility method for fixing the EKB document so it conforms to
## the new format. TODO

sub is_article {
  my $self = shift;

  # FIXME: obsolete, should be removed!
  if ($self->{options}{pub}) { return 1; }

  my $doctype = $self->get_document()->findvalue('//input/@type');
  return (defined $doctype) && (lc($doctype) eq "article"); 
}

# add attributes to input paragraphs to mark up section type
# this is only available for EKBs extracted from publications
sub tag_pub_sections {
  my $self = shift;

  my @paras = $self->get_paragraphs();
  foreach my $para (@paras) {
    my $file = $para->getAttribute('file');
    my $bn = basename($file, (".xml", ".txt"));
    my $sec_type;
    if ($bn =~ m{^0T}) {
      $sec_type = "title";
    } elsif ($bn =~ m{^1A}) {
      $sec_type = "abstract";
    } elsif ($bn =~ m{^2B_\d+i}) {
      $sec_type = "introduction";
    } elsif ($bn =~ m{^2B_\d+b}) {
      $sec_type = "background";
    } elsif ($bn =~ m{^2B_\d+r}) {
      $sec_type = "results";
    } elsif ($bn =~ m{^2B_\d+m}) {
      $sec_type = "methods";
    } elsif ($bn =~ m{^2B_\d+d}) {
      $sec_type = "discussion";
    } elsif ($bn =~ m{^2B_\d+c}) {
      $sec_type = "conclusion";
    } else {
      ERROR "Unrecognized section type; input file: %s", $bn;
    }
    set_attribute($para, 'sec-type', $sec_type);
  }
}

# print extraction type counts
sub info {
  my $self = shift;
  foreach my $type (@extractionTypes) {
    my $n = scalar($self->get_assertions($type));
    next unless $n;
    INFO "%d %ss", $n, $type;
  }

}

=head2 crop( $sid )

Removes all input and assertion elements except those for the utterance 
with the uttnum $sid.

This method will reduce the paragraph to just the desired sentence. As of this
version, sentences don't have frame numbers to identify precisely the region
in the paragraph from which they are derived. Therefore, cropping the
paragraph to the desired sentence is heuristic. However, to ensure that the
result is valid, there are some fairly strict checks. If anything is amiss, it
will fail rather than produce an invalid EKB. For example, EKBs derived from
XML inputs are likely to fail.

=cut

sub crop {
  my $self = shift;
  my $sid = shift;

  my $snode = $self->get_sentence($sid)
    or FATAL "Failed to find sentence with id=%s!", $sid;

  my $stext = $snode->textContent;
  my $pid = $snode->getAttribute('pid');
  my @assertions = $self->get_assertions({ uttnum => $sid });

  DEBUG 0, "Cropping: [pid=%s sid=%s] \"%s\"", $pid, $sid, $stext;
  DEBUG 3, "Found %d assertions", scalar(@assertions);
  
  # find text in paragraph
  my $pnode = $self->get_paragraph($pid);
  my $ptext = $pnode->textContent;
  DEBUG 3, "paragraph: \"%s\"", $ptext;
  my @psents = $self->get_sentences($pid);
  DEBUG 3, "Found %d sentences in this paragraph", scalar(@psents);
  my $shift = 0;
  # skip preceding sentences
  # - we do this word by word to account for potential differences in spacing
  my @b_words;
  foreach my $b_sent (@psents) {
    last if $sid eq $b_sent->getAttribute('id');
    push @b_words, split(/\s+/, $b_sent->textContent);
  }
  foreach my $b_word (@b_words) {
    DEBUG 3, "Removing: \"%s\"", $b_word;
    $ptext =~ m/^\s*/;
    my $b_shift = length($&);
    DEBUG 3, "space: \"%s\" (len=%d)", $&, $b_shift;
    $shift += $b_shift;
    $ptext = substr($ptext, $b_shift);
    my $i = index($ptext, $b_word);
    FATAL "Cropping failed!"
      unless ($i == 0);
    $b_shift = length($b_word);
    $ptext = substr($ptext, $b_shift);
    $shift += $b_shift;
  }
  $ptext =~ m/^\s*/;
  my $b_shift = length($&);
  DEBUG 3, "space: \"%s\" (len=%d)", $&, $b_shift;
  my $rtext = substr($ptext, $b_shift);
  $shift += $b_shift;
  DEBUG 3, "shift = $shift; paragraph: \"%s\"", $rtext;
  # trim to selected sentence
  # - we do this word by word to account for potential differences in spacing
  my @s_words = split(/\s+/, $stext);
  $ptext = "";
  while (my $s_word = shift @s_words) {
    DEBUG 3, "Adding: \"%s\"", $s_word;
    my $i = index($rtext, $s_word);
    FATAL "Cropping failed!"
      unless ($i == 0);
    $ptext .= $s_word;
    last unless @s_words;
    $rtext = substr($rtext, length($s_word));
    $rtext =~ m/^\s*/;
    $ptext .= $&;
    $rtext = substr($rtext, length($&));
  }
  DEBUG 3, "new paragraph: \"%s\"", $ptext;
  $pnode->removeChildNodes();
  $pnode->appendText($ptext);
  # ok, we succeeded thus far. now, clean up!
  # remove unwanted paragraphs and the corresponding sentences and assertions
  $self->filter({ paragraphs => [ $pid ],
		  sentences => [ $sid ] });
  # adjust frame numbers on remaining assertions
  foreach my $a ($self->get_assertions()) {
      my $s = $a->getAttribute('start');
      set_attribute($a, 'start', $s - $shift);
      my $e = $a->getAttribute('end');
      set_attribute($a, 'end', $e - $shift);
  }
}

=head2 filter()

Generic filter for removing paragraphs and/or sentences and related assertions.

Unlike crop(), this method does not attempt to keep frame numbers valid after
paragraphs are removed. 

=cut

sub filter {
  my $self = shift;
  my $opts = shift;

  return unless (defined($opts) and ref($opts) eq 'HASH');

  if (exists $opts->{paragraphs} ) {
    my @to_keep = @{ $opts->{paragraphs} };
    if (scalar(@to_keep)) {
      DEBUG 2, "Will keep paragraphs: %s", Dumper(\@to_keep);
      foreach my $p ($self->get_paragraphs) {
	my $p_id = $p->getAttribute('id');	
	next if grep { $p_id eq $_ } @to_keep;
	DEBUG 2, "Removing paragraph: %s", $p_id;
	$self->remove_paragraph($p_id);
      }
    }
  }
  if (exists $opts->{sentences}) {
    my @to_keep = @{ $opts->{sentences} };
    if (scalar(@to_keep)) {
      DEBUG 2, "Will keep sentences: %s", Dumper(\@to_keep);
      foreach my $s ($self->get_sentences) {
	my $s_id = $s->getAttribute('id');	
	next if grep { $s_id eq $_ } @to_keep;
	DEBUG 2, "Removing paragraph: %s", $s_id;
	$self->remove_sentence($s_id);
      }
    }
  }
}

sub remove_paragraph {
  my ($self, $pid) = @_;
  my $para = $self->get_paragraph($pid)
    or return;
  ($para->parentNode)->removeChild($para);
  my @sids = map { $_->getAttribute('id') } $self->get_sentences($pid);
  foreach my $sid (@sids) {
    $self->remove_sentence($sid);
  }
  DEBUG 3, "Removed paragraph $pid";
}

=head2 remove_sentence( $sid )

Removes the sentence with the id $sid and all assertions derived from it. As a
side effect, if the paragraph to which it belongs has no remaining sentences
in the EKB, that paragraph is removed as well.

=cut

# TODO: make it take either an id or the node itself
sub remove_sentence {
  my ($self, $sid) = @_;
  my $sent = $self->get_sentence($sid)
    or return;
  my $pid = $sent->getAttribute('pid');
  ($sent->parentNode)->removeChild($sent);
  my @as = $self->get_assertions({ uttnum => $sid });
  foreach my $a (@as) {
    $self->remove_assertion($a);
  }
  DEBUG 3, "Removed sentence $sid";
  DEBUG 3, "%d assertions left", scalar($self->get_assertions());
  if (scalar($self->get_sentences($pid)) == 0) {
    $self->remove_paragraph($pid);
  }
}

=head2 remove_assertion( $a )

Removes an XML representation of an assertion from the EKB document.

=cut

# TODO: make it take either an id or the node itself
sub remove_assertion {
  my ($self, $a) = @_;
  ($a->parentNode)->removeChild($a);
  DEBUG 2, "Removed assertion: %s", $a;
}

# 4. EKB getters and setters

# 4.1. EKB attributes

=head2 get_attr( $attribute )

Returns value of the EKB attribute.

=cut

## get EKB attribute
sub get_attr {
  my $self = shift;
  my $attr = shift;
  my $doc = $self->get_document()
    or return undef; 
  return $doc->documentElement()->getAttribute($attr);
}

=head2 set_attr( $attribute, $value )

Returns value of the EKB attribute to the given value.

=cut

## set EKB attribute
sub set_attr {
  my $self = shift;
  my ($attr, $value) = @_;
  my $doc = $self->get_document()
    or return undef; 
  return $doc->documentElement()->setAttribute($attr, $value);
}


=head2 get_paragraphs( )

Returns the list of paragraph nodes.

=cut

# get paragraphs
sub get_paragraphs {
  my $self = shift;
  my $doc = $self->get_document()
    or return undef; 
  return $doc->findnodes('//input//paragraph');
}

=head2 get_paragraph( $pid )

Returns the paragraph node with the given paragraph ID.

=cut

# get paragraph w/ given id
sub get_paragraph {
  my $self = shift;
  my $id = shift;
  my @paras = $self->get_paragraphs();
  return
    first { $_->getAttribute('id') eq $id } @paras;
}

=head2 get_sentences( $pid )

Returns the list of sentence nodes. If the optional argument is given, 
the list is restricted to the sentences with the given paragraph id.

=cut

# get sentence nodes
# optionally, a paragraph id may be specified to get only the sentences
# in that paragraph
sub get_sentences {
  my $self = shift;
  my $opt_pid = shift;
  my $doc = $self->get_document()
    or return undef; 
  my @result = $doc->findnodes('//input//sentence');
  if ($opt_pid) {
    @result =
      grep { $_->getAttribute('pid') eq $opt_pid } @result;
  }
  return @result;
}

=head2 get_sentence( $sid )

Returns the sentence node with the given sentence ID.

=cut

# get sentence node w/ given id
sub get_sentence {
  my $self = shift;
  my $id = shift;
  first { $_->getAttribute('id') eq $id } $self->get_sentences();
}

=head2 get_assertions( $atype, $attributes )

Returns a list of assertions. All arguments are optional.

If no arguments are present, returns all EKB assertions.
If the $atype argument is specified, returns all EKB assertions of 
the given type.
If the $attributes argument is specified, it must be a reference to a 
hash of attribute-value pairs; in this case, the resulting list is 
restricted to those assertions that have all those attributes, with the 
given values.

=cut

# N.B.: it is possible to do without $attributes; eg, the following are
# equivalent: 
#   get_assertions("TERM", { uttnum => 3});
#   grep { match_attributes($_, { uttnum => 3}) } get_assertions("TERM");
# FIXME: however, the attributes could be useful for improving efficiency,
# if checked during search
sub get_assertions {
  my $self = shift;
  my ($atype, $opt_attrs) = @_;

  DEBUG 2, "atype=%s, attrs=%s", $atype, $opt_attrs;

  if (ref($atype) eq "HASH") {
    $opt_attrs = $atype;
    $atype = undef;
  }
  my $doc = $self->get_document()
    or return undef;
  (! $atype) || is_assertion_type($atype)
    or do {
      ERROR "Unknown assertion type: %s", $atype;
      DEBUG 2, "called on bad assertion: %s", $a;
      return undef;
    };
  my @result =
    $atype
    ? $doc->findnodes('/ekb/' . $atype)
    : map { $doc->findnodes('/ekb/' . $_) } @extractionTypes;
  if ($opt_attrs) {
    @result = grep { match_attributes($_, $opt_attrs) } @result;
  }
  return @result;
}

=head2 get_assertion( $id, $atype, $attributes )

Returns the EKB assertion with the given id. Optionally, a type may be 
specified to get only the assertion of that type, if one exists. Returns
C<undef> if a matching assertion cannot be found.

...

=cut

# !experimental: optionally, a hash of attribute-value pairs may be specified
# to impose additional constraints on the desired assertion. this feature is
# experimental; it is possible to do without it
sub get_assertion {
  my $self = shift;
  my $id = shift;
  my ($atype, $opt_attrs) = @_;
  if (! $id) { # if the caller doesn't check, we need to fail safely
    ERROR "get_assertion: no id";
    return undef;
  }
  if (ref($atype) eq "HASH") {
    $opt_attrs = $atype;
    $atype = undef;
  }
  my $doc = $self->get_document()
    or return undef; 
  (! $atype) || is_assertion_type($atype)
    or do {
      ERROR "Unknown assertion type: %s", $atype;
      DEBUG 2, "called on bad assertion: %s", $a;
      return undef;
    };
  my ($result) =
    $atype
    ? $doc->findnodes('/ekb/'.$atype.'[@id="'.$id.'"]')
    : map { $doc->findnodes('/ekb/*'.'[@id="'.$id.'"]') } @extractionTypes;
  if ($opt_attrs) {
    match_attributes($result, $opt_attrs)
      or return undef;
  }
  return $result;
}

# find assertions referencing another one
# optionally, restrict assertions to a given type
sub find_referrers {
  my $self = shift;
  my ($id, $atype) = @_;

  # all assertions containing a descendant node that has an attribute id
  # referring to the arg id; exclusions: self and corefs
  # ref links can be via @id or @event
  my @result = 
    grep { $_->exists(".//*[name()!='coref' and \@id=\"$id\"] | .//*[\@event=\"$id\"]") }
    grep { $_->getAttribute('id') ne $id }
    $self->get_assertions($atype);
    
  DEBUG 3, "Found %d referrers for $id", scalar(@result);

  return @result;
}

# find instances of aggregate terms
sub find_instances {
  my $self = shift;
  my ($id) = @_; 

  my ($node) = $self->get_assertion($id);
  return () unless $node;
  DEBUG 3, "find_instances(%s)", $id;
    
  # check if this is an aggregate term
  if ($node->exists('aggregate')) {
    my @memberIds =
      map { $_->getAttribute('id')}
      $node->findnodes('aggregate/member');
    DEBUG 2, "aggregate: %s -> (%s)", $id, "@memberIds";
    return map { $self->find_instances($_) } @memberIds;
  } else {
    return ($node);
  }
}

=head2 add_assertion( $node )

Adds an XML representation of an assertion to the EKB document.

TODO: should be renamed to better reflect actual meaning

=cut

sub add_assertion {
  my ($self, $a) = @_;
  $self->get_document()->documentElement()->appendChild($a);
  DEBUG 2, "Added new assertion: %s", $a;
}

=head2 derive_assertion( $id, $atype )

Creates a new assertion based on the one with the given ID.

The new assertion will be a shallow copy of the old one, where only 
text-related attributes and properties are preserved. Specifically: 

=over 10

=item a) the new assertion will have the same type as the old one, unless 
$atype is specified, in which case the new assertion will be of the type 
$atype; 

=item b) text-related attributes (C<paragraph>, C<uttnum>, C<start> and
C<end>) are copied; and 

=item c) the only property copied is C<text>, if it exists.

=back

If successful, returns the newly created assertion. If an assertion with 
the given ID doesn't exist, returns C<undef>. 

=cut

sub derive_assertion {
  my $self = shift;
  my ($refid, $atype) = @_;
  my $ref = $self->get_assertion($refid)
    or do {
      ERROR "No assertion found with id=%s", $refid;
      return undef;
    };
  $self->make_assertion( $atype // get_assertion_type($ref),
			 { refid => $refid,
			   paragraph => $ref->getAttribute('paragraph'),
			   uttnum => $ref->getAttribute('uttnum'),
			   start => $ref->getAttribute('start'),
			   end => $ref->getAttribute('end') },
			 make_slot_node( text => get_slot_value($ref, 'text') )
		       );
}

=head2 clone_assertion( $a, $options )

Creates a new assertion as a full clone of $a.

The new assertion will be a deep copy of the old, cleaned up and with a new 
ID. The derivational history is kept via the C<refid> attribute.

The $options argument is a reference to a hash of attribute-value pairs. If 
given, the new assertion's attributes are set accordingly. Currently the 
only attribute supported is C<rule>.

If successful, returns the newly created assertion. If an assertion with 
the given ID doesn't exist, returns C<undef>. 

=cut

sub clone_assertion {
  my $self = shift;
  my ($ref, $opts) = @_;
  my $a = clone_node($ref, $self->new_id);
  $self->clean_assertion($a);
  if (ref($opts) eq 'HASH') {
    # should we append the rule???
    if (defined $opts->{'rule'}){
      	set_attribute($a, 'rule', $opts->{'rule'});
    }
  }
  $a;
}

=head2 clean_assertion( $assertion )

Removes unnecessary attributes and content from the assertion.

=cut

sub clean_assertion {
  my ($self, $a) = @_;
  # remove unnecessary attributes
  $a->removeAttribute('lisp');
  DEBUG 2, "Cleaning up: %s", $a;
  # remove unnecessary stuff from args & pseudoargs
  map { $self->_clean_arg($_) } ( $self->assertion_args($a),
				  $self->assertion_pseudoargs($a) );
}

# note: this is only useful for args extracted from text.
# Private!
sub _clean_arg {
  my ($self, $arg) = @_;
  DEBUG 2, "Cleaning up: %s", $arg;
  # all these should have an id referring to an extracted assertion
  my $id = $arg->getAttribute('id')
    # if no id, we leave it alone
    or return;
  # if id is a dangling reference, we remove it, and leave it alone
  # TODO: might want to reconsider
  unless ($self->get_assertion($id)) {
    $arg->removeAttribute('id');
    return;
  }
  # remove all content
  $arg->removeChildNodes();
  # remove all but selected (arg-dependent) atributes
  my $t = $arg->nodeName;
  my @attributes = $arg->attributes();
  foreach my $attr (@attributes) {
    my $aname = $attr->nodeName;
    next if $aname eq 'id';
    next if $aname eq 'role' and $t =~ m/^arg/;
    next if $aname eq 'mod' and $t eq 'location';
    $arg->removeAttribute($aname);
  }
  DEBUG 2, "Clean version: %s", $arg;
}

=head2 make_assertion( $atype, $attributes, @properties )

Creates and returns an assertion. 

This is basically just an OO version of
L<make_node()|/make_node(_$name,_$attributes,_@children_)>, with an additional
check that $atype is a proper assertion type.

The optional @content argument specifies the attributes and the slots of  the
assertion. It is passsed directly to
L<make_node()|/make_node(_$name,_$attributes,_@children_)>. Assertion
attributes correspond  directly to the XML element's attributes; assertion
properties must be XML nodes and they will be added as children in the 
assertion's XML Element representation. 

If an id for the assertion is not specified in $attributes, a new id is 
generated for the assertion. 

=cut

sub make_assertion {
  my $self = shift;
  my ($atype, @content) = @_;
  is_assertion_type($atype)
    or do {
      ERROR "Unknown assertion type: %s", $atype;
      # local $Data::Dumper::Terse = 1;
      # local $Data::Dumper::Indent = 0;
      # for my $frame (-3..0) {
      # 	my @cntxt = caller(-$frame);
      # 	ERROR "Trace [%d]: %s", -$frame, Dumper(\@cntxt);
      # }
      return undef;
    };
  my ($attributes, @properties) = _parse_node_args(@content);
  unless (defined($attributes) and exists $attributes->{'id'}) {
    $attributes->{'id'} = $self->new_id;
  }
  make_node($atype, $attributes, @properties);
}

=head2 modify_assertion( $a, $attributes, @properties )

Modifies an assertion by adding/replacing attributes and properties.

New attributes are simply added. Values of existing attributes are generally
replaced, with two exceptions: C<refid> and C<rule> attributes are
cummulative, so they're always added on rather than replaced. Moreover, the
value of C<refid> is a reference to a list, so multiple reference IDs can be
added on simultaneously (all regular attributes should have scalar values).

New properties are simply added. Generally, if a property already exists with
the same name, it will be deleted and a warning will be issued. However,
C<arg> properties are treated cumulatively (even when the C<role> value is the
same!).

If C<start>, C<end> and C<pid> attributes are set, the C<text> property is
calculated (if given in @properties, it will be overwritten).

See: L<append_to_attribute()|/>

=cut

sub modify_assertion {
  my $self = shift;
  my ($a, @content) = @_;
  my ($attrs, @children) = _parse_node_args(@content);
  {
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent = 0;
    DEBUG 2, "modify_assertion(%s, %s)", Dumper($attrs), join(", ", @children);
  }
  
  # append to refid, if needed
  if (exists $attrs->{'refs'}) {
    foreach my $id (@{$attrs->{'refs'}}) {
      append_to_attribute($a, 'refid', $id);
    }
    delete $attrs->{'refs'};
  }
  # append to rule, if needed
  if (exists $attrs->{'rule'}) {
    append_to_attribute($a, 'rule', $attrs->{'rule'});
    delete $attrs->{'rule'};
  }
  # set all other attributes
  set_attributes($a, %$attrs);

  # set children and warn on replacements
  foreach my $child (@children) {
    my $cname = $child->nodeName;
    unless (lc($cname) =~ m/^arg/) {
      my @s = $a->getChildrenByTagName($cname);
      if (scalar(@s) == 1) {
	$a->removeChild($s[0]);
	WARN "A child of the same type was deleted: %s", $s[0];
      } elsif (scalar(@s) > 1) {
	foreach my $c (@s) { $a->removeChild($c); }
	WARN "%d existing children of the same type were deleted", scalar(@s);
      }
    }
    $a->appendChild($child);
  }

  # set the 'text' property
  my $pid =$a->getAttribute('paragraph');
  my $start = $a->getAttribute('start');
  my $end = $a->getAttribute('end');
  if ((defined $pid) && (defined $start) && (defined $end)) {
      my $text = $self->get_span_text($pid, $start, $end);
      remove_elements($a, './text');
      $a->appendChild(make_slot_node(text => $text)); 
  }
  $a;
}

=head2 make_complex_term( $ids, $attributes, @properties )

Makes a complex term with component ids given in the list referenced by $ids.

The $ids argument is required (but can be a reference to an empty list). The
other arguments are optional.

Examples:

my $c = make_complex_term([$term_id1, $term_id2], {rule => 'MY_RULE'});

=cut

sub make_complex_term {
  my $self = shift;
  my ($ids, @content) = @_;
  my $a = $self->make_assertion( 'TERM',
				 @content );
  my @comps
    = map { make_node("component", { id => $_ }) } @$ids;
  if (@comps) {
    $a->appendChild(make_node("components", @comps));
  }
  $a;
}


=head2 make_aggregate_term( $ids, $operator, $attributes, @properties )

Makes an aggregate term with member ids given in the list referenced by $ids,
joined by $operator.

The $ids argument is required and has to be a reference to a (possibly empty)
list of ids. The $operator argument is required and must be a string. The
other arguments are optional.

Examples:

my $a = make_aggregate_term([$term_id1, $term_id2], 'AND', {rule => 'MY_RULE'});

=cut

sub make_aggregate_term {
  my $self = shift;
  my ($ids, $operator, @content) = @_;
  my $a = $self->make_assertion( 'TERM',
				 @content );
  my @comps
    = map { make_node("member", { id => $_ }) } @$ids;
  if (@comps) {
    $a->appendChild(make_node("aggregate", {operator => $operator}, @comps));
  }
  $a;
}


=head2 add_assertion_r( $atype, %args )

Creates and adds an assertion of the given type to the EKB document.

%args is a hash specifying the contents of the assertion. It may contain a
C<refs> key pointing to a list of reference assertions. The new assertion is
then created as a derivation of the first reference assertion, the effect
being that some of the content is copied from there. The rest of the
reference assertions are only used as mentions in a new attribute called
C<refid>.

Returns the id of the new assertion on success, and C<undef> on failure.

NOTE: This function uses a different convention than others for identifying 
attributes and properties; probably should be changed to avoid confusion.

=cut

sub add_assertion_r {
  my $self = shift;
  my $atype = shift;
  my %args = @_;

  my $a;
  
  # FIXME: the current signature mixes attributes, and property nodes;
  # we untangle things here, but i probably should change this!
  my @slots = $args{slots} ? @{$args{slots}} : (); delete $args{'slots'};

  if ($args{refs}) {
    my $base_id = shift @{ $args{refs} };
    $a = $self->derive_assertion($base_id, $atype)
      # FIXME: this is not robust, but bad things can happen if we go on!
      or FATAL "Could not derive assertion from id=%s", $base_id;
  } else {
    $a = $self->make_assertion($atype);
  }
  $self->modify_assertion($a,
			  \%args,
			  @slots );

  $self->add_assertion($a);

  return $a->getAttribute('id');
}

=head2 add_term_r( %args )

Creates and adds a TERM assertion to the EKB.

This is just a wrapper around L<add_assertion_r()|/add_assertion_r(_$atype,_%args_)>.

=cut

sub add_term_r {
  my $self = shift;
  $self->add_assertion_r('TERM', @_);
}

=head2 add_event_r( %args )

Creates and adds an EVENT assertion to the EKB.

This is mostly a wrapper around L<add_assertion_r()|/add_assertion_r(_$atype,_%args_)>.

=cut

sub add_event_r {
  my $self = shift;
  my $id = $self->add_assertion_r('EVENT', @_);
  { # FIXME: the following is for backward compatibility; should be removed!
    my $a = $self->get_assertion($id);
    my $p = get_slot_value($a, 'predicate');
    if (! defined $p) {
      $self->modify_assertion($a, make_predicate(get_slot_value($a, 'type')));
    }
  }
  return $id;
}

# =head2 add_complex_r( %args )

# Creates and adds a TERM assertion to the EKB, where the TERM is of
# type C<ONT::MACROMOLECULAR-COMPLEX>.

# This is a more specific version of L<add_term_r()|/add_term_r(_%args_)>.

# ...

# TODO: iirc, the idea was to move much of the work done in the reasoner when
# inferring complexes into this function. This is still work in progress!
# It is only used once in Drum.pm, where it is guarded by an option only used for
# testing.


# =cut

# idea: complex(X,Y)
#	= complex(X,Y) if X, Y are simple entities
#	= complex(X,Y1,Y2,...) if X is simple entity and Y=complex(Y1,Y2,...)
sub add_complex_r {
  my $self = shift;
  my %args = @_;

  my $a = $self->make_assertion('TERM');
  
  # get components and name
  my @comp_ids;
  my $name = "";
  if ($args{comps}) {
    my @comps = @{ $args{comps} }; delete $args{comps};
    # get component ids and names
    foreach my $c_id (@comps) {
      my $c = $self->get_assertion($c_id);
      my $c_type = get_slot_value($c, 'type');
      if (($c_type eq "ONT::MACROMOLECULAR-COMPLEX") &&
	  (my $c_comp = get_child_node($c, 'components')))
	{
	  push @comp_ids,
	    map { $_->getAttribute('id') }
	    $c_comp->childNodes();
	} else {
	  push @comp_ids, $c_id;
	}
      my $c_name = get_slot_value($c, "name") // "COMPLEX";
      $name .= ($name ? "/" : "") . $c_name;
    }
  }
  DEBUG 2, "Complex components: %s", Dumper(\@comp_ids);
  # clone components
  my @comp_nodes;
  foreach my $c_id (@comp_ids) {
    my $c = $self->get_assertion($c_id);
    my $c1 = $self->clone_assertion($c, { rule => $args{rule} });
    my $c1_id = $c1->getAttribute('id');
    $self->add_assertion($c1);
    push @comp_nodes, make_node("component", { id => $c1_id });
  }
  # make components node
  my $comps_node;
  if (@comp_nodes) {
    $comps_node = make_node("components", @comp_nodes);
  }
  $self->modify_assertion($a,
			  \%args,
			  make_slot_nodes( type => "ONT::MACROMOLECULAR-COMPLEX",
					   name => $name ),
			  $comps_node );

  return $a->getAttribute('id');
}

=head2 add_cause_r( %args )

Creates and adds a CC assertion to the EKB.

This is mostly a wrapper around L<add_assertion_r()|/add_assertion_r(_$atype,_%args_)>.

=cut

sub add_cause_r {
  my $self = shift;
  my $id = $self->add_assertion_r('CC', @_);
  # FIXME: the following is for backward compatibility; might want to make caller always use a type!
  my $a = $self->get_assertion($id);
  my $p = get_slot_value($a, 'type');
  if (! defined $p) {
    $self->modify_assertion($a, make_slot_node(type => 'ONT::CAUSE'));
  }
  return $id;
}

=head2 add_arg( $a, $role, $id, $pos )

Creates an argument element with the given ID and role name, and adds it as a
child to $a, which must be a relational assertion.

See L<make_arg()|/make_arg(_$role,_$id,_$pos )> for the meaning of the
arguments. 

If the optional $pos argument is given, the new argument is inserted so that
all arguments are in proper order (assuming other arguments have positional
information).

=cut

# Note: pos is the minimum arg position of new arg elem name [default: 1]
sub add_arg {
  my $self = shift;
  my ($a, $role, $argId, $pos) = @_;
  # assert that the modified assertion is a proper relation
  unless ($self->is_relation($a)) {
    ERROR "Cannot add argument to node: %s", $a;
    return;
  }
  # if a position is given, we heuristically make sure there is no clash 
  if (defined $pos) {
    while ($a->exists("./arg${pos}")) {
      $pos++;
    }
  }
  # make new arg node and add it to the assertion
  $a->appendChild(make_arg($role => $argId, $pos));
}

=head2 add_feature( $a, $feature, $value )

Adds a C<feature> node to an assertion's list of features.

For simple features, $value is a scalar (eg, 'TRUE'). Other features are
defined by their attributes, in which case $value is a reference to a hashlist
of attribute-value pairs.
 
Examples:

  $ekb->add_feature($a, 'active' => 'TRUE');
  $ekb->add_feature($a, 'location' => { id => 'T12345' });
  $ekb->add_feature($a, 'bound-to' => { id => 'T12346', event => 'E23456' });
  $ekb->add_feature($a, 'ptm' => { type => 'ONT::PHOSPHORYLATION', event => 'E23457' });
  $feat = $ekb->add_feature($a, 'inevent');

=cut

sub add_feature {
  my $self = shift;
  my ($a, $feature, $value) = @_;
  unless (defined $feature) {
    WARN "Tried to add an undefined feature.";
    return;
  }
  unless (defined $value) {
    WARN "Tried to set feature $feature to an undefined value.";

    # FIXME: we let things go through even when we don't have a value.
    # This is basically a HACK to allow the creation of certain complex features
    # by adding an empty feature which we can then modify as we please
    # (specifically, by adding children).
    # eventually i think this will go away, but first the ekb.dtd will have to
    # be revised to prohibit these complex "features" (TODO)

  }
  my $fs = get_child_node($a, "features");
  if (! defined $fs) {
    $fs = XML::LibXML::Element->new( "features" );
    $a->addChild($fs);
  }
  my $fnode;
  if (ref($value) eq 'HASH') {
    $fnode = make_node($feature, $value);
  } else { # assuming scalar; TODO: validate
    $fnode = make_slot_node($feature, $value);
  }
  $fs->addChild($fnode);
}


=head2 add_notfeature( $a, $feature, $value )

Adds a C<feature> node to an assertion's list of not-features.

For simple features, $value is a scalar (eg, 'TRUE'). Other features are
defined by their attributes, in which case $value is a reference to a hashlist
of attribute-value pairs.
 
Examples:

  $ekb->add_notfeature($a, 'active' => 'TRUE');
  $ekb->add_notfeature($a, 'location' => { id => 'T12345' });
  $ekb->add_notfeature($a, 'bound-to' => { id => 'T12346', event => 'E23456' });
  $ekb->add_notfeature($a, 'ptm' => { type => 'ONT::PHOSPHORYLATION', event => 'E23457' });

=cut

sub add_notfeature {
  my $self = shift;
  my ($a, $feature, $value) = @_;
  unless (defined $feature) {
    WARN "Tried to add an undefined feature.";
    return;
  }
  unless (defined $value) {
    WARN "Tried to set feature $feature to an undefined value.";
    return;
  }
  my $fs = get_child_node($a, "not-features");
  if (! defined $fs) {
    $fs = XML::LibXML::Element->new( "not-features" );
    $a->addChild($fs);
  }
  my $f;
  if (ref($value) eq 'HASH') {
    $f = make_node($feature, $value);
  } else { # assuming scalar; TODO: validate
    $f = make_slot_node($feature, $value);
  }
  $fs->addChild($f);
}

=head2 is_relation( $a )

Returns 1 iff $a is a relational assertion.

=cut

sub is_relation {
  my ($self, $a) = @_;
  return 0 unless defined $a;
  # TODO: add check that $a is a proper assertion
  return any {$_ eq $a->nodeName} @relationTypes;  
}


=head2 assertion_args( $a, $filter )

Returns list of (main) arguments for relational assertion $a.

If $a is not a relational assertion, it returns an empty list.

If the optional $filter argument is given, it must be an XPath predicate. The
result will be only those arguments matching the filter.

=cut

sub assertion_args {
  my ($self, $a, $filter) = @_;
  return () unless $self->is_relation($a);
  my @args = $a->findnodes('./*[starts-with(name(),"arg")]' . ($filter // ""));
  DEBUG 2, "Args: %s", join(",", @args);
  return @args;
}

=head2 assertion_pseudoargs( $a )

Returns list of pseudo-arguments (or satellite arguments) for relational assertion $a.

If $a is not a relational assertion, it returns an empty list.

=cut

sub assertion_pseudoargs {
  my ($self, $a) = @_;
  return () unless $self->is_relation($a);
  return (
	  get_children_by_name($a, 'location'),
	  get_children_by_name($a, 'cell-line'),
	  get_children_by_name($a, 'site'),
	  get_children_by_name($a, 'from-location'),
	  get_children_by_name($a, 'to-location')
	 );
}

# 5. Text operations

sub get_span_text {
  my $self = shift;
  my ($pid, $start, $end) = @_;

  my ($p_node) =
    $self->get_document()->findnodes('/ekb/input//paragraph[@id="'.$pid.'"]')
    or return undef;
  return substr($p_node->textContent, $start, ($end - $start));
}

=head2 new_id( $prefix )

Generates a new id. 

The prefix is optional; if none is used, the default is "R".

The default format is $prefix . "999999". Currently, there is no way to 
change this.

=cut

# I'm not sure this should be in the public interface! Maybe make it private?!?
sub new_id {
  my $self = shift;
  my $prefix = shift;
  $self->_gensym($prefix // "R", { padding => 6 });
}


# Private!
sub _gensym {
  my $self = shift;
  my $prefix = shift;
  my $opt = shift;
  my $padding = "";
  if (ref($opt) eq 'HASH') {
    $padding = ("0" . $opt->{'padding'}) // "";
  }
  return sprintf("%s%${padding}d", $prefix, ++($self->{symbol_table}{$prefix}));
}



=head1 UTILITY FUNCTIONS

This package implements and exports a number of utility functions, many of
which are operations on XML nodes. These are not part of the EKB OO
interface; they could be thought of as L<XML::LibXML::Node> extensions
performing EKB-related functions.

=cut

# 7. 

=head2 get_assertion_type( $a )

Returns the type of assertion $a, or C<undef> if $a is not an assertion.

=cut

sub get_assertion_type {
  my $a = shift;
  eval { $a->nodeType == XML_ELEMENT_NODE }
    or do {
      ERROR "Not an XML node: %s", $a;
      return undef;
    };
  my $atype = $a->nodeName;
  is_assertion_type($atype)
    or do {
      ERROR "Unknown assertion type: %s", $atype;
      DEBUG 2, "called on bad assertion: %s", $a;
      return undef;
    };
  return $atype;
}

=head2 is_assertion_type( $atype )

Returns true if $atype is a valid assertion type, and false otherwise.

=cut

sub is_assertion_type {
  my $atype = shift;
  any { $atype eq $_ } @extractionTypes; 
}

=head2 get_child_node( $node, $name )

Returns the first child of C<node> with given tag name.

=cut

sub get_child_node {
  my ($fNode) = get_children_by_name(@_);
  return $fNode;
}

=head2 get_children_by_name( $node, $name )

Returns all children of C<node> with given tag name.

=cut

sub get_children_by_name {
  return $_[0]->getChildrenByTagName($_[1]);
}

# get child nodes whose tag name matches a regex
# OBSOLETE as of 2017/04/21 -- will delete in a month
sub get_children_by_name_regex {
  my ($node, $tagRegex) = @_;
  { my ($package, $filename, $line, $callsub) = caller(1);
    WARN "get_children_by_name_regex is now obsolete. Called from: $callsub at $filename:$line";
  }
  return grep { $_->nodeName =~ $tagRegex } $node->childNodes();
}

# get child node with matching type and given attributes (optional)
# OBSOLETE as of 2017/04/21 -- will delete in a month
sub get_child_node_regex_attr {
  my ($node, $slot, $conds) = @_;
  { my ($package, $filename, $line, $callsub) = caller(1);
    WARN "get_children_by_name_regex is now obsolete. Called from: $callsub at $filename:$line";
  }
  my @cNodes = get_children_by_name_regex($node, $slot);
  return undef if scalar(@cNodes) == 0;
  DEBUG 3, "cNodes:\n%s", join("\n", @cNodes);
  my @fNodes = grep { match_attributes($_, $conds) } @cNodes;
  DEBUG 3, "fNodes:\n%s", join("\n", @fNodes);
  return undef if scalar(@fNodes) == 0;
  WARN("get_child_node_regex_attr: multiple nodes found! returning the first")
    if (scalar(@fNodes) > 1);
  return $fNodes[0];
}

# get child node that has given type and attributes (optional)
sub get_child_node_attr {
  my ($node, $slot, $conds) = @_;
  my @fNodes = $node->getChildrenByTagName($slot);
  return undef unless @fNodes;
  my ($fNode) = grep { match_attributes($_, $conds) } @fNodes;
  return $fNode;
}

=head2 get_slot_value( $node, $name )

Gets the value (text content) of $node's slot with the name $name.

=cut

# caution! this should only be used for simple slots with text values!
# TODO: check that this node doesn't haven non-text nodes as children and
# issue an error message if it does (or otherwise fail)
sub get_slot_value {
  my ($node, $slot) = @_;
  my ($fNode) = $node->getChildrenByTagName($slot);
  if (! defined $fNode) {
    return undef;
  }
  DEBUG 2, "get_slot_value($node, $slot) => %s", $fNode->textContent;
  return $fNode->textContent;
}

=head2 set_slot_value( $node, $name, $value )

Sets value (text content) of a slot.

=cut

# TODO: check that nothing bad is going on
sub set_slot_value {
  my ($node, $slot, $value) = @_;
  DEBUG 3, "node=%s\nslot=%s\nvalue=%s", $node, $slot, $value;
  my ($fNode) = $node->findnodes($slot);
  if (! defined $fNode) {
    return undef;
  }
  $fNode->removeChildNodes();
  $fNode->appendText($value);
  DEBUG 3, "modified fNode=%s", $fNode;
}

# remove a node's subelements, selected by an xpath
sub remove_elements {
  my ($node, $path) = @_;
  my @toDelete = $node->findnodes($path);
  foreach my $elem (@toDelete) {
    my $parent = $elem->parentNode;
    $parent->removeChild($elem);
  }
  # we may have empty elements, so we clean them all up
  clean_empty_leafs($node);
}

# recursively clean empty leafs of a node:
# no attributes, empty text, and no children
sub clean_empty_leafs {
  my $node = shift;
  my @nodeList = $node->childNodes();
  for my $n (@nodeList) {
    &clean_empty_leafs($n);
  }

  my $emptyElement = (($node->nodeType == XML_ELEMENT_NODE)
		      && !($node->hasChildNodes())
		      && !($node->hasAttributes()));
  my $emptyText = (($node->nodeType == XML_TEXT_NODE)
		   && ($node->textContent =~ m/^\s*$/));
  if ($emptyElement || $emptyText) {
    $node->parentNode->removeChild($node);
  }
}

=head2 make_arg( $role, $id, $pos )

Makes an argument element (for relational assertions) with the given ID and 
role name.

The optional C<pos> argument specifies the argument position (i.e., "arg1",
"arg2", etc.). It is kept only for historical reasons; generally it should
not be used.

=cut

# make an arg element for a relation assertion
sub make_arg {
  my ($role, $id, $pos) = @_;
  unless ((defined $id) and (defined $role)) {
    return undef;
  }
  # make new arg node
  make_node("arg".($pos // ""),
	    { id => $id, role => $role });
}

=head2 make_predicate( $type )

Makes a C<predicate> node with a given type (specifically, an ont-type).

FIXME: A predicate frame can also have a text slot!

=cut

sub make_predicate {
  my $type = shift;
  make_node("predicate", make_slot_node( type => $type ));
}

=head2 make_slot_node( $pname, $pvalue )

Makes a slot node with the given property name and value.

A slot node has the form C<E<lt>pnameE<gt>pvalueE<lt>/pnameE<gt>>. More
complex property nodes (with attributes and/or children) should be created
using L<make_node()|/make_node(_$name,_$attributes,_@children_)>.

=cut

sub make_slot_node {
  my ($pname, $pvalue) = @_;
  my $n = XML::LibXML::Element->new( $pname );
  $n->appendText($pvalue) if ($pvalue);
  return $n;
}

=head2 make_slot_nodes( $pname1 => $pvalue1, ... )

Makes a list of slot nodes with the given names and content.

See L<make_slot_node()|/make_slot_node(_$pname,_$pvalue_)>.

=cut

# usage: make_slot_nodes(a => v, b => w ...)
sub make_slot_nodes {
  my %args = @_;
  my @nodelist;
  while (my ($k, $v) = each(%args)) {
    push @nodelist, make_slot_node($k, $v);
  }
  return @nodelist;
}

# deep clone node
sub clone_node {
  my ($node, $id) = @_;
  my $clone = $node->cloneNode(1);
  # remove id, if any
  if ($clone->hasAttribute('id')) {
    $clone->removeAttribute('id');
    append_to_attribute($clone, 'refid', $node->getAttribute('id'));
  }
  # set id, if given
  if (defined $id) {
    set_attribute($clone, 'id', $id);
  }
  return $clone;
}

=head2 make_node( $name, $attributes, @children )

Makes an XML element node with a given name, attributes and children.

The optional argument $attributes is a reference to a hash of 
attribute-value pairs; they are added as attributes of the new node.
The optional argument @children must be a list of XML element nodes; they 
are added as children of the new node.

...

=cut

# make node of a given type
# optionally, attribute values may be given (key-value pairs)
# optionally, structural elems may be given (key-value pairs)
# optionally, children nodes may be given
# TODO:
# - change to use XML::LibXML::Attr, in which case it will be clear what should
#   be an attribute and what should be a child
sub make_node {
  my ($name, @args) = @_;
  my $node = XML::LibXML::Element->new( $name );
  # parse args
  my $attrs;
  if (@args && (ref($args[0]) eq 'HASH')) {
    $attrs = shift @args;
    set_attributes($node, %$attrs);
  }
  add_children($node, @args);
  return $node;
}

=head2 set_attribute( $node, $attribute, $value )

Sets or replaces the value of an attribute of a node.

Note: This only has effect if $value is defined! Use removeAttribute() to 
remove an attribute!

=cut

# XML::libXML::Element::setAttribute() doesn't seem to always update!
# so we use setValue() instead. i haven't investigated when and why this
# happens.
sub set_attribute {
  my ($node, $attr, $value) = @_;

  if (! defined $value) {
    WARN "Tried to set the value of %s to undef!", $attr;
    return;
  }
  
  my ($aNode) = $node->findnodes('./@'.$attr);
  if ($aNode) {
    $aNode->setValue($value);
  } else {
    $node->setAttribute($attr, $value);
  }
}

=head2 set_attributes( $node, $attribute => $value, ... )

Sets or replaces values for a set of node attributes.

=cut

sub set_attributes {
  my ($node, %attrs) = @_;
  foreach my $attr (keys %attrs) {
    set_attribute($node, $attr, $attrs{$attr});
  }
}

# add token to attribute, with separator (default: ",")
# FIXME: probably $sep should default to " " (NMTOKENS)
sub append_to_attribute {
  my ($node, $aname, $avalue, $sep) = @_;
  return unless defined $avalue;
  $sep = $sep // ",";
  my $new_avalue = $node->getAttribute($aname);
  $new_avalue .= $sep if ($new_avalue);
  $new_avalue .= $avalue;
  set_attribute($node, $aname, $new_avalue);
}

=head2 add_child( $node, $child)

Adds a child element to a node.

=cut

sub add_child {
  my ($node, $child) = @_;
  if ($child->nodeType == XML_ELEMENT_NODE) {
    $node->appendChild($child);
  } else {
    ERROR "Cannot append child (not an XML_ELEMENT_NODE): %s", $child;
  }
}

=head2 add_children( $node, @children)

Adds a set of children elements to a node.

=cut

sub add_children {
  my ($node, @children) = @_;
  foreach my $child (@children) {
    add_child($node, $child);
  }
}

# 8. Other utilities (not part of the EKB OO interface)

# check if a set of protein terms are equivalent
# n.b.: for proteins designated by a PROTEIN-FAMILY term, we look at all
# member protein dbids
# returns the "most representative" dbid
# nb: we exclude XFAM dbids, as they are not very reliable; for example,
# XFAM:PF00069 includes all proteins containing the protein kinase domain
# TODO: generalize?
sub check_protein_equivs {
  my @terms = @_;

  # we keep the data in these structure
  my %dbid_map;			# term id => [ @dbids ]
  my %counts;			# dbid => count of terms
  
  # 1. get all dbids
  foreach my $term (@terms) {
    my $id = $term->getAttribute('id');
    my $dbid_string = get_dbid_as_string($term);
    my @dbids = ( $dbid_string 
		  ? split(/\|/, $dbid_string)
		  : () );
    $dbid_map{$id}{'dbids'} =
      [ grep { $_ !~ qr{^XFAM} } @dbids ];
    foreach my $dbid (@dbids) {
      $counts{$dbid}++;
    }
  }
  {
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent = 0;
    WARN "checking equivs for: %s\ncounts: %s",
      Dumper(\%dbid_map), Dumper(\%counts); 
  }
  # 2. find the dbids appearing at least twice
  my $max_count = max map { $counts{$_} } keys %counts;
  return undef unless $max_count > 1;

  # 3. find representative: leftmost protein (must have dbid!) whose dbid
  # appears at least once in another term
  foreach my $term (@terms) {
    my $id = $term->getAttribute('id');
    my @term_dbids = @{$dbid_map{$id}{'dbids'}};
    my $found = any { $counts{$_} > 1 } @{$dbid_map{$id}{'dbids'}};
    if ($found) {
      DEBUG 3, "found representative: $id";
      return $id;
    }
  }

  WARN "equivs: seemingly equivalent, but no representative protein found!";
  
  return undef;
}

# get the dbid of a term
# FIXME: probably should join them with spaces (as NMTOKENS)
sub get_dbid_as_string {
  my $term = shift;
  # for simple entities we should have it already
  my $dbid = $term->getAttribute('dbid');
  if (! $dbid) {
    # for protein families, we gather the dbid's of the members
    # we don't check the type, as it may have changed
    if (&match_node($term, { SX => { 'members' => {} } } )) {
      my @dbids =
	map { $_->getAttribute('dbid') }
	$term->findnodes('members/member');
      return undef unless @dbids;
      $dbid = join("|", @dbids);
    } else {
      WARN "Not handled: <%s, %s>",
	$term->getAttribute('id'), $term->getAttribute('type');
    }
  }
  return $dbid;
}

### other utilities

sub __assert_eq {
  my ($a, $b) = @_;
  return 1 if ($a eq $b);
  ERROR "assertion failed: (\"%s\" eq \"%s\")", $a, $b;
  return 0;
}

# remove package from symbol
sub _remove_package {
  my $s = shift;
  $s =~ s/^[A-Z]+:://;
  return $s;
}

# timestamp
sub _timestamp
{
  my ($sec,$min,$hour,$mday,$mon,$year) = localtime(time);
  sprintf("%4d%02d%02dT%02d%02d%02d",$year+1900,$mon+1,$mday,$hour,$min,$sec);
}

# parse argument list for functions that take ($attribute, @children), where
# $attribute is optional
sub _parse_node_args {
  my @args = @_;
  my $attributes = undef;
  if (@args && (ref($args[0]) eq 'HASH')) {
    $attributes = shift @args;
  }
  return ($attributes, @args);
}

1;

=head1 AUTHOR

Lucian Galescu E<lt>lgalescu@ihmc.usE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2017 by Lucian Galescu E<lt>lgalescu@ihmc.usE<gt>

This module is free software. You may redistribute it and/or modify it under 
the terms of the Artistic License 2.0.

This program is distributed in the hope that it will be useful, but without 
any warranty; without even the implied warranty of merchantability or fitness 
for a particular purpose.

=cut
