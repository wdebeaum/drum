# EKB.pm
#
# Time-stamp: <Sat Nov  3 20:41:59 CDT 2018 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>,  3 May 2016
# $Id: EKB.pm,v 1.40 2018/11/04 03:04:56 lgalescu Exp $
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
# 2017/04/?? v1.14.0	lgalescu
# - a fair amount of cleanup
# - added &infer_assertion method to replace all &add_X_r methods, which used
#   some odd conventions, and are being phased out. the new method will be used
#   uniformly for all types of assertions
# - added utility functions for making components properties and complex names
# - improved method for finding aggregate members (recursively)
# - added methods for some commonly inferred assertions: &causes, &increases 
#   and &decreases
# - moved a few methods into package functions
# 2017/05/15 v1.14.1	lgalescu
# - added initialization from string
# 2017/05/22 (r.1.30) rcarff
# - added make_conjoined_event()
# - some other minor mods for feature handling
# 2017/05/25 v1.14.2	lgalescu
# - slight modification of the format for the 'inevent' feature
# 2017/05/29 v1.14.3	lgalescu
# - added a bit more documentation
# 2017/06/13 v1.14.4	lgalescu
# - modified normalization: now dangling IDs in arguments are left alone -- 
#   they are required for graphs to look good.
# 2018/10/14-19 v1.15.0 (r1.39) lgalescu
# - added method for setting the timestamp: set_timestamp()
# - added options for normalization; affects normalize() and clean_assertions()
# - added method for removing paragraphs: remove_paragraphs()
# - added method for querying an EKB: query_assertions()
# 2018/11/03 v1.16.0 lgalescu
# - added <document/> elements; source file is listed there now; paragraphs
#   now refer to the document using the docid attribute
# - added fix() method to transform old format to new one
# - many changes to handle document elements and ids,

# TODO:
# - maybe split off non-OO extensions for manipulating XML objects into a separate package?
# - add DTD validation

package EKB;

$VERSION = '1.16.0';

=head1 NAME

EKB - Perl module for handling EKBs

=head1 SYNOPSIS

OO interface:

  use EKB;
  $ekb = EKB->new(); # creates new, empty EKB
  $ekb = EKB->new($filepath); # creates new EKB, read from file
  $ekb = EKB->new($string); # creates new EKB, read from string

a. EKB operations

  $ekb->print($filepath); # writes EKB to file
  $ekb->normalize(); # normalizes EKB
  $ekb->crop($uttnum); # crops EKB to content derived from a given utterance
  $ekb->save(); # saves EKB (writes it to the same file it was read from)

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
  $ekb->make_conjoined_event($ids, $operator, $attributes, @properties);
  $ekb->make_aggregate_term($ids, $operator, $attributes, @properties);

Non-OO utility functions:

  $b = is_relation($a);
  $t = get_assertion_type($a);
  @args = assertion_args($a, $filter);
  @xargs = assertion_xargs($a);
  set_slot_value($n, $name, $value);
  $v = get_slot_value($n, $name);  
  $p = make_slot_node($pname => $pvalue);
  @plist = make_slot_nodes( $n1 => $v1, $n2 => $v2, ...);

  ...

=head1 DESCRIPTION

An EKB is an XML-based representation of events and concepts. EKBs are used 
in the TRIPS system to represent knowledge extracted from text.

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

=over

=item - the EKB includes a representation of the input text in two forms: 
as a collection of paragraphs, and as a collection of sentences. 

=item - the assertions include attributes pointing at the sentence, 
paragraph, and specific parts of the sentence from which the assertion was 
derived.

=back

Thus, the EKB representation can serve as an annotation format.

For reasoning purposes, only the assertions are meaningful. An EKB 
representation can be derived directly from AKRL -- the KR representation
language used in TRIPS -- in which case the EKB only contains assertions, 
suitable for reasoning. This functionality is used by some reasoners who 
understand the EKB representation but not the AKRL representation.

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
		  is_relation
		  get_assertion_type
		  is_assertion_type

		  assertion_args assertion_xargs
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
		  make_components make_complex_name

		  set_attribute
		  append_to_attribute

		  check_protein_equivs
               );



my @entityTypes = ('TERM');
my @relationTypes = ('EVENT', 'MODALITY', 'EPI', 'CC' );
my @extractionTypes = ( @entityTypes, @relationTypes );


=head1 CONSTRUCTOR

=head2 new( $input, @options )

Creates a new EKB. All arguments are optional.

With no arguments, returns an empty EKB object (ie, one backed by an empty
document).

If given, $input may be either a filename, or a string. The EKB returned is
backed by the document resulting from parsing the XML off the file or the
suring, respectively. On failure it returns C<undef>.

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
	      string => undef,
	      options => undef,
	      # DOM
	      document => undef,
	      # gensym counters
	      symbol_table => {},
	     };
  if (@_) {
    my $input = shift;
    no warnings;
    if (-f $input) {
      $self->{file} = $input;
    } else {
      $self->{string} = $input;
    }
    $self->{options} = shift;
  }
  bless $self, $class;
  if ($self->{file}) {
    $self->read()
      or return undef;
  } elsif ($self->{string}) {
    $self->fromString()
      or return undef;
  } else {
    $self->init();
  }
  # add a timestamp, if it doesn't have one
  unless ($self->get_attr('timestamp')) {
    $self->set_timestamp;
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

sub set_timestamp {
  my $self = shift;
  $self->set_attr('timestamp', _timestamp());
}

=head2 get_input( )

Returns the XML element containing the input data, if any.

=cut

sub get_input {
  my $self = shift;
  my ($input_elem) = $self->get_document()->findnodes('//input');
  return $input_elem;
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

=head2 fromString( $string )

Sets the document to one read from the given string.

=cut

# also handles XML::LibXML::Parser options, passed to load_xml
sub fromString {
  my $self = shift;
  my $string = $self->{string};

  DEBUG 2, "Initializing EKB from string with options:\n%s",
    Dumper($self->{options});

  my $xml_options = $self->get_option('xml') // {};
  # remove blank nodes, unless specified otherwise by the caller
  unless (exists $xml_options->{no_blanks}) {
    $xml_options->{no_blanks} = 1;
  }
  DEBUG 2, "Reading EKB from string with options:\n%s", Dumper($xml_options);

  eval {
    $self->{document} = XML::LibXML->load_xml(string => $string,
					      %$xml_options);
  } || do
  {
    ERROR "Error reading EKB from string: %s", $@;
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

=head2 fix( )

Upgrades older EKB formats to the current format. Limited capabilities.

=cut

sub fix {
  my ($self, $opts) = @_;

  DEBUG 2, "Fixing format...";

  my $docs = $self->get_docs;
  unless ($docs) {
    my %doc_elems;
    my @doc_files; # keep'em sorted
    my @paragraphs = $self->get_paragraphs;
    foreach my $p (@paragraphs) {
      my $file = $p->getAttribute('file');
      unless (exists $doc_elems{$file}) {
	$doc_elems{$file} = 
	  make_node("document", {
				 id => $self->_gensym("D", { padding => 4}),
				 file => $file,
				});
	push @doc_files, $file;
      }
      set_attribute($p, 'docid' => $doc_elems{$file}->getAttribute('id'));
      $p->removeAttribute('file');
    }
    if (%doc_elems) {
      my $docs_elem =
	make_node("documents", map { $doc_elems{$_} } @doc_files);
      my $input = $self->get_input;
      $input->insertBefore($docs_elem, $input->firstChild);
    }
  }
}

=head2 normalize( $opts )

Normalizes the EKB. Specifically, it modifies the EKB so that:

=over

=item - uttnums start at 1;

=item - lisp code is removed, unless given the option {keep_lisp => 1} ;

=item - duplicative information is removed from event arguments.

=item - arg# child elements are renamed to arg child elements.

=back

It also calls See: L<fix()|/> to fix formatting issues.

=cut

sub normalize {
  my ($self, $opts) = @_;

  DEBUG 2, "Normalizing...";

  # fix document format
  $self->fix;
  
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
  map { $self->clean_assertion($_, $opts) } @assertions;
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

  my $pid = $snode->getAttribute('pid');
  my $did = $self->get_paragraph($pid)->getAttribute('docid');

  my $stext = $snode->textContent;
  my @assertions = $self->get_assertions({ uttnum => $sid });

  DEBUG 0, "Cropping: [did=%s pid=%s sid=%s] \"%s\"", $did, $pid, $sid, $stext;
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

=head2 filter( $options )

Generic filter for removing parts of the input and related assertions.

$options is a reference to a hash. The only keys handled are C<documents>, 
C<paragraphs> and C<sentences>. The value for each such key is a reference to 
a list of appropriate IDs. 
The result of the filter is such that only the document/paragraphs/sentences
indicated are kept. 

Caveats:

=over

=item 1) Unlike crop(), this method does not attempt to keep frame numbers valid after paragraphs are removed. 

=item 2) Using multiple keys simultaneously that are not consistent with each 
other may not work as expected, so we discourge such use! 

=back

=cut

sub filter {
  my $self = shift;
  my $opts = shift;

  return unless (defined($opts) and ref($opts) eq 'HASH');

  # temporary, to handle EKBs in document-less EKBs  
  $self->fix;
  
  if (exists $opts->{documents} ) {
    my @to_keep = @{ $opts->{documents} };
    if (scalar(@to_keep)) {
      DEBUG 2, "Will keep documents: %s", Dumper(\@to_keep);
      foreach my $d ($self->get_docs) {
	my $docid = $d->getAttribute('id');	
	next if grep { $docid eq $_ } @to_keep;
	DEBUG 2, "Removing document: %s", $docid;
	$self->remove_document($docid);
      }
    }
  }
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
	DEBUG 2, "Removing sentence: %s", $s_id;
	$self->remove_sentence($s_id);
      }
    }
  }
}

=head2 remove_doc( $docid )

Removes the document with the id $docid, all its paragraphs and sentences, 
and all the assertions derived from it.

=cut

sub remove_doc {
  my ($self, $docid) = @_;
  my $doc = $self->get_doc($docid)
    or return;
  ($doc->parentNode)->removeChild($doc);
  my @pids = map { $_->getAttribute('id') } $self->get_paragraphs($docid);
  foreach my $pid (@pids) {
    $self->remove_paragraph($pid);
  }
  DEBUG 3, "Removed document $docid";
}

=head2 remove_paragraph( $pid )

Removes the paragraph with the id $pid, all its sentences and all assertions 
derived from it.

=cut

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


=head2 get_docs( )

Returns the list of document nodes.

=cut

sub get_docs {
  my $self = shift;
  my $doc = $self->get_document()
    or return undef; 
  return $doc->findnodes('//input//document');
}

=head2 get_doc( $docid )

Returns the document node with the given document ID.

=cut

sub get_doc {
  my ($self, $docid) = @_;
  my @docs = $self->get_docs;
  return
    first { $_->getAttribute('id') eq $docid } @docs;
}

=head2 set_docid ( $docid, $new_docid )

Replaces the document ID.

=cut

sub set_docid {
  my ($self, $docid, $new_docid) = @_;
  my $doc_elem = $self->get_doc($docid);
  my @p = $self->get_paragraphs($docid);
  set_attribute($doc_elem, 'id' => $new_docid);
  map { set_attribute($_, 'docid' => $new_docid ) } @p;
}

=head2 get_paragraphs( $docid )

Returns the list of paragraph nodes. If the optional argument is given, 
the list is restricted to the paragraphs with the given document ID.

=cut

sub get_paragraphs {
  my ($self, $docid) = @_;
  my $doc = $self->get_document()
    or return undef; 
  my @result = $doc->findnodes('//input//paragraph');
  if ($docid) {
    @result =
      grep { $_->getAttribute('docid') eq $docid } @result;
  }
  return @result;
}

=head2 get_paragraph( $pid )

Returns the paragraph node with the given paragraph ID.

=cut

sub get_paragraph {
  my ($self, $pid) = @_;
  my @paras = $self->get_paragraphs();
  return
    first { $_->getAttribute('id') eq $pid } @paras;
}

=head2 get_sentences( $pid )

Returns the list of sentence nodes. If the optional argument is given, 
the list is restricted to the sentences with the given paragraph ID.

=cut

sub get_sentences {
  my ($self, $pid) = @_;
  my $doc = $self->get_document()
    or return undef; 
  my @result = $doc->findnodes('//input//sentence');
  if ($pid) {
    @result =
      grep { $_->getAttribute('pid') eq $pid } @result;
  }
  return @result;
}

=head2 get_sentence( $sid )

Returns the sentence node with the given sentence ID.

=cut

sub get_sentence {
  my ($self, $sid) = @_;
  first { $_->getAttribute('id') eq $sid } $self->get_sentences();
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
  my ($self, $atype, $opt_attrs) = @_;

  if (ref($atype) eq "HASH") {
    $opt_attrs = $atype;
    $atype = undef;
  }
  {
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent = 0;
    DEBUG 2, "atype=%s, attrs=%s", $atype, Dumper($opt_attrs);
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
  my ($self, $id, $atype, $opt_attrs) = @_;
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

# N.B. currently $query can only be a structure expression that can be used
# by match_structure!
# example: { type => "ONT::THING" }
sub query_assertions {
  my ($self, $query) = @_;
  return grep { match_structure($_, $query) } $self->get_assertions();
}


# find assertions referencing another one
# optionally, restrict assertions to a given type
sub find_referrers {
  my ($self, $id, $atype) = @_;

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

=head2 find_members_rec( $id, $op_list )

Find recursively the members of an aggregate term with the given identifier.

If present, $op_list must be a reference to a list of allowable operators. If
not present, all operators are allowed. 

If $id is the identifier of a term that is not an aggregate, this method will
return just the term itself. 

=cut

# find instances of aggregate terms
sub find_members_rec {
  my $self = shift;
  my ($id, $ops) = @_;

  my $a = $self->get_assertion($id);
  return () unless $a;
  DEBUG 2, "find_members_rec(%s, %s)", $id, $ops ? "[".join(",", @$ops)."]" : "undef";
    
  # check if this is an aggregate term
  my $op = $a->findvalue('./aggregate/@operator');
  DEBUG 3, "op found: \"%s\"", $op;
  # and the operator matches the desired ones
  return ($a) unless $op and (!defined($ops) or (grep {$op eq $_} @$ops));
  my @memberIds =
    map { $_->getAttribute('id')}
    $a->findnodes('aggregate/member');
  DEBUG 2, "aggregate: %s -> (%s)", $id, "@memberIds";
  return map { $self->find_members_rec($_, $ops) } @memberIds;
}


=head2 add_assertion( $node )

Adds an XML representation of an assertion to the EKB document.

=cut

sub add_assertion {
  my ($self, $a) = @_;
  $self->get_document()->documentElement()->appendChild($a);
  DEBUG 2, "Added new assertion: %s", $a;
  $a;
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

=head2 derive_assertion( $id, $atype )

Creates a new assertion based on the one with the given ID.

The new assertion will be a shallow copy of the old one, where only 
text-related attributes and properties are preserved. Specifically: 

=over

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

=head2 clone_assertion( $a, $attributes )

Creates a new assertion as a clone of $a.

The new assertion will be a deep copy of the old, cleaned up and with a new 
ID. The derivational history is kept via the C<refid> attribute.

If the optional $attributes argument is given, the new assertion's attributes
are set accordingly. Currently the only attribute supported is C<rule>. This
behavior might change to support an unrestricted list of attribute-value
pairs.

If successful, returns the newly created assertion. If an assertion with 
the given ID doesn't exist, returns C<undef>. 

=cut

sub clone_assertion {
  my $self = shift;
  my ($ref, $attrs) = @_;
  my $a = clone_node($ref, $self->new_id);
  $self->clean_assertion($a);
  if (ref($attrs) eq 'HASH') {
    # should we append the rule???
    if (defined $attrs->{'rule'}){
      	set_attribute($a, 'rule' => $attrs->{'rule'});
    }
  }
  $a;
}

=head2 clean_assertion( $assertion, $opts )

Removes unnecessary attributes and content from the assertion.
Use { keep_lisp => } to keep lisp representation.

=cut

sub clean_assertion {
  my ($self, $a, $opts) = @_;
  # remove unnecessary attributes
  $a->removeAttribute('lisp')
    unless ($opts && $opts->{keep_lisp});

  DEBUG 2, "Cleaning up: %s", $a;
  # remove unnecessary stuff from args & pseudoargs
  map { $self->_clean_arg($_) } assertion_args($a), assertion_xargs($a);
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
  # if id is a dangling reference, we leave it alone
  unless ($self->get_assertion($id)) {
    return;
  }
  # remove all content
  $arg->removeChildNodes();
  # remove all but selected (arg-dependent) attributes
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
  set_attributes($a, $attrs);

  # set children and warn on replacements
  foreach my $child (@children) {
    my $cname = $child->nodeName;
    # TODO: should we be able to modify args through this method, as well?!?
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

Makes a macomolecular complex term with component ids given in the list
referenced by $ids.

The $ids argument is required (but can be a reference to an empty list). The
other arguments are optional.

Examples:

my $c = make_complex_term([$term_id1, $term_id2], {rule => 'MY_RULE'});

=cut

sub make_complex_term {
  my $self = shift;
  my $ids = shift;
  $self->make_assertion( 'TERM',
			 @_,
			 make_components(@$ids) );
}

=head2 make_complex_name( $components )

Makes a name for a macomolecular complex term from the names of its components. If any component doesn't have a name, it returns just the string "COMPLEX".

TODO: This function ought to be called automatically by EKB::make_complex_term().

=cut

sub make_complex_name {
  my @names = map { get_slot_value($_, "name") } @_;
  return
    (any { ! defined($_) } @names)
    ? "COMPLEX"
    : join("/", @names);
}

=head2 make_conjoined_event( $ids, $operator, $attributes, @properties )

Makes a conjoined event with member ids given in the list referenced by $ids,
joined by $operator.

The $ids argument is required and has to be a reference to a (possibly empty)
list of ids. The $operator argument is required and must be a string. The
other arguments are optional.

Examples:

my $a = make_conjoined_event([$term_id1, $term_id2], 'AND', {rule => 'MY_RULE'});

=cut

sub make_conjoined_event {
  my $self = shift;
  my ($ids, $operator, @content) = @_;
  my $a = $self->make_assertion( 'EVENT', @content );
  my @comps
      = map { make_node("member", { id => $_ }) } @$ids;
  if (@comps) {
    $a->appendChild(make_node("aggregate", {operator => $operator}, @comps));
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

OBSOLETE: use infer_assertion instead!

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
  $self->modify_assertion($a, \%args, @slots);
  $self->add_assertion($a);

  return $a->getAttribute('id');
}

=head2 infer_assertion( $atype, $attributes, @properties )

Creates and adds an assertion of the given type to the EKB document.

If the C<refid> attribute is set in $attributes, the new assertion is
derived from the assertion with the first id.

Returns the id of the new assertion on success, and C<undef> on failure.

=cut

sub infer_assertion {
  my $self = shift;
  my $atype = shift;
  my ($attrs, @children) = _parse_node_args(@_);
  my $a;
  my @refs = (exists $attrs->{refid}) ? split(",", $attrs->{refid}) : ();
  # derive or make new assertion 
  if (@refs) {
    my $base_id = $refs[0];
    $a = $self->derive_assertion($base_id, $atype)
      # FIXME: this is not robust, but bad things can happen if we go on!
      or FATAL "Could not derive assertion from id=%s", $base_id;
  } else {
    $a = $self->make_assertion($atype);
  }
  # add modifications
  $self->modify_assertion($a, $attrs, @children);
  # store
  $self->add_assertion($a);
  # return
  $a->getAttribute('id');
}

=head2 add_term_r( %args )

Creates and adds a TERM assertion to the EKB.

This is just a wrapper around L<add_assertion_r()|/add_assertion_r(_$atype,_%args_)>.

OBSOLETE

=cut

sub add_term_r {
  my $self = shift;
  $self->add_assertion_r('TERM', @_);
}

=head2 add_event_r( %args )

Creates and adds an EVENT assertion to the EKB.

Returns the id of the inferred event.

This is mostly a wrapper around L<add_assertion_r()|/add_assertion_r(_$atype,_%args_)>.

OBSOLETE

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

# This function has side effects: it infers bound versions of all components!

# Example:
	# $z_id =
	#   $ekb->add_complex_r( refs => [ $e_id ],
	# 		         comps => [ $x_id, $y_id ],
	# 		         rule => $rule->name,
	# 		       );

# =cut

# idea: complex(X,Y)
#	= complex(X,Y) if X, Y are simple entities
#	= complex(X,Y1,Y2,...) if X is simple entity and Y=complex(Y1,Y2,...)
# TODO (maybe)
# OBSOLETE
sub add_complex_r {
  my $self = shift;
  my %args = @_;

  if ($args{refs}) {
    my $base_id = shift @{ $args{refs} };
    $a = $self->derive_assertion($base_id, 'TERM')
      # FIXME: this is not robust, but bad things can happen if we go on!
      or FATAL "Could not derive assertion from id=%s", $base_id;
  } else {
    $a = $self->make_assertion('TERM');
  }

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
	  push @comp_ids, map { $_->getAttribute('id') } $c_comp->childNodes();
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
  $self->add_assertion($a);
  return $a->getAttribute('id');
}

# =head2 add_cause_r( %args )
# Creates and adds a CC assertion to the EKB.
# This is mostly a wrapper around L<add_assertion_r()|/add_assertion_r(_$atype,_%args_)>.
# =cut
# OBSOLETE
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

=head2 causes( $fid, $oid, $attrs )

Infers a CC assertion, setting the factor and the outcome to $fid and $oid,
respectively. 

If given, $attrs specifies additional attributes to be set for the new
assertion.

Returns the id of the inferred event.

See L<infer_assertion(_$atype,_$attributes,_@properties_)>

=cut

sub causes {
  my $self = shift;
  my ($factor_id, $outcome_id, $attrs) = @_;
  my $c = $self->infer_assertion( "CC",
				  $attrs,
				  make_slot_node(type => 'ONT::CAUSE'),
				  make_arg(':FACTOR' => $factor_id),
				  make_arg(':OUTCOME' => $outcome_id) );
  $c->getAttribute('id');
}

=head2 increases( $id, $attrs )

Infers an EVENT assertion representing a (quantitative) increase of whatever
is represented by the assertion with the id $id.

If given, $attrs specifies additional attributes to be set for the new
assertion.

Returns the id of the inferred event.

See L<add_event_r>

=cut

#TODO: should change to use add_assertion_r directly
sub increases {
  my $self = shift;
  my ($affected_id, $attrs) = @_;
  $self->add_event_r( refs => [$attrs->{refid}],
		      rule => $attrs->{rule},
		      slots => [ make_slot_node(type => 'ONT::INCREASE'), 
				 make_arg(':AFFECTED' => $affected_id) ] );
}

=head2 decreases( $id, $attrs )

Infers an EVENT assertion representing a (quantitative) decrease of whatever
is represented by the assertion with the id $id.

If given, $attrs specifies additional attributes to be set for the new
assertion.

Returns the id of the inferred event.

See L<add_event_r>

=cut

#TODO: should change to use add_assertion_r directly
sub decreases {
  my $self = shift;
  my ($affected_id, $attrs) = @_;
  $self->add_event_r( refs => [$attrs->{refid}],
		      rule => $attrs->{rule},
		      slots => [ make_slot_node(type => 'ONT::DECREASE'), 
				 make_arg(':AFFECTED' => $affected_id) ] );
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
  unless (is_relation($a)) {
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

Returns the feature element just created.

Examples:

  $ekb->add_feature($a, 'active' => 'TRUE');
  $ekb->add_feature($a, 'location' => { id => 'T12345' });
  $ekb->add_feature($a, 'bound-to' => { id => 'T12346', event => 'E23456' });
  $ekb->add_feature($a, 'ptm' => { type => 'ONT::PHOSPHORYLATION', event => 'E23457' });
  $ekb->add_feature($a, 'inevent' => { id => 'E23458' });

=cut

# FIXME: Roger changed the signature to have values as a list; in all
# likelihood, this is to accommodate "complex" features, so he can pass on
# children. since the feature nodes are being simplified, i think this will
# become unnecessary. at any rate, this doesn't look like it can hurt in any
# way.
sub add_feature {
  my $self = shift;
  my ($a, $feature, @value) = @_;
  unless (defined $feature) {
    WARN "Tried to add an undefined feature.";
    return;
  }

  unless (scalar(@value) > 0) {
    WARN "Tried to set feature $feature to an undefined value.";

    # FIXME: we let things go through even when we don't have a value.
    # This is basically a HACK to allow the creation of certain complex features
    # by adding an empty feature which we can then modify as we please
    # (specifically, by adding children).
    # eventually i think this will go away, but first the ekb.dtd will have to
    # be revised to prohibit these complex "features" (TODO)

    # update (2017/05/24): i think complex features have disappeared, now that
    # i changed the inevent element and moved it inside mods

  }

  {
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent = 0;
    DEBUG 2, "Adding feature %s = %s to %s", $feature, Dumper(@value), $a;
  }
  my $fs = get_child_node($a, "features");
  if (! defined $fs) {
    $fs = XML::LibXML::Element->new( "features" );
    $a->addChild($fs);
  }
  my $fnode;
  if (ref($value[0]) ne '') {
    $fnode = make_node($feature, @value);
  } else { # assuming scalar; TODO: validate
    $fnode = make_slot_node($feature, $value[0]);
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
  my ($a, $feature, @value) = @_;
  unless (defined $feature) {
    WARN "Tried to add an undefined feature.";
    return;
  }
  unless (scalar(@value) > 0) {
    WARN "Tried to set feature $feature to an undefined value.";
    return;
  }
  my $fs = get_child_node($a, "not-features");
  if (! defined $fs) {
    $fs = XML::LibXML::Element->new( "not-features" );
    $a->addChild($fs);
  }
  my $f;
  if (ref($value[0]) ne '') {
    $f = make_node($feature, @value);
  } else { # assuming scalar; TODO: validate
    $f = make_slot_node($feature, $value[0]);
  }
  $fs->addChild($f);
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

=head2 new_id( $prefix, $length )

Generates a new id. 

Both parameters are optional. The default prefix is "R", and the default 
length is 6.

The id generated will be the concatenation of $prefix and a number with 
$length digits. Currently, there is no way to change this format.

This function guarantees that the id is unique by always incrementing the 
number portion of the id.

=cut

# I'm not sure this should be in the public interface! Maybe make it private?!?
sub new_id {
  my ($self, $prefix, $length) = @_;
  $self->_gensym($prefix // "R", { padding => $length // 6 });
}


# Private!
sub _gensym {
  my ($self, $prefix, $opt) = @_;
  my $n_fmt = "";
  if (ref($opt) eq 'HASH') {
    $n_fmt = ("0" . $opt->{'padding'}) // "";
  }
  return sprintf("%s%${n_fmt}d", $prefix, ++($self->{symbol_table}{$prefix}));
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

=head2 is_relation( $a )

Returns 1 iff $a is a relational assertion.

=cut

sub is_relation {
  my $a = shift;
  return 0 unless (defined $a) ;
  my $atype = get_assertion_type($a);
  return any {$_ eq $atype} @relationTypes;  
}

=head2 assertion_args( $a, $filter )

Returns list of (main) arguments for relational assertion $a.

If $a is not a relational assertion, it returns an empty list.

If the optional $filter argument is given, it must be an XPath predicate. The
result will be only those arguments matching the filter.

=cut

sub assertion_args {
  my ($a, $filter) = @_;
  return () unless is_relation($a);
  my @args = $a->findnodes('./*[starts-with(name(),"arg")]' . ($filter // ""));
  DEBUG 2, "Args: %s", join(",", @args);
  return @args;
}

=head2 assertion_xargs( $a )

Returns list of satellite ('extra') arguments for relational assertion $a.

If $a is not a relational assertion, it returns an empty list.

=cut

sub assertion_xargs {
  my $a = shift;
  return () unless is_relation($a);
  return (
	  get_children_by_name($a, 'location'),
	  get_children_by_name($a, 'cell-line'),
	  get_children_by_name($a, 'site'),
	  get_children_by_name($a, 'from-location'),
	  get_children_by_name($a, 'to-location')
	 );
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
  eval { $_[0]->nodeType == XML_ELEMENT_NODE }
    or do {
      local $Data::Dumper::Terse = 1;
      local $Data::Dumper::Indent = 0;
      for my $frame (-3..0) {
	my @cntxt = caller(-$frame);
	WARN "Trace [%d]: %s", -$frame, Dumper(\@cntxt);
      }
    };
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
  return unless @toDelete;
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

TODO: rename to make_slot

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

TODO: rename to make_slots

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

=head2 make_components( @ids )

Creates a components property and returns it. 

If the list of ids is empty, it will return C<undef>.

=cut

sub make_components {
  # make all component references
  my @comps = map { make_node("component", { id => $_ }) } @_;
  # make components node
  my $comps_node;
  if (@comps) {
    $comps_node = make_node("components", @comps);
  }
  $comps_node; 
}

# deep clone node
sub clone_node {
  my ($node, $id) = @_;
  my $clone = $node->cloneNode(1);
  # remove id, if any
  if ($clone->hasAttribute('id')) {
    $clone->removeAttribute('id');
    # FIXME: should we really do this here (instead of on demand)?!?
    append_to_attribute($clone, 'refid' => $node->getAttribute('id'));
  }
  # set id, if given
  if (defined $id) {
    set_attribute($clone, 'id' => $id);
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
  my ($attrs, @children) = _parse_node_args(@args);
  set_attributes($node, $attrs);
  add_children($node, @children);
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
  my ($node, $attrs) = @_;
  return unless defined $attrs;
  foreach my $attr (keys %$attrs) {
    set_attribute($node, $attr => $attrs->{$attr});
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
  set_attribute($node, $aname => $new_avalue);
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

=head1 BUGS

Probably.

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
