#!/usr/bin/perl

# text-to-lattice-test.pl - convert plain text to lattice-test format for use by the TRIPS parser

=usage

cd $TRIPS_BASE/src/TextTagger/
./text-to-lattice-test.pl [TextTagger options...] <plain-text.txt >lattice-test.lisp

=cut

# expect to be run from src/TextTagger/
BEGIN { $ENV{TRIPS_BASE} = "../.."; }
use lib $ENV{TRIPS_BASE} . "/src/TextTagger/Perl/";
use lib $ENV{TRIPS_BASE} . "/src/";

package TTLT;
use TextTagger;
@ISA = qw(TextTagger);

use TextTagger::Escape qw(escape_for_quotes);
use KQML::KQML;

use strict vars;

sub init {
  my $self = shift;
  open DEVNULL, ">/dev/null" or die "Can't open /dev/null: $!";
  $self->{out} = \*DEVNULL; # redirect KQML to /dev/null, so we only get lattice-test on STDOUT
  $self->SUPER::init();
}

# we call this function when an error occurred, so check if that's the case and
# convert it back into a "die" (otherwise just return)
sub reply_to_msg {
  my ($self, $msg, $reply) = @_;
  $reply = KQML::KQMLReadFromString($reply) if (ref($reply) eq '');
  if ($reply->[2]->[0] eq 'reject') {
    die eval($reply->[2]->[2]); # extract the actual die message
  }
}

# override IKMU to output lattice-test format
sub imitateKeyboardManagerUtterance {
  my ($self, $clauseTag, $tripsTags) = @_;
  my ($text, $uttnum) = ($clauseTag->{text}, $clauseTag->{uttnum});
  print "(LATTICE-TEST :ID UTT" . sprintf("%03d", $uttnum + 1) .
        " :UTT \"" . escape_for_quotes($text) . "\"" . 
	" :PARSEABLE T :LAT (lattice\n";
  for my $tripsTag (@$tripsTags) {
    # remove :uttnum
    pop @$tripsTag;
    pop @$tripsTag;
    print KQML::KQMLAsString($tripsTag) . "\n";
  }
  print "))\n\n";
}

# initialize TextTagger
my $tt = TTLT->new('-connect', 'no', @ARGV);
$tt->init();

@ARGV = ();
while (<>)
{
  chomp;
  next if (/^\s*$/);
  # send a fake message to cause IKMU to be called
  my $msg = KQML::KQMLKeywordify(KQML::KQMLReadFromString(
    "(request :content (tag" .
    " :text \"" . escape_for_quotes($_) . "\"" .
    " :imitate-keyboard-manager t" .
    " :split-clauses t" .
# default type (for STEP):
#    " :type default" .
# type roughly equivalent to the original ttlt:
    " :type (or words punctuation named-entities street-addresses)" .
    "))"
    ));
  $tt->receive_request($msg, KQML::KQMLKeywordify($msg->{':content'}));
}

