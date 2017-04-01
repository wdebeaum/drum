# EKBAgent.pm
#
# Time-stamp: <Fri Mar 31 17:27:55 CDT 2017 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 13 Feb 2017
#

#----------------------------------------------------------------
# Description:
# A TRIPS Agent for handling EKBs

# Messaging:
#
# 1. EKB inference
# on receiving:
#   (REQUEST 
#    :content (do-ekb-inference :ekb "EKB_PATH" [:return-string B])
#    :reply-with R1)
# runs inference on the EKB read from file with path EKB_PATH, saves
# result to a new file, and, if all is well, sends:
#   (REPLY :in-reply-to R1
#     :content (done :result "EKB"))
# where:
# - EKB is:
#   - the KQML-stringified EKB when B is TRUE
#   - the name of the file containing the inferred EKB, when B is FALSE
# Note: If :return-string is ommitted, the result is the file name

# 2. AKRL-to-EKB
# IN:
# OUT:
#

package EKBAgent;
use TripsModule::TripsModule;
@ISA = qw(TripsModule);

use strict vars;

use File::Basename;
use File::Spec::Functions;

use EKB;
use EKB::Reasoner::Drum;

use AKRL::AKRLList;
use AKRL2EKB;

use util::Log;

sub init {
  my $self = shift;
  $self->{name} = 'EKB-AGENT';
  $self->SUPER::init();
  $self->send_subscriptions();
  print ("Initialized EKBAgent\n");
}

sub send_subscriptions {
  my $self = shift;
  # DRUM: run reasoner
  $self->send_msg('(subscribe :content (request &key :content (do-ekb-inference . *)))');
  # AKRL 2 EKB helper
  $self->send_msg('(subscribe :content (request &key :content (get-ekb-representation . *)))');
}


sub receive_request {
  my ($self, $msg, $content) = @_;
  my $verb = lc($content->{verb});

  print Dumper ($content);
  print ("Received a request for: $verb\n");

  eval {
  if ($verb eq 'do-ekb-inference') {
    # get EKB file
    my $ekb_file = $content->{':ekb'};
    unless (defined($ekb_file)) {
      ERROR ":ekb parameter missing";
      $self->reply_to_msg($msg, "(reply :content (failure :reason \"The :ekb parameter is missing.\"))");
      return;
    }
    $ekb_file = KQML::KQMLStringAtomAsPerlString($ekb_file);
    # does caller want actual EKB as string?
    my $return_string = TripsModule::boolean_opt(':return-string',
						 $content->{':return-string'} // 0);
    # is pub EKB?
    # so far, this was an external parameter; i think i should make it part of the EKB encoding itself!!! [FIXME]
    # do inference
    my $result = $self->do_inference($ekb_file,
				     { return_string => $return_string});
    if ($result) {
      $self->reply_to_msg($msg,
			  "(reply :content (done :result \"".
			  escape_string($result) .
			  "\"))");
    } else {
      $self->reply_to_msg($msg, "(reply :content (failure :reason \"Couldn't do it.\"))");
    }
  }
  elsif ($verb eq 'get-ekb-representation')
    {
      # get ids
      my $ids = $content->{':ids'};
      # get AKRL context
      my $akrl = $content->{':context'};
      unless (defined($akrl)) {
        ERROR ":context parameter missing";
        $self->reply_to_msg($msg, "(reply :content (failure :reason \"The :context parameter is missing.\"))");
        return;
      }

      # parse the AKRL
      $akrl = parseAKRL($akrl);

      # Convert it to EKB
      my $ekb = toEKB($akrl, $ids);

      if ($ekb) {
        my $ekbString = $ekb->toString();
	$ekb->print($ekb->get_attr('timestamp') . ".ekb");
        $ekbString = escape_string($ekbString);
        $self->reply_to_msg($msg, "(reply :content (done :result \"".$ekbString."\"))");
      } else {
        $self->reply_to_msg($msg, "(reply :content (failure :reason \"Couldn't do it.\"))");
      }
    }

  } || $self->reply_to_msg($msg, "(reply :content (failure :reason \"" . escape_string($@) . "\"))");

}

# runs DRUM reasoner over an EKB file, saves the result into another EKB file
# returns the resulting EKB file on success, and undef on error.
sub do_inference {
  my ($self, $ekb_file, $opts) = @_;

  # ingest source EKB
  my $ekb = EKB->new($ekb_file)
    or return undef;
  ## FIXME: we'll probably want to always normalize! but maybe we should've
  ## done this already for the original EKB?!?
  $ekb->normalize();
  # run DRUM reasoner
  ## FIXME: what about options?!?
  my $reasoner = EKB::Reasoner::Drum->new($ekb);
  $reasoner->run();
  # make and set result EKB file path
  my $dir = dirname($ekb_file);
  my $bname = basename($ekb_file, ".ekb");
  my $res_file = catfile($dir, $bname . "_e.ekb");  
  $ekb->set_file($res_file);
  # save
  $ekb->save();
  # return
  if (ref($opts) eq 'HASH' and $opts->{return_string}) {
    return $ekb->toString();
  }
  return $res_file;
}

sub escape_string {
  my $str = shift;
  $str =~ s/\\/\\\\/g;
  $str =~ s/"/\\"/g;
  return $str;
}

1;
