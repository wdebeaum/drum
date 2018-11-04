# EKBAgent.pm
#
# Time-stamp: <Sat Nov  3 20:56:49 CDT 2018 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 13 Feb 2017
#

=head1 NAME

EKBAgent

=head2 SYNOPSIS

Description:

    A TRIPS Agent for handling Extraction Knowledge Bases (EKBs).

Arguments:

    -store <path>

    Path to folder where EKBs are stored.

    -testFile <testFileName>

    All assertions contained in the file, testFileName, are sent to the
    system using the :IMITATE-KEYBOARD-MANAGER request message.  This
    will result an :INTERPRET-SPEECH-ACT Tell message which contains AKRL.
    The AKRL is converted to EKB. Then the assertion is sent to the drum
    web parser to generate drum EKB. Finally EKB::Compare is called on to
    compare the two version of EKB.  The resulting summary is displayed
    on the console.

    -compareEnabled

    When this parameter is specified, the EKBAgent continues to process
    :INTERPRET-SPEECH-ACT Tell messages even after processing any test
    file assertions.  If it is not specified then only test file assertions
    are processed.

    -saveResultsFolder <folder>

    If specified then any EKB comparisons will be saved in the specified
    folder.  If not specified then they are stored in './ekb-agent-tests'.
    Separate files for Drum EKB, Bob EKB, AKRL, and DIFF are created
    using the assertion as the filename prefix followed by "-drum.ekb",
    "-bob.ekb", ".akrl", and "-DIFF.txt" respectively.

Usage:

    EKBAgent can be run in multiple ways based on the arguments passed in.
    In all cases the EKBAgent handles the messages described in the
    'Messaging' section.

    Examples:

          perl EKBAgent.pl

        This is the default way to run the agent.  No assertions will be
        compared.

          perl EKBAgent.pl -testFile AKRL/drumBobTestsAll.txt

        This will compare all assertions in the specified file and display the
        results O the console. Results are stored in './ekb-agent-tests'.

          perl EKBAgent.pl -testFile AKRL/drumBobTestsAll.txt -saveResultsFolder ARKL/results

        This will compare all assertions in the specified file and display the
        results on the console and save the EKB, AKRL, and results in the
        specified foler.

          perl EKBAgent.pl -compareEnabled -saveResultsFolder ARKL/results

        This will compare all assertions entered into the BOB chat dialog and
        display the results on the console and save the EKB, AKRL, and results
        in the specified foler.

          perl EKBAgent.pl -compareEnabled -testFile AKRL/drumBobTestsAll.txt -saveResultsFolder ARKL/results

        This will compare all assertions in the specified file and entered
        into the BOB chat dialog. The results will also be displayed on the
        console and the EKB, AKRL, and results will be saved in the specified
        foler.

Messaging:

    1. EKB inference

        on receiving:

         (REQUEST
          :content (do-ekb-inference :ekb "EKB_PATH" [:return-string B])
          :reply-with R1)

        runs inference on the EKB read from file with path EKB_PATH, saves
        result to a new file, and, if all is well, sends:

         (REPLY :in-reply-to R1
           :content (done :result "EKB"))

        where:
         - EKB is:
           - the KQML-stringified EKB when B is TRUE
           - the name of the file containing the inferred EKB, when B is FALSE
             (or :return-string is ommitted)

    2. AKRL-to-EKB

      on receiving:

        (REQUEST
          :content (get-ekb-representation [:ids (id1 id2 ...)] :context "AKRL_TEXT")
          :reply-with R1)

      runs the AKRL toEKB method to convert the specified ids in the AKRL_TEXT
      into EKB format.  If no ids are specified, then all valid AKRL terms are
      processed and returned in EKB format.  The resulting EKB is then sent as:

        (REPLY :in-reply-to R1
          :content (done :result "EKB_TEXT"))

      where:
        - EKB_TEXT is KQML-stringified EKB.

    3. Handling EKB persistent storage

       The agent can maintain persistent storage. The -store option must be 
       given to enable this behavior.

       Operations allowed (via REQUEST performatives):

       - get-ekb-store-contents: returns the contents of the EKB store
       - store-ekb: adds EKB to store
       - remove-ekb: removes EKB from store
       - remove-doc: removes extractions for the given document from all 
         stored EKBs (typically just one).
       - get-ekb: returns EKB for a given document from store (if it exists)
       - query-ekb: returns an EKB with parts of the EKB store (from possibly 
         multiple documents) containing extractions that match a query
       - show-ekb: displays an EKB, assuming an appropriate display is 
         available

       [TODO: flesh out messaging]

Functions (incomplete):

=cut

package EKBAgent;
use TripsModule::TripsModule;
@ISA = qw(TripsModule);

use strict vars;

#use re 'debug';

use Cwd 'realpath';
use File::Basename;
use File::Spec::Functions;
use File::Slurp qw(read_file);
use File::Path qw(make_path);
use Data::Dumper;

use KQML::KQML;

use EKB;
use EKB::Compare;
use EKB::Reasoner::Drum;
use EKB::Reasoner::CWMS;
use EKB::Store;

use AKRL::AKRLList;
use AKRL2EKB;

use util::Log;
#local $util::Log::DebugLevel = 2;
#local $util::Log::CallerInfo = 0;
#local $util::Log::Quiet = 0;

sub init {
  my $self = shift;
  $self->{name} = 'EKB-AGENT';
  $self->{usage} .= " [-store <path to EKB store>] [-testFile <file containing assertions to test>] [-compareEnabled] [-saveResultsFolder <path to output folder>]";
  
  $self->{store} = undef;  # EKB Store

  $self->{compareEnabled} = 0;
  $self->{saveResultsFolder} = 'ekb-agent-tests'; # default path for tests
  $self->{testing} = 0;

  $self->SUPER::init();
  $self->handle_parameters();
  $self->send_subscriptions();

  INFO ("Initialized $self->{name}\n");

  if ($self->{testing}) {
    $self->sendNextTest();
  }
}

sub handle_parameters {
  my $self = shift;
  my @argv = @{$self->{argv}};
  
  eval {
    while (@argv) {
      my $opt = shift @argv;
      if ($opt eq '-store') {
	die "-store option requires an argument"
	  unless (@argv > 0);
	my $store_path = shift @argv;
	unless (-d $store_path) {
	  FATAL ("Folder doesn't exist: $store_path");
	} 
	$self->{store} = EKB::Store->new($store_path);
        INFO ("EKB store: $store_path");
      }
      elsif ($opt eq '-debugLevel')
      {
        die "-debugLevel option requires an argument"
            unless (@argv > 0);
        my $debugLevel = shift(@argv);
	$util::Log::DebugLevel = $debugLevel;
	$util::Log::CallerInfo = 1 if ($debugLevel > 1);
        INFO ("debug level set to $debugLevel.");
      }
      elsif ($opt eq '-testFile') {
        die "-testFile option requires an argument"
            unless (@argv > 0);
        my $testFile = shift(@argv);
        my @text = read_file($testFile);
        $self->{tests} = \@text;
        $self->{testing} = 1;
        DEBUG (2, "Testing Assertions from : $testFile");
      }
      elsif ($opt eq '-compareEnabled')
      {
        $self->{compareEnabled} = 1;
        INFO ("EKB Compare is ENABLED.");
      }
      elsif ($opt eq '-saveResultsFolder') {
        die "-saveResultsFolder option requires an argument"
            unless (@argv > 0);
        $self->{saveResultsFolder} = shift @argv;
        INFO ("EKB Compare results will be saved in: $self->{saveResultsFolder}");
      }
    }
    1
  } || die "$@\n$self->{usage}\n";
}

sub send_subscriptions {
  my $self = shift;
  # DRUM: run reasoner
  $self->send_msg('(subscribe :content (request &key :content (do-ekb-inference . *)))');
  # AKRL 2 EKB helper
  $self->send_msg('(subscribe :content (request &key :content (get-ekb-representation . *)))');
  # save EKBs to store
  $self->send_msg('(subscribe :content (request &key :content (store-ekb . *)))');
  # remove EKB from store
  $self->send_msg('(subscribe :content (request &key :content (remove-ekb . *)))');
  # remove doc from store
  $self->send_msg('(subscribe :content (request &key :content (remove-doc . *)))');
  # show docs in EKB store
  $self->send_msg('(subscribe :content (request &key :content (get-ekb-store-contents . *)))');
  # get EKB for file (from EKB store)
  $self->send_msg('(subscribe :content (request &key :content (get-ekb . *)))');
  # query EKBs from store
  $self->send_msg('(subscribe :content (request &key :content (query-ekb . *)))');
  # show EKB
  $self->send_msg('(subscribe :content (request &key :content (show-ekb . *)))');
 
  if ($self->{compareEnabled} or $self->{testing}) {
    $self->send_msg('(subscribe :content (request &key :content (interpret-speech-act . *)))');
    $self->send_msg('(subscribe :content (tell &key :content (utterance . *)))');
    $self->send_msg('(subscribe :content (request &key :content (generate &key :content (ont::clarify-goal . *))))');
  }
}

sub receive_tell {
  my ($self, $msg, $content) = @_;
  my $sender = lc($msg->{':sender'});
  my $verb = lc($content->{verb});

  DEBUG (2, "Received a TELL: $verb from '$sender'.");

  if ($verb eq 'start-conversation') {
    # ignore
  }
  elsif ($verb eq 'utterance') {
    my $text = $content->{':text'};
    
    DEBUG (2, "Got utterance: $text");
    
    if ($sender eq 'texttagger' or $sender eq lc($self->{name}))
      {
	$text = KQML::KQMLStringAtomAsPerlString($text);
	if (defined($text) and $text ne 'no')
	  {
	    $self->{textToParse} = $text;
	  }
      }
  }
  else {
    $self->reply_to_msg($msg, "(error :comment \"Cannot handle $verb\")");
  }
}

sub receive_request {
  my ($self, $msg, $content) = @_;
  my $sender = lc($msg->{':sender'});
  my $verb = lc($content->{verb});

  DEBUG (3, Dumper ($content));
  DEBUG (2, "Received a request for: '$verb' from '$sender'\n");

  eval {
    if ($verb eq 'do-ekb-inference') {
      $self->handle_request_ekb_inference($msg, $content);
      return 1;
    } elsif ($verb eq 'get-ekb-representation') {
      $self->handle_request_get_ekb_representation($msg, $content);
      return 1;
    } elsif ($verb eq 'store-ekb') {
      $self->handle_request_store_ekb($msg, $content);
      return 1;
    } elsif ($verb eq 'remove-doc') {
      $self->handle_request_remove_doc($msg, $content);
      return 1;
    } elsif ($verb eq 'remove-ekb') {
      $self->handle_request_remove_ekb($msg, $content);
      return 1;
    } elsif ($verb eq 'get-ekb-store-contents') {
      $self->handle_request_get_store_contents($msg, $content);
      return 1;
    } elsif ($verb eq 'get-ekb') {
      $self->handle_request_get_ekb($msg, $content);
      return 1;
    } elsif ($verb eq 'query-ekb') {
      $self->handle_request_query_ekb($msg, $content);
      return 1;
    } elsif ($verb eq 'show-ekb') { 
      $self->handle_request_show_ekb($msg, $content);
      return 1;
    } elsif ($verb eq 'generate') {
      if ($self->{testing}) {
        sleep 1;
        if ($self->{testing}) {
          $self->sendNextTest();
        }
      }
      return 1;
    } elsif ($verb eq 'interpret-speech-act') {
      $self->handle_request_interpret_speech_act($msg, $content);
    } else {
      DEBUG (2, " - Failed to handle message: ".escape_string($@));
      $self->reply_to_msg($msg, "(reply :content (failure :reason \"".escape_string($@)."\"))");
    }
  } || $self->reply_to_msg_debug($msg, "(reply :content (failure :reason \"" . escape_string($@) . "\"))");

}

sub reply_to_msg_debug {
  my ($self, $msg, $reply) = @_;

  DEBUG (2, " - Eval Failed : '$reply'");
  $self->reply_to_msg($msg, $reply);
}

sub receive_reply {
  my ($self, $msg, $content) = @_;
  my $sender = lc ($msg->{':sender'});

  DEBUG (2, "Received a REPLY: $content->{':result'} [from] $sender.");
}

sub receive_sorry {
  my ($self, $msg, $content) = @_;
  my $sender = lc ($msg->{':sender'});

  DEBUG (2, "Received a SORRY: $content->{':result'} [from] $sender.");
}

# handler for 'do-ekb-inference' requests
sub handle_request_ekb_inference {
  my ($self, $msg, $content) = @_;

  # get reasoner
  my $domain = $content->{':domain'};
  unless (defined($domain)) {
    ERROR ":domain parameter missing";
    $self->reply_to_msg($msg, "(reply :content (failure :reason \"The :domain parameter is missing.\"))");
    return;
  }
  $domain = KQML::KQMLStringAtomAsPerlString($domain);
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
  my $result = $self->do_inference($domain,
				   $ekb_file,
				   {
				    return_string => $return_string});
  if ($result) {
    $self->reply_to_msg($msg,
			"(reply :content (done :result \"".
			escape_string($result) .
			"\"))");
  } else {
    $self->reply_to_msg($msg, "(reply :content (failure :reason \"Couldn't do it.\"))");
  }
}

# handler for 'get-ekb-representation' requests
sub handle_request_get_ekb_representation {
  my ($self, $msg, $content) = @_;

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

# handler for 'store-ekb' requests
sub handle_request_store_ekb {
  my ($self, $msg, $content) = @_;

  my $ekb_file = $content->{':ekb'}
    or do {
      ERROR ":ekb parameter missing";
      $self->reply_to_msg($msg, "(reply :content (failure :reason \"The :ekb parameter is missing.\"))");
      return;
    };
  $ekb_file = KQML::KQMLStringAtomAsPerlString($ekb_file);
  my $ekb = EKB->new($ekb_file)
    or do {
      ERROR "Error reading EKB file: $ekb_file";
      $self->reply_to_msg($msg, "(reply :content (failure :reason \"Cannot read or interpret EKB file.\"))");
      return;
    };

  $self->{store}->add_ekb($ekb);

  # FIXME: failures
  
  my $reply = "(reply :content (done))";
  $self->reply_to_msg($msg, $reply);
}

# handler for 'get-ekb-store-contents' requests
sub handle_request_get_store_contents {
  my ($self, $msg, $content) = @_;

  my %data_index = $self->{store}->getIndex;
  DEBUG (1, "store index: %s", Dumper(\%data_index));
  my $result =
    join(" ",
	 map { sprintf("(document :id \"%s\" :ekb \"%s\")",
		       $_, $data_index{$_}) }
	 keys %data_index);

  if ($result) {
    $result = "(" . $result . ")";
  } else {
    $result = "NIL";
  }
  my $reply = "(reply :content (ekb-store :content " . $result . "))";
  $self->reply_to_msg($msg, $reply);
}

# handler for 'remove-ekb' requests
sub handle_request_remove_ekb {
  my ($self, $msg, $content) = @_;

  my $ekb_file = $content->{':ekb'}
    or do {
      ERROR ":ekb parameter missing";
      $self->reply_to_msg($msg, "(reply :content (failure :reason \"The :ekb parameter is missing.\"))");
      return;
    };
  $ekb_file = KQML::KQMLStringAtomAsPerlString($ekb_file);

  $self->{store}->remove_ekb($ekb_file)
    or do {
      $self->reply_to_msg($msg, "(reply :content (failure :reason \"EKB not found in store\"))");
      return;
    };

  my $reply = "(reply :content (done))";
  $self->reply_to_msg($msg, $reply);
}

# handler for 'get-ekb' requests
sub handle_request_get_ekb {
  my ($self, $msg, $content) = @_;

  my $doc = $content->{':document'}
    or do {
      ERROR ":document parameter missing";
      $self->reply_to_msg($msg, "(reply :content (failure :reason \"The :document parameter is missing.\"))");
      return;
    };
  $doc = KQML::KQMLStringAtomAsPerlString($doc);

  my $ekb = $self->{store}->get_doc($doc)
    or do {
      $self->reply_to_msg($msg, "(reply :content (failure :reason \"No EKB found\"))");
      return;
    };

  my $ekbString = escape_string($ekb->toString);
  my $reply = "(reply :content (done :result \"" . $ekbString . "\"))";
  $self->reply_to_msg($msg, $reply);
}

# handler for 'remove-doc' requests
sub handle_request_remove_doc {
  my ($self, $msg, $content) = @_;

  my $doc = $content->{':document'}
    or do {
      ERROR ":document parameter missing";
      $self->reply_to_msg($msg, "(reply :content (failure :reason \"The :document parameter is missing.\"))");
      return;
    };
  $doc = KQML::KQMLStringAtomAsPerlString($doc);

  $self->{store}->remove_doc($doc)
    or do {
      $self->reply_to_msg($msg, "(reply :content (failure :reason \"Document not found\"))");
      return;
    };
  my $reply = "(reply :content (done))";
  $self->reply_to_msg($msg, $reply);
}

# handler for 'query-ekb' requests
sub handle_request_query_ekb {
  my ($self, $msg, $content) = @_;

  my $query = $content->{':query'}
    or do {
      ERROR ":query parameter missing";
      $self->reply_to_msg($msg, "(reply :content (failure :reason \"The :query parameter is missing.\"))");
      return;
    };
  $query = KQML::KQMLKeywordify($query);
  my $ekb_query = $self->parse_query($query)
    or do {
      ERROR ":query unparseable";
      $self->reply_to_msg($msg, "(reply :content (failure :reason \"Could not parse :query value.\"))");
      return;
    };

  my $reply;
  my $result_ekbs = $self->{store}->query($ekb_query);
  if (defined($result_ekbs) && scalar(@$result_ekbs)) {
    # for now we just pick the first ekb
    # TODO: combine results into single EKB? return multiple?
    my $result_ekb = $result_ekbs->[0];
    my $ekbString = escape_string($result_ekb->toString);
    $reply = "(reply :content (done :result \"" . $ekbString . "\"))";
  } else {
    $reply = "(reply :content (done :result NIL))";
  }
  $self->reply_to_msg($msg, $reply);
}

# parse KQML query into an EKB query (a structure pattern for now)
sub parse_query {
  my ($self, $query) = @_;
  DEBUG (1, "Query: %s", Dumper($query));
  my $structure;
  if (my $v = $query->{':type'}) {
    $structure->{'type'} = KQML::KQMLStringAtomAsPerlString($v);
  }
  DEBUG (1, "Parsed query: %s", Dumper($structure));
  return $structure;
}


# handler for 'show-ekb' requests
# TODO: is this relevant? do I have to modify, eg to display EKB in focus?
sub handle_request_show_ekb {
  my ($self, $msg, $content) = @_;

  my $ekb_file = $content->{':ekb'}
    or do {
      ERROR ":ekb parameter missing";
      $self->reply_to_msg($msg, "(reply :content (failure :reason \"The :ekb parameter is missing.\"))");
      return;
    };
  $ekb_file = KQML::KQMLStringAtomAsPerlString($ekb_file);
  my $ekb = EKB->new($ekb_file)
    or do {
      ERROR "Error reading EKB file: $ekb_file";
      $self->reply_to_msg($msg, "(reply :content (failure :reason \"Cannot read or interpret EKB file.\"))");
      return;
    };
  $ekb->normalize();
  my $ekbString = escape_string($ekb->toString);
  my $display_action = "(request :receiver GraphDisplay :content (display-ekb :ekb \"". $ekbString ."\"))";
  $self->send_msg($display_action);
  $self->reply_to_msg($msg, "(reply :content (done))");
}

# handler for 'interpret-speech-act' requests
sub handle_request_interpret_speech_act {
  my ($self, $msg, $content) = @_;

  if (($self->{testing} or $self->{compareEnabled})) {
    INFO("TESTING string : '".$self->{textToParse}."'");
    $self->{drumEKBText} = undef;
    $self->{agentEKBText} = undef;
    $self->{akrlText} = undef;

    my $assertion = $content->{':content'};
    $assertion = KQML::KQMLKeywordify($assertion);
    $assertion = $assertion->{':context'};
    $self->{akrlText} = KQML::KQMLAsString($assertion);

    if (defined ($self->{akrlText})) {
      # parse the AKRL
      my $akrl = parseAKRL($self->{akrlText});

      # Convert it to EKB
      my $ekb = toEKB($akrl);

      if (defined($ekb)) {
	$self->{agentEKBText} = $ekb->toString();
      }
    }

    $self->{drumEKBText} = getDRUM_EKB($self->{textToParse});

    # Compare the xml from both.
    $self->compareEKB();
  }
  return 1;
}


# runs the specified domain reasoner over an EKB file, saves the result into
# another EKB file
# returns the resulting EKB file on success, and undef on error.
sub do_inference {
  my ($self, $domain, $ekb_file, $opts) = @_;

  # ingest source EKB
  my $ekb = EKB->new($ekb_file)
    or return undef;
  ## FIXME: we'll probably want to always normalize! but maybe we should've
  ## done this already for the original EKB?!?
  $ekb->normalize();
  # open the proper reasoner
  ## FIXME: what about options?!?
  my $reasoner =
    ($domain eq "DRUM") ? EKB::Reasoner::Drum->new($ekb) :
    ($domain eq "CWMS") ? EKB::Reasoner::CWMS->new($ekb) :
    EKB::Reasoner->new($ekb);
  # run reasoner
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

=head2 sendNextTest()

If a test file was specified then read the next assertion and send
it as if it was entered from the BOB chat dialog.  If there are no more
assertions, print out Finished.  If a line is blank or starts with a '#'
it is ignored.

=cut

sub sendNextTest
{
  my ($self) = @_;

  if ($self->{testing} == 0)
  {
    return 0;
  }

  my $tests = $self->{tests};
  my $test = undef;
  while (defined($tests) and scalar(@$tests) > 0 and !defined($test))
  {
    $test = shift @$tests;
    $test =~ s/^\s+|\s+$//g;
    if ($test eq '' or $test =~ /^#/ or $test =~ /^\/\//)  # ignore commented out lines
    {
      $test = undef;
    }
  }

  if (!defined($test))
  {
    $self->send_msg('(BROADCAST :CONTENT (TELL :CONTENT (START-CONVERSATION)))');
    INFO "*********************************";
    INFO "*** Finished all Test Strings ***";
    INFO "*********************************";
    $self->{testing} = 0;

    if (!$self->{compareEnabled}) {
      # TODO:  Figure out how to unsubscribe to messages since we no longer need to handle these.
#      $self->send_msg('(unsubscribe :content (request &key :content (interpret-speech-act &key :content (assertion . *))))');
#      $self->send_msg('(unsubscribe :content (tell &key :content (utterance . *)))');
#      $self->send_msg('(unsubscribe :content (request &key :content (generate &key :content (ont::clarify-goal . *))))');
    }

    return 0;
  }

  $self->send_msg('(BROADCAST :CONTENT (TELL :CONTENT (START-CONVERSATION)))');
  $self->send_msg('(request :content (tag :text "'.$test.'" :IMITATE-KEYBOARD-MANAGER t))');

  #    $self->{uttnum} = $self->{uttnum} + 1;
  #    $self->send_msg('(tell :content (utterance :channel Desktop :direction input :mode text :uttnum '.$self->{uttnum}.' :text "'.$test.'"))');

  return 1;
}

=head2 compareEKB( )

After receiving the Drum and Bob versions of the EKB, the text is converted to
EKB objects then run through the EKB::Compare class to determine the differences

=cut

sub compareEKB
{
  my ($self) = @_;

  my $drumEKB = EKB->new($self->{drumEKBText});
  my $agentEKB = EKB->new($self->{agentEKBText});

  $self->{textToParse} =~ s/\.+$//; # remove trailing .'s

  if (defined($self->{saveResultsFolder})) {
    writeToFile($self->{saveResultsFolder}, $self->{textToParse}, "akrl", $self->{akrlText});
    writeToFile($self->{saveResultsFolder}, $self->{textToParse}."-drum", "ekb", $drumEKB->toString);
    writeToFile($self->{saveResultsFolder}, $self->{textToParse}."-bob", "ekb", $agentEKB->toString);
  }

  my $compare = EKB::Compare->new ($drumEKB, $agentEKB,
                                   "normalize",    1,
                                   "ignore_text",  1);
  $compare->compare();
  my $summary = $compare->summary();
  if ($summary->{del} == 0 and $summary->{ins} == 0)
  {
    INFO ("  - $summary->{eql} Drum and Bob EKB elements's are equal for: '$self->{textToParse}'");
  }
  else
  {
    INFO ("  - Drum and Bob EKB's are NOT EQUAL for '$self->{textToParse}'");

    #        INFO "====================== AKRL ==========================";
    #        INFO $self->{akrlText};
    #
    #        INFO "====================== DRUM ==========================";
    #        INFO $self->{drumEKBText};
    #
    #        INFO "===================== EKBAgent =======================";
    #        INFO $self->{agentEKBText};

    if (defined($self->{saveResultsFolder})) {
      writeToFile($self->{saveResultsFolder}, $self->{textToParse}."-DIFF", "txt", $compare->diffs_as_string());
    }

    INFO "====================== DIFFS =========================";
    INFO $compare->diffs_as_string();
    INFO "======================================================";
  }
}


=head2 getDRUM_EKB (assertion)

The assertion parameter is sent to the trips on-line drum parser and the results
are parsed for the embeded EKB.  If all goes well the EKB is returned as a text
string.  If there are any issues then an error code is returned.

=cut

sub getDRUM_EKB
{
  my($assertion) = @_;
  my $outfile = "drumEKB.txt";
  my $headfile = "drumEKBhead.txt";
  my $modifiedInput = $assertion;
  $modifiedInput =~ s/ /+/g;
  my $pmcurl = "http://trips.ihmc.us/parser/cgi/drum-dev?input=$modifiedInput";
  my $rc = 0;
  my @httpCode;

  # Use curl to get the drum ekb. (-s) disable curl output, (-o) save the results in $outfile and
  # (-D) the http response header in $headfile, and (-L) follow redirects.
  $rc = system ('curl', '-s', '-o', $outfile, '-D', $headfile, '-L', $pmcurl);
  $rc = $rc >> 8;

  if ($rc != 0)
  {
    unlink $headfile;
    unlink $outfile;
  }
  else
  {
    open (HEADER, "<$headfile");
    my @lines;
    while (<HEADER>)
    {
      push(@lines, $_);
    }
    close (HEADER);
    unlink $headfile;

    # Parse the http header for the response code and then determine if there was an http issue.
    @httpCode = split(' ', $lines[0]);

    $rc = int($httpCode[1]); ## convert

    if ($rc < 200 or $rc >= 300)
    {
      unlink $outfile;
    }
    else
    {
      $rc = readEKBFromFile($outfile);
      unlink $outfile;
    }
  }

  return $rc;
}

=head2 readEKBFromFile (filename)

Reads the EKB from a file named filename, which contains the result of the
curl call to the trips drum-dev parser.

=cut

sub readEKBFromFile
{
  my ($filename) = @_;

  unless (-f $filename) {
    return undef;
  }

  # read the data file and strip out the ekb
  open (EKBFile, "<$filename");
  my @lines;
  my $line;
  my $ekbData;
  my $foundEKBStart = 0;
  while(<EKBFile>)
  {
    $line = $_;
    #print ("Checking: $line\n");
    if ($foundEKBStart)
    {
      if (index($line, "</ekb") != -1)
      {
        @lines = split('</ekb', $line);
        $ekbData = $ekbData . $lines[0] . '</ekb>';
        last;
      }
      else
      {
        $ekbData = $ekbData . $line;
      }
    }
    else
    {
      # looking for start of EKB
      if (index($line, "<ekb") != -1)
      {
        @lines = split ("<ekb", $line);
        $foundEKBStart = 1;
        $line = $lines[1];
        if (index($line, "</ekb") != -1)
        {
          @lines = split('</ekb', $line);
          $ekbData = "<ekb" . $lines[0] . '</ekb>';
          last;
        }
        else
        {
          $ekbData = "<ekb" . $line;
        }
      }
    }
  }
  close (EKBFile);

  return $ekbData;
}

=head2 writeToFile (parentFolder, filename, extension, string)

Writes the string to a file named filename in the parentFolder.  If the
parentFolder does not exists, it is created.

=cut

sub writeToFile
{
  my ($parentFolder, $filename, $extension, $string) = @_;

  if (!defined ($string) or !defined ($filename))
  {
    return undef;
  }

  if (!defined($parentFolder))
  {
    $parentFolder = '';
  }
  elsif (!-d $parentFolder)
  {
    # if parent folder does not exist then create it.
    make_path $parentFolder;
  }

  if (!defined($extension))
  {
    $extension = "txt";
  }

  $filename =~ s/\.+$//;          # remove trailing .'s
  $filename =~ s/\//-P-SLASH-/g;  # replace slashes so they do not make new folders
  $filename =~ s/,/-P-COMMA-/g;   # replace commas.
  $filename =~ s/ /_/g;           # replace spaces.

  open (my $fh, '>', $parentFolder.'/'.$filename.'.'.$extension);
  print $fh $string;
  close $fh;
}

1;

=head1 BUGS

Probably.

=head1 AUTHORS

Roger Carff E<lt>rcarff@ihmc.usE<gt> (Initial implementation)

Lucian Galescu E<lt>lgalescu@ihmc.usE<gt> (Subsequent modifications)

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2018 by Lucian Galescu E<lt>lgalescu@ihmc.usE<gt>

This module is free software. You may redistribute it and/or modify it under 
the terms of the Artistic License 2.0.

This program is distributed in the hope that it will be useful, but without 
any warranty; without even the implied warranty of merchantability or fitness 
for a particular purpose.

=cut
