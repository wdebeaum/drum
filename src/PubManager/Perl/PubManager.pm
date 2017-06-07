#!/usr/bin/perl

package PubManager;
use TripsModule::TripsModule;
@ISA = qw(TripsModule);

use strict vars;
use Cwd qw(abs_path);
use File::Spec::Functions;
use PubManager::Utils qw(setOutputFolder setXMLOutputFolder processPMCDoc getErrorMsg getErrorCode setDebug);
use util::Log;
local $util::Log::Caller_Info = 0;

# This will Flush STDOUT if it is not commented out.
#BEGIN { $| = 1 }

sub init {
    my $self = shift;
    $self->{name} = 'PUB-MANAGER';
    $self->{usage} .=
        " [-cache <Path to where publication files will be stored.>]";
    $self->SUPER::init();
    # debugging?
    setDebug($self->{debug});
    # default cache folder
    $self->{cache} = ".";
    # parameters
    $self->handle_parameters();
    # set our cache directory structure
    $self->{xml_dir} = catdir($self->{cache}, "xml");
    $self->{ppp_dir} = catdir($self->{cache}, "ppp");
    $self->validateCache();
    # subscriptions
    $self->send_msg('(subscribe :content (request &key :content (pub-pull . *)))');
    INFO "Done initializing PUB-MANAGER.";
}

sub handle_parameters {
    my $self = shift;
    my @argv = @{$self->{argv}};

    eval {
        while (@argv) {
            my $opt = shift @argv;
            if ($opt eq '-cache') {
                die "-cache option requires an argument"
                    unless (@argv > 0);
                $self->{cache} = shift(@argv);
            }
        }
        1
    } || die "$@\n$self->{usage}\n";

    INFO 'Using cache folder: "%s"', $self->{cache};

  }

sub validateCache {
    my $self = shift;
    if (setXMLOutputFolder($self->{xml_dir}) != 0)
    {
        my $responseCode = getErrorCode();
        my $responseMsg = getErrorMsg();
        ERROR "Unable to set the xml output folder($responseCode) : $responseMsg";
        die "$@\n$self->{usage}\n";
    }
}

sub receive_request {
    my ($self, $msg, $content) = @_;
    my $verb = lc($content->{verb});
    if ($verb eq 'pub-pull') {
        # Look for a string in the pmcid field
        my $id = $content->{':pmcid'};
        unless (defined($id))
        {
            ERROR ":pmcid parameter is missing from the request.";
            $self->reply_to_msg($msg, "(reply :content (failure :msg \"The :pmcid parameter is missing from the request.\"))");
            return;
        }

        if (ref($id) eq 'ARRAY' or ref($id) eq 'HASH')
        {
            $self->reply_to_msg($msg, "(reply :content (failure :msg \"The :pmcid parameter must be a string or a token.\"))");
            return;
        }

	# we allow the id to be either a token or a string (no escapes)
	$id =~ s/^"//;
	$id =~ s/"$//;
	
        # see if a folder for this doc already exists
        my $outputDir = catdir($self->{ppp_dir}, $id);
        my $responseCode = 0;
        my $responseMsg = "Successful";
        if (-d $outputDir)
        {
            WARN "Already processed this ID($id).";
        }
        else
        {
            INFO "Processing PMC ID($id).";
            # set the output folder
            if (setOutputFolder($outputDir) != 0)
            {
                $responseCode = getErrorCode();
                $responseMsg = getErrorMsg();
                ERROR "Unable to set the output folder ($responseCode) : $responseMsg";
                $self->reply_to_msg($msg, "(reply :content (failure :msg \"$responseMsg\"))");
                return;
            }

            # process this doc id
            processPMCDoc($id);
            my $errorCode = getErrorCode();
            if ($errorCode)
            {
                $responseCode = getErrorCode();
                $responseMsg = getErrorMsg();
                ERROR "Unable to process the PMC Document $id ($responseCode) : $responseMsg";
                $self->reply_to_msg($msg, "(reply :content (failure :msg \"$responseMsg\"))");
                return;
            }
        }
        $outputDir = abs_path($outputDir);
        INFO "Successfully processed $outputDir";
        $self->reply_to_msg($msg, "(reply :content (done :output-folder \"$outputDir\"))");
    } else {
        ERROR "Unknown request verb $verb";
        $self->reply_to_msg($msg, qq/(error :comment "unknown request verb $verb")/);
    }
}

1;
