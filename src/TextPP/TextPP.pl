#!/usr/bin/perl

#
# TextPP.pl
#
# Time-stamp: <Wed Jul  7 15:56:31 CDT 2010 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 11 Jul 2007
#

#----------------------------------------------------------------
# Description:
# A text preprocessing TRIPS module. Uses Shlomo Yona's sentencizer and 
# pm variants of MIT WSJ/BN preprocessing tools.

#----------------------------------------------------------------
# History:
# 2007/07/11 lgalescu  - Started. Based on previous java-wrapped code.
# 2007/??/?? nblaylock - Added address parsing.
# 2007/12/13 lgalescu  - Added sentencizing only (no PP).
# 2010/07/07 lgalescu  - Changed -connect option to conform to standard TRIPS
#                        usage; removed non-standard option -host.
 
#----------------------------------------------------------------
# Usage:
# 

my $usage;

BEGIN {
    # Just use basename of program, rather than whole path.
    $0 =~ s:.*/::;
    $usage = "Usage: $0 [-debug BOOL] [-log BOOL] [-connect BOOL|HOST:PORT]\n";

    use warnings;
}

# external modules
use Data::Dumper; $Data::Dumper::Indent = 1;
use IO::Socket;

# TRIPS modules
use lib "$ENV{TRIPS_BASE}/etc/perl/";

use KQML::KQML;

use AddressParser::AddressParser;

# Shlomo Yona's sentencizer
use Lingua::EN::Sentence qw(get_sentences);



# HTML character codes and ISO8859-1 codes to ASCII
use TextPP::toASCII qw(toASCII);

# perlmod variants of MIT WSJ/BN preprocessing tools
use TextPP::NumHack qw(NumHack);
use TextPP::NumProc qw(NumProc);
use TextPP::AbbrProc qw(AbbrProc);
use TextPP::PuncProc qw(PuncProc);

#----------------------------------------------------------------
# Command-line arguments
#----------------------------------------------------------------

# options
my %options = (
    debug => 0,
    log => 0,
    connect => 1,
    host => $ENV{'TRIPS_HOST'} || 'localhost',
    port => $ENV{'TRIPS_PORT'} || 6200,
    );

while ($arg = shift) {
    if ($arg eq '-debug') {
	defined( $arg = shift ) or die $usage;
	$options{debug} = scalar2boolean($arg);
    } elsif ($arg eq '-log') {
	defined( $arg = shift ) or die $usage;
	$options{log} = scalar2boolean($arg);
    } elsif ($arg eq '-connect') {
	defined( $arg = shift ) or die $usage;
	if ($arg =~ /^([^:]+):(\d+)$/) {
	    $options{connect} = 1;
	    $options{host} = $1;
	    $options{port} = $2;
	} else {
	    $options{connect} = scalar2boolean($arg);
	}
    } 
}

&log("TextPP configuration:\n" . Data::Dumper->Dump([\%options], ["*options"]));

#----------------------------------------------------------------
# Globals
#----------------------------------------------------------------
my $in = \*STDIN;
my $out = \*STDOUT;

#----------------------------------------------------------------
# Main
#----------------------------------------------------------------
# connect to facilitator
if ($options{connect}) {
    my $sock = IO::Socket::INET->new( Proto => "tcp",
                                      PeerAddr => $options{host},
                                      PeerPort => $options{port},
        );
    die "$0: can't connect to facilitator: $!\n" unless ($sock);
    $sock->autoflush(1);
    $in = $out = $sock;
}

&send('(register :name TextPP)');
&send('(subscribe :content (request &key :content (read-text . *)))');
&send('(subscribe :content (request &key :content (sentencize . *)))');
&send('(subscribe :content (request &key :content (parse-address . *)))');
&send('(tell :content (module-status ready))');

while(1) {
    my $perf = KQML::KQMLRead($in);
    last if (!$perf);
    if (ref($perf)) {
	my %msg = %{KQML::KQMLKeywordify($perf)};
	&log("Received:", KQML::KQMLAsString($perf));
	&debug("Received:", Data::Dumper->Dump([\%msg], ["*msg"]));
	my $verb = lc($msg{'verb'});
	if ($verb eq 'request') {
	    &receiveRequest($perf);
	} elsif ($verb eq 'tell') {
	    &receiveTell($perf);
	} elsif ($verb eq 'error') {
	    # ignore
	} else {
	    &errorReply($perf, "don\'t know how to handle $verb");
	}
    }
}

#----------------------------------------------------------------
# Subroutines
#----------------------------------------------------------------

## process request to preprocess (aka normalize) text 
sub processTextSeen {
    my $perf = shift;

    my %msg = %{KQML::KQMLKeywordify($perf)};
    my %content = %{KQML::KQMLKeywordify($msg{':content'})};
    my $inText = $content{':text'};
    my $inFile = $content{':file'};

    if (defined($inText) && defined($inFile)) {
	&errorReply($perf, "can\'t use both :text and :file");
	return;
    } elsif (defined($inFile)) {
        unless ( KQML::KQMLAtomIsString($inFile) ) {
	    &errorReply($perf, ":file argument must be a string");
	    return;
	}
	$inFile = KQML::KQMLStringAtomAsPerlString($inFile);
	unless ( open(INF, "<$inFile") ) {
	    &errorReply($perf, "cannot find file: $inFile");
	    return; 
	}
	$inText = '';
	while(<INF>) {
	    $inText .= $_;
	}
	close(INF);
    } elsif (defined($inText)) {
        unless ( KQML::KQMLAtomIsString($inText) ) {
	    &errorReply($perf, ":text argument must be a string");
	    print STDERR "text argument was:\n$inText\n";
	    return;
	}
	$inText = KQML::KQMLStringAtomAsPerlString($inText);
    }

    my $outText = textPreProcess($inText);

    if (defined($inFile)) {
	unless ( open(OUTF, ">$inFile.tpp") ) {
	    &errorReply($perf, "cannot open file for writing: $inFile.tpp");
	    return; 
	}
	print OUTF $outText;
	close(OUTF);

	&replyWithFile($perf, "$inFile.tpp");
    } else {
	&replyWithText($perf, $outText);
    }
}

# this is the actual text normalization process
sub textPreProcess {
    my $text = shift;

    # 1. ascii-fy 
    # 1.1. try some known conversions to readable chars
    $text = toASCII($text);

    # 1.2. eliminate anything non-printable
    $text =~ s/[^[:print:]\s]//g;

    # 2. sentencize
    my $sentences = get_sentences($text);

    # 3. numhack
    $text = '';
    foreach my $sentence (@$sentences) {
	$text .= NumHack($sentence) . "\n";
    }

    # 4. numproc: has upfront overhead, so should pass all the text at once!
    # TextPP::NumProc::setverbose(1);
    $text = NumProc($text);

    # 5. abbrproc: has upfront overhead, so should pass all the text at once!
    # TextPP::AbbrProc::setverbose(1);
    $text = AbbrProc($text);

    # 6. puncproc
    TextPP::PuncProc::setnp(1);
    $text = PuncProc($text);

    return $text;
}

## process request for parse-address
## (request :content (parse-address :content "40 S. Alcaniz Pensacola, FL 32502"))
sub processParseAddress {
    my $perf = shift;
    my %msg = %{KQML::KQMLKeywordify($perf)};
    my %content = %{KQML::KQMLKeywordify($msg{':content'})};

    my $text = $content{':content'};

    unless ( KQML::KQMLAtomIsString($text) ) {
	&errorReply($perf, ":content must be a string");
	return;
    }
    $text = KQML::KQMLStringAtomAsPerlString($text);

    my $akrl = AddressParser::parseAddress($text);

    &reply($perf, $akrl);

    return $akrl;
} # end sub processParseAddress


## process request to sentencize text
sub processSentencize {
    my $perf = shift;
    my %msg = %{KQML::KQMLKeywordify($perf)};
    my %content = %{KQML::KQMLKeywordify($msg{':content'})};

    my $inText = $content{':text'};
    my $inFile = $content{':file'};

    if (defined($inText) && defined($inFile)) {
	&errorReply($perf, "can\'t use both :text and :file");
	return;
    } elsif (defined($inFile)) {
        unless ( KQML::KQMLAtomIsString($inFile) ) {
	    &errorReply($perf, ":file argument must be a string");
	    return;
	}
	$inFile = KQML::KQMLStringAtomAsPerlString($inFile);
	unless ( open(INF, "<$inFile") ) {
	    &errorReply($perf, "cannot find file: $inFile");
	    return;
	}
	$inText = '';
	while(<INF>) {
	    $inText .= $_;
	}
	close(INF);
    } elsif (defined($inText)) {
        unless ( KQML::KQMLAtomIsString($inText) ) {
	    &errorReply($perf, ":text argument must be a string");
	    return;
	}
	$inText = KQML::KQMLStringAtomAsPerlString($inText);
    }
    
    my $outText = join("\n", @{get_sentences($inText)});

    &replyWithText($perf, $outText);
}

## process requests
sub receiveRequest {
    my $perf = shift;

    my %msg = %{KQML::KQMLKeywordify($perf)};
    my $sender = $msg{':sender'};
    my $replyWith = $msg{':reply-with'};
    my %content = %{KQML::KQMLKeywordify($msg{':content'})};
    my $verb = lc($content{'verb'});

    if ($verb eq 'read-text') {
	&sendBusySignal("red");
	&processTextSeen($perf);
	&sendBusySignal("green");
    } elsif ($verb eq 'parse-address') {
      &processParseAddress($perf);
    } elsif ($verb eq 'sentencize') {
      &processSentencize($perf);
    } elsif ($verb eq 'save-state') {
	# ignore
    } elsif ($verb eq 'load-state') {
	# ignore
    } elsif ($verb eq 'exit') {
	my $status = 0;
	if (length(@{$content{'rest'}}) > 0) {
	    $status = int($content{'rest'}->[0]);
	}
	exit $status;
    } else {
	&errorReply($perf, "Bad request: $verb");
    }
}

## process tells
sub receiveTell {
    my $perf = shift;

    my %msg = %{KQML::KQMLKeywordify($perf)};
    my $sender = $msg{':sender'};
    my $replyWith = $msg{':reply-with'};
    my %content = %{KQML::KQMLKeywordify($msg{':content'})};
    my $verb = lc($content{'verb'});

    if ($verb eq 'start-conversation') {
	# ignore
    } else {
	&errorReply($perf, "Bad tell: $verb");
    }
}

## KQML: send, reply
sub send {
    my $msg = shift;

    print $out $msg . "\n";
    &log("Sent:", $msg);
}

sub errorReply {
    my $replyToMsg = shift;
    my $comment = shift;

    my %msg = %{KQML::KQMLKeywordify($replyToMsg)};
    my $sender = $msg{':sender'};
    my $replyWith = $msg{':reply-with'};

    my $replyMsg = "(error :receiver $sender " 
	. (defined($replyWith) ? ":in-reply-to $replyWith " : "")
	. ":comment \"$comment\")";

    &send($replyMsg);
}

sub reply {
    my $replyToMsg = shift;
    my $content = shift;

    my %msg = %{KQML::KQMLKeywordify($replyToMsg)};
    my $sender = $msg{':sender'};
    my $replyWith = $msg{':reply-with'};

    my $replyMsg = "(reply :receiver $sender " 
	. (defined($replyWith) ? ":in-reply-to $replyWith " : "")
	. ":content ($content))";

    &send($replyMsg);
}

sub replyWithText {
    my $replyToMsg = shift;
    my $text = shift;

    my $content = "text-read :text \"".escapeQuotes($text)."\"";

    &reply($replyToMsg, $content);

}

sub replyWithFile {
    my $replyToMsg = shift;
    my $file = shift;

    my $content = "text-read :file \"$file\"";

    &reply($replyToMsg, $content);

}

sub sendBusySignal {
    my $state = shift;

    my $msg = "(request :content (trafficlight " . $state . "))";

    &send($msg);
}

## convenience subs
sub scalar2boolean {
    my $i = shift;
    if ($i =~ /^(?:t|true|yes|1)$/i) {
	return 1;
    } elsif ($i =~ /^(?:f|false|no|nil|0)$/i) {
	return 0;
    } else {
	&debug("$0: not sure about the boolean value of $i... assuming undef");
	# basically, we default to false
	return undef;
    }
}

sub eql {
    my ($a, $b) = @_;

    return (lc($a) eq lc($b));
}

sub escapeQuotes {
    my $text = shift;
    $text =~ s/([\"\'])/\\$1/gs;

    return $text;
}

## debugging
sub debug {
    return unless $options{debug};
    print STDERR "DEBUG [$0]:\n";
    foreach $arg (@_) {
	print STDERR $arg."\n";
    }
}

## logging
sub log {
    return unless $options{log};
    foreach $arg (@_) {
	print STDERR $arg."\n";
    }
    print STDERR "\n";
}
