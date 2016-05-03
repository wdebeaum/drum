#!/usr/bin/perl
#
# File: extract-messages.pl
# Creator: George Ferguson
# Created: Wed Jul 25 11:36:11 2007
# Time-stamp: <Wed Jul 25 15:37:57 EDT 2007 ferguson>
#
# Extract messages from a facilitator log.
#

my $path;
BEGIN {
    my $TRIPS_BASE_DEFAULT = "/usr/local";
    my $TRIPS_BASE = $ENV{TRIPS_BASE} || $TRIPS_BASE_DEFAULT;
    $path = "$TRIPS_BASE/etc";
}
use lib $path;
use KQML::KQML;

my %patterns;
my $params;

#
# -sender matches `S' attribute in log for received (`R') elements
# -receiver matches `R' attribute in log for sent (`S') elements
# :<NAME> matches NAME'd parameter of KQML message
#
my $usage = "extract-messages [-sender REGEXP] [-receiver REGEXP] [:<NAME> REGEXP] [FILES]";

# Process cmd-line
while (@ARGV) {
    if ($ARGV[0] =~ /^-?-h/) {
	print "$usage\n";
	exit 0;
    } elsif ($ARGV[0] eq '--') {
	last;
    } elsif ($ARGV[0] eq '-sender') {
	my $key = shift @ARGV;
	$patterns{$key} = shift @ARGV;
    } elsif ($ARGV[0] eq '-receiver') {
	my $key = shift @ARGV;
	$patterns{$key} = shift @ARGV;
    } elsif ($ARGV[0] =~ /^:/) {
	my $key = shift @ARGV;
	push @params, $key;
	$patterns{$key} = shift @ARGV;
    } elsif ($ARGV[0] =~ /^-/) {
	print STDERR "$usage\n";
	exit 1;
    } else {
	last;
    }
}

# stdin if no files given
if (!@ARGV) {
    unshift @ARGV, '-';
}

# Now process input
foreach my $f (@ARGV) {
    open(IN, "<$f") || die("$0: can't read $f: $!\n");
    while (!eof(IN)) {
	#XML element start
	my $start = <IN>;
	my $sender = '';
	my $receiver = '';
	my $endtag;
	if ($start =~ /^<LOG/ || $start =~ /^<EXIT/) {
	    # Gobble blank line
	    <IN>;
	    next;
	} elsif ($start =~ /^<R T="..:..:..\..." S="([^"]+)">/) {
	    $sender = $1;
	    $endtag = '</R>';
	} elsif ($start =~ /^<S T="..:..:..\..." R="([^"]+)">/) {
	    $receiver = $1;
	    $endtag = '</S>';
	} elsif ($start =~ /^<ERROR/) {
	    # Gobble rest of error message
	    while (<IN> ne "</ERROR>\n") {
	    }
            # Gobble blank line
	    <IN>;
	    next;
	} else {
	    die("$0: $f: $.: unrecognized XML start: $start");
	}
	if ((!$patterns{-sender} ||
	     ($sender && $sender =~ /$patterns{-sender}/i)) &&
	    (!$patterns{-receiver} ||
	     ($receiver && $receiver =~ /$patterns{-receiver}/i))) {
	    #print "match: sender=$sender, receiver=$receiver\n";
	    # Read performative
	    my $err;
	    my $msg = KQML::KQMLRead(\*IN, \$err);
	    if (!ref($msg)) {
		print STDERR "$0: error reading message: $msg: $err\n";
	    }
	    # Check params if any
	    my $ok = 1;
	    if (@params) {
		my $hashref = KQML::KQMLKeywordify($msg);
		foreach my $key (@params) {
		    my $str = KQML::KQMLAsString($hashref->{$key});
		    if ($str !~ /$patterns{$key}/i) {
			$ok = 0;
			last;
		    }
		}
	    }
	    if ($ok) {
		print KQML::KQMLAsString($msg) . "\n";
	    }
	    # Skip final newline
	    <IN>;
	    # Skip XML end
	    <IN>;
	} else {
	    # Skip to XML end
	    while (<IN> ne "$endtag\n") {
	    }
	}
    }
    close(IN);
}
