#!/usr/bin/perl
#
# extract)msgs.pl: Extract messages from TRIPS Facilitator logs
#
# George Ferguson, ferguson@cs.rochester.edu,  4 Sep 2003
# $Id: extract-msgs.pl,v 1.1.1.1 2005/01/14 19:48:08 ferguson Exp $
#
# Assumes simple pseudo-XML log file format.
#
# Note that --from pattern matches messages received by the Facilitator,
# while --to matches messages sent from the Facilitator. Because of the
# way the Facilitator handles broadcasts, it is not possible without parsing
# the messages themselves to find messages from X to Y. Thus it is unlikely
# that you will want to useboth --from and --to, although I don't forbid
# it in the code.
#

use Getopt::Long;
my $from = '';
my $to = '';
$result = GetOptions ("from=s" => \$from,
		      "to=s"   => \$to);

#print "from=\"$from\", to=\"$to\", argv=" . join(',',@ARGV) . "\n";

my $output = 0;
while (<>) {
    if (/^<R.*S="([^"]+)"/) {
	# Received message in log
	$output = ($from && $1 =~ /$from/io);
    } elsif (/^<S.*R="([^"]+)"/) {
	# Sent message in log
	$output = ($to && $1 =~ /$to/io);
    } elsif (/^<\//) {
	# End of received message
	$output = 0;
    } elsif ($output) {
	print;
    }
}
