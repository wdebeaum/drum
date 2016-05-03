#!$PERL -T $PERL_FLAGS
#
# run-perl-cgi.pl - Perl script wrapper for launching a Perl CGI script safely
# William de Beaumont
# 2014-09-26
#
# This file will be customized for a specific application by setting
# certain variables starting with $. If this file's name is not
# "run-perl-cgi.pl", then you are looking at such a customization.
#

use lib '$TRIPS_BASE/etc/$MODULE';
use lib '$TRIPS_BASE/etc';

$ENV{TRIPS_BASE} = '$TRIPS_BASE';
for my $var (keys %ENV) {
  delete $ENV{$var} if ($ENV{$var} =~ /^\(\) {/); # guard against Shellshock
}

do '$TRIPS_BASE/etc/$MODULE/$MAIN' or die($@ or $!);

