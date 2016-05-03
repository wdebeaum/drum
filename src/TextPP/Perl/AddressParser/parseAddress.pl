# this is a command-line utility for calling parseAddress

use lib "..";

use AddressParser::AddressParser;

my $text = shift;
if (!defined($text) || ($text eq '')) {
  print "usage: perl parseAddress.pl <text address>\n";
  exit;
}

my $akrl = AddressParser::parseAddress($text);

print "$akrl\n";
