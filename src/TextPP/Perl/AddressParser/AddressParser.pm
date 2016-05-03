package AddressParser;

use strict;
use warnings;

require Exporter;

our @ISA = qw(Exporter);

our @EXPORT_OK = 
('parseAddress');

our $VERSION = '0.01';

use Geo::StreetAddress::US;
use re 'eval';
#----------------------------------------------------------------
# Global variables
#


#----------------------------------------------------------------
# Methods
#

# this parses a text address and returns an address object
#  (in AKRL) or returns "unable-to-parse-address" if unsuccessful
sub parseAddress($) {
  my $text = shift(@_);

  # from wikipedia (Trim)
  $text =~ s/^\s+|\s+$//g ; # remove both leading and trailing whitespace
  if ($text eq '') { # empty
    return 'unable-to-parse-address';
  }

  my ($address,$type);
  ($type,$address) = parse($text);

  if ($address eq '') { # unsuccessful
    return 'unable-to-parse-address';
  }


  # print out what's there
#   my ($key,$value);
#   while (($key, $value) = each(%$address)){
#     if (defined($value)) {
#       print "\t$key: $value\n";
#     }
#   }

  if ($type eq 'MailingAddress') {
    return &packageAddress($address,$text);
  } elsif ($type eq 'City') {
    return &packageCity($address);
  } else {
    die "unknown type parsed: type $type from text $text\n";
  }
} # end parseAddress

# this packages up a MailngAddress for output (also pass in the original text)
sub packageAddress($$) {
  my $address = shift(@_);
  my $original_text = shift(@_);

  my @defs = ();
  my $retval = "";

  # do the street address
  my $streetAddressRef = &stringGenSym();

  my @addressElements = ();
  my %addressElementsByName = ();
  for my $key (qw(number fraction prefix street type suffix unit)) {
    if (exists($address->{$key}) and
        defined($address->{$key}) and
	$address->{$key} ne '') {
      push @addressElements, $address->{$key};
      $addressElementsByName{$key} = $address->{$key};
    }
  }

  my $addressName = $original_text; # just keep the same thing they gave us as the name

  my $streetAddressName = join(' ',@addressElements);
  my $addressKeywordArgs = join('', map { " :$_ \"$addressElementsByName{$_}\"" } keys %addressElementsByName);
  if ($streetAddressName) {
    push @defs, "(ONT::the $streetAddressRef :instance-of ONT::|Street-Address| :|the-name| \"$streetAddressName\"$addressKeywordArgs)";
  }

  # city and state
  my ($cityRef,$cityDef,$cityName,$stateRef,$stateDef,$stateName);
  ($cityRef, $cityDef, $cityName, $stateRef, $stateDef, $stateName) = &getCityAndStateDefs($address);
  push @defs, $cityDef;
  push @defs, $stateDef;

  # the zipcode
  my $zipcodeRef = &stringGenSym();
  my $zipcodeName = $address->{'zip'};
  if ($zipcodeName) {
    push @defs, "(ONT::the $zipcodeRef :instance-of ONT::|Zip-Code| :|the-name| \"$zipcodeName\")";
  }

  # the mailng-address
  my $addressRef = &stringGenSym(); # the top-level mailing address ref
  my $addressDef = "(ONT::the $addressRef :instance-of ONT::|Mailing-Address| :|street-address| $streetAddressRef :|in-city| $cityRef :|in-state| $stateRef";
  if ($zipcodeName) {
    $addressDef .=  " :|zip-code| $zipcodeRef";
  }
  $addressDef .= " :|the-name| \"$addressName\")";

  unshift @defs, $addressDef;

  $retval = "parsed-address :content " . $addressRef
    . " :context (" . join(" ",@defs) . ")";

  return $retval;
} # end sub packageAddress

# this tries parsing as different objects and returns the first successful parse and
# its type (as a string)
# or undef if not successful
sub parse($) {
  my $text = shift(@_);

  my $address;
  # try parsing as a complete address
  $address = Geo::StreetAddress::US->parse_address($text,0);
  if ($address ne '') {
    return ("MailingAddress",$address);
  }

  my $to_parse = "1 Maple " . $text;

  $address = Geo::StreetAddress::US->parse_address($to_parse,0); # don't normalize results
  if ($address ne '') {
    return ("City",$address);
  }

  return ('','');
} # end sub parse

# this packages a city
sub packageCity($) {
  my $address = shift(@_);

  my $retval = "";
  my @defs = ();

  my ($cityRef,$cityDef,$cityName,$stateRef,$stateDef,$stateName);
  ($cityRef, $cityDef, $cityName, $stateRef, $stateDef, $stateName) = &getCityAndStateDefs($address);
  push @defs, $cityDef;
  push @defs, $stateDef;

  $retval = "parsed-address :content " . $cityRef
    . " :context (" . join(" ",@defs) . ")";

  return $retval;
} # end sub packageCity

# the internal counter for stringGenSym
our $GenSymCount = 0;

# this tries to mimic gensym -- creates and returns a new string (prefixed by the
# argument string if given) which supposedly is unique in terms of
# other things we've generated so far
sub stringGenSym {
  my $prefix = shift;
  if (!defined($prefix) || ($prefix eq '')) {
    $prefix = "PV";
  }

  $GenSymCount++;

  return "ONT::" . $prefix . $GenSymCount;
} # end sub stringGenSym

# (common) helper function to process the city and state info from a parse
# (this is common to both parse-city and parse-address) 
# returns values ($cityRef, $cityDef, $cityName, $stateRef, $stateDef, $stateName)
sub getCityAndStateDefs {
  my $address = shift;

  # the state
  my $stateRef = &stringGenSym();
  my $stateName = $address->{'state'};
  my $stateDef =  "(ONT::the $stateRef :instance-of ONT::|Political-State| :|the-name| \"$stateName\")";

  # the city
  my $cityRef = &stringGenSym();
  my $cityName = $address->{'city'};
  my $cityDef = "(ONT::the $cityRef :instance-of ONT::|City| :|the-name| \"$cityName\" :|in-state| $stateRef)";

  return ($cityRef,$cityDef,$cityName,$stateRef,$stateDef,$stateName);
} # end sub getCityAndStateDefs

1;
__END__

=head1 NAME

AddressParser::AddressParser - Perl extension for parsing text addresses into ontological objects

=head1 SYNOPSIS

  use AddressParser::AddressParser;

  $address_akrl_text = parseAddress($text);

=head1 DESCRIPTION

B<parseAddress> takes a text address and returns an AKRL string of the address object according to the ontology.  It works for full mailing addresses (e.g., "40 S. Alcaniz St Pensacola FL") or also
city names with state (e.g., "Rochester New York")  If the parse isn't successful, returns "unable-to-parse-address"


=head2 EXPORT

None by default.

=head1 AUTHOR

Nate Blaylock

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2007 by Nate Blaylock

=cut
