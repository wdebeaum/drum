#!/usr/bin/perl

package TextTagger::StreetAddresses;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(tag_street_addresses);

use Geo::StreetAddress::US;
use TextTagger::Util qw(match2tag);

my $number_re = $Geo::StreetAddress::US::Addr_Match{number};
my $fraction_re = $Geo::StreetAddress::US::Addr_Match{fraction};
my $street_re = $Geo::StreetAddress::US::Addr_Match{street};
my $unit_re = $Geo::StreetAddress::US::Addr_Match{unit};
my $street_address_re = qr/(?<!\p{Sc})$number_re(?!\%)\W*
                           (?:$fraction_re\W*)?
			   $street_re
			   (?:\W+$unit_re)?
			   (?=\W)
			  /ix;

use strict vars;

sub tag_street_addresses
{
  my $self = shift;
  my $original_text = shift;
  my @addresses = ();
  # NOTE: this won't get a street address if something else that looks like a street address precedes and overlaps it
  while ($original_text =~ /$street_address_re/g)
  {
    my $tag = { type => "street-address",
		match2tag()
              };
    # trim trailing space
    if ($tag->{lex} =~ s/\s+$//) {
      $tag->{end} -= length($&);
    }
    push @addresses, $tag;
  }
  return [@addresses];
}

push @TextTagger::taggers, {
  name => "street_addresses",
  tag_function => \&tag_street_addresses,
  output_types => ['street-address'],
  input_text => 1
};

1;
