#!/usr/bin/perl

package TextTagger::StreetAddresses;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(tag_street_addresses);

use Geo::StreetAddress::US;
use TextTagger::Util qw(match2tag);

# construct a street address regex from bits of Geo::StreetAddress::US,
# excluding the "city, state zipcode" line
my $number_re = $Geo::StreetAddress::US::Addr_Match{number};
my $fraction_re = $Geo::StreetAddress::US::Addr_Match{fraction};
#my $street_re = $Geo::StreetAddress::US::Addr_Match{street};
# the library's street regex matches almost anything (e.g. "pm" in "2 pm"),
# we'd like to be more specific
my $direct_re = $Geo::StreetAddress::US::Addr_Match{direct};
my $type_re = $Geo::StreetAddress::US::Addr_Match{type};
my $street_re = qr/
		   (?:
		     # e.g. South Alcaniz
		     (?: $direct_re ) \W+ \w+ \b
		     |
		     # e.g. Clinton Avenue South
		     (?: [^\w,]+ \w+ ){0,3}?
		     (?: [^\w,]+ (?: $type_re ) \b )
		     (?: [^\w,]+ (?: $direct_re ) \b )?
		   )
		  /ix;
my $unit_re = $Geo::StreetAddress::US::Addr_Match{unit};
my $street_address_re = qr/
			   # building numbers except currency and percent
			   (?<! \p{Sc} ) $number_re (?! \% ) \W*
                           (?: $fraction_re \W* )?
			   $street_re
			   (?: \W+ $unit_re )?
			   (?= \W | $) # like one-sided \b
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
