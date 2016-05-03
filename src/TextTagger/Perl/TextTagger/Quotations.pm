#!/usr/bin/perl

package TextTagger::Quotations;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(tag_quotations);

use TextTagger::Util qw(match2tag);

use strict vars;

sub tag_quotations
{ # get strings within "s, where there is no whitespace immediately within the quotes
  my $self = shift;
  my $str = shift;
  my @quotes = ();
  while ($str =~ /"\S(?:.*?\S)?"/g)
  {
    push @quotes, { type => "quotation",
                    match2tag()
                    };
  }
  return [@quotes];
}

push @TextTagger::taggers, {
  name => "quotations",
  tag_function => \&tag_quotations,
  output_types => ['quotation'],
  input_text => 1
};

1;
