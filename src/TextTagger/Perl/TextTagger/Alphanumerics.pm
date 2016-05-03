#!/usr/bin/perl

package TextTagger::Alphanumerics;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(tag_alphanumerics);

use TextTagger::Util qw(match2tag);

use strict vars;

sub tag_alphanumerics
{ # get strings of capital letters, numbers, and (optionally) dashes
  my $self = shift;
  my $str = shift;
  my @ans = ();
  while ($str =~ /[A-Z0-9][A-Z0-9\-]*/g)
  {
    my ($before, $after) = ($`, $');
    my $an_tag = { type => "named-entity",
                   match2tag()
                   };
    push @ans, $an_tag if (length($an_tag->{lex}) > 1 and
                           $before !~ /[a-z]$/ and
			   $after !~ /^[a-z]/ and
                           $an_tag->{lex} =~ /[A-Z]/);
  }
  return [@ans];
}

push @TextTagger::taggers, {
  name => "alphanumerics",
  tag_function => \&tag_alphanumerics,
  output_types => ['named-entity'],
  input_text => 1
};

1;
