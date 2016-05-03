#!/usr/bin/perl

package TextTagger::Escape;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(unescape_backslashes escape_for_quotes escape_for_pipes un_pipe_quote);

use strict vars;

sub unescape_backslashes
{
  my $str = shift;
  $str =~ s/\\(.)/$1/g; # I don't think I need to worry about \t\r\n etc.
  return $str;
}

sub escape_for_quotes
{
  my $str = shift;
  $str =~ s/\\/\\\\/g;
  $str =~ s/"/\\"/g;
  return $str;
}

sub escape_for_pipes
{
  my $str = shift;
  $str =~ s/\\/\\\\/g;
  $str =~ s/\|/\\|/g;
  return $str;
}

sub un_pipe_quote
{
  my $str = shift;
  $str = unescape_backslashes($1) if ($str =~ /^\|(.*)\|$/);
  return $str;
}

1;
