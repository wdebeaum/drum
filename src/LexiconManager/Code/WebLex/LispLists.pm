#!/usr/bin/perl

package LispLists;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(sexpr2list list2sexpr getNextSexpr tokenize);

use strict vars;

# convert a list of '(' ')' and other tokens into a nested perl arrayref
# (the first '(' of the list must be removed first)
sub sexpr2list
{
  my $input = shift;
  my $output = [];
  while (my $token = shift @$input)
  {
    if ($token eq ')')
    {
      last;
    } elsif ($token eq '(')
    {
      push @$output, sexpr2list($input);
    } else
    {
      $token = lc($token)
        unless ($token =~ /^"/);
      push @$output, $token;
    }
  }
  return $output;
}

# inverse of sexpr2list, except it returns a single string, not tokens
sub list2sexpr
{
  my $input = shift;
  my $output =
    '(' . 
      join(' ',
        map
          { (ref($_) eq 'ARRAY')?
	      list2sexpr($_) :
	      $_
	  }
	  @$input
      ) .
    ')';
  return $output;
}

# get a list of tokens representing the next S-expression in a file, given a
# reference to the filehandle
sub getNextSexpr
{
  my $fh = shift;
  my $parenlevel = 0;
  my $sexpr = '';
  while (<$fh>)
  {
    chomp;
    
    #protect characters in quotations:
    s/"([^"]*)"/
      my $quote = $1;
      $quote =~ s{;}{<SEMICOLON>}g;
      $quote =~ s{\(}{<LPAREN>}g;
      $quote =~ s{\)}{<RPAREN>}g;
      "\"$quote\""/eg;
    
    s/;.*$//;
    $parenlevel += @{[ /\(/g ]} - @{[ /\)/g ]};

    # reverse protection:
    s/<SEMICOLON>/;/g;
    s/<LPAREN>/(/g;
    s/<RPAREN>/)/g;
    
    $sexpr .= $_;
    if ($parenlevel == 0 and $sexpr =~ s/^\s*\(//)
    {
      #tokenize s-expression
      $sexpr =~ s/\(/ ( /g;
      $sexpr =~ s/\)/ ) /g;
      $sexpr =~ s/"/ " /g;
      $sexpr =~ s/^\s+//;
      $sexpr =~ s/\s+$//;
      my @sexpr = split /\s+/, $sexpr;
      #merge "ed tokens
      for (my $i = 0; $i < @sexpr; $i++)
      {
	if ($sexpr[$i] eq '"')
	{
	  my $j;
	  for ($j = $i+1; $j < @sexpr; $j++)
	  {
	    last if ($sexpr[$j] eq '"');
	  }
          # replace the list of tokens in "s with their concatenation
	  splice(@sexpr, $i, $j - $i + 1,
	    '"' . join(' ', @sexpr[($i+1)..($j-1)]) . '"');
	}
      }
      return @sexpr;
    }
  }
  return (); #couldn't get whole sexpr
}

# convert a string containing lisp S-expressions to a list of tokens
sub tokenize
{
  $_ = shift;

  #protect characters in quotations:
  s/"([^"]*)"/
    my $quote = $1;
    $quote =~ s{;}{<SEMICOLON>}g;
    $quote =~ s{\(}{<LPAREN>}g;
    $quote =~ s{\)}{<RPAREN>}g;
    $quote =~ s{ }{<SPACE>}g;
    "\"$quote\""/eg;

  # remove comments
  s/;.*$//;

  #tokenize s-expression
  s/\(/ ( /g;
  s/\)/ ) /g;
  s/^\s+//;
  s/\s+$//;
  my @sexpr = split /\s+/, $_;

  # reverse protection:
  for (@sexpr)
  {
    if (/^".*"$/)
    {
      s/<SEMICOLON>/;/g;
      s/<LPAREN>/(/g;
      s/<RPAREN>/)/g;
      s/<SPACE>/ /g;
    }
  }
  return @sexpr;
}

1;

