#!/usr/bin/perl -p

# update-hierarchy-mapping-offsets.pl - use the WNsnsmap package to update 
#					file offsets in the hierarchy-mapping
#					hash in wordnet-to-trips.lisp

=usage
download and unpack the WNsnsmap package for the conversion you want to do
change $senseMapBaseName below to reflect the right versions
cd to the place you unpacked WNsnsmap (where X.YtoX.Y.* is)
update-hierarchy-mapping-offsets.pl <old-hierarchy-mapping.lisp >new-h-m.lisp

(it might work to just put the entire wordnet-to-trips.lisp through the filter, but I wouldn't count on it not changing anything else)
=cut

# change this if the versions you're converting between are different than these
BEGIN { $senseMapBaseName = '2.1to3.0'; }

if (/\("(n|v)" (\d{8})\)/)
{
  ($before, $pos, $oldOffset, $after) = ($`, $1, $2, $');
  print STDERR "old offset is $oldOffset\n";
  if ($pos eq 'n')
  {
    $fullPOS = 'noun';
  } else # $pos eq 'v'
  {
    $fullPOS = 'verb';
  }
  $mapping = ${[`grep -P '$oldOffset \\S+ \\d{8}' $senseMapBaseName.$fullPOS.mono`]}[0];
  print STDERR "mono mapping is: $mapping\n";
  if ($mapping =~ /$oldOffset \S+ (\d{8})/)
  {
    print STDERR "found new offset: $1\n";
    $_ = "$before(\"$pos\" $1)$after";
  } else # not in .mono, try .poly
  {
    $mapping = ${[`grep -P '^\\d+ .*?;$oldOffset;' $senseMapBaseName.$fullPOS.poly`]}[0];
    print STDERR "poly mapping is: $mapping\n";
    if ($mapping =~ /;$oldOffset;/)
    {
      @fields = split(/\s+/, $mapping);
      shift @fields;
      shift @fields;
      $_ = '';
      for $field (@fields)
      {
	if ($field =~ /;(\d{8});/)
	{
	  print STDERR "found new offset: $1\n";
	  $_ .= "$before(\"$pos\" $1)$after";
	}
      }
    } else
    {
      print STDERR "Warning: no mapping for old offset $oldOffset found; passing through.\n";
    }
  }
}

