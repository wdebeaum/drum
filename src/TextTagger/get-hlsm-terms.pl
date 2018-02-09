#!/usr/bin/env perl

# get-hlsm-terms.pl - get a terms TSV file from the HMS LINCS Small Molecule CSV file

use lib "./Perl";
use TextTagger::Util qw(structurally_equal);
use TextTagger::Normalize qw(normalize);

sub unquote {
  my $str = shift;
  # NOTE: this doesn't handle escaped (doubled) "s within a quoted field, but
  # that's OK because this file doesn't have any.
  return (($str =~ /^"(.*?)"$/) ? $1 : $str);
}

# the file uses DOS line endings, and lines may contain newlines without
# carriage returns, so we must explicitly set the record separator to get whole
# "lines"
$/ = "\r\n";

my $headings = <>; # skip headings

my %norm2unnorm2entries = ();

sub add_entry {
  my ($unnorm, $id, $name, $status) = @_;
  my $norm = normalize($unnorm);
  my $entry = [$id, $name, $status];
  push @{$norm2unnorm2entries{$norm}{$unnorm}}, $entry
    unless (grep { structurally_equal($_, $entry) }
		 @{$norm2unnorm2entries{$norm}{$unnorm}});
}

# input CSV into entries
while (<>) {
  chomp;
  # split on the fields themselves instead of commas, since some quoted fields
  # contain commas
  my @cols = split(/((?:^|(?<=,))(?:[^,"]*|"[^"]+")(?:(?=,)|$))/);
  (@cols >= 28 and @cols <= 30) # they added a comments field after I wrote this
    or die "Expected 14-15 columns but got " . (scalar(@cols)/2);
  @cols = map { unquote($_) } @cols;
  # NOTE: we're using the PubChem CID instead of the HMS LINCS ID because
  # that's what Ben Gyori wanted, and we already had the PC Lisp package
  # defined but mostly unused, and doing it this way lets me put off dealing
  # with dbxrefs some more (see bob:#27).
  my ($name, $alt_names, $id) = @cols[3,5,9];
  next if ($id eq '');
  $id = "PC:$id";
  my @alt_names = split(/; /, $alt_names);
  # remove alt names that normalize the same as main name (many just add a dash)
  my $norm = normalize($name);
  @alt_names = grep { $norm ne normalize($_) } @alt_names;
  add_entry($name, $id, $name, "Name");
  for (@alt_names) { add_entry($_, $id, $name, "Alternative Name"); }
}

# output TSV from entries
for my $norm (sort keys %norm2unnorm2entries) {
  print $norm;
  my $first = 1;
  my $unnorm2entries = $norm2unnorm2entries{$norm};
  for my $unnorm (sort keys %$unnorm2entries) {
    if ($first) {
      $first = 0;
    } else {
      print "\t,";
    }
    print "\t$unnorm";
    my $entries = $unnorm2entries->{$unnorm};
    for my $entry (@$entries) {
      print join("\t", '', @$entry);
    }
  }
  print "\n";
}
