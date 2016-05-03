#!/usr/bin/env perl

# merge-terms-files.pl - merge TSV terms files in the format that the Drum tagger uses
# Each row consists of the normalized term followed by comma-separated groups
# of columns, each of which starts with an unnormalized term that normalizes to
# the normalized term at the start of the row.
# normalized ( unnormalized col col col ... ','? )+

use strict vars;

# read a line from a file and split on tabs into a listref, return undef on eof
sub read_row {
  my $fh = shift;
  local $_ = <$fh>;
  if (defined($_)) {
    chomp;
    return [split(/\t/, $_, -1)];
  } else {
    return undef;
  }
}

@ARGV > 0 or die "USAGE: $0 input1.tsv input2.tsv ... >output.tsv";

# open all the input files
my @input_files =
  map { my $fh; open $fh, "<$_" or die "Can't open $_: $!"; $fh } @ARGV;
# read the first row in each
my @input_rows = map { read_row($_) } @input_files;

my $output_row_count = 0;
for(;;) {
  # find the minimum term and the indices into input_files/rows that have that
  # term
  my $min_term = undef;
  my @min_indices = ();
  for my $i (0..$#input_rows) {
    if (defined($input_rows[$i])) {
      if (defined($min_term) and $input_rows[$i][0] eq $min_term) {
	push @min_indices, $i;
      } elsif ((not defined($min_term)) or $input_rows[$i][0] lt $min_term) {
	$min_term = $input_rows[$i][0];
	@min_indices = ($i);
      }
    }
  }
  last unless (defined($min_term)); # all input rows undef, so all files at eof
  # build %unnormalized2triples from the selected input rows, and read new rows
  # from those files
  my %unnormalized2triples = ();
  for my $i (@min_indices) {
    # get the comma-separated groups of columns in @{$input_rows[$i]} except
    # the first element, which is the normalized term
    my $j = 1;
    my @comma_groups = ();
    while ($j < @{$input_rows[$i]}) {
      push @comma_groups, [];
      while ($j < @{$input_rows[$i]} and $input_rows[$i][$j] ne ',') {
	push @{$comma_groups[-1]}, $input_rows[$i][$j];
	$j++;
      }
      $j++;
    }
    # the first item in each group is the unnormalized term, so add the rest to
    # the appropriate hash entry
    for my $group (@comma_groups) {
      push @{$unnormalized2triples{$group->[0]}}, @{$group}[1..$#$group];
    }
    # read the next row for input $i
    $input_rows[$i] = read_row($input_files[$i]);
  }
  # build the output row from %unnormalized2triples
  my @output_row = ($min_term);
  my $first = 1;
  for my $unnormalized (sort keys %unnormalized2triples) {
    if ($first) {
      $first = 0;
    } else {
      push @output_row, ',';
    }
    push @output_row, $unnormalized, @{$unnormalized2triples{$unnormalized}};
  }
  # write the output row
  print join("\t", @output_row) . "\n";
  $output_row_count++;
}

