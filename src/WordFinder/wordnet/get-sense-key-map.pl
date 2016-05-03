#!/usr/bin/perl

# get-sense-key-map.pl - get a simplified version of the sense map which only maps sense keys that differ
# William de Beaumont
# 2009-02-17

=usage
cd $WN_SENSE_MAP_DIR
get-sense-key-map.pl >sense-key-map.txt
for sk in `sed -e 's/::.*//' <sense-key-map.txt `
do fgrep "$sk" ~/step/src/LexiconManager/Data/*.lisp
done
edit ~/step/src/LexiconManager/Data/*.lisp, fix the (small number of) senses
=cut

# base name for sense map data files
$basename = '2.1to3.0';

%map = ();

sub read_mono_file {
  open FH, "<$_[0]" or die "Can't open $_[0]: $!";
  while (<FH>) {
    my @fields = split(/\s+/, $_);
    @fields == 4 or die "wrong number of fields on line: $_";
    push @{$map{$fields[0]}}, $fields[2] if ($fields[0] ne $fields[2]);
  }
  close FH;
}

sub read_poly_file {
  open FH, "<$_[0]" or die "Can't open $_[0]: $!";
  while (<FH>) {
    my @fields = split(/\s+/, $_);
    @fields >= 2 or die "wrong number of fields on line: $_";
    my @senses = map { [split(/;/,$_)]->[0] } @fields[1..$#fields];
    my $from_sense = $senses[0];
    my @to_senses = @senses[1..$#senses];
    for $to_sense (@to_senses) {
      push @{$map{$from_sense}}, $to_sense if ($from_sense ne $to_sense);
    }
  }
  close FH;
}

read_mono_file($basename . ".noun.mono");
read_mono_file($basename . ".verb.mono");
read_poly_file($basename . ".noun.poly");
read_poly_file($basename . ".verb.poly");

for $from_sense (sort keys %map) {
  print "$from_sense " . join(' ', @{$map{$from_sense}}) . "\n";
}

