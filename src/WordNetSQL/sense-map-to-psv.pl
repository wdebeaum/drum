#!/usr/bin/perl -n

# sense-map-to-psv.pl - given a bunch of WordNet sensemap files with names of
# the form $(OLD)to$(NEW).{noun,verb}.{mono,poly}, output a single
# pipe-separated values file to import into SQLite3

use strict vars;

$ARGV =~ /(?:^|\/)(\d+(?:\.\d+)*)to(\d+(?:\.\d+)*).(noun|verb).(mono|poly)$/
  or die "Bogus sensemap filename: $ARGV";
my ($old, $new, $pos, $semy) = ($1,$2,$3,$4);
my $ss_type = substr($pos,0,1);

chomp;
my @fields = split(/[\s;]+/);
if ($semy eq 'mono') {
  @fields == 4 or die "Bogus mono line in $ARGV:\n$_\n";
  my ($old_sk, $old_sso, $new_sk, $new_sso) = @fields;
  my $lemma = $old_sk;
  $lemma =~ s/%.*//;
  print "$old|$old_sk|$old_sso|1|$new|$new_sk|$new_sso|1|$lemma|$ss_type|100\n"
} else { # poly
  @fields >= 4 or die "Bogus poly line in $ARGV:\n$_\n";
  my ($score, $old_sk, $old_sso, $old_sn) = splice(@fields, 0, 4);
  my $lemma = $old_sk;
  $lemma =~ s/%.*//;
  while (@fields >= 3) {
    my ($new_sk, $new_sso, $new_sn) = splice(@fields, 0, 3);
    print "$old|$old_sk|$old_sso|$old_sn|$new|$new_sk|$new_sso|$new_sn|$lemma|$ss_type|$score\n";
  }
  die "Bogus poly line in $ARGV:\n$_\n" unless (@fields == 0);
}

