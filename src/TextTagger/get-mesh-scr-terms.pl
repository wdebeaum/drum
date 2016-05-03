#!/usr/bin/env perl

# get-mesh-scr-terms.pl - get terms for Drum tagger from MeSH Supplementary Concept Records
# William de Beaumont
# 2015-10-26

use lib "./Perl";
use TextTagger::Normalize qw(normalize);

use strict vars;

my %ui_to_name = ();
my %term_to_infos = ();

my $current_name = undef;
my @current_synonyms = ();
my @current_types = ();
my $current_ui = undef;
my $prev_key = undef;

eval {
while (<>) {
  chomp;
  if (/^\*NEWRECORD/) {
    $current_name = undef;
    @current_synonyms = ();
    @current_types = ();
    $current_ui = undef;
    $prev_key = undef;
  } elsif (/^\s*$/) { # blank line at end of record
    if (defined($current_ui) and
        # "Organic Chemical", "Pharmacologic Substance", "Antibiotic", and "Clinical Drug"
        grep /^T(109|121|195|200)$/, @current_types) {
      if (defined($current_name)) {
        $ui_to_name{$current_ui} = $current_name;
	push @{$term_to_infos{normalize($current_name)}{$current_name}},
	     { ui => $current_ui, status => 'NM' };
      }
      for (@current_synonyms) {
	push @{$term_to_infos{normalize($_)}{$_}},
	     { ui => $current_ui, status => 'SY' };
      }
    }
  } elsif (/^(\w+) = /) {
    my ($key, $val) = ($1, $');
    if ($key eq 'NM') { # NaMe
      $current_name = $val;
    } elsif ($key eq 'UI') { # Unique Identifier
      $current_ui = $val;
    } elsif ($key eq 'ST') { # UMLS Semantic Type
      push @current_types, $val;
    } elsif ($key eq 'SY') { # SYnonym
      my @fields = split(/\|/, $val);
      my $map_str = pop @fields;
      my @field_types = split(//, $map_str);
      die "malformed SYnonym field: $val\n" unless ($#fields == $#field_types);
      my $term = undef;
      for my $i (0..$#fields) {
	my ($val, $type) = ($fields[$i], $field_types[$i]);
	if ($type eq 'a') { # the term itself
	  $term = $val;
	} elsif ($type =~ /^[b-f]$/) {
	  # b = semantic type
	    # ignore, redundant with "ST = "
	  # c = lexical type
	  # d = semantic relation
	  # e = thesaurus ID
	  # f = date
	  # ignore all of these
	} else {
	  die "bogus SYnonym subfield type $type\n";
	}
      }
      push @current_synonyms, $term if (defined($term));
    }
    $prev_key = $key;
  } elsif ($prev_key eq 'NO') { # NOtes can extend multiple lines
    # ignore
  } else {
    die "malformed MeSH SCR line: $_\n";
  }
}
1 } || die "on line $.: $@";

for my $normalized (sort keys %term_to_infos) {
  print $normalized;
  my $first = 1;
  for my $unnormalized (sort keys %{$term_to_infos{$normalized}}) {
    if ($first) {
      $first = 0;
    } else {
      print "\t,";
    }
    print "\t$unnormalized";
    for my $info (@{$term_to_infos{$normalized}{$unnormalized}}) {
      print "\tMESH:$info->{ui}\t$ui_to_name{$info->{ui}}\t$info->{status}";
    }
  }
  print "\n";
}

