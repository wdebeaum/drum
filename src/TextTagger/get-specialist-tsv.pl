#!/usr/bin/env perl

# get-specialist-tsv.pl - get a TSV file suitable for feeding to terms2 from the LEXICON and MISC/inflections.table files from the SPECIALIST Lexicon

use lib "./Perl";
use TextTagger::Normalize qw(normalize);

use Data::Dumper;

use strict vars;

die "USAGE: TRIPS_BASE=/path/to/trips $0 LEXICON inflections.table >specialist.tsv" unless (@ARGV == 2 and exists($ENV{TRIPS_BASE}));

my %pos_inflection_to_penn_pos = qw{
  adj|comparative	JJR
  adj|positive		JJ
  adj|superlative	JJS
  adv|comparative	RBR
  adv|positive		RB
  adv|superlative	RBS
  noun|plur		NNS
  noun|sing		NN
  prep|base		IN
  pron|base		PRP
  verb|infinitive	VB
  verb|past		VBD
  verb|past_part	VBN
  verb|pres(fst_plur,second,thr_plur)	VBP
  verb|pres(thr_sing)	VBZ
  verb|pres_part	VBG
};

=omitted
These are left out because we're only interested in open-class words, and
"base" is often just an alias for another form above:
  adj|base		JJ
  adv|base		RB
  aux|base
  aux|infinitive
  aux|past
  aux|past(fst_plur,second,thr_plur)
  aux|past(fst_plur,second,thr_plur):negative
  aux|past(fst_sing,thr_sing)
  aux|past(fst_sing,thr_sing):negative
  aux|past:negative
  aux|past_part
  aux|pres(fst_plur,second,thr_plur)
  aux|pres(fst_plur,second,thr_plur):negative
  aux|pres(fst_sing)
  aux|pres(fst_sing,fst_plur,second,thr_plur)
  aux|pres(fst_sing,fst_plur,second,thr_plur):negative
  aux|pres(thr_sing)
  aux|pres(thr_sing):negative
  aux|pres_part
  compl|base
  conj|base		CC
  det|base		DT
  modal|base
  modal|past
  modal|past:negative
  modal|pres
  modal|pres:negative
  noun|base		NN
  verb|base		VB
=cut

my %eui_to_info = ();

open LEX, "<$ARGV[0]" or die "Can't open lexicon file $ARGV[0]: $!";
my ($base, $info) = undef;
while (<LEX>) {
  chomp;
  s/^\s+//;
  if (/^{base=/) {
    $base = $';
  } elsif (/^}$/) {
    $base = undef;
    $info = undef;
  } elsif (not defined($base)) {
    # do nothing, just skip the rest of the elsifs
  } elsif (/^entry=(E\d+)$/) {
    $info = [];
    $eui_to_info{$1} = $info;
  } elsif (/^(?:(?:in|di|cplx|)tran|link|compl)/) { # complement info
    s/np\|(.*?)\|/np($1)/g;
    push @$info, $_;
  } elsif (/^(?: cat=(?:\w+)$ | nominalization | (?:abbreviation|acronym)_of= |
  		 proper$ )/x) {
    push @$info, $_;
  }
}
close LEX;

# scrape words from TRIPS lexicon lisp data files
my %words_in_trips_lexicon = ();
my $lxmdn = "$ENV{TRIPS_BASE}/src/LexiconManager/Data/new/";
opendir LXMDN, $lxmdn or die "Can't open lexicon data directory $lxmdn: $!";
while (my $entry = readdir LXMDN) {
  next unless ($entry =~ /\.lisp$/);
  open WORD, "<$lxmdn/$entry" or die "Can't open lexicon data file $entry: $!";
  while (<WORD>) {
    next if (/\((define-words|syntax|lf|templ|w::(vform|agr|disj|conj|seq)) |:(pos|templ) /i);
    while (/w::([^\s\(\);]+)/ig) {
      $words_in_trips_lexicon{lc($1)} = 1;
    }
  }
  close WORD;
}
closedir LXMDN;
delete $words_in_trips_lexicon{'^'};
delete $words_in_trips_lexicon{'-'};

sub already_in_trips_lexicon {
  return exists($words_in_trips_lexicon{lc(shift(@_))});
}

my %inflected_to_eui_pos_list = ();
my %eui_to_citation = ();

open INFL, "<$ARGV[1]" or die "Can't open inflections table $ARGV[1]: $!";
while (<INFL>) {
  chomp;
  my ($inflected, $pos, undef, $inflection, undef, $eui, undef, $citation) =
    split(/\|/);
  next unless (exists($eui_to_info{$eui}));
  next if ($inflected =~ /'s$/ and $pos eq 'noun' and $inflection eq 'plur');
  my $penn_pos = $pos_inflection_to_penn_pos{"$pos|$inflection"};
  next unless (defined($penn_pos));
  if ($pos eq 'noun' and grep { $_ eq 'proper' } @{$eui_to_info{$eui}}) {
    $penn_pos =~ s/^NN/NNP/;
  }
  # mark single words we already have in the TRIPS lexicon
  if (already_in_trips_lexicon($citation) or
      ($inflected =~ /[A-Z]/ and already_in_trips_lexicon($inflected))) {
    # if it's in TRIPS already and it's not a noun inflection, get rid of it now
    next unless (grep { $_ eq $penn_pos } qw(NN NNS NNP NNPS));
    # otherwise mark it so we can get rid of it after using it in Drum tagger
    $penn_pos =~ s/^NNP?/AITL/; # Already In Trips Lexicon
  }
  my $normalized = normalize($inflected);
  push @{$inflected_to_eui_pos_list{$normalized}{$inflected}}, [$eui, $penn_pos]
    unless (grep { $_->[0] eq $eui and $_->[1] eq $penn_pos }
    		 @{$inflected_to_eui_pos_list{$normalized}{$inflected}});
  $eui_to_citation{$eui} ||= $citation;
}
close INFL;

# a few manual er/or citation form corrections
$eui_to_citation{E0016301} = 'chelator';
$eui_to_citation{E0022854} = 'dilator';
$eui_to_citation{E0024545} = 'effector';
$eui_to_citation{E0034634} = 'inhibitor';
$eui_to_citation{E0059156} = 'suppressor';

for my $normalized (sort keys %inflected_to_eui_pos_list) {
  next if (length($normalized) == 1); # no single letters please
  print $normalized;
  my $first = 1;
  for my $inflected (sort keys %{$inflected_to_eui_pos_list{$normalized}}) {
    if ($first) {
      $first = 0;
    } else {
      print "\t,";
    }
    print "\t$inflected";
    for my $eui_pos (sort { $a->[0] cmp $b->[0] }
			  @{$inflected_to_eui_pos_list{$normalized}{$inflected}}) {
      my ($eui, $pos) = @$eui_pos;
      my $info = join("\\n", grep !/^(?:cat=\w+|proper)$/, @{$eui_to_info{$eui}});
      print "\t$pos\t$eui\t$eui_to_citation{$eui}\t$info";
    }
  }
  print "\n";
}

