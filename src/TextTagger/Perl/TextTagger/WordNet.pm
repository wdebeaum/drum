#!/usr/bin/perl

package TextTagger::WordNet;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(init_word_net_tagger tag_word_net_words fini_word_net_tagger);

use IPC::Open2;

use strict vars;

my $debug = 0;

my ($terms_in, $terms_out, $terms_pid);

sub init_word_net_tagger {
  $terms_pid = open2($terms_in, $terms_out,
                     $ENV{TRIPS_BASE} . "/bin/terms",
                     $ENV{TRIPS_BASE} . "/etc/TextTagger/wn-multiwords.tsv");
  binmode $terms_in, ':utf8';
  binmode $terms_out, ':utf8';
}

sub fini_word_net_tagger {
  close($terms_in);
  close($terms_out);
  waitpid $terms_pid, 0;
}

sub tag_word_net_words {
  my $self = shift;
  my $str = shift;
  $str =~ s/\n/ /g;
  my @terms = ();
  print $terms_out "$str\n";
  print STDERR "$str\n"
    if ($debug);
  while ((my $term = <$terms_in>)) {
    print STDERR $term
      if ($debug);
    chomp $term;
    last if ($term eq '');
    my ($lex, $start, $end, @rest) = split(/\t/, $term);
    die "Bogus output from term tagger (WN): '$term'"
      unless ($start =~ /^\d+$/ and $end =~ /^\d+$/ and scalar(@rest) % 2 == 0);
    while (@rest) {
      my $penn_pos = shift(@rest);
      my $sense_key = shift(@rest);
      push @terms, { type => "sense",
		     lex => $lex,
		     start => $start,
		     end => $end,
		     'penn-pos' => [$penn_pos],
		     'wn-sense-keys' => [$sense_key]
		   };
    }
    print STDERR "Got term '$lex'.\n"
      if ($debug);
  }
  return [@terms];
}

push @TextTagger::taggers, {
  name => "word_net",
  init_function => \&init_word_net_tagger,
  tag_function => \&tag_word_net_words,
  fini_function => \&fini_word_net_tagger,
  output_types => ['sense'],
  input_text => 1
};

1;
