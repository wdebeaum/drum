#!/usr/bin/perl

package TextTagger::Terms;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(init_term_tagger ready_term_tagger tag_terms);

use IPC::Open2;

use strict vars;

my $debug = 0;

my ($terms_in, $terms_out, $terms_pid);

sub init_term_tagger
{ # TODO make this more configurable
  $terms_pid = open2($terms_in, $terms_out,
                     $ENV{TRIPS_BASE} . "/bin/terms",
                     $ENV{TRIPS_BASE} . "/etc/TextTagger/terms.txt");
  binmode $terms_in, ':utf8';
  binmode $terms_out, ':utf8';
}

sub ready_term_tagger
{
  tag_terms(undef, "ready");
  return 1;
}

sub tag_terms
{
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
    die "Bogus output from term tagger: '$term'"
      unless ($term =~ /\s+(\d+)\s+(\d+)$/);
    push @terms, { type => "named-entity",
                   lftype => ["GEOGRAPHIC-REGION"],
		   lex => $`,
		   start => $1,
		   end => $2
		 };
    print STDERR "Got term '$`'.\n"
      if ($debug);
  }
  return [@terms];
}

push @TextTagger::taggers, {
  name => "terms",
  init_function => \&init_term_tagger,
  ready_function => \&ready_term_tagger,
  tag_function => \&tag_terms,
  output_types => ['named-entity'],
  input_text => 1
};

1;
