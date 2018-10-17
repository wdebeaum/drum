#!/usr/bin/perl

package TextTagger::Terms;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(init_term_tagger ready_term_tagger tag_terms fini_term_tagger);

use IPC::Open2;

use strict vars;

my $debug = 0;

# blacklist closed-class temporal names
# see also PersonalNames.pm
my @blacklist = (
  # week days
  qw(Sunday Monday Tuesday Wednesday Thursday Friday Saturday),
  # months
  qw(January February March April May June July August September October November December),
  # seasons
  qw(Spring Summer Fall Autumn Winter)
);

my ($terms_in, $terms_out, $terms_pid);

sub init_term_tagger
{ # TODO make this more configurable
  $terms_pid = open2($terms_in, $terms_out,
                     $ENV{TRIPS_BASE} . "/bin/terms",
                     $ENV{TRIPS_BASE} . "/etc/TextTagger/terms.txt");
  binmode $terms_in, ':utf8';
  binmode $terms_out, ':utf8';
}

sub fini_term_tagger {
  close($terms_in);
  close($terms_out);
  waitpid $terms_pid, 0;
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
    my ($lex, $start, $end) = ($`, $1, $2);
    next if (grep { $lex eq $_ } @blacklist);
    push @terms, { type => "named-entity",
                   lftype => ["GEOGRAPHIC-REGION"],
		   lex => $lex,
		   start => $start,
		   end => $end
		 };
    print STDERR "Got term '$lex'.\n"
      if ($debug);
  }
  return [@terms];
}

push @TextTagger::taggers, {
  name => "terms",
  init_function => \&init_term_tagger,
  ready_function => \&ready_term_tagger,
  tag_function => \&tag_terms,
  fini_function => \&fini_term_tagger,
  output_types => ['named-entity'],
  input_text => 1
};

1;
