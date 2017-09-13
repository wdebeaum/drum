#!/usr/bin/perl

package TextTagger::NamesFromFile;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(init_names_from_file tag_names_from_file fini_names_from_file);

use IPC::Open2;

use strict vars;

my $debug = 0;

my ($terms_in, $terms_out, $terms_pid);

sub init_names_from_file {
  my $self = shift;
  $terms_pid = open2($terms_in, $terms_out,
                     $ENV{TRIPS_BASE} . "/bin/terms",
                     $self->{names_file});
  binmode $terms_in, ':utf8';
  binmode $terms_out, ':utf8';
}

sub fini_names_from_file {
  close($terms_in);
  close($terms_out);
  waitpid $terms_pid, 0;
}

sub tag_names_from_file {
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
    my ($lex, $start, $end, @rest) = split(/\t/, $term, -1);
    die "Bogus output from term tagger (NamesFromFile): '$term'"
      unless ($start =~ /^\d+$/ and $end =~ /^\d+$/ and scalar(@rest) == 0);
    push @terms, +{
      type => 'named-entity',
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
  name => "names_from_file",
  init_function => \&init_names_from_file,
  tag_function => \&tag_names_from_file,
  fini_function => \&fini_names_from_file,
  output_types => ['named-entity'],
  input_text => 1
};

1;
