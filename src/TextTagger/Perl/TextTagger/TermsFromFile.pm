#!/usr/bin/perl

package TextTagger::TermsFromFile;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(init_terms_from_file tag_terms_from_file);

use IPC::Open2;
use KQML::KQML;
use TextTagger::Normalize qw($dash_re is_bad_match);
use TextTagger::Tags2Trips qw(trips2tagNative);

use strict vars;

my $debug = 0;

my ($terms_in, $terms_out, $terms_pid);

sub init_terms_from_file {
  my $self = shift;
  $terms_pid = open2($terms_in, $terms_out,
                     $ENV{TRIPS_BASE} . "/bin/terms2",
                     $self->{terms_file});
  binmode $terms_in, ':utf8';
  binmode $terms_out, ':utf8';
}

sub tag_terms_from_file {
  my ($self, $str) = @_;
  $str =~ s/\n/ /g;
  print $terms_out "$str\n";
  print STDERR "$str\n"
    if ($debug);
  my @terms = ();
  while ((my $term = <$terms_in>)) {
    print STDERR $term
      if ($debug);
    chomp $term;
    last if ($term eq '');
    my ($normalized_matched_variant, $start, $end, @rest) = split(/\t/, $term, -1);
    die "Bogus output from term tagger (TermsFromFile): '$term'"
      unless ($start =~ /^\d+$/ and $end =~ /^\d+$/ and @rest);
    # remake lex from original input, since the matched variant could be
    # different
    my $lex = substr($str, $start, $end - $start);
    next if (is_bad_match($str, $lex, $start, $end));
    # read native-format tags from @rest
    for my $tag_str (@rest) {
      my $tag_kqml = KQML::KQMLReadFromString($tag_str);
      die "failed to read tag as KQML list: $tag_str"
        unless (ref($tag_kqml) == 'ARRAY');
      my $tag = trips2tagNative($tag_kqml);
      push @terms, +{ %$tag, lex => $lex, start => $start, end => $end };
    }
    print STDERR "Got term '$lex'.\n"
      if ($debug);
  }
  return [@terms];
}

push @TextTagger::taggers, {
  name => "terms_from_file",
  init_function => \&init_terms_from_file,
  tag_function => \&tag_terms_from_file,
  output_types => [@TextTagger::all_tag_types],
  input_text => 1
};

1;
