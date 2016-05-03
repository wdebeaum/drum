package TextTagger::TermsInput;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(load_input_terms get_input_terms add_input_terms clear_input_terms tag_input_terms);

use KQML::KQML;
use TextTagger::Tags2Trips qw(trips2tagNative tags2trips);
use TextTagger::Util qw(match2tag);

use strict vars;

my @input_terms = ();

sub load_input_terms {
  my $arg = shift;
  @input_terms = map { trips2tagNative($_) } @$arg;
}

sub get_input_terms {
  return [tags2trips(\@input_terms, 'native')];
}

sub add_input_terms {
  my $arg = shift;
  push @input_terms, map { trips2tagNative($_) } @$arg;
}

sub clear_input_terms {
  @input_terms = ();
}

sub tag_input_terms {
  my ($self, $text) = @_;
  my @tags = ();
  for my $term (@input_terms) {
    my $search = ($term->{lex} || $term->{text});
# simple way
#    while ($text =~ /\b\Q$search\E\b/g) {
# HACK for DRUM to allow (various kinds of) dash insertion and case variants
    $search = join("[\x{2010}-\x{2015}\x{2212}-]?", map { quotemeta($_) } split(//, $search));
    while ($text =~ /\b$search\b/gi) {
      push @tags, +{ %$term, match2tag() };
    }
  }
  return [@tags];
}

push @TextTagger::taggers, {
  name => "terms_input",
  tag_function => \&tag_input_terms,
  # can output any type of tag
  output_types => [@TextTagger::all_tag_types],
  input_text => 1
};

1;

