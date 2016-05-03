package TextTagger::OneSentencePerLine;
require Exporter;

@ISA = qw(Exporter);
@EXPORT_OK = qw(tag_lines);

use TextTagger::Util qw(match2tag);

use strict vars;

sub tag_lines {
  my ($self, $text) = @_;
  my @lines = ();
  while ($text =~ /[^\r\n\S]*\S[^\r\n]*(?:\r|\n|\r\n|\n\r|$)/g) {
    my $line = { type => 'sentence', match2tag() };
    # sentence tags use text, not lex
    $line->{text} = $line->{lex};
    delete($line->{lex});
    push @lines, $line;
  }
  return [@lines];
}

push @TextTagger::taggers, {
  name => "one_sentence_per_line",
  tag_function => \&tag_lines,
  output_types => ['sentence'],
  input_text => 1
};

1;

