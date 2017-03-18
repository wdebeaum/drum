package TextTagger::RomanNumerals;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(tag_roman_numerals);

use TextTagger::Util qw(match2tag);

use strict vars;

my %letter2value = qw(I 1 V 5 X 10 L 50 C 100 D 500 M 1000);

# given a string that contains a valid roman numeral letter sequence, return
# the value it represents
sub parse_roman_numeral {
  my @chars = split(//, uc(shift));
  my $prev_letter_val = 1000;
  my $val = 0;
  for my $letter (@chars) {
    my $letter_val = $letter2value{$letter};
    $val -= 2 * $prev_letter_val if ($prev_letter_val < $letter_val);
    $val += $letter_val;
    $prev_letter_val = $letter_val;
  }
  return $val;
}

sub tag_roman_numerals {
  my ($self, $text) = @_;
  my @tags = ();
  while ($text =~ 
	 # pure version
         # /\b[ivxlcdm]+\b/gi
         # with exceptions for music notation
	 /\b[IVXLCDMivxlcdm]+(?!(?=\w)(?![2-7bdoø]\b))/g
	) {
    my $tag = { type => "number", match2tag() };

    # another music exception: take off a "d" if the previous letter is i or v
    $tag->{lex} =~ s/(?<=[IViv])d$//;
    $tag->{end} = $tag->{start} + length($tag->{lex});

    my $islower = ($tag->{lex} =~ /[c-x]/);
    my $isupper = ($tag->{lex} =~ /[C-X]/);
    # disallow mixed case
    next if ($isupper and $islower);
    # disallow (some?) nonstandard sequences
    next if ($tag->{lex} =~ /
      # no 4x repeats
      IIII | VVVV | XXXX | LLLL | CCCC | DDDD | MMMM |
      # powers of 10 can't directly precede larger letters unless it's only one
      # and it directly precedes one of the two next larger letters
      I.+?[VXLCDM] | X.+?[LCDM] | C.+?[DM] |
      I[LCDM] | X[DM] |
      # powers of 10 can't go backwards more than 2 letters
      IXC | XCM |
      # multiples of 5 can't precede larger letters at all
      V.*?[XLCDM] | L.*?[CDM] | D.*?M
    /ix);
    $tag->{'domain-specific-info'} = +{
      domain => 'general',
      type => 'roman-numeral',
      case => ($isupper ? 'upper' : 'lower'),
      value => parse_roman_numeral($tag->{lex})
    };
    push @tags, $tag;
  }
  return [@tags];
}

push @TextTagger::taggers, {
  name => "roman_numerals",
  tag_function => \&tag_roman_numerals,
  output_types => [qw(number)],
  input_text => 1
};

1;

