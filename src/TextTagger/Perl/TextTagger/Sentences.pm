#!/usr/bin/perl

package TextTagger::Sentences;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(tag_sentences);

use Lingua::EN::Sentence qw(get_sentences);
use TextTagger::XMLInput qw(process_xml_tags);

use strict vars;

sub tag_sentences
{
  my $self = shift;
  my $original_text = shift;
  if ($self->{xml_tags_mode} eq 'replace-with-spaces') {
    # remove spaces used to replace XML tags/entities
    my $no_spaces = '';
    my $prev_space_end = 0;
    for my $space (@{$self->{next_xml_input_spaces}}) {
      $no_spaces .= substr($original_text, $prev_space_end, $space->{start} - $prev_space_end);
      $prev_space_end = $space->{end};
    }
    $no_spaces .= substr($original_text, $prev_space_end);
    $original_text = $no_spaces;
  }
  my $remaining_original_text = $original_text;
  my $almost_original_text = $original_text;
  $almost_original_text =~ s/\s/ /g; # apparently Lingua::EN::Sentence removes newlines and who knows what else
  my $rot_start = 0;
  my @sentences = ();
  for my $sent_text (@{get_sentences($almost_original_text) or [$original_text]})
  {
    my $sent_text_re = quotemeta($sent_text);
    $sent_text_re =~ s/\\\s/\\s/g; # whitespace matches any whitespace
    die "sentence not found in original text: $sent_text\nremaining original text was: $remaining_original_text\n"
      unless ($remaining_original_text =~ /$sent_text_re/);
    my $start = $rot_start + $-[0];
    my $end = $rot_start + $+[0];
    $rot_start += $+[0];
    $remaining_original_text = $';
    push @sentences, { type => "sentence",
    		       text => $sent_text,
		       start => $start,
		       end => $end,
                       };
  }
  if ($self->{xml_tags_mode} eq 'replace-with-spaces') {
    # add back spaces replacing XML tags/entities
    # NOTE: spaces are assumed to be in order
    for my $space (@{$self->{next_xml_input_spaces}}) {
      my $space_length = $space->{end} - $space->{start};
      for my $sentence (@sentences) {
	# insert space in text if necessary
	if ($sentence->{start} < $space->{start} and
	    $sentence->{end} > $space->{start}) {
	  substr($sentence->{text},
	         $space->{start} - $sentence->{start}, 0, (' 'x$space_length));
	}
	# adjust sentence start/end offsets if they're after the space
	$sentence->{start} += $space_length
	  if ($sentence->{start} >= $space->{start});
	$sentence->{end} += $space_length
	  if ($sentence->{end} > $space->{start});
      }
    }
  }
  return [@sentences];
}

push @TextTagger::taggers, {
  name => "sentences",
  tag_function => \&tag_sentences,
  output_types => ['sentence'],
  input_text => 1
};

1;
