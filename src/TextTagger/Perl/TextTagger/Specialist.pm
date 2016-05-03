#!/usr/bin/perl

package TextTagger::Specialist;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(init_specialist tag_specialist_info);

use IPC::Open2;
use TextTagger::Normalize qw($dash_re normalize characterize_match score_match is_bad_match);

use strict vars;

my $debug = 0;

my ($terms_in, $terms_out, $terms_pid);

sub init_specialist {
  $terms_pid = open2($terms_in, $terms_out,
                     $ENV{TRIPS_BASE} . "/bin/terms2",
                     $ENV{TRIPS_BASE} . "/etc/TextTagger/specialist.tsv");
  binmode $terms_in, ':utf8';
  binmode $terms_out, ':utf8';
}

sub tag_specialist_info {
  my ($self, $str, @input_tags) = @_;
# old string input
#  $str =~ s/\n/ /g;
#  print $terms_out "$str\n";
#  print STDERR "$str\n"
#    if ($debug);
# new tags input
  print $terms_out "_BEGIN_WORD_LATTICE_\n";
  # keep the start offsets of sentences so we can use the fact that a match is
  # at the start of a sentence while scoring its case match
  my %sentence_starts = ();
  my %entries = (); # avoid printing duplicate entries
  for my $tag (@input_tags) {
    $sentence_starts{$tag->{start}} = 1 if ($tag->{type} eq 'sentence');
    next unless (grep { $_ eq $tag->{type} } qw(word prefix ending number punctuation subword subnumber));
    my $entry = "$tag->{lex}\t$tag->{start}\t$tag->{end}\n";
    next if (exists($entries{$entry}));
    $entries{$entry} = 1;
    print $terms_out $entry;
  }
  print $terms_out "_END_WORD_LATTICE_\n";
  my @terms = ();
  while ((my $term = <$terms_in>)) {
    print STDERR $term
      if ($debug);
    chomp $term;
    last if ($term eq '');
    # if we get a subset of the input lattice back first, ignore it (it's
    # useful for Drum.pm, but not here so much)
    if ($term eq '_BEGIN_WORD_LATTICE_') {
      while ($term = <$terms_in>) {
        print STDERR $term
	  if ($debug);
	last if ($term eq "_END_WORD_LATTICE_\n");
      }
      next;
    }
    my ($normalized_matched_variant, $start, $end, @rest) = split(/\t/, $term, -1);
    die "Bogus output from term tagger (SPECIALIST): '$term'"
      unless ($start =~ /^\d+$/ and $end =~ /^\d+$/); # and scalar(@rest) % 4 == 0);
    # remake lex from original input, since the matched variant could be
    # different
    my $lex = substr($str, $start, $end - $start);
    # skip abbreviations at the end of sentences that include the period (at
    # least in DRUM they're not likely to be correct)
    next if ($lex =~ /\.$/ and
             grep {
	       $_->{type} eq 'sentence' and
	       # only whitespace between period and end of sentence tag
	       ( $_->{end} >= $end and
		 substr($str, $end, $_->{end} - $end) =~ /^\s*$/
	       )
	     } @input_tags);
    next if (is_bad_match($str, $lex, $start, $end));
    while (@rest) {
      my $matched_variant = shift(@rest); # unnormalized
      my $match = characterize_match($lex, $matched_variant, $sentence_starts{$start});
      # require dashes between digits to be preserved
      if ($match->{'range-dash-no-dash'} or $match->{'no-dash-range-dash'}) {
	# eat @rest until next comma or EOL
	while (@rest) {
	  last if (shift(@rest) eq ',');
	  shift(@rest);
	  shift(@rest);
	}
	next;
      }
      $match->{score} = score_match($match);
      while (@rest) {
	my $penn_pos = shift(@rest);
	last if ($penn_pos eq ',');
	my $eui = shift(@rest);
	my $citation_form = shift(@rest);
	my @info = split(/\\n/, shift(@rest));
	my $is_plural = ($penn_pos =~ /^(?:NNP?|AITL)S$/);
	# if matching a plural single letter, insist the letter be capital
	next if ($lex =~ /^[a-z][Ss]$/ and $is_plural and
	         $citation_form =~ /^[A-Z]$/);
	# if matching a plural made with -s, insist the s be lower case
	next if ($lex =~ /S$/ and $is_plural and
		 normalize($citation_form) . 's' eq
		   $normalized_matched_variant);
	my @complements = ();
	my %links = ();
	for (@info) {
	  if (/^(?:(?:in|di|cplx|)tran|link|compl)/) {
	    push @complements, $_;
	  } elsif (/^(nominalization(?:_of)?)=([^\|]+)\|([^\|]+)(?:\|(E\d+))?$/) {
	    my ($label, $link_cf, $link_cat, $link_eui) = ($1,$2,$3,$4);
	    $label =~ s/_/-/;
	    push @{$links{$label}}, +{
	      type => 'specialist',
	      ($link_eui ? (eui => $link_eui) : ()),
	      # cat always a noun when it's a nominalization, don't bother
	      # adding
	      ($label eq 'nominalization' ? () : (cat => $link_cat)),
	      'citation-form' => $link_cf
	    };
	  } elsif (/^((?:abbreviation|acronym)_of)=([^\|]+)(?:\|(E\d+))?$/) {
	    my ($label, $link_cf, $link_eui) = ($1,$2,$3);
	    $label =~ s/_/-/;
	    push @{$links{$label}}, +{
	      type => 'specialist',
	      ($link_eui ? (eui => $link_eui) : ()),
	      'citation-form' => $link_cf
	    };
	  }
	}
	my @old_terms = grep {
	  $_->{start} == $start and
	  $_->{end} == $end and
	  $_->{'domain-specific-info'}{eui} eq $eui
	} @terms;
	if (@old_terms) { # just add the new matched variant to the old term
	  push @{$old_terms[0]{matches}}, $match;
	} else { # make a new term
	  push @terms, { type => "pos",
			 lex => $lex,
			 start => $start,
			 end => $end,
			 'penn-pos' => [$penn_pos],
			 'domain-specific-info' => +{
			   domain => 'drum', # FIXME Specialist could potentially be used for general English
			   type => 'specialist',
			   eui => $eui,
			   'citation-form' => $citation_form,
			   matches => [$match],
			   (@complements>0 ? (complements => [@complements]) :()),
			   %links
			 }
		       };
	}
      }
    }
    print STDERR "Got term '$lex'.\n"
      if ($debug);
  }
  return [@terms];
}

push @TextTagger::taggers, {
  name => "specialist",
  init_function => \&init_specialist,
  tag_function => \&tag_specialist_info,
  output_types => ['pos'],
  optional_input_types => [qw(sentence prefix ending number subword subnumber punctuation)],
  input_text => 1,
  input_types => ['word']
};

1;
