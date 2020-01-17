#!/usr/bin/env perl

# get-xfam-terms.pl - get a terms TSV file from Xfam (Pfam, Dfam...) data

use lib "./Perl";
use TextTagger::Normalize qw(normalize);

use strict vars;

my $next_line = undef;
my $current_ac = undef;
my $current_id = undef;
my @current_infos = ();
my %term_to_infos = ();
my %ac_to_id = ();
my %ac_to_tp = ();

# downcase initial caps if it's just a capitalized word and not an acronym or
# single letter
# NOTE: not quite the same as TextTagger::Normalize::uncapitalize()
sub downcase_initial {
  my $str = shift;
  $str =~ s/^\p{Lu}(?=\p{Ll}+(?!\p{L}))/lc($&)/e;
  return $str;
}

while ($_ = ($next_line || <>)) {
  undef $next_line;
  chomp;
  /^#=GF (..)   / or next;
  my ($code, $line) = ($1, $');
  if (length($line) == 64) { # long line, see if it wrapped
    $next_line = <>;
    if (defined($next_line)) {
      chomp $next_line;
      if ($next_line =~ /^#=GF $code   /) {
	# next line is same kind, assume the lines wrapped
	$line .= $';
	undef $next_line;
      }
    } # otherwise we pick up $next_line on the next loop iteration
  }
  if ($code eq 'ID') { # start of entry, IDentification
    if (defined($current_ac) and defined($current_id) and @current_infos and
        $current_id !~ /^DUF\d+$/) {
      $ac_to_id{$current_ac} = $current_id;
      for (@current_infos) {
	my $unnormalized = $_->{unnormalized};
	my $normalized = normalize($unnormalized);
	push @{$term_to_infos{$normalized}{$unnormalized}},
	     { ac => $current_ac, status => $_->{status} };
      }
    }
    undef $current_ac;
    undef $current_id;
    @current_infos = ();

    $line =~ s/_/ /g;
    $current_id = downcase_initial($line);
    push @current_infos, { unnormalized => $current_id, status => 'ID' };
  } elsif ($code eq 'PI') { # Previous Identifications
    $line =~ s/_/ /g;
    push @current_infos,
	 map { +{ unnormalized => downcase_initial($_), status => 'PI' } }
	     split(/;\s*/, $line);
  } elsif ($code eq 'AC') { # ACcession number
    $line =~ s/\.\d+$//; # remove version number
    $current_ac = "XFAM:$line";
  } elsif ($code eq 'DE') { # DEscription
    push @current_infos,
	 { unnormalized => downcase_initial($line), status => 'DE' };
  } elsif ($code eq 'TP') { # TyPe
    $ac_to_tp{$current_ac} = $line;
  } elsif ($code eq 'WK') { # WiKipedia link
    next if ($line eq 'Domain_of_unknown_function'); # way too many of these
    $line =~ s/_/ /g;
    $line = downcase_initial($line);
    # remove disambiguation in parens at the end
    $line =~ s/ \([^\(\)]\)$//;
    push @current_infos, { unnormalized => $line, status => 'WK' };
  }
}

for my $normalized (sort keys %term_to_infos) {
  next if (length($normalized) == 1); # no single letters please
  next if ($normalized =~ /duf\d+\)?$/);
  print $normalized . "\t" .
        join("\t,\t",
	     map {
	       my $unnormalized = $_;
	       join("\t",
	            $unnormalized,
		    map {
		      my ($info, $ac) = ($_, $_->{ac});
		      ("$ac $ac_to_tp{$ac}", $ac_to_id{$ac}, $info->{status})
		    } @{$term_to_infos{$normalized}{$unnormalized}}
	       );
	     } sort keys %{$term_to_infos{$normalized}}
	) . "\n";
}

