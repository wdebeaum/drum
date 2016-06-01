#!/usr/bin/env perl

use lib "./Perl";
use TextTagger::OBOParser qw(read_header read_stanza remove_trailing_modifier);
use TextTagger::Util qw(lisp_intern);
use IO::Handle;
use FileCache; # so we can open more files than the OS technically lets us

use strict vars;

# a list of symbol packages to limit processing to may be specified on the
# command line (this is used for EFO since it has a lot of extra ones)
my @packages = @ARGV;
@ARGV = ();

read_header(STDIN); # discard (TODO use for provenance)

my %ont_to_prefix_to_file = ();

sub file_for_id {
  my $id = shift;
  my ($ont, $id_num) = split(/::/, $id);
  $ont = uc($ont);
  my $prefix = sprintf("%06d", ($id_num / 10));
  unless (exists($ont_to_prefix_to_file{$ont})) {
    system("mkdir -p drum-dsl/$ont");
    $ont_to_prefix_to_file{$ont} = {};
  }
  my $f;
  if (exists($ont_to_prefix_to_file{$ont}{$prefix})) {
    $f = cacheout $ont_to_prefix_to_file{$ont}{$prefix};
  } else {
    $f = cacheout "drum-dsl/$ont/$prefix.lisp";
    print $f "(pos N\n\n";
    $ont_to_prefix_to_file{$ont}{$prefix} = $f;
  }
  return $f;
}

sub process_id {
  my $id = shift;
  $id = remove_trailing_modifier($id);
  $id =~ s/^CL:/CO:/; # can't use CL because that's Common Lisp
  $id =~ s/:/::/;
  return $id;
}

until (STDIN->eof) {
  my %stanza = read_stanza(STDIN);
  next unless ($stanza{stanza_name} eq 'Term');
  next if (exists($stanza{is_obsolete}));
  my $id = process_id($stanza{id}[0]);
  my $pkg = $id;
  $pkg =~ s/:.*//;
  next unless (@packages == 0 or grep { $_ eq $pkg } @packages);
  my $concept = "(concept $id";
  my @name = map { lisp_intern(uc($_)) } 
  	     split(/\s+/,
	     remove_trailing_modifier($stanza{name}[0]));
  $concept .= "\t(sense (word ";
  $concept .= "(" if (@name > 1);
  $concept .= join(' ', @name);
  $concept .= ")" if (@name > 1);
  $concept .= "))";
  my @all_ids = ($id);
  my $need_newline = 0;
  if (exists($stanza{alt_id})) {
    my $pkg = $id;
    $pkg =~ s/::.*//;
    my @alt_id =
      grep { # only use alt_ids from the same package as the main id
	my $alt_pkg = $_;
	$alt_pkg =~ s/::.*//;
	$pkg eq $alt_pkg
      } map { process_id($_) } @{$stanza{alt_id}};
    if (@alt_id) {
      $concept .= "\n  (alias " . join(' ', @alt_id) . ")";
      push @all_ids, @alt_id;
      $need_newline = 1;
    }
  }
  if (exists($stanza{is_a})) {
    my @is_a = map { process_id($_) } @{$stanza{is_a}};
    if (@packages) {
      @is_a = grep {
	my $pkg = $_;
	$pkg =~ s/:.*//;
	grep { $_ eq $pkg } @packages
      } @is_a;
    }
    if (@is_a) {
      $concept .= "\n  (inherit " . join(' ', @is_a) . ")";
      $need_newline = 1;
    }
  }
  $concept .= "\n  " if ($need_newline);
  $concept .= ")\n";
  for (@all_ids) {
    my $f = file_for_id($_);
    print $f $concept;
  }
}

for my $ont (keys %ont_to_prefix_to_file) {
  for my $prefix (sort keys %{$ont_to_prefix_to_file{$ont}}) {
    my $f = cacheout $ont_to_prefix_to_file{$ont}{$prefix};
    print $f ")\n";
    cacheout_close $f;
  }
}

