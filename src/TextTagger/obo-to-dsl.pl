#!/usr/bin/env perl

use lib "./Perl";
use TextTagger::OBOParser qw(read_header read_stanza remove_trailing_modifier);
use TextTagger::Util qw(lisp_intern);
use IO::Handle;
use FileCache; # so we can open more files than the OS technically lets us

use strict vars;

# lists of symbol packages to limit processing to may be specified on the
# command line (this is used for EFO since it has a lot of extra ones)
# @term_packages may be used for whole terms, while @reln_packages may be used
# for targets of relationships only; the two are specified in order, separated
# by a comma as its own word. If there is no comma, they're all @term_packages.
# Note that we include @term_packages in @reln_packages, because packages used
# for whole terms may also be used for relationship targets.
my @term_packages = ();
for (@ARGV) {
  last if ($_ eq ',');
  push @term_packages, $_;
}
my @reln_packages = grep { $_ ne ',' } @ARGV;
@ARGV = ();

read_header(STDIN); # discard (TODO use for provenance)

my %ont_to_prefix_to_file = ();

sub file_for_id {
  my $id = shift;
  my ($ont, $id_num) = split(/::/, $id);
  $id_num =~ s/^\||\|$//g; # remove pipequotes if present
  $ont = uc($ont);
  my $f;
  my $prefix = sprintf("%06d", ($id_num / 10));
  if ($ont eq 'FPLX') { # special case for famplex (IDs are non-numeric)
    $id_num =~ /^\w/ or die "famplex id doesn't match /^\\w/: $id_num";
    $prefix = uc($&);
  }
  unless (exists($ont_to_prefix_to_file{$ont})) {
    system("mkdir -p drum-dsl/$ont");
    $ont_to_prefix_to_file{$ont} = {};
  }
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
  # $id =~ s/:/::/;
  my ($pkg, $name) = split(/:/, $id);
  $id = lisp_intern($name, $pkg);
  return $id;
}

until (STDIN->eof) {
  my %stanza = read_stanza(STDIN);
  next unless ($stanza{stanza_name} eq 'Term');
  next if (exists($stanza{is_obsolete}));
  my $id = process_id($stanza{id}[0]);
  my $pkg = $id;
  $pkg =~ s/:.*//;
  next unless (@term_packages == 0 or grep { $_ eq $pkg } @term_packages);
  next if ($pkg eq 'PR' and not $id =~ /^PR::00\d{7}/); # no UP IDs plz
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
    if (@reln_packages) {
      @is_a = grep {
	my $pkg = $_;
	$pkg =~ s/:.*//;
        ($pkg ne 'PR' or /^PR::00\d{7}/) and # no UP IDs plz
	grep { $_ eq $pkg } @reln_packages
      } @is_a;
    }
    if (@is_a) {
      $concept .= "\n  (inherit " . join(' ', @is_a) . ")";
      $need_newline = 1;
    }
  }
  if (exists($stanza{relationship})) {
    my @relns = map { [split(/\s+/)] } @{$stanza{relationship}};
    my @has_role =
      map { process_id($_->[1]) }
      grep { $_->[0] eq 'has_role' } @relns;
    # TODO check @*_packages? not really necessary for what we use it for
    if (@has_role) {
      $concept .= "\n  (> CHEBI::has_role " . join(' ', @has_role) . ")";
      $need_newline = 1;
    }
  }
  $concept .= "\n  " if ($need_newline);
  $concept .= ")\n";
  for (@all_ids) {
    my $f = file_for_id($_);
    binmode $f, ':utf8';
    print $f $concept;
  }
}

for my $ont (keys %ont_to_prefix_to_file) {
  for my $prefix (sort keys %{$ont_to_prefix_to_file{$ont}}) {
    my $f = cacheout $ont_to_prefix_to_file{$ont}{$prefix};
    binmode $f, ':utf8';
    print $f ")\n";
    cacheout_close $f;
  }
}

