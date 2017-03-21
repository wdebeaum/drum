#!/usr/bin/env perl

# go_protmods.pl
#
# Time-stamp: <Mon Mar 20 10:59:31 CDT 2017 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 13 Mar 2017
#

#----------------------------------------------------------------
# Description:
# Extracts a subset of GO containing protein modification entries
# suitable for lexical development. 
# If an existing lexicon exists, it can be compared against the
# extracted terms to check coverage.
# A dot file can also be produced.

#----------------------------------------------------------------
# History:
# 2017/03/13 lgalescu v1.0
# - script produces a dot file suitable for analysis
# 2017/03/15 lgalescu v2.0
# - script outputs an OBO file, as well
# 2017/03/19 lgalescu v3.0
# - cleaned up; significant re-factoring.
# - Moved to TRIPS/TextTagger component (updated packages).

#----------------------------------------------------------------
# Usage:
# 
my $usage;

BEGIN {
    # Just use basename of program, rather than whole path.
    $0 =~ s:.*/::;
    $usage = "Usage: $0 [-lex LEX] -dot DOT -out OUT-OBO IN-OBO\n";
}

###----------------------------------------------------------------
## Modules & Features
#

$VERSION = '3.0';

use lib "./Perl";

use TextTagger::OBOGraph;

use Getopt::Long;
use Data::Dumper;
use File::Slurp;
use List::Util qw(any all);

###----------------------------------------------------------------
## Command-line arguments
#

our (
     $oboFile,
     $outFile,
     $lexFile,
     $dotFile,
     $debug,
    );

GetOptions(
	   'o|out=s' => \$outFile,
	   'g|dot=s' => \$dotFile,
	   'l|lex=s' => \$lexFile,
	   'd|debug' => \$debug,
	   'h|help') or die $usage;

die $usage if $opt_help;

my $oboFile = shift;

# checks
-f $oboFile
  or die $usage;
if ($lexFile) {
  -f $lexFile
    or die "File not found: $lexFile\n";
}

###================================================================
## Globals and Constants
#

# GO ontology
my $go; 

# root of sub-ontology 
my $root = 'GO:0006464'; # cellular protein modification process)

# undesirable term ids in the output OBO (might otherwise be included)
my @exclusions = qw/GO:0035551 GO:0032446/;
# desirable term ids in the output OBO (might otherwise be excluded)
my @inclusions = qw/GO:0006493/;

# lexicon
my %lexicon;

# sub-ontology graph
my %mods;

###================================================================
## Main
#

# make GO graph
$go = TextTagger::OBOGraph->new($oboFile);

# read lexicon
if ($lexFile) {
  %lexicon = map { $_ => 0 } read_file($lexFile, chomp => 1);
}

get_protmods();

warn "mods:\n" . Dumper(\%mods)
  if $debug;

to_dot($dotFile) if $dotFile;
to_obo($outFile) if $outFile;

exit 0;

###================================================================
## Subs
#

# main function: look for protein modifications that are good candidates
# for lexicalization
sub get_protmods {
  # get all descendants of the sub-ontology root node
  %mods = map {$_ => {}} $go->descendants($root);
  warn sprintf("Found %d descendants.\n", scalar(keys %mods))
    if $debug;

  my @allnames = map { $go->get_term_name($_) } keys %mods;
  $mods{$root} = {};

  # candidates
  foreach my $mod (keys %mods) {
    # root is just that, not an actual candidate
    next if $mod eq $root;

    my $name = $go->get_term_name($mod);

    # in our lexicon?
    $mods{$mod}{lex} = check_lex($name);
    
    # check inclusions
    if (grep {$mod eq $_} @inclusions) {
      warn "$mod $name => inclusion\n"
	if $debug;
      next;
    }
    
    # check exclusions
    if (grep {$mod eq $_} @exclusions) {
      warn "$mod $name => exclusion\n"
	if $debug;
      $mods{$mod}{skipped} = 1;
      next;
    }
    
    # check name to see if it's what we're looking for
    # TODO: look in synonyms as well!
    #       eg: GO:0008612 (protein hypusination)
    unless (grep { $_ =~ /^protein .*ion$/ } $name) {
      $mods{$mod}{skipped} = 1;
      next;
    }

    # skip if there's another candidate with a more generic name
    # TODO: consider whether this strategy is a good one!
    if (any { is_more_specific($name, $_) } @allnames) {
      warn "$mod $name => skipped\n"
	if $debug;
      $mods{$mod}{skipped} = 1;
      next;
    } else {
      # a bona fide candidate
      warn "$mod $name => $mods{$mod}{lex}\n"
	if $debug;
    }
  }

  # parents 
  foreach my $mod (keys %mods) {
    next if $mod eq $root;
    next if $mods{$mod}{skipped};
    # get parents
    $mods{$mod}{is_a} = [ grep { in_network($_) } $go->parents($mod) ];
    # if no parents, find new reattachment node(s)
    if ($debug) {
      local $Data::Dumper::Terse = 1;
      local $Data::Dumper::Indent = 0;
      warn sprintf("$mod has %d proper parents: %s\n",
		   scalar(@{ $mods{$mod}{is_a} }),
		   Dumper($mods{$mod}{is_a}))
	if $debug;
    }
    if (scalar(@{ $mods{$mod}{is_a} }) == 0) {
      $mods{$mod}{is_d} = [ new_parents($mod) ];
    }
  }
  warn sprintf("Found %d matching descendants.\n",
	       -1 + grep { in_network($_) } keys %mods)
    if $debug;
  if (%lexicon) {
    warn sprintf("Lexicon:\n\t%d matched\n\t%d matched with prefixes\n\t%d not matched (%s)\n",
		 scalar(grep {$lexicon{$_} > 0} keys %lexicon),
		 scalar(grep {$lexicon{$_} == 2} keys %lexicon),
		 scalar(grep {$lexicon{$_} == 0} keys %lexicon),
		 join(", ", grep {$lexicon{$_} == 0} keys %lexicon)
		)
      if $debug;
  }
}

sub in_network {
  my $m = shift;
  return ($go->is_descendant_of($m, $root) && !$mods{$m}{skipped});
}

sub new_parents {
  my $m = shift;
  return () unless $go->is_descendant_of($m, $root);
  my @ps = $go->parents($m); 
  my @new = grep { in_network($_) } @ps;
  if ($debug) {
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent = 0;
    warn sprintf("new_parents($m): immediate: %s; out of: %s\n",
		 Dumper(\@new), Dumper(\@ps));
  }
  return @new if scalar(@new);
  @new = Set::Scalar->new(map {new_parents($_)} @ps)->members;
  if ($debug) {
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent = 0;
    warn sprintf("new_parents($m): distant: %s\n", Dumper(\@new));
  }
  return @new;
}

## Lexical functions

# returns:
# 0 if no match
# 1 if perfect match (recorded)
# 2 if prefixed match (recorded; implies 1)
sub check_lex {
  my $name = shift;
  foreach my $w (keys %lexicon) {
    if ($name =~ m/^protein ((?:cis|de|auto|trans|poly|mono)-?)*$w$/i) {
      return $lexicon{$w} = ($1 ? 2 : 1);
    }
  }
  return 0;
}

# check if a name simply adds modifiers to a more generic name
# test: protein Y X << protein X
# TODO: X Y << X 
sub is_more_specific {
  my ($a, $b) = @_;
  $b =~ m/^(protein) (.*)$/;
  my ($p, $m) = ($1, $2);
  return ($a =~ m/^$p( [\w-]+)+ $m$/);
}


## DOT functions

sub to_dot {
  my ($file) = @_;

  open my $dot, ">$file"
    or die $!;

  sub node_name {
    my $n = shift; $n =~ s/:/_/; $n;
  }

  sub print_node {
    my ($id, %opts) = @_;
    printf $dot "%s [label=\"%s\"%s]\n",
      node_name($id),
      sprintf("%s\\n%s", $id, $go->get_term_name($id)),
      join(",", "", map { $_.'='.$opts{$_} } keys %opts);
  }

  sub print_edge {
    my ($a, $b, %opts) = @_;
    printf $dot "%s -> %s [%s]\n",
      node_name($a), node_name($b),
      join(",", map { $_.'='.$opts{$_} } keys %opts);
  }

  sub node_color {
    my $code = shift;
    return ('azure', 'cyan', 'seagreen1')[$code];
  }
  
  # top
  print $dot "digraph GoProtMods {\nrankdir=LR;splines=true;overlap=false\nnode [shape=box,color=blue]\n\n";

  # root
  print_node($root,
	     'color' => 'red',
	     'style' => 'filled',
	     'fillcolor' => 'mistyrose');

  # nodes
  my @mod_ids =
    sort { length($go->get_term_name($a))
	   <=>
	   length($go->get_term_name($b)) }
    grep { $_ ne $root }
    keys %mods;
  foreach my $m (@mod_ids) {
    next if ($mods{$m}{skipped});
    print_node($m,
	       'style' => 'filled',
	       'fillcolor' => node_color($mods{$m}{lex}));
    my @isa = @{ $mods{$m}{is_a} };
    if (@isa) {
      foreach my $p (@{$mods{$m}{is_a}}) {
	print_edge($m, $p);
      }
    } else {
      foreach my $p (@{$mods{$m}{is_d}}) {
	print_edge($m, $p,  'style' => 'dashed');
      }
    }
  }
  
  # bottom
  print $dot "}\n";

  close $dot;
}
  
## OBO functions
  
sub to_obo {
  my ($file, $mods) = @_;

  our $obo;
  
  sub header {
    printf $obo "format-version: 1.2
date: %s
saved-by: lgalescu
auto-generated-by: $0
subsetdef: go_protmod \"GO Protein Modifications\"
default-namespace: gene_ontology
ontology: go_protmods\n\n", _timestamp();
  }
  
  sub stanza {
    my $id = shift;
    my $name = $go->get_term_name($id);
    printf $obo "[Term]\n";
    printf $obo "id: %s\n", $id;
    printf $obo "name: %s\n", $name;
    printf $obo "namespace: biological_process\n";
    my @syns = $go->get_term_synonyms($id, 'synonym', 'EXACT' => 1);
    foreach my $syn (@syns) {
      # eliminate more verbose synonyms
      next if is_more_specific($syn, $name);
      # eliminate shortcuts; we're going to deal with these later
      next if $name eq "protein " .  $syn;
      printf $obo "synonym: \"%s\" EXACT []\n", $syn;
    }
    printf $obo "subset: go_protmod\n";
  }

  open $obo, ">$file"
    or die $!;
  # prologue
  header();
  my $n = 0;
  foreach my $m (keys %mods) {
    next if $mods{$m}{skipped};
    $n++;
    stanza($m);
    my @isas = @{ $mods{$m}{is_a} };
    if (@isas) {
      foreach my $p (@isas) {
	printf $obo "is_a: %s ! %s\n", $p, $go->get_term_name($p);
      }
    } elsif ($m eq $root) {
      # nop
    } else {
      foreach my $p (@{$mods{$m}{is_d}}) {
	printf $obo "is_a: %s ! %s\n", $p, $go->get_term_name($p);
      }
      printf $obo "comment: The is-a relation is derived from GO by skipping some intermediary terms.\n";
    }
    print $obo "\n";
  }
  close $obo;
  warn "Wrote $n terms to OBO file\n";
}

## varia

sub _timestamp
{
  my ($sec,$min,$hour,$mday,$mon,$year) = localtime(time);
  sprintf("%02d:%02d:%4d %02d:%02d",$mday,$mon+1,$year+1900,$hour,$min);
}

