#!/usr/bin/perl

# Store.pm
#
# Time-stamp: <Sat Nov  3 20:16:07 CDT 2018 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>,  3 Nov 2018
#

#----------------------------------------------------------------
# Description:
# 

#----------------------------------------------------------------
# History:
# 2018/11/03 v1.0 lgalescu
# - Created by moving here functionality from EKBAgent.pm.
# - Changed index to be based on document IDs instead of file locations.

#----------------------------------------------------------------
# Usage:
# 
package EKB::Store;

$VERSION = '1.0';

use strict 'vars';
use warnings;

use Data::Dumper;
use Cwd 'realpath';
use File::Basename;
use File::Spec::Functions;
use List::Util qw(first);

use EKB;

use util::Log;

sub new {
  my $class = shift;
  my $self = {
	      # path to store folder
	      path => undef,
	      # data is organized by document (via a unique document id):
	      # { <docid> => { path => <doc_path>, ekb => <ekb> } }
	      data => undef, 
	      # gensym counters: { prefix => count }
	      symbol_table => {},
	      };
  bless $self, $class;
  if (@_) {
    my $p = shift;
    $self->setPath($p);
    $self->load();
  }
  return $self;

}

sub setPath {
  my ($self, $path) = @_;
  $self->{path} = $path;
  DEBUG 2, "Store path: $path";
}

sub getPath {
  my $self = shift;
  return $self->{path};
}

# returns a map from document to ekb file
sub getIndex {
  my $self = shift;
  return
    map { $_ => $self->{data}{$_}{ekb}->get_file } keys %{ $self->{data} };
}
  
# loads EKB store info into $self->{data}
sub load {
  my $self = shift;

  my $path = $self->getPath;
  opendir my($store), $path
     or do {
       ERROR("Cannot open $path");
       return;
     };

  my @ekb_files = grep { /\.ekb$/ } readdir $store;
  DEBUG 1, "Found %d ekbs: %s", scalar(@ekb_files), "@ekb_files";
  # clean index first
  $self->{data} = {};
  foreach my $ekb_file (@ekb_files) {
    my $ekb_path = catfile($path, $ekb_file);
    $self->load_ekb($ekb_path);
  }

  if ($self->empty) {
    WARN "No document EKBs in store!";
  } else {
    DEBUG 1, "Store has EKBs for %d documents", scalar(keys %{$self->{data}});
  }
}

# load EKB file from store
sub load_ekb {
  my ($self, $ekb_file) = @_;
  
  DEBUG 2, "Loading from store: %s", $ekb_file;
  my $ekb = EKB->new($ekb_file);
  $ekb->normalize( { keep_lisp => 1, unique_ids => 1 } );
  my @docs = $ekb->get_docs;
  foreach my $d (@docs) {
    $self->add_doc($d->getAttribute('file'), $ekb_file);
  }
}

# add EKB file to store
sub add_ekb {
  my ($self, $ekb) = @_;

  my $ekb_file = catfile($self->getPath(), basename($ekb->get_file));
  $ekb->set_file($ekb_file);
  $ekb->normalize( { keep_lisp => 1, unique_ids => 1 } );
  $ekb->print($ekb_file);

  DEBUG 2, "Added to store: %s", $ekb_file;
  my @docs = $ekb->get_docs;
  foreach my $d (@docs) {
    $self->add_doc($d->getAttribute('file'), $ekb_file);
  }
}

# remove EKB file from store
sub remove_ekb {
  my ($self, $ekb_file) = @_;

  my $ekb_dir = dirname(realpath($ekb_file));
  if ($ekb_dir ne realpath($self->{path})) {
    ERROR "$ekb_file is not part of the EKB store";
    return;
  }
  unless (-f $ekb_file) {
    ERROR "File not found: $ekb_file";
    return;
  }
  
  unlink $ekb_file;
  INFO "Removed ekb: $ekb_file";
  # update the store (reload);  FIXME: should do it in memory
  $self->load;
  foreach my $d (keys %{$self->{data}}) {
    if ($self->{data}{$d}{ekb} eq $ekb_file) {
      delete $self->{data}{$d};
    }
  }
  
  return 1;
}

# add <doc_path, ekb> to EKB store
# Side effect: we generate a new docid, to ensure uniqueness during session
sub add_doc {
  my ($self, $doc_path, $ekb_path) = @_;

  my $ekb = EKB->new($ekb_path);
  $ekb->normalize( { keep_lisp => 1, unique_ids => 1 } );
  my $doc_elem =
    first { $_->getAttribute('file') eq $doc_path } $ekb->get_docs;
  my $old_docid = $doc_elem->getAttribute('id');
  $ekb->filter( { documents => [ $old_docid ] });
  my $docid = $self->_gensym("D", { padding => 4});
  $ekb->set_docid($old_docid, $docid);
  $ekb->save;

  # TODO: check that the ekb is not empty!!!
  
  my $doc_basename = basename($doc_path, (".xml", ".txt"));
  $ekb->print($doc_basename . ".ekb");

  # remove the document, if it is already in store;
  my $curr_docid = $self->find_doc($doc_path);
  if ($curr_docid) {
    $self->remove_doc($curr_docid);
  }
  
  $self->{data}{$docid} = { path => $doc_path,
			    ekb => $ekb };
  DEBUG 2, "Document in store: %s (%s => %s)",
    $docid, $doc_path, $ekb->get_file();
}

# get EKB for a document from the EKB store; returns undef if none is found
sub get_doc {
  my ($self, $docid) = @_;

  if ($self->empty) {
    ERROR "No document EKBs in store!";
    return;
  }
  unless (exists $self->{data}{$docid}) {
    ERROR "Document not found in store: $docid";
    return;
  }

  return $self->{data}{$docid}{ekb};
}

# find if document with given path is in store
sub find_doc {
  my ($self, $doc_path) = @_;
  grep { $self->{data}{$_}{path} eq $doc_path }
    keys %{$self->{data}};
}

# remove doc with given id from store
sub remove_doc {
  my ($self, $docid) = @_;

  my $doc_ekb = $self->get_doc($docid)
    or do {
      return;
    };

  my $ekb_file = $doc_ekb->get_file;
  DEBUG 2, "Found $docid in $ekb_file";

  my $ekb = EKB->new($ekb_file);
  $ekb->remove_doc($docid);
  my @docids = map { $_->getAttribute('id') } $ekb->get_docs;
  if (@docids) {
    $ekb->save;
    WARN "Removed $docid from $ekb_file";
  } else {
    unlink $ekb_file;
    WARN "Deleted $ekb_file (docid: $docid)";
  }

  delete $self->{data}{$docid};
  1;
}


# check if EKB store is empty
sub empty {
  my ($self) = @_;
  return !scalar(keys %{$self->{data}});
}

# query EKBs from store
sub query {
  my ($self, $query) = @_;

  my @result;
  foreach my $docid (keys %{ $self->{data} }) {
    WARN "Checking $docid...";
    my $ekb = $self->{data}{$docid}{ekb};
    my @assertions = $ekb->query_assertions($query);
    WARN "Got %d assertions from %s", scalar(@assertions), $ekb->get_file();
    next unless @assertions;
    my %uttnums;
    foreach my $a (@assertions) {
      $uttnums{ $a->getAttribute('uttnum') } = 1;
    }
    $ekb->filter( { sentences => [ keys(%uttnums) ] });
    push @result, $ekb;
  }
  return \@result;
}

# private functions

sub _new_id {
  my ($self, $prefix, $length) = @_;
  $self->_gensym($prefix // "D", { padding => $length // 4 });
}

sub _gensym {
  my ($self, $prefix, $opt) = @_;
  my $n_fmt = "";
  if (ref($opt) eq 'HASH') {
    $n_fmt = ("0" . $opt->{'padding'}) // "";
  }
  return sprintf("%s%${n_fmt}d", $prefix, ++($self->{symbol_table}{$prefix}));
}


1;
