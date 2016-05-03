#!/usr/bin/perl

package Graphviz;
use TripsModule::TripsModule;
use Graphviz::Configuration;
@ISA = qw(TripsModule);
use strict vars;

sub init {
  my $self = shift;
  $self->{name} = "Graphviz";
  $self->{graphnum} = 0;
  $self->SUPER::init();
  $self->handle_parameters();
}

sub handle_parameters {
  my $self = shift;
  $self->{usage} .=
    " [-display-enabled boolean]" .
    " [-graphviz-cmd cmd]";
  # defaults
  if (`uname` =~ /^Darwin/) {
    # use this on Mac
    $self->{graphviz_cmd} = "open -a Graphviz %s";
  } else {
    # use this if you don't have ImageMagick, or if you want to edit the graph
    # $self->{graphviz_cmd} = "dotty %s";
    # use this if you want a better-looking graph
    $self->{graphviz_cmd} = "dot -Tpng %s |display -alpha Off -resize \"70%\" -sharpen 1 -";
  }
  $self->{display_enabled} = 0;

  while (@{$self->{argv}}) {
    my $opt = shift @{$self->{argv}};
    if ($opt eq '-display-enabled') {
      $self->{display_enabled} = TripsModule::boolean_opt($opt, shift @{$self->{argv}});
    } elsif ($opt eq '-graphviz-cmd') {
      $self->{graphviz_cmd} = shift @{$self->{argv}};
      die "-graphviz-cmd must contain '%s' so it can be substituted with the name of a dot file" unless ($self->{graphviz_cmd} =~ /%s/);
    } else {
      die "Unknown option: '$opt'\n" . $self->{usage} . "\n";
    }
  }
  $SIG{CHLD} = 'IGNORE';
  $self->send_msg("(subscribe :content (tell &key :content (lf-graph . *)))");
}

sub escape_for_quotes { 
  my $str = shift;
  $str =~ s/\\/\\\\/g;
  $str =~ s/"/\\"/g;
  return $str;
}

sub receive_tell {
  my ($self, $msg, $content) = @_;
  $content->{verb} = lc($content->{verb});
  eval {
    if ($content->{verb} eq 'lf-graph') {
      my $graph = $content->{':content'};
      die "'lf-graph' tell is missing a :content string"
	unless (KQML::KQMLAtomIsString($graph));
      $graph = KQML::KQMLStringAtomAsPerlString($graph);
      my $graphfile = sprintf("lf-graph-%03d.dot", $self->{graphnum}++);
      open GRAPHVIZ, ">$graphfile" or die "Can't write graph file $graphfile: $!";
      print GRAPHVIZ $graph;
      close GRAPHVIZ;
      if ($self->{display_enabled}) {
	my $pid = fork;
	if ($pid == 0) { # child 
	  exec("sh", "-c", sprintf($self->{graphviz_cmd}, $graphfile));
	}
      }
    } else {
      die "Unknown tell verb " . $content->{verb};
    }
    return 1
  } || $self->reply_to_msg($msg, "(tell :content (reject :reason \"" . escape_for_quotes($@) . "\"))\n");
}

sub receive_request {
  my ($self, $msg, $content) = @_;
  $content->{verb} = lc($content->{verb});
  eval {
    if ($content->{verb} eq 'disable-display') {
      $self->{display_enabled} = 0;
    } elsif ($content->{verb} eq 'enable-display') {
      $self->{display_enabled} = 1;
    } else {
      die "Unknown request verb " . $content->{verb};
    }
    return 1
  } || $self->reply_to_msg($msg, "(tell :content (reject :reason \"" . escape_for_quotes($@) . "\"))\n");
}

Graphviz->new(@ARGV)->run();

