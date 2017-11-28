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
  $self->send_msg("(subscribe :content (request &key :content (update-plan-gui . *)))");
}

sub escape_for_quotes { 
  my $str = shift;
  $str =~ s/\\/\\\\/g;
  $str =~ s/"/\\"/g;
  return $str;
}

sub display_graph_file {
  my ($self, $graphfile) = @_;
  # TODO keep track of child so we can kill it later to hide a graph if we want (or in the case of Graphviz.app, tell it to close?)
  my $pid = fork;
  if ($pid == 0) { # child 
    exec("sh", "-c", sprintf($self->{graphviz_cmd}, $graphfile));
  }
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
	$self->display_graph_file($graphfile);
      }
    } else {
      die "Unknown tell verb " . $content->{verb};
    }
    return 1
  } || $self->reply_to_msg($msg, "(tell :content (reject :reason \"" . escape_for_quotes($@) . "\"))\n");
}

# CWMS-style failure reports
sub program_error {
  my $msg = shift;
  return ['failure', ':type', 'cannot-perform', ':reason',
    ['program-error', ':message', '"' . escape_for_quotes($msg) . '"']];
}

sub missing_argument {
  my ($op, $arg) = @_;
  return ['failure', ':type', 'failed-to-interpret', ':reason',
    ['missing-argument',
      ':operator', $op,
      ':argument', $arg
    ]
  ];
}

sub invalid_argument {
  my ($operation, $argument, $expected) = @_;
  my $op = $operation->{verb};
  my $got = $operation->{$argument};
  return ['failure', ':type', 'failed-to-interpret', ':reason',
    ['invalid-argument',
      ':operator', $op,
      ':argument', $argument,
      ':expected', '"' . escape_for_quotes($expected) . '"',
      ':got', $got
    ]
  ];
}

sub unknown_action {
  my $what = shift;
  return ['failure', ':type', 'failed-to-interpret', ':reason',
    ['unknown-action', ':what', $what]
  ];
}

sub unknown_object {
  my $what = shift;
  return ['failure', ':type', 'cannot-perform', ':reason',
    ['unknown-object', ':what', $what]
  ];
}

my %justify2nl = ("center" => "\\n", "left" => "\\l", "right" => "\\r");

sub escape_for_dot {
  my ($str, $justify) = @_;
  return $str unless ($str =~ /\W|^\d/); # return if valid ID, no quotes needed
  return $str if ($str =~ /^-?(?:\.\d+|\d+(?:\.\d*)?)$/); # numbers are IDs too
  $justify ||= "left";
  my $nl = $justify2nl{$justify};
  $str =~ s/\\/\\\\/g;
  $str =~ s/"/\\"/g;
  $str =~ s/\r\n|\n\r|\r|\n/$nl/g;
  return "\"$str\"";
}

# remove the package from a symbol string (including keywords)
sub nopkg {
  my $str = shift;
  $str =~ s/^(?:[\w-]+:)?://;
  return $str;
}

sub write_plan_graph {
  my ($self, $plan) = @_;
  my $filename = $plan->{id};
  $filename =~ s/[^\w-]/_/g; # sanitize filename
  $filename .= '.dot';
  # NOTE: filename doesn't include version, because we want to have the new
  # version show up in the same window as the old
  my $graph_name = escape_for_dot("$plan->{id}v$plan->{version}");
  my $knowns_str = '';
  my $prev = 'known_heading';
  for (@{$plan->{knowns}}) {
    my $ps = escape_for_dot($_->{id});
    my $label = nopkg($_->{id}) . ": [" . nopkg($_->{name}) . " $_->{value}]";
    $knowns_str .=
      "    $prev -> $ps\n" .
      "    $ps " .
      "[label=" . escape_for_dot($label) .
        (exists($_->{color}) ? ",fontcolor=$_->{color}" : "") .
      "]\n";
    $prev = $ps;
  }
  my $operators_str = '';
  for (@{$plan->{operators}}) {
    $operators_str .=
      '    ' . escape_for_dot($_->{id}) .
      ' [label=' . escape_for_dot($_->{label}) .
        (exists($_->{color}) ? ",color=$_->{color}" : "") .
      "]\n";
  }
  my $goals_str = '';
  $prev = 'unknown_heading';
  for (@{$plan->{goals}}) {
    my $ps = escape_for_dot($_->{id});
    my $label =
      nopkg($_->{id}) . ": [" . nopkg($_->{name}) .
	join('', map {
	  my $arg_id = $_;
	  my ($arg) = grep { $_->{id} eq $arg_id } @{$plan->{knowns}};
	  "\n\t$arg->{name} " . (($arg->{value} =~ /^nil$/i) ? '?' : $arg->{value})
	} @{$_->{arguments}}) .
      "]";
    $goals_str .=
      "    $prev -> $ps\n" .
      "    $ps " .
      "[label=" . escape_for_dot($label) .
        (exists($_->{color}) ? ",fontcolor=$_->{color}" : "") .
      "]\n";
    $prev = $ps;
  }
  my $edges_str = '';
  for (@{$plan->{links}}) {
    $edges_str .=
      "  " . escape_for_dot($_->{source}, "center") .
      " -> " . escape_for_dot($_->{target}, "center") .
      " [label=" . escape_for_dot(nopkg($_->{label})) .
        (exists($_->{color}) ? ",color=$_->{color}" : "") .
      "]\n";
  }
  open my $fh, ">$filename" or die "can't open $filename for writing: $!";
  print $fh <<EOG;
digraph $graph_name {
  graph [rankdir=LR,nodesep=0.1,splines=line]
  node [shape=box,fontcolor=white,color=gray40,fillcolor=gray40,style="rounded,filled",fontname="sans-serif"]
  subgraph knowns {
    graph [rank=source]
    node [shape=none,fontcolor=black,fontsize=12,style=""]
    edge [color=white]
    known_heading [label="Known\\nParameters",fontsize=16]
    $knowns_str
  }
  subgraph goals {
    graph [rank=sink]
    node [shape=none,fontcolor=black,fontsize=12,style=""]
    edge [color=white]
    unknown_heading [label="Goal (unknown)\\nParameters",fontsize=16]
    $goals_str
  }
  $edges_str
  $operators_str
}
EOG
  close $fh;
  return $filename;
}

sub update_plan {
  my ($self, $content) = ($_[0], KQML::KQMLKeywordify($_[1]));
  my $verb = lc($content->{verb});
  exists($content->{':id'}) or die missing_argument($verb, ':id');
  my $plan_id = $content->{':id'};
  my $plan = undef;
  if ($verb eq 'new-plan') {
    $plan = $self->{plans}{$plan_id} = +{
      id => $plan_id,
      knowns => [],
      operators => [],
      goals => [],
      links => []
    };
  } elsif (exists($self->{plans}{$plan_id})) {
    $plan = $self->{plans}{$plan_id};
    # remove red color of formerly-new graph elements
    for my $elem (@{$plan->{knowns}}, @{$plan->{operators}}, @{$plan->{goals}},
		  @{$plan->{links}}) {
      delete $elem->{color};
    }
  } else {
    die unknown_object($plan_id);
  }
  $plan->{version} = $content->{':version'};
  if (exists($content->{':knowns'})) {
    # TODO be consistent between map and foreach
    # TODO factor out common parts of these loops
    my $i = 0;
    push @{$plan->{knowns}}, map {
      my $p = KQML::KQMLKeywordify($_);
      defined($p) or die invalid_argument($content->{':knowns'}, $i, 'list');
      exists($p->{':id'}) or die missing_argument('parameter', ':id');
      exists($p->{':id-code'}) or die missing_argument('parameter', ':id-code');
      $i++;
      +{
	id => $p->{':id'},
	name => KQML::KQMLAsString($p->{':id-code'}),
	value =>
	  (exists($p->{':value'}) ? KQML::KQMLAsString($p->{':value'}) : '?'),
	color => 'red' # new
      }
    } @{$content->{':knowns'}};
  }
  if (exists($content->{':goals'})) {
    my $i = 0;
    push @{$plan->{goals}}, map {
      my $p = KQML::KQMLKeywordify($_);
      defined($p) or die invalid_argument($content->{':goals'}, $i, 'list');
      exists($p->{':id'}) or die missing_argument('parameter', ':id');
      exists($p->{':id-code'}) or die missing_argument('parameter', ':id-code');
      $i++;
      +{
	id => $p->{':id'},
	name => KQML::KQMLAsString($p->{':id-code'}),
	arguments =>
	  ((ref($p->{':arguments'}) eq 'ARRAY') ? $p->{':arguments'} : []),
	value =>
	  (exists($p->{':value'}) ? KQML::KQMLAsString($p->{':value'}) : '?'),
	color => 'red' # new
      }
    } @{$content->{':goals'}};
  }
  if ($verb eq 'new-operator') {
    exists($content->{':operator'}) or die missing_argument('new-operator', ':operator');
    my $op = KQML::KQMLKeywordify($content->{':operator'});
    defined($op) or die invalid_argument($content, ':operator', 'list');
    exists($op->{':id'}) or die missing_argument('operator', ':id');
    exists($op->{':service'}) or die missing_argument('operator', ':service');
    push @{$plan->{operators}}, +{
      id => $op->{':id'},
      label => $op->{':service'},
      color => 'red' # new
    };
    if (exists($op->{':from'}) and $op->{':from'} !~ /^nil$/i) {
      ref($op->{':from'}) eq 'ARRAY' or die invalid_argument($op, ':from', 'list');
      my $i = 0;
      for my $link (@{$op->{':from'}}) {
	(ref($link) eq 'ARRAY' and @$link == 2) or die invalid_argument($op->{':from'}, $i, 'pair');
	push @{$plan->{links}}, +{
	  source => $link->[1],
	  target => $op->{':id'},
	  label => $link->[0],
	  color => 'red' # new
	};
	$i++;
      }
    }
    if (exists($op->{':to'}) and $op->{':to'} !~ /^nil$/i) {
      ref($op->{':to'}) eq 'ARRAY' or die invalid_argument($op, ':to', 'list');
      my $i = 0;
      for my $link (@{$op->{':to'}}) {
	(ref($link) eq 'ARRAY' and @$link == 2) or die invalid_argument($op->{':to'}, $i, 'pair');
	push @{$plan->{links}}, +{
	  source => $op->{':id'},
	  target => $link->[1],
	  label => $link->[0],
	  color => 'red' # new
	};
	$i++;
      }
    }
  } elsif ($verb ne 'new-plan') {
    die unknown_action($verb);
  }
  return $plan;
}

sub receive_request {
  my ($self, $msg, $content) = @_;
  $content->{verb} = lc($content->{verb});
  eval {
    if ($content->{verb} eq 'disable-display') {
      $self->{display_enabled} = 0;
    } elsif ($content->{verb} eq 'enable-display') {
      $self->{display_enabled} = 1;
    } elsif ($content->{verb} eq 'update-plan-gui') {
      eval {
	exists($content->{':content'}) or die missing_argument('update-plan-gui', ':content');
	ref($content->{':content'}) eq 'ARRAY' or die invalid_argument($content, ':content', 'list');
	my $plan = $self->update_plan($content->{':content'});
	my $file = $self->write_plan_graph($plan);
	$self->display_graph_file($file);
        return 1
      # CWMS-style failure report
      } || $self->reply_to_msg($msg,
	     ['tell', ':content', ['report', ':content',
	       (ref($@) ? $@ : program_error($@))
	     ]]);
    } else {
      die "Unknown request verb " . $content->{verb};
    }
    return 1
  } || $self->reply_to_msg($msg, "(tell :content (reject :reason \"" . escape_for_quotes($@) . "\"))\n");
}

Graphviz->new(@ARGV)->run();

