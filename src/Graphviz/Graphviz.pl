#!/usr/bin/perl

package Graphviz;
use TripsModule::TripsModule;
use Graphviz::Configuration;
use Graphviz::Escape qw(escape_for_quotes escape_for_dot);
use Graphviz::Errors qw(nested_error unknown_action missing_argument invalid_argument unknown_object get_typed_value get_typed_argument);
use Graphviz::WindowManager;
use File::Spec;
@ISA = qw(TripsModule);

use strict vars;

sub init {
  my $self = shift;
  $self->{name} = "Graphviz";
  $self->{graphnum} = 0;
  $self->{wm} = Graphviz::WindowManager->new($self);
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
  #$SIG{CHLD} = 'IGNORE';
  $self->send_msg("(subscribe :content (tell &key :content (lf-graph . *)))");
  $self->send_msg("(subscribe :content (request &key :content (update-plan-gui . *)))");
  # talk to ourselves in order to get a report that the user closed a window
  # into the main process
  $self->send_msg("(subscribe :content (tell &key :sender $self->{name} :content (report &key :content (closed :what * :who usr))))");
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
    } elsif ($content->{verb} eq 'report' and lc($msg->{':sender'}) eq lc($self->{name})) {
      my $report = KQML::KQMLKeywordify($content->{':content'});
      if ($report->{verb} eq 'closed' and lc($report->{':who'}) eq 'usr' and
	  exists($report->{':what'})) {
	# we're telling ourself that the user closed a window
	$self->{wm}->closed_window($report->{':what'});
      }
    } else {
      die "Unknown tell verb " . $content->{verb};
    }
    return 1
  } || $self->reply_to_msg($msg, "(tell :content (reject :reason \"" . escape_for_quotes($@) . "\"))\n");
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
  $filename = File::Spec->rel2abs($filename);
  # NOTE: filename doesn't include version, because we want to have the new
  # version show up in the same window as the old
  my $graph_name = escape_for_dot("$plan->{id}v$plan->{version}");
  open my $fh, ">$filename" or die "can't open $filename for writing: $!";
  print $fh <<EOH;
digraph $graph_name {
  graph [rankdir=LR]
  node [shape=none]
EOH
  for my $id (keys %{$plan->{parts}}) {
    my $part = $plan->{parts}{$id};
    my $verb = lc($part->{verb});
    if ($verb eq 'state') {
      my $caption = ($part->{state_index} == 0 ? "Begin state" : ($part->{state_index} == $plan->{num_states}-1 ? "End state" : "Intermediate state " . $part->{state_index}));
      print $fh qq(  "$id" [label=<\n     <table border="0" cellspacing="0" cellborder="1" align="left">\n     <tr><td border="0">$caption</td></tr>\n);
      for my $status (qw(achieved unknowns knowns)) {
	if (exists($part->{$status})) {
	  print $fh qq(     <tr><td height="100">\n      <table border="0" cellborder="0" align="center">\n       <tr><td align="left" valign="top"><font point-size="10">$status</font></td></tr>\n);
	  for my $param (@{$part->{$status}}) {
	    my $param_id = $param->{':id'};
	    my $id_nopkg = nopkg($param_id);
	    exists($param->{':id-code'}) or
	      die missing_argument($param->{verb}, ':id-code');
	    my $id_code = KQML::KQMLAsString($param->{':id-code'});
	    print $fh qq(     <tr><td port="$id_nopkg">$id_nopkg);
	    my $arguments =
	      get_typed_argument($param, ':arguments', 'list', []);
	    if (@$arguments) {
	      print $fh ": $id_code";
	      for my $arg_id (@$arguments) {
		exists($plan->{params}{$arg_id}) or die unknown_object($arg_id);
		my $arg = $plan->{params}{$arg_id};
		my $arg_id_code = get_typed_argument($arg,':id-code', 'symbol');
		print $fh qq(<br/>$arg_id_code $arg_id: );
		if (exists($arg->{':value'}) and
		    lc($arg->{':value'}) ne 'nil') {
		  print $fh KQML::KQMLAsString($arg->{':value'});
		} else {
		  print $fh '?';
		}
	      }
	      if (exists($param->{':value'}) and
		  lc($param->{':value'}) ne 'nil') {
		# value after arguments, treat it as the :value argument
		my $value = KQML::KQMLAsString($param->{':value'});
		print $fh qq(<br/>:value $value);
	      }
	    } elsif (exists($param->{':value'}) and
		     lc($param->{':value'}) ne 'nil') {
	      # value after no arguments, just put it inline
	      print $fh ': ' . KQML::KQMLAsString($param->{':value'});
	    } else {
	      # no value after no arguments
	      print $fh ": ?";
	    }
	    print $fh qq(</td></tr>\n);
	  }
	  print $fh qq(      </table>\n     </td></tr>\n);
	}
      }
      print $fh qq(    </table>\n  >]\n);
    } elsif ($verb eq 'operator') {
      my $from = get_typed_argument($part, ':from', 'symbol');
      my $to = get_typed_argument($part, ':to', 'symbol');
      my $from_map = get_typed_argument($part, ':from-map', 'list', 0);
      my $to_map = get_typed_argument($part, ':to-map', 'list', 0);
      my $service = get_typed_argument($part, ':service', 'symbol', 0);
      if ($from_map and $to_map and
	  $service and lc($service) ne 'nil') { # service node
	print $fh qq(  "$id" [label="$service"]\n);
	my @constraining_edges = ();
	my @unconstraining_edges = ();
	for my $in_edge (@$from_map) {
	  my ($label, $tail_id);
	  eval {
	    @$in_edge == 2 or die;
	    ($label, $tail_id) =
	      map { get_typed_value('symbol', $_) } @$in_edge;
	    1
	  } || die invalid_argument($part, ':from-map', 'list of pairs of symbols');
	  exists($plan->{params}{$tail_id}) or die unknown_object($tail_id);
	  my $param = $plan->{params}{$tail_id};
	  my $id_nopkg = nopkg($tail_id);
	  my $unconstraining = ($param->{state} ne $from);
	  my $str =
	    qq(  "$param->{state}" -> "$id" [tailport="$id_nopkg",label="$label") .
	    ($unconstraining ? qq(,constraint=false) : '') .
	    qq(]\n);
	  if ($unconstraining) {
	    push @unconstraining_edges, $str;
	  } else {
	    push @constraining_edges, $str;
	  }
	}
	for my $out_edge (@$to_map) {
	  my ($label, $head_id);
	  eval {
	    @$out_edge == 2 or die;
	    ($label, $head_id) =
	      map { get_typed_value('symbol', $_) } @$out_edge;
	    1
	  } || die invalid_argument($part, ':to-map', 'list of pairs of symbols');
	  exists($plan->{params}{$head_id}) or die unknown_object($head_id);
	  my $param = $plan->{params}{$head_id};
	  my $unconstraining = ($param->{state} ne $to);
	  my $id_nopkg = nopkg($head_id);
	  my $str =
	    qq(  "$id" -> "$param->{state}" [headport="$id_nopkg",label="$label") .
	    ($unconstraining ? qq(,constraint=false) : '') .
	    qq(]\n);
	  if ($unconstraining) {
	    push @unconstraining_edges, $str;
	  } else {
	    push @constraining_edges, $str;
	  }
	}
	# print constraining edges before unconstraining ones to help dot
	# arrange nodes using the constraining ones
	print $fh @constraining_edges, @unconstraining_edges;
      } else { # gap edge
	print $fh qq(  "$from" -> "$to" [label="???",color=gray,penwidth=3,minlen=3]\n); # FIXME minlen?
      }
    } else {
      die unknown_action($verb); # FIXME maybe?
    }
  }
  print $fh "}\n";
  close $fh;
  return $filename;
}

sub update_plan {
  my ($self, $content) = ($_[0], KQML::KQMLKeywordify($_[1]));
  my $plan = {
    id => get_typed_argument($content, ':id', 'symbol'),
    version => get_typed_argument($content, ':version', 'integer', 0),
    parts => {}, # states and operators (that become nodes and edges)
    params => {} # parameters (that become ports on state nodes)
  };
  # TODO more input format checking and output string escaping
  my $i = 0;
  for my $kpart (get_typed_argument($content, ':initial-state', 'list'),
		 @{get_typed_argument($content, ':intermediate-states','list', [])},
		 get_typed_argument($content, ':goal-state', 'list')) {
    my $part = undef;
    eval {
      $part = get_typed_value('performative', $kpart);
      1
    } || die ['failure', ':type', 'failed-to-interpret', ':reason',
    	       ['invalid-argument',
	         ':operator', $content->{verb},
	         ':argument', [qw(or :initial-state :intermediate-states :goal-state)],
		 ':expected', '"performative"',
		 ':got', $kpart]];
    $part->{state_index} = $i++;
    my $part_id = get_typed_argument($part, ':id', 'symbol');
    $plan->{parts}{$part_id} = $part;
    for my $status (qw(achieved unknowns knowns)) {
      my $params = get_typed_argument($part, ":$status", 'list', []);
      if (@$params) {
	$part->{$status} = []; # NOTE: no colon
	for my $param_kqml (@$params) {
	  my $param = undef;
	  eval {
	    $param = get_typed_value('performative', $param_kqml);
	    1
	  } || die invalid_argument($part, ":$status", 'performative');
	  # save the state this param is part of, so that we can make edges
	  # properly (NOTE: :from-map edges don't always come from the :from
	  # state)
	  $param->{state} = $part_id;
	  push @{$part->{$status}}, $param;
	  my $param_id = get_typed_argument($param, ':id', 'symbol');
	  $plan->{params}{$param_id} = $param;
	}
      }
    }
  }
  $plan->{num_states} = $i;
  for my $kpart (@{get_typed_argument($content, ':causal-gaps', 'list', [])},
		 @{get_typed_argument($content, ':graph', 'list', [])}) {
    my $part = undef;
    eval {
      $part = get_typed_value('performative', $kpart);
      1
    } || die ['failure', ':type', 'failed-to-interpret', ':reason',
    	       ['invalid-argument',
	         ':operator', $content->{verb},
	         ':argument', [qw(or :causal-gaps :graph)],
		 ':expected', '"performative"',
		 ':got', $kpart]];
    my $part_id = get_typed_argument($part, ':id', 'symbol');
    $plan->{parts}{$part_id} = $part;
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
	#$self->display_graph_file($file);
	my $opened = $self->{wm}->display_graph($file, {}); # TODO config
	my $reply =
	  [ 'tell', ':content', ['report', ':content', $opened],
	    (exists($msg->{':reply-with'}) ?
	      (':in-reply-to', $msg->{':reply-with'}) : ())
	  ];
	$self->send_msg($reply);
        return 1
      # CWMS-style failure report
      } || $self->reply_to_msg($msg,
	     [ 'tell', ':content', ['report', ':content',
	       nested_error('', $@)
	     ]]);
    } elsif (grep { $_ eq $content->{verb} } @{$self->{wm}{verbs}}) {
      $self->{wm}->handle_window_management_request($msg, $content);
    } else {
      die "Unknown request verb " . $content->{verb};
    }
    return 1
  } || $self->reply_to_msg($msg, "(tell :content (reject :reason \"" . escape_for_quotes($@) . "\"))\n");
}

Graphviz->new(@ARGV)->run();

