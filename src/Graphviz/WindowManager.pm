package Graphviz::WindowManager;
# See also ../ImageDisplay/WindowManager.js and
# ../ChartDisplay/JavaFXWindowManager.java. This version mostly follows the
# JavaScript version, except that the WindowManager is an object (not a global
# windows variable) and its methods return answers synchronously (and throw
# failures as exceptions), instead of passing them to callbacks asynchronously.
# Also, the AppleScript used on the Mac side is different, since Graphviz.app
# isn't really AppleScriptable the way Preview is, so we have to go through
# System Events, which has a different interface.

use Data::Dumper;
use File::Spec;
use KQML::KQML;
use Graphviz::Escape qw(escape_for_quotes);
use Graphviz::Errors qw(unknown_action missing_argument invalid_argument unknown_object nested_error);

use strict vars;

my $debug = 0;

sub new {
  my ($class, $module) = @_;
  my $self = {
    module => $module,
    next_id_number => 0,
    windows => {}, # from KQML window IDs to (on Mac) app/process names and window titles, or (on Linux) PIDs and native (X11) window IDs
    verbs => [qw(get-windows describe configure close)] # request verbs we do
  };
  bless $self, $class;
};

sub get_windows {
  keys %{$_[0]{windows}};
}

sub next_id {
  my $self = shift;
  lc($self->{module}{name}) . '-win-' . ($self->{next_id_number}++)
}

sub closed_window {
  my ($self, $id) = @_;
  if (exists($self->{windows}{$id})) {
    waitpid $self->{windows}{close_pid}, 0;
    delete $self->{windows}{$id};
  }
}

if (`uname` =~ /^Darwin/) { # MacOS
# use AppleScript and Graphviz.app

print STDERR "On macOS; using AppleScript and Graphviz.app\n" if ($debug);

sub do_apple_script {
  my ($ing, @lines) = @_;
  print STDERR map { "$_\n" } @lines if ($debug);
  my @args = map { ('-e', $_) } @lines;
  my $pid = open KID, '-|';
  defined($pid) or die "while $ing: can't fork: $!";
  if ($pid) { # parent
    undef $/;
    my $output = <KID>;
    my $success = close KID;
    unless ($success) {
      if ($!) {
	die "while $ing: failed to close osascript output filehandle: $!";
      } else {
	die "while $ing: osascript failed with status $?";
      }
    }
    return $output;
  } else { # child
    exec('osascript', @args) or die "while $ing: can't exec osascript: $!";
  }
}

*display_graph = sub {
  my ($self, $filename, $config) = @_;
  my $app = 'Graphviz';
  # Graphviz.app uses the filename without the directory part as the title
  my $title = (File::Spec->splitpath($filename))[2];
  # tell Graphviz to open the file
  do_apple_script("spawning $app app",
    "tell application \"$app\"",
    "  open \"$filename\"",
    'end tell'
  );
  my $id = $self->next_id();
  $self->{windows}{$id} = { application => $app, title => $title };
  # configure the window
  my $config_reply = $self->configure_window($id, $config);
  # wait for the window to close in a background process
  my $close_pid = fork;
  defined($close_pid) or die "can't fork: $!";
  if ($close_pid) { # parent
    $self->{windows}{$id}{close_pid} = $close_pid;
    return ['opened', ':what', $id, ':who', 'sys'];
  } else { # child
    do_apple_script("waiting for $id to close",
      "tell application \"System Events\"",
      # FIXME: Identifying the window by its title is error prone, since
      # another window could potentially have the same title, but there's not
      # much we can do about that short of rewriting Graphviz.app. It probably
      # won't be a problem in practice, though.
      "  repeat while (exists first window of process \"$app\" where title is \"$title\")",
      '    delay 1',
      '  end repeat',
      'end tell'
    );
    # FIXME: same sync issue as Linux version below
    $self->{module}->send_msg(['tell', ':content', ['report', ':content', ['closed', ':what', $id, ':who', 'usr']]]);
    exit(0);
  }
};

*describe_window = sub {
  my ($self, $id) = @_;
  exists($self->{windows}{$id}) or die unknown_object($id);
  my $app = $self->{windows}{$id}{application};
  my $title = $self->{windows}{$id}{title};
  my $pos_and_size =
    do_apple_script("getting description of $id",
      "tell application \"System Events\"",
      "  set win to (first window of process \"$app\" where title is \"$title\")",
      '  {position, size} of win',
      'end tell'
    );
  $pos_and_size =~ s/\s+$//;
  my @f = split(', ', $pos_and_size);
  die "Malformed output from osascript while getting description of $id: $pos_and_size" unless (@f == 4);
  my ($left, $top, $width, $height) = @f;
  return ['answer', ':description', ['window',
    ':title', '"' . escape_for_quotes($title) . '"',
    ':left', $left, ':top', $top, ':width', $width, ':height', $height
  ] ];
};

# functions for constructing the AppleScript for configure_window

# NOTE: we can't set the title of a Graphviz window :(

sub add_set_bounds {
  my ($script, $reply, $bounds) = @_;
  push @$script,
    '  set position of win to {' .
    $bounds->{':left'} . ', ' . $bounds->{':top'} . '}',
    '  set size of win to {' .
    $bounds->{':width'} . ', ' . $bounds->{':height'} . '}';
  push @$reply, map { ($_, $bounds->{$_}) } qw(:left :top :width :height);
}

# FIXME? this does more than it needs to if only position or only size is set
*configure_window = sub {
  my ($self, $id, $config) = @_;
  my $ing = "configuring $id";
  exists($self->{windows}{$id}) or die unknown_object($id);
  my $app = $self->{windows}{$id}{application};
  my $title = $self->{windows}{$id}{title};
  my $reply = ['window', ':id', $id];
  my $answer = ['answer', ':description', $reply];
  my $script = [
    "tell application \"System Events\"",
    "  set win to (first window of process \"$app\" where title is \"$title\")"
  ];
  if (exists($config->{':title'})) {
    die "Can't set title of a Graphviz window";
  }
  if (exists($config->{':left'}) or exists($config->{':top'}) or
      exists($config->{':width'}) or exists($config->{':height'})) {
    # have some bounds info
    unless (exists($config->{':left'}) and exists($config->{':top'}) and
	    exists($config->{':width'}) and exists($config->{':height'})) {
      # don't have all the bounds
      # get the current bounds first
      my $old_config = $self->describe_window($id)->[2]; # :description
      $old_config = KQML::KQMLKeywordify($old_config);
      # use them as defaults to fill in the config
      $config = +{ %$old_config, %$config };
    }
    add_set_bounds($script, $reply, $config);
    push @$script, 'end tell';
    do_apple_script($ing, @$script);
  }
  return $answer;
};

*close_window = sub {
  my ($self, $id) = @_;
  exists($self->{windows}{$id}) or die unknown_object($id);
  my $app = $self->{windows}{$id}{application};
  my $title = $self->{windows}{$id}{title};
  my $close_pid = $self->{windows}{$id}{close_pid};
  delete $self->{windows}{$id};
  # kill the process waiting for the window to close so that we don't send two
  # close notifications
  kill 'TERM', $close_pid;
  waitpid $close_pid, 0;
  # tell the application to close the window by activating the app/win and
  # sending it a command-W keystroke
  do_apple_script("closing $id",
    'tell application "System Events"',
    # NOTE: activating the window doesn't work unless the app is active first
    "  tell application \"$app\" to activate",
    "  tell process \"$app\" to click menu item \"$title\" of menu \"Window\" of menu bar 1",
    '  keystroke "w" using command down',
    'end tell'
  );
  return ['closed', ':what', $id, ':who', 'sys'];
};

} else { # not MacOS, assume Linux or something else with X11
# use wmctrl, dot from graphviz, and display from ImageMagick

print STDERR "Not on macOS; using wmctrl, dot, and display\n" if ($debug);

# wait for a window to open with $id in the title, and return its native ID
sub wait_for_window_to_open {
  my ($self, $id) = @_;
  # poll every second, trying to raise the window with the (KQML) ID in the
  # title and get the native ID from wmctrl's verbose output
  my $wmc_output = '';
  for(;;) {
    $wmc_output = `wmctrl -R $id -v 2>&1`;
    my $exit_code = $?;
    last if ($exit_code == 0);
    print STDERR "wmctrl -R $id -v 2>&1\n$wmc_output", "exit code: $exit_code\n" if ($debug);
    sleep(1);
  }
  $wmc_output =~ /Using window: (0x[0-9a-f]{8})/
    or die "can't find native ID for $id";
  return $1;
}

*display_graph = sub {
  my ($self, $filename, $config) = @_;
  my $id = $self->next_id();
  # NOTE: putting the ID in the title helps us find it initially, so we can get
  # the native ID
  my $title = $id . ': ' . (File::Spec->splitpath($filename))[2];
  my $pid = fork;
  if ($pid == 0) { # child
    eval {
      # NOTE: this is mostly equivalent to the IO stuff below, except it's less secure and doesn't get killed properly
      #system("dot -Tpng \"$filename\" |display -title \"$title\" -alpha Off -resize \"70%\" -sharpen 1 -");
      # ugh, b01's "dot" doesn't work, use my gentoo install instead
      if (`hostname` eq "b01.cs.rochester.edu\n") {
        $ENV{PATH} = "/localdisk/gentoo/usr/bin:$ENV{PATH}";
	print STDERR "Ugh.\n";
      }
      open my $dot, '-|', 'dot', '-Tpng', $filename
	or die "failed to spawn dot";
      binmode $dot;
      my $png;
      {
	local $/;
        $png = <$dot>;
      }
      close $dot;
      my $display_pid =
        open my $display, '|-', 'display', '-title', $title, qw(-alpha Off -resize 70% -sharpen 1 png:-)
          or die "failed to spawn display";
      # make sure SIGTERM is forwarded to display
      $SIG{TERM} = sub {
	kill 'TERM', $display_pid;
	exit(0);
      };
      binmode $display;
      print $display $png;
      close $display;
      # FIXME this is a bit iffy: we're sending a message from a separate
      # process with no synch, so this could stomp on a message sent from the
      # original process at the same time
      $self->{module}->send_msg(['tell', ':content', ['report', ':content', ['closed', ':what', $id, ':who', 'usr']]])
	# If the command fails, it will have nonzero status. If the system kills
	# us, we won't get here. We only get here with zero status (success) if
	# it ran and the user closed it.
	if ($? == 0);
      1
    } || do { print STDERR $@; exit(1); };
    exit(0);
  } else { # parent
    $self->{windows}{$id} = {
      pid => $pid,
      close_pid => $pid, # so that &closed can wait for the child
      native_id => $self->wait_for_window_to_open($id)
    };
    my $config_reply;
    eval {
      $config_reply = $self->configure_window($id, $config);
      1
    } || print STDERR "failed to configure new window $id: " . Data::Dumper->Dump([$config_reply],['*config_reply']);
    return ['opened', ':what', $id, ':who', 'sys'];
  }
};

my $wmctrl_row_re = qr/
  ^
  ( 0x [0-9a-f]{8} ) \s+	# X11 window ID
  -? \d+ \s+			# desktop number
  ( -? \d+ ) \s+		# left
  ( -? \d+ ) \s+		# top
  ( \d+ ) \s+			# width
  ( \d+ ) \s+			# height
  \S+ \s+			# hostname
  ( .* )			# title
  $
/x;
*describe_window = sub {
  my ($self, $id) = @_;
  exists($self->{windows}{$id}) or die unknown_object($id);
  my $native_id = $self->{windows}{$id}{native_id};
  for my $row (`wmctrl -l -G`) {
    chomp $row;
    if ($row =~ $wmctrl_row_re && $1 eq $native_id) {
      my ($left, $top, $width, $height, $title) = ($2, $3, $4, $5, $6);
      return ['answer', ':description', ['window',
        ':title', '"' . escape_for_quotes($title) . '"',
	':left', $left, ':top', $top, ':width', $width, ':height', $height
      ] ];
    }
  }
  die "failed to find native ID $native_id in window list returned by wmctrl";
};

*configure_window = sub {
  my ($self, $id, $config) = @_;
  exists($self->{windows}{$id}) or die unknown_object($id);
  my $native_id = $self->{windows}{$id}{native_id};
  # construct wmctrl args and KQML answer at the same time
  my @args = ();
  my $new_desc = ['window', ':id', $id];
  my $answer = ['answer', ':description', $new_desc];
  # title
  if (exists($config->{':title'})) {
    push @args, '-T', KQML::KQMLStringAtomAsPerlString($config->{':title'});
    push @$new_desc, ':title', $config->{':title'};
  }
  # geometry
  my $haveGeom = 0;
  my @mvarg = (0,-1,-1,-1,-1);
  my $i = 1;
  for my $arg (qw(:left :top :width :height)) {
    if (exists($config->{$arg})) {
      $haveGeom = 1;
      $mvarg[$i] = $config->{$arg};
      push @$new_desc, $arg, $config->{$arg};
    }
    $i++;
  }
  push @args, '-e', join(',', @mvarg) if ($haveGeom);
  # do the configuration (if necessary) and return the answer
  system('wmctrl', '-i', '-r', $native_id, @args) if (@args);
  return $answer;
};

*close_window = sub {
  my ($self, $id) = @_;
  exists($self->{windows}{$id}) or die unknown_object($id);
  my $pid = $self->{windows}{$id}{pid};
  delete $self->{windows}{$id};
  kill 'TERM', $pid;
  waitpid $pid, 0;
  return ['closed', ':what', $id, ':who', 'sys'];
};

} # end platform-specific stuff

sub handle_window_management_request {
  my ($self, $msg, $content) = @_;
  my $verb = lc($content->{verb});
  my $answer;
  my $broadcast = 0;
  eval {
    if ($verb eq 'get-windows') {
      $answer = ['answer', ':windows', [$self->get_windows()]];
    } else {
      exists($content->{':what'}) or die missing_argument(':what');
      my $id = lc($content->{':what'});
      if ($verb eq 'describe') {
	$answer = $self->describe_window($id);
      } elsif ($verb eq 'configure') {
	die invalid_argument($content, ':title', 'string')
	  if (exists($content->{':title'}) and
	      not KQML::KQMLAtomIsString($content->{':title'}));
	$answer = $self->configure_window($id, $content);
      } elsif ($verb eq 'close') {
	$answer = $self->close_window($id);
	$broadcast = 1;
      } else {
	die unknown_action($verb);
      }
    }
    1
  } || do {
    $answer = nested_error("while handling $verb request: ", $@);
    $broadcast = 0;
  };
  my $reply =
    [ ($broadcast ? 'tell' : 'reply'),
      ':content', ['report', ':content', $answer],
      (exists($msg->{':reply-with'}) ?
        (':in-reply-to', $msg->{':reply-with'}) : ()),
      ((exists($msg->{':sender'}) and not $broadcast) ? 
        (':receiver', $msg->{':sender'}) : ())
    ];
  $self->{module}->send_msg($reply);
}

1;

