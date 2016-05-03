#!/usr/bin/perl -T

use CGI qw/:standard :cgi-lib/;
use IO::Socket;
use POSIX qw/strftime/;
use KQML::KQML;
use TripsModule::TripsModule;

use strict vars;

#=development settings
my $site_base_url = 'http://localhost/web-tools'; # used for fetch redirect
my $num_instances = 1;
my $base_port = 6200;
my $stuck_threshold = 600; # seconds
#=cut

=production settings
my $site_base_url = 'http://trips.ihmc.us/parser/papers'; # used for fetch redirect
my $num_instances = 3;
my $base_port = 6210;
my $stuck_threshold = 600; # seconds
=cut

sub quote_for_kqml {
  $_ = shift @_;
  s/[\\"]/\\$&/g;
  qq/"$_"/;
}

sub begin_html {
  my ($subtitle, @extra_headers) = @_;
  $subtitle = ': ' . $subtitle if ($subtitle);
  header(-type => 'text/html', -charset => 'UTF-8', @extra_headers) . <<EOP;
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>run-pmcid$subtitle</title>
<style type="text/css">
dl.short dd {
  display: inline;
  margin: 0;
}

dl.short dd:after {
  display: block;
  content: '';
}

dl.short dt {
  display: inline-block;
  font-weight: bold;
  margin-right: 1em;
}

dt:after {
  content: ': ';
}
</style>
</head>
<body>
<h1>run-pmcid</h1>
EOP
}

my $build_date = undef;
my %pmcid2build_dates = ();
sub form {
  my $bd = (defined($build_date) ?
	      strftime("%Y%m%dT%H%M", localtime($build_date)) :
	      'unknown');
  my $p2bd_json =
    '{' .
    join(',',
      map { "'$_':[" .
            join(',', map { "'$_'" } @{$pmcid2build_dates{$_}}) .
	    "]"
	  }
      sort keys %pmcid2build_dates
    ) .
    '}';

  <<EOF;
<hr>
<script type="text/javascript">
var pmcid2build_dates = $p2bd_json;
window.onload = function() {
  var pmcids = document.getElementById('pmcids');
  var ih = '';
  var keys = Object.keys(pmcid2build_dates).sort();
  for (var i = 0; i < keys.length; i++) {
    ih += '<option value="' + keys[i] + '">';
  }
  pmcids.innerHTML = ih;
  
  var build_date = document.getElementById('build-date');
  var pmcid = document.getElementById('pmcid');
 
  var update_build_date_options = function() {
    var ih = '';
    var build_dates = pmcid2build_dates[pmcid.value];
    if (build_dates !== undefined) {
      for (var i = 0; i < build_dates.length; i++) {
	ih += '<option>' + build_dates[i] + '</option>';
      }
      if (ih == '') {
	ih = '<option value="no-output-yet">-- no output yet for PMC' + pmcid.value + ' --</option>';
      }
    } else {
      ih = '<option value="invalid-pmc-id">-- enter a valid PMC ID # above first --</option>';
    }
    build_date.innerHTML = ih;
  }
  update_build_date_options();
  pmcid.onchange = update_build_date_options;

  // prevent pressing enter in pmcid field from submitting the form
  var dont_submit = function(e) {
    if (e.keyCode == 13) {
      update_build_date_options();
      e.preventDefault();
      return false;
    }
  }
  pmcid.onkeydown = dont_submit;
  pmcid.onkeyup = dont_submit;
  pmcid.onkeypress = dont_submit;
}
</script>
<form action="#">
<p>PMC<input type="text" placeholder="enter PMC ID #" name="pmcid" id="pmcid" list="pmcids"></p>
<datalist id="pmcids">
</datalist>
<p>
<input type="submit" name="op" value="fetch">
output of system built on
<select name="build-date" id="build-date">
</select>
</p>
<p><input type="submit" name="op" value="start"> new run; current system build date is $bd.</p>
</form>
<form action="#" method="POST"><!-- POST convinces the browser to actually reload the page if we're already on the status page -->
<p><input type="submit" name="op" value="status"> report</p>
</form>
EOF
}

sub connect_to_instance {
  my $index = shift;
  my $tm = TripsModule->new(
    '-name', 'web' . int(rand(10000)),
    '-connect', 'localhost:' . ($base_port + $index)
  );
  $tm->init();
  $tm->ready();
  return $tm;
}

sub get_instance_status {
  my $tm = shift;
  my $reply_content =
    $tm->send_and_wait('(request :receiver drum :content (get-status))');
  die "bogus reply to get-status: $reply_content"
    unless (ref($reply_content) eq 'ARRAY');
  return KQML::KQMLKeywordify($reply_content);
}

sub disconnect_from_instance {
  my $tm = shift;
  close $tm->{in};
}

sub get_all_instance_statuses {
  my @statuses = ({})x$num_instances;
  for my $i (0..$#statuses) { # TODO parallelize?
    eval {
      my $instance = connect_to_instance($i);
      $statuses[$i] = get_instance_status($instance);
      disconnect_from_instance($instance);
      1
    } || do {
      $statuses[$i] = KQML::KQMLKeywordify(KQML::KQMLReadFromString(
        '(status :error ' . quote_for_kqml($@) . ' :idle false)'
      ));
    }
  }
  return @statuses;
}

sub format_instance_statuses {
  my @statuses = @_;
  my $str = '<dl>';
  for my $i (0..$#statuses) {
    $str .= '<dt>Instance ' . ($i + 1) . ' status<dd><dl class="short">';
    my @list = @{$statuses[$i]{list}};
    for (my $j = 1; $j < @list; $j += 2) {
      my ($key, $val) = @list[$j,$j+1];
      if ($key eq ':scheduled-tasks') { # special case for list value
	$str .= "<dt>Number of scheduled tasks<dd>" . scalar(@$val);
      } else {
	$key =~ s/^:([a-z])/uc($1)/e;
	$key =~ s/-/ /g;
	$str .= "<dt>$key<dd>$val";
      }
    }
    $str .= '</dl>';
  }
  $str .= '</dl>';
  return $str;
}

# fork, exec/read, and return stdout in a string, like backticks, but safer
# because we don't use the shell to interpret the command, and we use a minimal
# environment
sub safer_backticks {
  my ($prog, @args) = @_;
  my $ret = undef;
  my $pid = open(KID, '-|');
  defined($pid) or die "can't fork: $!";
  if ($pid) { # parent
    local $/; # slurp mode
    $ret = <KID>;
    close KID;
  } else { # child
    %ENV = ( PATH => '/bin:/usr/bin' ); # minimal environment
    (exec {$prog} $prog, @args) or die "can't exec: $!";
  }
  return $ret;
}

sub kebi_mebi_gibi {
  my $num = shift;
  if ($num < 1024) {
    return $num . ' ';
  } elsif ($num < 1024 * 1024) {
    return (int($num / 1024) . ' K');
  } elsif ($num < 1024 * 1024 * 1024) {
    return (int($num / (1024 * 1024)) . ' M');
  } else {
    return (int($num / (1024 * 1024 * 1024)) . ' G');
  }
}

sub get_system_load {
  my $sar_output = safer_backticks(qw(sar 1 1));
  $sar_output =~ /(\d+(?:\.\d*)?)\s*$/ or die "bogus sar output: $sar_output";
  my $avg_idle_percentage = $1; # works for both OSX and Linux

  my $mem_stat = undef;
  my $uname = safer_backticks('uname');
  if ($uname =~ /Darwin/) {
    my $vm_stat_output = safer_backticks('vm_stat');
    $vm_stat_output =~ /Pages free:\s*(\d+)/
      or die "bogus vm_stat output: $vm_stat_output";
    my $num_free_pages = $1;
    $mem_stat = kebi_mebi_gibi($num_free_pages * 4096) . 'B of RAM free';
  } elsif ($uname =~ /Linux/) {
    my $vmstat_output = safer_backticks('vmstat');
    my $last_line = [split(/\n/, $vmstat_output)]->[-1];
    $last_line =~ s/^\s+//;
    my @fields = split(/\s+/, $last_line);
    my $kebibytes_free = $fields[3];
    $mem_stat = kebi_mebi_gibi($kebibytes_free * 1024) . 'B of RAM free';
  } else {
    $mem_stat = "Unknown system type, skipping memory statistics";
  }

  return "<p>CPU $avg_idle_percentage\% idle. $mem_stat.</p>\n";
}

my $input_dir = undef;
my $output_dir = undef;

sub fill_pmcid2build_dates {
  opendir ID, $input_dir or die "Can't open input dir: $!";
  my @pmcids = grep { /^\d+$/ and -d "$input_dir/$_" } readdir ID;
  closedir ID;

  for my $pmcid (@pmcids) {
    $pmcid2build_dates{$pmcid} = [];
    if (-d "$output_dir/$pmcid") {
      opendir OD, "$output_dir/$pmcid" or die "Can't open output dir $output_dir/$pmcid: $!";
      @{$pmcid2build_dates{$pmcid}} =
	sort { $b cmp $a } # most recent first
	map { my $d = $_; $d =~ s/^PMC\d+-//; $d =~ s/\.ekb$//; $d }
	grep { /^PMC$pmcid-\d{8}T\d{4}\.ekb$/ }
	readdir OD;
    }
  }
}

sub run_pmcid {
  my ($tm, $pmcid, $output_file) = @_;
  # make sure the output dir exists
  my $output_dir = $output_file;
  $output_dir =~ s/[^\/]+$//;
  unless (-d $output_dir) {
    mkdir($output_dir) or die "failed to create output dir $output_dir: $!";
    chmod(0777, $output_dir) or die "failed to set permissions on output dir $output_dir: $!";
  }
  # send request to DrumGUI
  my ($reply_content, $reply) =
    $tm->send_and_wait(
      '(request :receiver drum :content (run-pmcid' .
      ' :folder ' . quote_for_kqml($input_dir) .
      ' :pmcid "' . $pmcid . '"' .
      ' :save-to ' . quote_for_kqml($output_file) .
      '))');
  # interpret reply
  if ($reply->{verb} eq 'error') {
    die "got error from DrumGUI: " . $reply->{':comment'};
  } elsif ($reply->{verb} eq 'reply' and
           ref($reply_content) eq 'ARRAY' and
	   lc($reply_content->[0]) eq 'accepted') {
    return; # everything's OK
  }
  # if we get here, we got a weird reply from DrumGUI
  die "got bogus reply from DrumGUI: " . KQML::KQMLAsString($reply->{list});
}

sub validate_param {
  my ($params, $key, $regexp) = @_;
  exists($params->{$key}) or die "missing $key";
  $params->{$key} =~ $regexp or die "malformed $key";
  return $1;
}

eval {

  exists($ENV{TRIPS_BASE}) or die "TRIPS_BASE environment variable undefined.";
  (-d $ENV{TRIPS_BASE}) or die "\$TRIPS_BASE=\"$ENV{TRIPS_BASE}\" is not a directory.";
  $input_dir = $ENV{TRIPS_BASE} . '/etc/Data/pmc-oa';
  (-d $input_dir) or die "\$input_dir=\"$input_dir\" is not a directory.";
  $output_dir = $ENV{TRIPS_BASE} . '/www/pmc-oa-output';
  (-d $output_dir) or die "\$output_dir=\"$output_dir\" is not a directory.";

  my $core_file = $ENV{TRIPS_BASE} . '/etc/trips-drum.core';
  (-e $core_file) or die "\$core_file=\"$core_file\" doesn't exist.";
  $build_date = [stat($core_file)]->[9]; #mtime

  my %params = Vars();

  # read directory listings unless we're just fetching one output file
  fill_pmcid2build_dates() unless ($params{op} eq 'fetch');

  if (not exists($params{op})) {
    print begin_html(), form(), end_html();
  } elsif ($params{op} eq 'start') { # start processing a paper
    my $pmcid = validate_param(\%params,'pmcid', qr/^(?:pmc)?(\d+)$/i);
    my $bd = strftime("%Y%m%dT%H%M", localtime($build_date));
    my $output_file = "$output_dir/$pmcid/PMC$pmcid-$bd.ekb";
    my $output_url = "$site_base_url/pmc-oa-output/$pmcid/PMC$pmcid-$bd.ekb";
    my $started_html ="started PMC$pmcid, output will be available <a href=\"$output_url\">here</a>.";
    my $already_started = (-e $output_file);
    my @statuses = ();
    unless ($already_started) {
      @statuses = get_all_instance_statuses();
      for (@statuses) {
	if ($_->{':dataset'} eq "\"$pmcid\"") {
	  $already_started = 1;
	  last;
	}
      }
    }
    if ($already_started) {
      print begin_html('already started'), "already $started_html", form(), end_html();
    } else {
      my $idle_index = undef;
      my $least_busy_unstuck_index = undef;
      for my $i (0..$#statuses) {
	if ($statuses[$i]{':idle'} =~ /t|yes/i) {
	  $idle_index = $i;
	  last;
	} elsif (exists($statuses[$i]{':error'})) {
	  # error is a kind of stuck
	} elsif ($statuses[$i]{':secs-since-last-activity'} <
		   $stuck_threshold) {
	  if ((not defined($least_busy_unstuck_index)) or
	      scalar(@{$statuses[$i]{':scheduled-tasks'}}) <
	      scalar(
	        @{$statuses[$least_busy_unstuck_index]{':scheduled-tasks'}}
	      )
	     ) {
	    $least_busy_unstuck_index = $i;
	  }
	}
      }
      my $index;
      if (defined($idle_index)) {
	$index = $idle_index;
      } elsif (defined($least_busy_unstuck_index)) {
	$index = $least_busy_unstuck_index;
      } else {
	die "all instances are stuck, try again later";
      }
      my $instance = connect_to_instance($index);
      run_pmcid($instance, $pmcid, $output_file);
      print begin_html('started'), $started_html, form(), end_html();
    }
  } elsif ($params{op} eq 'fetch') { # attempt to fetch the result
    eval {
      my $pmcid = validate_param(\%params,'pmcid', qr/^(?:pmc)?(\d+)$/i);
      die "no output yet for PMC$pmcid (use 'start' instead of 'fetch' to start a new run)"
	if ($params{'build-date'} eq 'no-output-yet');
      die "invalid PMC ID #"
	if ($params{'build-date'} eq 'invalid-pmc-id');
      my $bd = validate_param(\%params,'build-date', qr/^(\d{8}T\d{4})$/);
      print redirect("$site_base_url/pmc-oa-output/$pmcid/PMC$pmcid-$bd.ekb");
      1;
    } || do {
      # we didn't do this before because we thought we were fetching, but now
      # that we're still putting up a form, we need this again:
      fill_pmcid2build_dates();
      die;
    }
  } elsif ($params{op} eq 'status') { # report on system status
    my $system_load = get_system_load();
    my @statuses = get_all_instance_statuses();
    my $status_str = format_instance_statuses(@statuses);
    print begin_html('status', -expires => 'now'), $system_load, $status_str, form(), end_html();
  }

  1;
} || do {
  print begin_html('error'), "Error: $@", form(), end_html();
}
