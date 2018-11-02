#!/usr/bin/perl
#
# ekb_test.pl
#
# Time-stamp: <Fri Aug 11 09:49:09 CDT 2017 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 18 Jun 2016
#

#----------------------------------------------------------------
# Description:
# Manage EKB tests (make/update/run)

#----------------------------------------------------------------
# History:
# 2016/06/16 v0.0	lgalescu
# - Created, by joining previous scripts.
# 2016/07/17 v1.0	lgalescu
# - Updated command line options and their handling.
# - Added examples to help info.
# 2016/09/17 v1.1	lgalescu
# - Added assertion counts to summary
# 2016/09/30 v1.2	lgalescu
# - Nomenclature: will use "test suite" and "test case" from now on.
# - Changed some command line option names and shortcuts.
# - Added small optimization.
# 2016/10/03 v1.3	lgalescu
# - Changed one command line option shortcut.
# - Added cropping functionality (making a test out of a single sentence 
#   from a multi-sentence input EKB) for the 'add' command.
# 2016/10/04 v1.3.1	lgalescu
# - Extended cropping functionality to the 'update' command.
# 2016/10/10 v1.3.2	lgalescu
# - Fixed bug (optimization in v1.2 was not correct).
# - Simplified code a bit.
# 2017/02/03 v1.3.3     lgalescu
# - Adapted to work as part of EKB TRIPS component
# 2017/06/22 v1.3.4	lgalescu
# - Added individual test results to overall summary.

#----------------------------------------------------------------
# Usage:
# 
my $usage;

BEGIN {
    # Just use basename of program, rather than whole path.
    $0 =~ s:.*/::;

    $usage = "usage: $0 [OPTIONS] CMD [CMD-OPTIONS]

COMMANDS:

  new
\tMake a new test suite.
  add
\tAdd test to test suite.
  delete, del
\tDelete existing test.
  update, up
\tUpdate existing test.
  run
\tRun a test.

OPTIONS:

  -d, -debug LEVEL
\tSets debugging level.
  -h, -help
\tPrint usage information.

COMMAND OPTIONS:

1. common to all commands

  -t, -tsd FILE, -set FILE, -suite FILE 
\tFILE contains a test suite definition. May be repeated for the 
\trun command.

2. 'new' command options:

  -co, -options CMP-OPTS
\tAdd EKB::Compare options for the new test suite.

3. 'add' command options:

  -n, -name SYMBOL, -case SYMBOL
\tName of the test (will not be added if in use already).
  -e, -ekb EKB-FILE
\tEKB file for this test.
  -s ID, -sentence ID
\tCrop EKB to the sentence with the specified ID (number).
  -co, -options CMP-OPTS
\tAdd options for the test.

4. 'delete' command options:

  -n, -name SYMBOL
\tName of the test to be deleted.

5. 'update' command options:

Note: Order of update operations is not guaranteed. Updates are made 
incrementally, not in bulk. Therefore, care should be taken when trying to
make multiple updates with a single command. 

  -co, -options CMP-OPTS
\tAdd options for test suite. If an option exists already, its value may be 
\tupdated this way.
  -do, -delete-options CMP-OPTS
\tDelete options for test suite. Non-existent options will be ignored.
  -n, -name SYMBOL
\tName of the test.
  -m, -new-name SYMBOL
\tNew name of the test (will not be changed if in use already).
  -to, -test-options CMP-OPTS
\tAdd options to test.
  -dto, -delete-test-options CMP-OPTS
\tDelete options for test. Non-existent options will be ignored.
  -e, -ekb EKB-FILE
\tUpdate reference EKB for test.
  -s ID, -sentence ID
\tCrop EKB to the sentence with the specified ID (number).
  -r, -ref-file EKB-FILE
\tName under which EKB file is saved. If different from default, it will be
\tadded as an additional reference EKB for the given test. Requires -e.

6. 'run' command options:

  -i, -in DIR
\tRead EKBs to test from DIR (default: .).
  -o, -out DIR
\tWrite results to FOLDER (default: STDOUT). If folder doesn't exist, it 
\twill be created.
  -co, -options CMP-OPTS
\tAdd options for current run. If an option exists already, its value may be
\tmodified, but will not be saved.

EXAMPLES

$0 new -t newset.tsd -co ignore_ekb_id
\tCreates new test suite with 'ignore_ekb_id' as global test option.

$0 add -t newset.tsd -n test1 -co ignore_dbid -e ekbs/ekb1.ekb
\tAdds new test, named 'test1', to test suite 'newset', with 'ignore_dbid' as 
\tspecific test option, setting reference text and EKB from the given EKB file.

$0 del -t newset.tsd -n test1 
\tDeletes test 'test1' from test suite 'newset'.

$0 up -t newset.tsd -n test1 -do ignore_ekb_id -m my-test1 -dto ignore_dbid -e ekbs/ekb1.ekb
\tUpdates test suite 'newset': deletes global test option 'ignore_ekb_id',
\tdeletes 'ignore_dbid' as specific test option, updates 'test1' 
\treference text and EKB with information from the given EKB file, and renames
\ttest 'test1' to 'my-test1'.

$0 run -t testset1.tsd -t testset2.tsd -i ekbs -o results
\tRuns two test suites on EKBs in ./ekbs/ and saves outputs in ./results/
";
  }

# Get TRIPS_BASE from environment
BEGIN {
    $TRIPS_BASE_DEFAULT = ".";
    $TRIPS_BASE = $ENV{'TRIPS_BASE'} || $TRIPS_BASE_DEFAULT;
    warn "TRIPS_BASE=$TRIPS_BASE\n";
  }

use strict 'vars';
use warnings;

my $VERSION = "1.3.4";

# local (TRIPS) Perl libraries
use lib "$main::TRIPS_BASE/etc/";
use lib "$main::TRIPS_BASE/etc/EKBAgent";

use Cwd qw(realpath);
use Data::Dumper;
use File::Basename;
use File::Path  qw(make_path);
use File::Spec::Functions;
use Getopt::Long;
use List::Util qw(max any);

use EKB;
use EKB::Test;

use util::Log;

our (
     @tsdFiles,
     $testName,
     $newName,
     $ekbFile,
     $sentId,
     $refFile,
     $inDir,
     $outDir,
     $options,
     $delOpts,
     $testOpts,
     $delTestOpts,
     $debugLevel,
     $help,
    );

my @COMMANDS = qw(new add delete update run);

@tsdFiles = ();
$inDir = ".";

# we want to parse options in two pasess:
# - first, global options, up to the command
# - second, command options
# so we configure Getopt to pass through @ARGV
Getopt::Long::Configure qw/pass_through/;

GetOptions(
	   'h|help' => \$help,
	   'd|debug=i' => \$debugLevel,
	  );

die $usage if $help;

$debugLevel = $debugLevel // 0;
local $util::Log::DebugLevel = $debugLevel;
local $util::Log::CallerInfo = ($debugLevel > 1);

my $command = shift
  or die $usage;

# expand shortcuts
$command = 'delete' if ($command eq 'del');
$command = 'update' if ($command eq 'up');
grep { $command eq $_ } @COMMANDS
  or FATAL "Unrecognized command: $command";

GetOptions(
	   't|suite|set|tsd=s' => \@tsdFiles,
	   'n|case|name=s' => \$testName,
	   'm|new-name=s' => \$newName,
	   'e|ekb=s' => \$ekbFile,
	   's|sentence=s' => \$sentId,
	   'r|ref-name=s' => \$refFile,
	   'i|in=s' => \$inDir,
	   'o|out=s' => \$outDir,
	   'co|options=s' => \$options,
	   'do|delete-options=s' => \$delOpts,
	   'to|test-options=s' => \$testOpts,
	   'dto|delete-test-options=s' => \$delTestOpts,
	  ) or die "Something's wrong";

$options = $options // "";

unless (scalar(@tsdFiles) > 0) {
  FATAL "TSD file(s) not specified. Try '$0 -help'";
}

-d $inDir
  or FATAL "%s: Directory not found", $inDir;

if ($command eq "new") {
  t_new(); 
}
elsif ($command eq "add") {
  # must have a testname and an ekb
  unless (defined $testName) {
      FATAL "Must specify a testname. Try '$0 -help'";
  }
  unless (defined $ekbFile) {
    FATAL "Must specify an EKB file. Try '$0 -help'";
  }
  
  t_add();
}
elsif ($command eq "delete") {  
  # must have a testname
  unless ($testName) {
    FATAL "Must specify a testname. Try '$0 -help'";
  }

  t_delete();
}
elsif ($command eq "update") {
  t_update();
}
elsif ($command eq "run") {
  if ((defined $outDir) && ! (-d $outDir)) {
    make_path ($outDir)
      or FATAL "Directory cannot be created: %s", $outDir;
  }
  t_run();
}
else {
  die $usage;
}

exit 0;


sub t_new {
  my $tsd = shift @tsdFiles;
  if (-f $tsd) {
    FATAL "TSD file exists already: $tsd";
  }
  my $test = EKB::Test->new( tsd => $tsd,
			     options => $options );
  $test->write();
}

sub t_add {
  my $tsd = shift @tsdFiles;
  unless (-f $tsd) {
    FATAL "Cannot find TSD file: $tsd";
  }
  my $test = EKB::Test->new( tsd => $tsd );

  my %opts = ( options => $options );
  if (defined($sentId)) {
    $opts{select} = $sentId;
  }
  
  $test->add_test($testName, $ekbFile, %opts);
  $test->write();
}

sub t_delete {
  my $tsd = shift @tsdFiles;
  unless (-f $tsd) {
    FATAL "Cannot find TSD file: $tsd"
  }
  my $test = EKB::Test->new( tsd => $tsd );
  $test->test_delete($testName);
  $test->write();
}

sub t_update {
  my $tsd = shift @tsdFiles;
  unless (-f $tsd) {
    FATAL "Cannot find TSD file: $tsd"
  }
  my $test = EKB::Test->new(tsd => $tsd);

  # add set options?
  if ($options) {
    $test->add_options($options);
  }
  # delete set options?
  if ($delOpts) {
    $test->delete_options($delOpts);
  }
  # add test options?
  if ($testOpts) {
    # must have a testname
    unless (defined $testName) {
      FATAL "Must specify a testname. Try '$0 -help'";
    }
    $test->test_add_options($testName, $testOpts);
  }
  # delete test options?
  if ($delTestOpts) {
    # must have a testname
    unless (defined $testName) {
      FATAL "Must specify a testname. Try '$0 -help'";
    }
    $test->test_delete_options($testName, $delTestOpts);
  }
  # test name change?
  if (defined $newName) { 
    # must have a testname
    unless (defined $testName) {
      FATAL "Must specify a testname. Try '$0 -help'";
    }
    $test->test_rename($testName, $newName);
    $testName = $newName;
  }
  # test data change?
  if (defined $ekbFile) {
    if (defined $refFile) {
      
    } else {
      $test->set_test_data($testName, $ekbFile, $sentId);
    }
  }
  $test->write();
}

sub t_run {
  # gather all files
  my $ekb_path = catfile($inDir, "*.ekb");
  my @ekb_files = glob qq($ekb_path);

  my $a = scalar(@ekb_files);

  unless (@ekb_files) {
    INFO "Nothing to do: no EKB files found in %s.", $inDir;
    return;
  }
  
  INFO "Running %d test suites on %d EKB files",
    scalar(@tsdFiles), $a;
  DEBUG 0, "Test suites: %s", Dumper(\@tsdFiles);
  
  # gather all testsets
  my @testsets;
  foreach my $tsd (@tsdFiles) {
    push @testsets, EKB::Test->new(tsd => $tsd);
  }
  # match hyp EKBs against testsets
  foreach my $ekb_file (@ekb_files) {
    unless (any { $_->match_ekb_hyp($ekb_file) } @testsets) {
      WARN "No test case for: $ekb_file";
    }
  }
  # test counts
  my ($n, $p, $f, $s) = (0, 0, 0, 0);
  # assertion counts
  my ($del, $ins, $eql) = (0, 0, 0);
  foreach my $testset (@testsets) {
    $testset->run($options);
    if ($outDir) {
      $testset->save_run_results($outDir);
    }
    foreach my $tname ($testset->tests()) {
      my $test = $testset->get_test($tname);
      if (exists $test->{result}) {
	if ($test->{result}) {
	  $p++;
	} else {
	  $f++;
	}
	$del += $test->{comp}->summary->{del};
	$ins += $test->{comp}->summary->{ins};
	$eql += $test->{comp}->summary->{eql};
      }
    }
  }
  $n = $p + $f;
  unless ($n) {
    INFO "No tests were run";
    return;
  }
  $s = $a - $n;
  my $w = length($a);
  printf "Summary:\n";
  printf "%${w}d EKBs tested\n", $a;
  printf "%${w}d skipped\n", $s;
  printf "%${w}d passed (%.2f%%)\n", $p, 100*$p/$n;
  printf "%${w}d failed (%.2f%%)\n", $f, 100*$f/$n;
  printf "\n";
  printf "Assertion counts:\n";
  $w = max length($del + $eql), length($ins + $eql);
  printf " REF\t%${w}d\n", $del + $eql;
  printf " HYP\t%${w}d\n", $ins + $eql;
  printf " DEL\t%${w}d\n", $del;
  printf " INS\t%${w}d\n", $ins;
  printf " EQL\t%${w}d (%.2f%%)\n", $eql, 100*$eql/($del + $eql);
  printf "\n";
  printf "========\n";
  printf "Tests\n";
  foreach my $testset (@testsets) {
    foreach my $tname ($testset->tests()) {
      my $test = $testset->get_test($tname);
      printf "%s::%s %s",
	$testset->name,
	$tname,
	(exists $test->{result}) ? ($test->{result} ? "passed" : "failed") : "skipped";
      if ((exists $test->{result}) and ! $test->{result}) {
	printf " (P=%.2f R=%.2f)",
	  $test->{comp}->summary->{p}, $test->{comp}->summary->{r};
      }
      print "\n";
    }
  }
  
  if ($outDir) {
    printf "\nDetails can be found in %s\n", $outDir;
  }
}

