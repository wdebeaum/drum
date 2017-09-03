# Test.pm
#
# Time-stamp: <Wed Aug 16 18:42:21 CDT 2017 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 18 Jun 2016
#

#----------------------------------------------------------------
# Description:
# Package for representing EKB tests.

#----------------------------------------------------------------
# History
# 2016/06/18 v0.0 lgalescu
# - Created (mostly imported from previous scripts).
# 2016/07/18 v1.0 lgalescu
# - Module is release-worthy.
# 2016/09/18 v1.1 lgalescu
# - Minor fixes.
# 2016/09/30 v1.2 lgalescu
# - Nomenclature: will use "test suite" and "test case" from now on.
# 2016/10/03 v1.2.1 lgalescu
# - Minor code improvements.
# 2016/10/17 v1.2.2 lgalescu
# - Save diffs to file only when there are some.
# 2016/10/18 v1.2.3 lgalescu
# - Changed slightly the structure of the output.
# 2017/06/04 v1.2.4 lgalescu
# - Changed test matching to use the EKB id and the name of the folder of
#   the input file as test set and, respectively, test case names.
# 2017/06/24 v1.3 lgalescu
# - Now allowing multiple refrence EKBs!

package EKB::Test;

$VERSION = '1.3.0';

use strict 'vars';
use warnings;
use open qw(:utf8 :std);

use Data::Dumper;

use File::Basename; # basename fileparse
use File::Path qw(make_path);
use File::Spec::Functions; # catfile catdir
use File::Slurp;
use List::Util qw(first);

use EKB;
use EKB::Compare;

use util::Log;

# * test suite
#   name: test suite name
#   base: test base directory
#   options: EKB::Compare options, common to all tests
#   tests: hash of tests, keyed to test names
# * test attributes:
#   options: EKB::Compare options, test-specific
#   hyp:
#   hyp_ekb:
#   comp:
#   result:
# examples:
# EKB::Test->new( tsd => "test.tsd" )
# EKB::Test->new( tsd => "test.tsd",
#                 options => "no_normalization,ignore_dbids=0" )
sub new {
  my $class = shift;
  my @args = @_;
  my $self = { name => undef,
	       options => undef,
	       base => undef,
	       tsd => undef,
	       tests => {},
	     };
  bless $self, $class;
  $self->_init(@args);
  return $self;
}

# initialize from arguments
sub _init {
  my $self = shift;
  my %args = @_;

  DEBUG 1, "init with options:\n%s", Dumper(\%args);

  my $tsd_file = $args{tsd};
  $self->{tsd} = $tsd_file;
  $self->{name} = basename($tsd_file, ".tsd");
  if (defined $tsd_file) {
    $self->{base} = dirname $tsd_file;
  }
  # if tsd file exists, we read it
  if (-f $tsd_file) {
    $self->_read();
  }
  if (defined $args{options}) {
    $self->add_options($args{options});
  }
  DEBUG 2, "got:\n%s", Dumper($self);
}

## input/output

# read from test-suite file (.tsd)
# format:
# - lines starting with "#" are comments and will be ignored
# - blank lines (containing only whitespace) wil be ignored
# <test_suite> ::= <suite_decl> <test_decl>*
# <suite_decl> ::= <suite_name> <suite_opts>?
# <suite_name> ::= Suite: <name> \n
# <suite_opts> ::= <options>
# <test_decl> ::= <test_name> \n <input_def>? <output_def>? <test_opts>?
# <test_name> ::= Case: <name>
# <input_def> ::= Input: <file> \n
# <output_def> ::= Output: <file> \n
# <test_opts> ::= <options>
# <options> ::= <cmp_opt> | <cmp_opt>, <options>?
# <cmp_opt> ::= <cmp_opt:bool> | <cmp_opt_val>
# <cmp_opt:bool> ::= ignore_ekb_id | ignore_ekb_complete | ignore_dbids
# <cmp_opt_val> ::= only_uttnums=<n_list>
# <n_list> ::= <n> | <n>,<n_list>
# <name> ::= #string#
# <file> ::= #filename#
#    N.B.: these are relative to the directory where the tsd file resides
# <n> ::= #number#
sub _read {
  my $self = shift;
  my $tsd_file = $self->{tsd};

  # if file exists, we read it
  open my $fh, "<", $tsd_file
    or FATAL "Cannot read tsd file $tsd_file: $!";
    
  my ($state, $consuming) = (0, 1);
  my ($key, $value);
  my $line;
  my $line_no = 0;
  my $test;
  my @allowed_keys = ();
  while (1) {
    if ($consuming) {
      if (eof($fh)) {
	last if $state;
	FATAL "Empty tsd file!";
      } else {
	$line = <$fh>;
	$line_no++;
	chomp $line;
	# comments
	next if $line =~ m{^#};
	# trim whitespace from end of line
	$line =~ s{\s*$}{};
	# blanks
	next unless $line;
	# line structure: "<key>:<value>"
	($key, $value) = ($line =~ m{^(\S+)\s*:\s*(\S.*)});
	$key //= "";
	$value //= "";
      }
    }
    if ($state == 0) {
      # 0 -[Suite]-> 1%<suite_decl>%
      if ($key eq "Suite") {
	$self->{name} = $value;
	$state = 1; $consuming = 1;
      } else {
	@allowed_keys = ("Suite");
	$state = 9; $consuming = 0;
      }
    } elsif ($state == 1) {
      # 1 -[Options]-> 2%<suite_opts>%
      # 1 -[Case]-> 2%%
      if ($key eq "Options") {
	$self->options(_options_parse($value));
	$state = 2; $consuming = 1;
      } elsif (grep {$key eq $_} ("Test", "Case")) {
	$state = 2; $consuming = 0;
      } else {
	@allowed_keys = ("Options", "Case");
	$state = 9; $consuming = 0;
      }
    } elsif ($state == 2) {
      # 2 -[Case]-> 3%<test_decl>%
      if (grep {$key eq $_} ("Test", "Case")) {
	# we set defaults here; they can be overriden
	$test = { input => $value . ".txt",
		  output => [ $value . ".ekb" ] };
	$self->{tests}{$value} = $test;
	$state = 3; $consuming = 1;
      } else {
	@allowed_keys = ("Case");
	$state = 9; $consuming = 0;
      }	    
    } elsif ($state == 3) { 
      # 3 -[Input]-> 4%<test_decl_input>%
      # 3 -[Output]-> 4%%
      # 3 -[Options]-> 5%%
      # 3 -[Case]-> 3%%
      if ($key eq "Input") {
	$test->{input} = $value;
	$state = 4; $consuming = 1;
      } elsif ($key eq "Output") {
	$state = 4; $consuming = 0;
      } elsif ($key eq "Options") {
	$state = 5; $consuming = 0;
      } elsif (grep {$key eq $_} ("Test", "Case")) {
	$state = 2; $consuming = 0;
      } else {
	@allowed_keys = ("Case", "Input", "Output", "Options");
	$state = 9; $consuming = 0;
      }
    } elsif ($state == 4) {
      # 4 -[Output]-> 4%<test_decl_result>%
      # 4 -[Options]-> 5%%
      # 4 -[Case]-> 3%%
      if ($key eq "Output") {
	push @{ $test->{output} }, $value;
	$state = 4; $consuming = 1; 
      } elsif ($key eq "Options") {
	$state = 5; $consuming = 0;
      } elsif (grep {$key eq $_} ("Test", "Case")) {
	$state = 2; $consuming = 0;
      } else {
	@allowed_keys = ("Case", "Output", "Options");
	$state = 9; $consuming = 0;
      }
    } elsif ($state == 5) {	# waiting for <test_opts>
      if ($key eq "Options") {
	$test->{options} = { _options_parse($value) };
	$state = 2; $consuming = 1;
      } elsif (grep {$key eq $_} ("Test", "Case")) {
	$state = 2; $consuming = 0;
      } else {
	@allowed_keys = ("Case", "Options");
	$state = 9; $consuming = 0;
      }
    } elsif ($state == 9) {	# error
      FATAL "Bad line in tsd file: [%d] %s (Expected: %s)",
	$line_no, $line, join("|", @allowed_keys);
    }
  }
    
  close $fh;

  my @tests = $self->tests();
  INFO "Test suite \"%s\": Read %d test cases: (%s).",
    $self->name(), scalar(@tests), join(" ", @tests);
}

# print test
sub write {
  my $self = shift;
  my $tsd_file = $self->{tsd};

  open my $fh, ">", $tsd_file
    or FATAL "Cannot write to tsd file $tsd_file: $!";

  printf $fh "Suite: %s\n", $self->name();
  printf $fh "Options: %s\n\n", _options_tostring($self->options())
    if $self->options();
  printf $fh "\n";
  foreach my $tn ($self->tests()) {
    printf $fh "Case: %s\n", $tn;
    my $test = $self->{tests}{$tn};
    printf $fh "Input: %s\n", $test->{input}
      if ($test->{input}
	  &&
	  (basename($test->{input}, ".txt") ne $tn));
    if ($test->{output}) {
      foreach my $out (@{$test->{output}}) {
	printf $fh "Output: %s\n", $out
	  unless basename($out, ".ekb") eq $tn;
      }
    }
    printf $fh "Options: %s\n", _options_tostring($test->{options})
      if ($test->{options} && %{$test->{options}});
    printf $fh "\n";
  }
  
  close $fh;

  my @tests = $self->tests();
  INFO "Test suite \"%s\": Wrote %d test cases: (%s).",
    $self->name(), scalar(@tests), join(" ", @tests);
}

## getters and setters

# get or set&get name
sub name {
  my $self = shift;
  if (@_) {
    $self->{name} = shift;
  }
  return $self->{name};
}

# get or set&get base
sub base {
  my $self = shift;
  if (@_) {
    $self->{base} = shift;
  }
  return $self->{base};
}

# get or set&get options
sub options {
  my $self = shift;
  if (@_) {
    $self->{options} = { @_ };
  }
  my @opts = %{ $self->{options} }
    if $self->{options};
  return @opts;
}

# get list of testnames
sub tests {
  my $self = shift;
  return sort keys %{ $self->{tests} };
}

# get test by name
sub get_test {
  my $self = shift;
  my $tname = shift;
  
  return undef unless (exists $self->{tests}{$tname});
  return $self->{tests}{$tname};
}

# get data dir
sub get_data_dir {
  my $self = shift;
  return catdir($self->base(), $self->name());
}

## updates

# add/change option
sub add_option {
  my $self = shift;
  my ($option, $value) = @_;
  $self->{options}{$option} = $value;
}

# delete option
sub delete_option {
  my $self = shift;
  my $option = shift;
  delete $self->{options}{$option};
}

# add/change options from string
sub add_options {
  my $self = shift;
  my $options = shift;
  my %options = _options_parse($options);
  while (my ($o, $v) = each %options) {
    $self->add_option( $o => $v );
  }
}

# delete options from string
sub delete_options {
  my $self = shift;
  my $options = shift;
  $options =~ s/^\s+//;
  $options =~ s/\s+$//;
  my @options = split(/,\s*/, $options);
  foreach my $o (@options) {
    $self->delete_options($o);
  }
}

# add test
sub add_test {
  my $self = shift;
  my $tname = shift;
  my $ekb_file = shift;
  ERROR "Incorrect options for test \"%s\": (%s)", $tname, join(" ", @_)
    if scalar(@_) % 2;
  my %def = @_;
  if ($self->get_test($tname)) {
    ERROR "Test name %s exists already in %s", $tname, $self->name();
    return undef;
  }

  # check if we should select only part of the EKB
  my $select = undef;
  if (defined $def{select}) {
    $select = $def{select};
  }
  
  # set test data files
  $self->set_test_data($tname, $ekb_file, $select);
  
  # TODO: flesh out handling of options
  $self->{tests}{$tname} = {};
  if (defined $def{options}) {
    $self->{tests}{$tname}{options} = { _options_parse($def{options}) };
  }

  DEBUG 2, "added test %s: %s", $tname, Dumper($self->{tests}{$tname});

  return $self->{tests}{$tname};
}

# set test data files
sub set_test_data {
  my $self = shift;
  my $tname = shift;
  my $ekb_file = shift;
  my $sid = shift;
  
  # read EKB
  my $ekb = EKB->new($ekb_file)
    or FATAL "Could not read EKB from: %s", $ekb_file;

  # select for sentence id, if desired
  if (defined $sid) {
    $ekb->crop($sid);
  }

  # get text
  my ($first_para) = $ekb->get_paragraphs();
  my $text = $first_para->textContent;
  # add \n at the end if it's not there already
  chomp $text;
  $text .= "\n";
  # set new EKB id
  $ekb->set_attr('id', $tname);
  # normalize EKB
  $ekb->normalize();

  # done: write files
  my $test_dir = catdir($self->base(), $self->name());
  make_path($test_dir); # if error will fail later
  my $in_file = catfile($test_dir, $tname . ".txt");
  write_file $in_file, { binmode => ":utf8" }, $text
    or FATAL "Cannot write to file $in_file";
  my $out_file = catfile($test_dir, $tname . ".ekb");
  $ekb->print($out_file);

  return 1;
}

# change test name
# N.B.: this is done as add_test(new_name) + delete_test(old_name)
sub test_rename {
  my $self = shift;
  my ($tname, $new_tname) = @_;
  my $test = $self->get_test($tname);
  unless ($test) {
    WARN "Test %s not found in suite %s", $tname, $self->name();
    return;
  }
  my $new = $self->add_test($new_tname, $self->test_get_output($tname));
  $new->{options} = $test->{options};
  DEBUG 2, "temp:\n%s", Dumper($self);
  $self->test_delete($tname);
  DEBUG 2, "new:\n%s", Dumper($self);
}

# delete a test from this suite
sub test_delete {
  my $self = shift;
  my $tname = shift;
  unless ($self->get_test($tname)) {
    WARN "Test %s not found in suite %s", $tname, $self->name();
    return;
  }
  my $in_file = $self->test_get_input($tname);
  my $out_file = $self->test_get_output($tname);
  unlink $in_file
    or warn "Could not delete $in_file: $!";
  unlink $out_file
    or warn "Could not delete $out_file: $!";
  delete $self->{tests}{$tname};
}

# add/change test option
sub test_add_option {
  my $self = shift;
  my ($tname, $option, $value) = @_;
  my $test = $self->get_test($tname);
  unless ($test) {
    WARN "Test %s not found in suite %s", $tname, $self->name();
    return;
  }
  $test->{options}{$option} = $value;
}

# delete test option
sub test_delete_option {
  my $self = shift;
  my ($tname, $option) = @_;
  my $test = $self->get_test($tname);
  unless ($test) {
    WARN "Test %s not found in suite %s", $tname, $self->name();
    return;
  }
 (exists $test->{options})
   or return undef;
  delete $test->{options}{$option};
}

# add/change test options from string
sub test_add_options {
  my $self = shift;
  my ($tname, $options) = @_;
  my $test = $self->get_test($tname);
  unless ($test) {
    WARN "Test %s not found in suite %s", $tname, $self->name();
    return;
  }
  my %options = _options_parse($options);
  DEBUG 1, "test %s: adding options:\n%s", $tname, Dumper(\%options);
  while (my ($o, $v) = each %options) {
    DEBUG 1, "test %s: adding %s => %s", $tname, $o, $v;
    $self->test_add_option($tname, $o => $v );
  }
}

# delete test options from string
sub test_delete_options {
  my $self = shift;
  my ($tname, $options) = @_;
  my $test = $self->get_test($tname);
  unless ($test) {
    WARN "Test %s not found in suite %s", $tname, $self->name();
    return;
  }
  $options =~ s/^\s+//;
  $options =~ s/\s+$//;
  my @options = split(/,\s*/, $options);
  foreach my $o (@options) {
    $self->test_delete_option($tname, $o );
  }
}

# get test input filename
sub test_get_input {
  my $self = shift;
  my $tname = shift;
  my $test = $self->get_test($tname);
  unless ($test) {
    WARN "Test %s not found in suite %s", $tname, $self->name();
    return undef;
  }
  my $in_file =
    (exists  $test->{input})
    ? $test->{input}
    : $tname . ".txt";
  return catfile($self->get_data_dir(), $in_file);
}

# get test ekb filename
sub test_get_output {
  my $self = shift;
  my $tname = shift;
  my $test = $self->get_test($tname);
  unless ($test) {
    WARN "Test %s not found in suite %s", $tname, $self->name();
    return undef;
  }
  unless (exists $test->{output}) {
    ERROR "No reference EKB set for $tname!";
    return undef;
  }
  return map { catfile($self->get_data_dir(), $_) } @{ $test->{output} };
}

# get reference EKB
sub test_get_ref_ekbs {
  my $self = shift;
  my $tname = shift;
  my @ekb_files = $self->test_get_output($tname)
    or return undef;
  return map { EKB->new($_) } @ekb_files;
}

# set test hyps
sub test_set_hyp {
  my $self = shift;
  my ($tname, $hyp_file, $hyp_ekb) = @_;
  my $test = $self->get_test($tname);
  unless ($test) {
    WARN "Test %s not found in suite %s", $tname, $self->name();
    return undef;
  }
  $test->{hyp} = $hyp_file;
  $test->{hyp_ekb} = $hyp_ekb;
}

# get test hypothesis EKB file
sub test_get_hyp {
  my $self = shift;
  my $tname = shift;
  my $test = $self->get_test($tname);
  unless ($test) {
    WARN "Test %s not found in suite %s", $tname, $self->name();
    return undef;
  }
  return $test->{hyp} if (exists $test->{hyp});
  return undef;
}
# get test hypothesis EKB
sub test_get_hyp_ekb {
  my $self = shift;
  my $tname = shift;
  my $test = $self->get_test($tname);
  unless ($test) {
    WARN "Test %s not found in suite %s", $tname, $self->name();
    return undef;
  }
  return $test->{hyp_ekb} if (exists $test->{hyp_ekb});
  return EKB->new($test->{hyp}) if (exists $test->{hyp});
  return undef;
}

## run

sub test_run {
  my $self = shift;
  my ($test, $options) = @_;

  my @refs = $self->test_get_ref_ekbs($test); 
  my $hyp = $self->test_get_hyp_ekb($test);
  unless ($hyp) {
    # nothing to do
    return;
  }
  my @options = $self->options();
  push @options, _options_parse($options);
  DEBUG 2, "Run options for %s::\n%s", $test, Dumper(\@options);

  # TODO: loop for all refs, choose the best!
  foreach my $ref (@refs) {
    my $comp = EKB::Compare->new( $ref,
				  $hyp,
				  @options
				);
    my $result = $comp->compare();
    DEBUG 1, "ref: %s\n%s", $ref->get_file, Dumper($comp->summary);
    if (! exists $self->{tests}{$test}{comp}) {
      $self->{tests}{$test}{comp} = $comp;
      $self->{tests}{$test}{result} = $result;
    }
    else {
      my $prev_comp = $self->{tests}{$test}{comp};
      if ($comp->summary->{r} > $prev_comp->summary->{r}) {
	DEBUG 1, "found a better match";
	$self->{tests}{$test}{comp} = $comp;
	$self->{tests}{$test}{result} = $result;	
      }
    }
  }
}

sub run {
  my $self = shift;
  my $options = shift;

  $self->add_options($options);
  
  foreach my $test ($self->tests()) {
    $self->test_run($test);
  }
}

# a match against a test is successful iff the ekb was generated from the
# test's input
# we verify this by looking at paragraph/@file; ekb id must match test name
# and lastdir must match the test set name
# TODO: we might want to match based on text itself -- eg, if testname has
# changed 
sub match_ekb_hyp {
  my $self = shift;
  my ($ekb_file) = @_;
  
  my $ekb = EKB->new($ekb_file);
  unless ($ekb) {
    ERROR "Could not read EKB from: %s. Skipping it.", $ekb_file;
    return 0;
  }
  my ($para) = $ekb->get_paragraphs();
  my ($input_fn, $input_dir) = fileparse($para->getAttribute('file'),
					 ".txt");
  return unless $self->name eq basename($input_dir);
  my $tname = first { $ekb->get_attr('id') eq $_ } $self->tests();
  if ($tname) {
    INFO "Matched %s against %s:%s",
      $ekb_file, $self->name, $tname;
    $self->test_set_hyp($tname, $ekb_file, $ekb);
    return 1;
  }
  ERROR "Could not match %s. Skipping it.", $ekb_file;
  return 0;
}

# save run results
sub save_run_results {
  my $self = shift;
  my $dir = shift;
  my $summ_file = catfile($dir, $self->name() . ".summ");
  open my $fh, ">", $summ_file
    or FATAL "Cannot write to %s", $dir;

  printf $fh "Test suite: %s\n", $self->name();
  my @tests = $self->tests();
  my ($np, $nf, $ns) = (0, 0, 0); # passed, failed, skipped
  my $test_details = "";
  foreach my $tname (@tests) {
    my $test = $self->get_test($tname);
    if (! defined($test->{result})) {
      $ns++;
      next;
    }
    if ($test->{result}) {
      $np++;
    } else {
      $self->test_save_diffs($tname, $dir);
      $nf++;
    }
    $test_details .=
      sprintf("Test %s %s\n",
	      $tname, _result_to_string($test->{result}));
  }
  printf $fh "Tests in set: %d\n", scalar(@tests);
  printf $fh "Performed: %d\n", $np+$nf;
  printf $fh "  Passed: %d (%.2f%%)\n", $np, 100*$np/($np+$nf)
    if ($np+$nf) > 0;
  printf $fh "  Failed: %d (%.2f%%)\n", $nf, 100*$nf/($np+$nf)
    if ($np+$nf) > 0;
  printf $fh "Skipped: %d\n", $ns
    if ($ns > 0);
  print $fh '='x8 . "\n";
  print $fh "Tests\n";
  printf $fh $test_details;
  close $fh;
}

sub test_save_diffs {
  my $self = shift;
  my $test = shift;
  my $dir = shift;
  my $diff_dir = catdir($dir, $self->name());
  make_path($diff_dir); # if error will fail later
  my $diff_file = catfile($dir, $self->name(), $test . ".diff");
  open my $fh, ">", $diff_file
    or FATAL "Cannot write to %s", $dir;
  binmode $fh, ":encoding(UTF-8)";
  printf $fh "Test %s\n", $test;
  printf $fh "REF: %s\n", $self->test_get_output($test);
  printf $fh "HYP: %s\n", $self->test_get_hyp($test);
  printf $fh "%s\n", ($self->get_test($test))->{comp}->diffs_as_string;
  close $fh;
}
  
## helpers

# parse options string
# examples:
# 1. o1=v1,o2,o3=[v2 v3]
# 2. o1 => v1, o2, o3 => [v2  v3]
# both result in { o1 => v1, o2 => 1, o3 => [v2, v3] }
sub _options_parse {
  my $opt_string = shift;

  my %opts;

  return %opts unless $opt_string;
  
  $opt_string =~ s/^\s+//;
  foreach my $opt (split(/,\s*/, $opt_string)) {
    $opt =~ s/\s+$//;
    $opt
      or FATAL "Cannot parse option: $opt";
    my ($o, $v) = split(/\s*=>?\s*/, $opt);
    $o =~ m{^\w+$}
      or FATAL "Cannot parse option: $opt";
    (! defined $v) || ($v =~ m{\S})
      or FATAL "Cannot parse option: $opt";
    (! exists $opts{$o})
      or FATAL "Duplicate option: $o";
    $v = $v // 1;
    if ($v =~ m{^\[(.*)\]\s*$}) { # multi-value
      my @vals = split(/ +/, $1);
      $opts{$o} = \@vals;
    } else {
      $opts{$o} = $v;
    }
  }

  return %opts;
}

# inverse of _options_parse
sub _options_tostring {
  my %opts = @_;

  my $result = "";

  foreach my $o (sort keys %opts) {
    my $v = $opts{$o};
    $result .= ", "
      if $result;
    $result .= "$o => ";
    $result .=
      (ref($v) eq 'ARRAY') 
      ? "[" . join(" ", @$v) . "]"
      : $v;
  }

  return $result;
}

sub _result_to_string {
  my $r = shift;
  if ($r == 1) {
    return "passed";
  } elsif ($r == 0) {
    return "failed";
  } else {
    return "not tested";
  }
}

1;
