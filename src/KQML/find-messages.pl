#!/usr/bin/perl

# find-messages.pl
#
# Time-stamp: <Tue Dec 11 20:32:44 CST 2018 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>,  7 Jun 2006
#

#----------------------------------------------------------------
# Description:
# Filters TRIPS Facilitator logs for messages from/to given modules.

#----------------------------------------------------------------
# History:
# 2006/06/07 lgalescu
# - Created.
# 2007/02/12 lgalescu
# - Mod: Added message filters -- for now only w/ -cfg!
# - Changed module name RE to match anonymous module names (ie, SocketClient-\d+).
# 2007/02/14 lgalescu
# - Mod: everything is now case-insensitive.
# 2007/02/16 lgalescu
# - Fix: bugs.
# 2007/02/22 lgalescu
# - Fix: command-line params allow message filters.
# - Add: line number mode (-n)
# 2007/03/26 lgalescu
# - Fix: bug (heading was repeated for every line in multi-line messages).
# 2008/12/02 lgalescu
# - Add: -debug option
# 2010/05/10 lgalescu
# - Fix: multi-line messages
# - Add: RE patterns
# 2010/05/25 lgalescu
# - Add: no markup output
# 2016/10/21 lgalescu
# - Add: pattern for any module.
# 2016/10/25 lgalescu
# - Now using Getopt::Long for handling command line parameters.
# - Added option to print log info (first line of log).
# - Improved help info.
# 2016/12/15 lgalescu
# - Fixed bug where any-filters wouldn't apply to named modules.
# 2017/08/18 lgalescu
# - Fixed handling of regex filters.
# 2017/09/21 lgalescu
# - Handle exclusions in configuration file.
# - Added quiet mode.
# - Added possibility of specifying input logs on the command line.
# - Now parsing messages via KQML::KQML.
# - Todo: use KQML::KQML for reading messages, as well!
# 2017/09/22 lgalescu
# - Changed how multiple patterns can be specified as values of a single option
#   (separator was space, no it's comma). Now patterns don't need special 
#   handling, they can all be handled via GetOpt.
# - Renamed to find-messages.pl (was: checklog).
# - Added to TRIPS in src/KQML/.
# 2017/10/02 lgalescu
# - Fixed bug.
# 2018/12/10 lgalescu
# - Added possibility of specifying performative in module filters.
# - Simplified grammar somewhat.

#----------------------------------------------------------------
# Usage:
# 
# the configuration file format:
#   CFG_FILE  ::= (LINE\n)*
#   LINE      ::= FROM | TO | EXCLUDE | COMMENT
#   FROM      ::= "from:" M_PATTERN [...]
#   TO        ::= "to:" M_PATTERN [...]
#   EXCLUDE   ::= "exclude:" X_PATTERN [...]
#   M_PATTERN ::= MODULE(:FILTER)* | M_PATTERN,M_PATTERN
#   FILTER    ::= C_PATTERN | /PERL-PATTERN/
#   C_PATTERN ::= (PERF#)?CONT-HEAD
#   X_PATTERN ::= PERF(#CONT-HEAD)? | X_PATTERN,X_PATTERN
#   MODULE    ::= <symbol> | "*"
#   PERF      ::= KQML-PERFORMATIVE
#   CONT-HEAD ::= <symbol>
#   COMMENT   ::= "#.*"

# command line options
#   options   ::= option*
#   option    ::= gen_opt | from_opt | to_opt | excl_opt
#   gen_opt   ::= -d N | -n | -non | -m | -nom | -i | -noi | -cfg FILE | -q
#   from_opt  ::= -from M_PATTERN
#   to_opt    ::= -to M_PATTERN
#   excl_opt  ::= -x X_PATTERN

my $usage;
my $path;

BEGIN {
  # Just use basename of program, rather than whole path.
  $0 =~ s:.*/::;
  $usage = "Usage: $0 [OPTIONS] [PATTERNS] [FILES]

OPTIONS

  --debug N, -d N
\tSets debugging level (default: 0).

  --line-numbers, -n
  --noline-numbers, -no-line-numbers, -non
\tControls whether line numbers are printed or supressed (default: supressed).

  --markup, -m
  --nomarkup, --no-markup, -nom
\tControls whether log markup is printed or supressed (default: printed).

  --loginfo, -i
  --nologinfo, --no-loginfo, -noi
\tControls whether log info is printed or supressed (default: supressed).

  --cfg FILE, -c FILE
\tReads filtering rules from configuration file (if option is repeated, only the last one matters).

  --quiet, -q
\tQuiet mode: output is supressed. Useful when only the exit status is of interest.


PATTERNS

1. exclusion patterns

  --exclude PERF[#CONTENT][,...]
  -x PERF[#CONTENT][,...]
  Exclude messages -- from any module! -- matching the performative PERF and, if 
  given, the specified content. Can be repeated. Multiple patterns can also be 
  joined with a comma.
  Note: Currently the content can only be identified by the head. 

2. search patterns

  --from MODULE[:FILTER]*
  -f MODULE[:FILTER]*
  --to MODULE[:FILTER]*
  -t MODULE[:FILTER]*
  where:
  - MODULE is the name of a module/component; \"*\" will match any 
    module/component
  - FILTER may be a symbolic pattern or a regex pattern, as follows: 
    - [PERF#]CONTENT
      This filter catches messages based on the content (head). Optionally, the 
      performative may be specified as an additional constraint (Note: it is 
      unusual for the same content to appear in different performatives).
    - /REGEX_PATTERN/
      This filter (using Perl regexp syntax) applies to the whole performative. 
      N.B.: When using regex patterns, you might have to quote the full pattern 
      or use double slashes. 
      While regex filters are more flexible, be warned that KQML syntax does not 
      specify the order in which performative attributes appear. Thus, the 
      following are not equivalent (though they will likely be in most cases):
        -from MODULE:TELL#REPORT
        -from MODULE:\"/^\\(TELL :CONTENT \\(REPORT/\"
  If no filter is specified, all messages from or to (depending on the option) 
  MODULE match (except for the ones specifically excluded).

  Both options can be repeated. Also, multiple patterns can be joined with a 
  comma.


FILES

A list of facilitator.log files. If no file arguments are specified, the standard input is used.


EXIT STATUS 

0 if any messages are found.
1 if no messages are found.
";

  my $TRIPS_BASE_DEFAULT = "/usr/local";
  my $TRIPS_BASE = $ENV{TRIPS_BASE} || $TRIPS_BASE_DEFAULT;
  $path = "$TRIPS_BASE/etc";
}

use Getopt::Long;
use Data::Dumper;

use lib $path;
use KQML::KQML;

#----------------------------------------------------------------
# Global variables
#----------------------------------------------------------------     

# filter structures
# %h = { module => { [ content  => { X => 1, Y => 1, ... }
#                      patterns => { /P/ => 1, /Q/ => 1, ... } 
#                    ] } }
my %hfrom;
my %hto;

my @line;

my $exit_status = 1;

#----------------------------------------------------------------
# Command-line arguments
#----------------------------------------------------------------
my $debugLevel = 0;

our (
     $cfg_file,
     $lineno_flag,
     $markup_flag,
     $loginfo_flag,
     $quiet_flag,
     @excluded, 
     @from, 
     @to, 
    );

# defaults
$lineno_flag  = 0;
$markup_flag  = 1;
$loginfo_flag = 0;
$quiet_flag   = 0;

# parse command-line options
GetOptions(
	   'c|cfg=s' => \$cfg_file,
           'd|debug=i' => \$debugLevel,
	   'f|from=s' => \@from,
           'h|help' => \$help,
	   'i|loginfo!' => \$loginfo_flag,
	   'm|markup!' => \$markup_flag,
	   'n|line-numbers!' => \$lineno_flag,
	   'q|quiet!' => \$quiet_flag,
	   't|to=s' => \@to,
	   'x|exclude=s' => \@excluded,
          );

die $usage if $help;

@excluded = split(/,/,join(',',@excluded));
@from = split(/,/,join(',',@from));
@to = split(/,/,join(',',@to));

foreach my $fpat (@from) {
  process_cfg_opt(\%hfrom, uc($fpat));
}
foreach my $fpat (@to) {
  process_cfg_opt(\%hto, uc($fpat));
}
if (defined $cfg_file) {
  read_cfg($cfg_file);
}

propagate_any_filters();

debug(2, "from:\n" . Dumper(\@from) . "to:\n" . Dumper(\@to));
debug(2, "from:\n" . Dumper(\%hfrom) . "to:\n" . Dumper(\%hto));

# if we don't have any filters, we match anything
if (!%hfrom and !%hto) {
  process_cfg_opt(\%hfrom, "*");
  process_cfg_opt(\%hto, "*");
}

$Data::Dumper::Terse = 1;

debug(1, sprintf("Options: -n=%d -m=%d -i=%d", $lineno_flag, $markup_flag, $loginfo_flag));
debug(1, "from:\n" . Dumper(\%hfrom) . "to:\n" . Dumper(\%hto));
debug(1, "exclusions:\n" . Dumper(\@excluded));

#----------------------------------------------------------------
# Main
#----------------------------------------------------------------
my @from = keys %hfrom;
my @to = keys %hto;

my $print_msg_maybe = 0; # controls what we buffer (optimization)

my $lineno = 0;
my $filter;

# N.B.: we are only looking for messages! XML elements that do not contain messages 
# are skipped automatically.
while (<>) {
  $lineno++;
  # look for log info
  if (/^<LOG/ && $loginfo_flag) {
    print;
  }
  # look for next message
  elsif (/^<R.*S=\"([-A-Z]+\d*)\">/i) {
    # new message from $1
    if ($hp = get_filters($1, \%hfrom)) {
      debug(2, "matched :from " . uc($1) . " [$.]");
      $print_msg_maybe = 1;
      $buffer = $_ if $markup_flag;
    }
  }
  elsif (/^<S.*R=\"([-A-Z]+\d*)\">/i) {
    # new message to $1
    if ($hp = get_filters($1, \%hto)) {
      debug(2, "matched :to " . uc($1) . " [$.]");
      $print_msg_maybe = 1;
      $buffer = $_ if $markup_flag;
    }
  }
  elsif (/^<\/[SR]>/) {
    # message finished
    my $print_msg = 0;
    if ($print_msg_maybe) {
      debug(3, "message: $buffer");
      my ($perf, $content) = parse_kqml($buffer);
      {
	debug(2, "  performative: $perf; content: $content");
	local $Data::Dumper::Indent = 0;
	debug(2, "  hp:" . Dumper($hp));
      }
      if (is_excluded($perf,$content)) { # excluded
	# nop
      } elsif ($hp == 1) {				# unconditional print
	$print_msg = 1;
      } elsif (exists $hp->{content}{$content}) {	# always print content specified in filter
	$print_msg = 1;
      } elsif (exists $hp->{content}{"$perf#$content"}) {	# always print content specified in filter
	$print_msg = 1;
      } else {						# check patterns
	if (exists $hp->{patterns}) {
	  foreach $pattern (keys %{ $hp->{patterns} }) {
	    $pattern = substr($pattern,1, -1); # remove slashes
	    if ($buffer =~ m/$pattern/i) {
	      $print_msg = 1;
	      last;
	    }
	  }
	}
      }
      if ($print_msg) {
	$buffer .= $_ if $markup_flag;
	$exit_status = 0;
	printf "%s\n", $buffer
	  unless $quiet_flag;
      }
    }
    $buffer = "";
    $print_msg_maybe = 0;
  }
  else {
    # body of message (or junk?)
    if ($print_msg_maybe) {
      $buffer .= ($lineno_flag ? $lineno . ": " : "") . $_;
    }
  }
}

exit $exit_status;

#----------------------------------------------------------------
# Subroutines
#----------------------------------------------------------------
sub read_cfg {
  my $file = shift;

  open(CFG, "<$file")
    or { warn "File not found: $file\n" && return };

  while (<CFG>) {
    next if (/^#/);		# skip comments
    next if (/^\s*$/);		# skip blank lines
    chomp;
    my $cfg_opt = $_;
    debug(2, "cfg line: $cfg_opt");
    # performative exclusions
    if (m/^exclude:\s*(.*)$/) {
      push @excluded, split(/\s+/, $1);
      next;
    }
    # message rules
    $cfg_opt =~ m/^(from|to):\s*/
      or die "Incorrect option: $cfg_opt";
    my $spec = lc($1);
    $cfg_opt =~ s/^\S+:\s*//;
    if ($spec eq "from") {
      process_cfg_opt(\%hfrom, $cfg_opt);
    } elsif ($spec eq "to") {
      process_cfg_opt(\%hto, $cfg_opt);
    } else {
      warn "Unexpected line in cfg file: $_\n";
    }
  }
    
  close(CFG);
}

# grammar:
# <cfg_opt> ::= <mod_opt>( <mod_opt>)*
# <mod_opt> ::= <module>(:<filter>(,<filter>)*)?
# <module> ::= MODULE_NAME | "*"
# <filter> ::= CONTENT_NAME | /REGEX/
sub process_cfg_opt {
  my $hp = shift;			# which hash to use
  my $arg = shift;
  debug(2, "cfg opt: $arg");
  my @mod_opts = parse_cfg_opt(uc($arg));	# config line

  foreach $mod_opt (@mod_opts) {
    debug(2, Dumper($mod_opt));
    my ($module, @filters) = @$mod_opt;
    debug(2, "handling: $module [@filters]");
    if (@filters) {
      foreach $f (@filters) {
	add_hash_2($hp, $module, $f);
      }
    } else {
      add_hash_2($hp, $module, undef);
    }
  }
}

# iteratively parse configuration option for all module filters
sub parse_cfg_opt {
  my $s = shift;
  my @result;
  while (length($s) > 0) {
    my ($mod_opt, $rest) = parse_mod_opt($s);
    push @result, $mod_opt;
    $s = $rest;
  }
  debug(2, sprintf("Found %d mod_opts", scalar(@result)));
  return @result;
}

# parse a module filter
# $s => $module:$filters $rest
sub parse_mod_opt {
  my $s = shift;
  $s =~ m/^([^:\s]*)(?:(:.*)|(?:\s+(.*))?)/
    or die "Incorrect configuration string: $s";
  my ($module, $filters, $rest) = ($1, $2, $3);
  debug(2, "module=$module, filters=$filters, rest=$rest");
  if ($filters) {
    my ($filters, $rest) = parse_filters($filters);
    return [$module, @$filters], $rest;
  } else {
    return [$module], $rest;
  }
}

# recursively parse a set of joined filters
# $s => :$filters $rest
sub parse_filters {
  my $s = shift;
  if (length($s) == 0) {
    return;
  }
  my ($filter, $filters, $rest);
  if (substr($s,0,2) eq ':/') {
    # regex filter
    $s =~ m{^:(/(?:\\/|[^/])*/)(?:(:.*)|(?:\s+(.*))?)}
      or die "Incorrect filter: $s";
    ($filter, $filters, $rest) = ($1, $2, $3);
    debug(2, "got regex filter: $filter");
  } elsif (substr($s,0,1) eq ':') {
    # name filter
    $s =~ m/^:([^:\s]*)(?:(:.*)|(?:\s+(.*))?)/
      or die "Incorrect filter: $s";
    ($filter, $filters, $rest) = ($1, $2, $3);
    debug(2, "got content filter: $filter");
  } else {
    die "Incorrect configuration string: $s";
  }
  if ($filters) {
    my ($more_filters, $rest) = parse_filters($filters);
    return [$filter, @$more_filters], $rest;
  } else {
    return [$filter], $rest;
  }
}

sub add_hash_2 {
  my $hp = shift;		# pointer to hash table
  my $k1 = shift;		# level 1 key
  my $k2 = shift;		# level 2 key

  if (! exists $hp->{$k1}) {	# no level-1 key (new module)
    if (defined $k2) {		# new level-2 key
      debug(3, "new L-2: <$k1, $k2>");
      if ($k2 =~ m|\A/(.*)/\Z|) { # re
	$hp->{$k1}{patterns}{$k2} = 1;
      } else {			# content heading
	$hp->{$k1}{content}{$k2} = 1;
      }
    } else {			# new level-1 key
     debug(3, "new L-1: <$k1>");
      $hp->{$k1} = 1;
    }
  } elsif ($hp->{$k1} == 1) {	# no level-2 keys 
    if (defined $k2) {		# replace level-1 key w/ new level-2 key
      delete $hp->{$k1};
      debug(3, "delete L-1; new L-2: <$k1, $k2>");
      if ($k2 =~ m|\A/(.*)/\Z|) { # re
	$hp->{$k1}{patterns}{$k2} = 1;
      } else {			# content heading
	$hp->{$k1}{content}{$k2} = 1;
      }
    } else {			# ignore: duplicate level-1 key
      debug(3, "duplicate L-1: <$k1>");
    }
  } elsif (! defined $k2) { # ignore: request for level-1 when we already have level-2 keys
    debug(3, "ignore L-1: <$k1>; have L-2");
  } elsif ((exists $hp->{$k1}{patterns}{$k2}) ||
	   (exists $hp->{$k1}{content}{$k2})) { # ignore: duplicate level-2 key
    debug(3, "duplicate L-2: <$k1, $k2>");
  } else {			# new level-2 key
    debug(3, "new L-2: <$k1, $k2>");
    if ($k2 =~ m|\A/.*/\Z|) {	# re
      $hp->{$k1}{patterns}{$k2} = 1;
    } else {			# content heading
      $hp->{$k1}{content}{$k2} = 1;
    }
  } 
}

sub pprint_hash_2 {
  my $hp = shift;		# pointer to hash table
  my $result;

  foreach $k1 (sort keys %{ $hp }) { # modules
    $result .= sprintf("%s => {", $k1);
    if ($hp->{$k1} != 1) {
      foreach $kt (keys %{ $hp->{$k1} }) { # "content" &| "patterns"
	$result .= sprintf("\n\t%s => {", $kt);
	@keys2 = keys %{ $hp->{$k1}{$kt} }; # content heads, patterns 
	$result .= sprintf($keys2[0]);
	foreach $k2 (@keys2[1..$#keys2]) {
	  $result .= sprintf(", %s", $k2);
	}
	$result .= sprintf("}");
      }
    }
    $result .= sprintf("\n}\n");
  }

  return $result;
}

# propagate "any" filters to named modules
sub propagate_any_filters {
  foreach my $filter (\%hfrom, \%hto) {
    if (exists $filter->{"*"}) {
      foreach my $m (keys %$filter) {
	next if $m eq "*";
	foreach my $type ("content", "patterns") {
	  if ((exists $filter->{"*"}{$type}) and (exists $filter->{$m}{$type}))
	    {
	      foreach my $c (keys $filter->{"*"}{$type}) {
		add_hash_2($filter, $m, $c);
	      }
	    }
	}
      }
    }
  }
}

# find applicable filters for module $m from $filter hash
sub get_filters {
  my ($m, $filter) = @_;
  if (exists $filter->{uc($m)}) {
    return $filter->{uc($m)};
  }
  if (exists $filter->{"*"}) {
    return $filter->{"*"};
  }
  return undef;
}

# check if performative is excluded
sub is_excluded {
  my ($perf, $chead) = @_;
  foreach my $e (@excluded) {
    if (uc($perf) eq uc($e)) {
      debug(1, "  excluded: $perf");
      return 1;
    } elsif (uc("$perf#$chead") eq uc($e)) {
      debug(1, "  excluded: $perf#$chead");
      return 1;
    }
  }
  return 0;
}

sub parse_kqml {
  my $msg = KQML::KQMLReadFromString(shift)
    or return undef;
  my $msg_hash = KQML::KQMLKeywordify($msg);
  my $perf = $msg_hash->{'verb'};
  my $content = $msg_hash->{':content'};
  my $content_verb;
  if (!defined $content) {
  } elsif (KQML::KQMLAtomIsString($content)) {
    $content_verb = $content;
  } else {
    my $content_hash = KQML::KQMLKeywordify($content);
    $content_verb = $content_hash->{'verb'};
  }
  return (uc($perf), uc($content_verb));
}

sub debug {
  my $level = shift;
  my $msg = shift;
  
  if ($level <= $debugLevel) {
    print STDERR $msg, "\n";
  }
}
