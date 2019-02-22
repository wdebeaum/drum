# Log.pm
#
# Time-stamp: <Thu Feb 21 12:00:31 CST 2019 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 29 Apr 2016
#

# Basic logging utilities

#----------------------------------------------------------------
# History
# 2016/04/29 v1.0	lgalescu
# - Created, based on code I had been moving from script to script
#   for many years. Added pod info.
# 2016/04/30 v1.1	lgalescu
# - Added caller info to messages. Improved pod.
# 2016/06/06 v1.2	lgalescu
# - Changed to use IO::Handle.
# 2016/10/05 v1.3	lgalescu
# - Removed use of IO::Handle -- I don't know what I was thinking!
# - Fixed caller information.
# 2017/01/11 v1.4	lgalescu
# - Made default not to print caller info (too verbose).
# 2017/02/03 v1.4.1	lgalescu
# - Made part of TRIPS util (changed package)
# 2019/02/21 v1.4.2	lgalescu
# - Fixed calls to printf to avoid invalid conversion warnings.

package util::Log;

$VERSION = '1.4.2';
  
use strict 'vars';
use warnings;
no warnings "uninitialized";
use open qw(:utf8 :std);

require Exporter;

our @ISA = qw(Exporter);

our @EXPORT = qw(
		  INFO WARN DEBUG ERROR FATAL
	       );

our $DebugLevel = 0;
our $Quiet = 0;
our $CallerInfo = 0;

# autoflush STDERR
my $oldfh = select(STDERR); $| = 1; select($oldfh);

sub INFO {
    my ($fmt, @args) = @_;
    unless (@args) {
	push @args, $fmt;
	$fmt = "%s";
    }
    printf STDERR "[INFO] ".$fmt."\n", @args;
}

sub WARN {
    my ($fmt, @args) = @_;
    return if $Quiet;
    my $call_info = "";
    if ($CallerInfo) {
	my ($caller, $file, $line) = caller(0);
	$call_info = sprintf(" at %s:%d", $file, $line);
    }
    unless (@args) {
	push @args, $fmt;
	$fmt = "%s";
    }
    printf STDERR "[WARNING%s] ".$fmt."\n", $call_info, @args;
}

sub DEBUG {
    my ($level, $fmt, @args) = @_;
    return if $Quiet;
    return if $level > $DebugLevel;
    my $call_info = "";
    if ($CallerInfo) {
	my ($caller, $file, $line, $func) = caller(0);
	$call_info = sprintf(" at %s:%d in %s()", $file, $line, $func);
    }
    unless (@args) {
	push @args, $fmt;
	$fmt = "%s";
    }
    printf STDERR "[DEBUG(%d)%s] ".$fmt."\n", $level, $call_info, @args;
}

sub ERROR {
    my ($fmt, @args) = @_;
    return if $Quiet;
    my $call_info = "";
    if ($CallerInfo) {
	my ($caller, $file, $line, $func) = caller(0);
	$call_info = sprintf(" at %s:%d in %s()", $file, $line, $func);
    }
    unless (@args) {
	push @args, $fmt;
	$fmt = "%s";
    }
    printf STDERR "[ERROR%s] ".$fmt."\n", $call_info, @args;
}

sub FATAL {
    my ($fmt, @args) = @_;
    my $call_info = "";
    if ($CallerInfo) {
	my ($caller, $file, $line, $func) = caller(0);
	$call_info = sprintf(" at %s:%d in %s()", $file, $line, $func);
    }
    unless (@args) {
	push @args, $fmt;
	$fmt = "%s";
    }
    die sprintf("[FATAL%s] ".$fmt."\n", $call_info, @args);
}

1;

__END__

=pod

=head1 NAME

util::Log - a few basic logging utilities

=head1 VERSION

version 1.1

=head1 SYNOPSIS

  use util::Log;
  $util::Log::DebugLevel = 3;
  INFO "Nice day, today!";
  WARN "It may be raining on %s!", "Tuesday";
  DEBUG 2, "It will be raining for %d days", 3; # will print
  DEBUG 4, "Expected rain amount: %.2f inches", 2.09; # will not print
  ERROR "You can't take the car because you don't own one!";
  {
    local $util::Log::Quiet = 1;
    DEBUG 1, "Be sure to take your umbrella!"; # will not print
  }
  DEBUG 1, "Umbrella not found!";
  FATAL "I can't take it any more! ($!)";

=head1 DESCRIPTION

This logger uses the following (admittedly, idiosyncratic) conventions:

=over

=item 1. B<INFO> and B<FATAL> messages are always printed.

=item 2. Setting B<$util::Log::Quiet> to C<1> disables all B<WARN>, B<DEBUG> 
and B<ERROR> messages.

=item 3. B<DEBUG> messages include a level that controls whether they should 
be printed. If that level is lower or equal to B<$util::Log::DebugLevel>, 
the message is printed (unless overridden by B<$util::Log::Quiet>).

=back

As can be seen in the examples above, this logger's functions can use 
B<sprintf> formatting.

All logging messages are printed to B<stderr>.

All logging messages except for B<INFO> may print the file and line that 
generated the message. You can turn this off by setting 
B<$util::Log::CallerInfo> to C<1> (default is C<0>).

=head1 DEPENDENCIES

None.

=head1 SEE ALSO

L<Carp>

=head1 BUGS AND LIMITATIONS

No known bugs.

=head1 AUTHOR

Lucian Galescu <lgalescu@ihmc.us>

=head1 COPYRIGHT AND LICENCE

Copyright (C) 2016-2019 by Lucian Galescu <lgalescu@ihmc.us>.

This module is free software. You can redistribute it and/or modify it under 
the terms of the Artistic License 2.0.

This program is distributed in the hope that it will be useful, but without 
any warranty; without even the implied warranty of merchantability or fitness 
for a particular purpose.

=cut

