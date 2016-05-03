#!/usr/grads/bin/perl5 -w
#
# KQML.pm: New Perl5 module for handling KQML messages
#
# George Ferguson, ferguson@cs.rochester.edu, 14 Mar 1997
# Time-stamp: <Wed Jul 11 10:08:21 EDT 2007 ferguson>
#
# To view the documentation contained in this file, do:
#	pod2man KQML.pm | nroff -man | less
#
# There is a test program at the end of this file, after the END marker.
#
# Note that this representation keeps the distinction between items that
# came in as KQML strings, and those that were simply tokens. KQML strings
# keep their delimiting quotes and escape sequences. Tokens are also
# represented using Perl strings, but they don't have delimiting quotes.
# Use KQMLAtomIsString to tell if a particular Perl string represents a
# KQML string. If it returns true, use KQMLStringAtomAsPerlString to
# convert the string to a native Perl string (losing the quotes and
# interpreting the escape sequences).
#
# TODO: Quotations. They are handled properly in the Java version
# of this library, but at a significant cost in comlexity.
#

package KQML;

require 5.001;
require Exporter;
@ISA = qw(Exporter);
$VERSION = "1.00"; $VERSION = "1.00"; # Twice to shut up -w warning
@EXPORT    = qw(KQMLRead KQMLRead1 KQMLReadFromString KQMLInputCB
		KQMLKeywordify KQMLUnkeywordify KQMLAsString KQMLAtomIsString
		KQMLStringAtomAsPerlString);
@EXPORT_OK = qw(KQMLRead KQMLRead1 KQMLReadFromString KQMLInputCB
		KQMLKeywordify KQMLUnkeywordify KQMLAsString KQMLAtomIsString
		KQMLStringAtomAsPerlString);

use strict;
use warnings;

=head1 NAME

KQML - Read and parse KQML messages

=head1 SYNOPSIS

 use KQML;

 $lolref = KQMLRead(\*FILEHANDLE, \$text);

 $lolref = KQMLRead1(\*FILEHANDLE, \$text);

 $lolref = KQMLReadFromString($input);

 $MW->fileevent(STDIN,
                'readable' => [\&KQMLInputCB, \*STDIN, \&receiveFunc]);
 sub receiveFunc {
     my($lolref, $text) = @_;
     ...
 }

 $hashref = KQMLKeywordify($lolref);
 $hashref->{'list'} == $lolref
 $hashref->{'verb'} == first token
 $hashref->{':KEY'} == VALUE for each keyword/value argument
 $hashref->{'rest'} == rest of arguments

 unless (KQMLAtomIsString($hashref->{':TEXT'})) {
     die ":TEXT argument must be a string\n";
 }
 $text = KQMLStringAtomAsPerlString($hashref->{':TEXT'});

=head1 DESCRIPTION

B<KQMLRead> tries to read a KQML performative from the given filehandle
(passed as a typeglob). It returns:

=over

=item a)

A reference to a list of lists if successful (this is described in more
detail below);

=item b)

0 if EOF is reached on the filehandle;

=item c)

A scalar less than 0 if a read or parse error occurs (values less than 100
signify system call errors; other values are parse errors).

=back

B<KQMLRead1> reads a single character from the given filehandle and uses it
in an ongoing parse of a KQML performative. That is, successive calls will
read successive characters and try to parse them together. It returns as
for B<KQMLRead>, or B<undef> if the parse is not yet complete. Multiple
parses can be ongoing on different filehandles simultaneously.

B<KQMLReadFromString> tries to parse a performative out of the given
string. It returns as for B<KQMLRead>, and returns B<undef> if the
parse was incomplete at the end of the string. There is no way to know
whether there were unparsed characters left in the string.

B<KQMLInputCB> is intended for use with Perl/Tk, as the SYNPOSIS example
shows. It should be called with two parameters:

=over

=item a)

The filehandle typeglob; and

=item b)

A reference to a function to call when the read is complete.

=back

The callback function (B<receiveFunc> in the example) will be called with
two parameters:

=over

=item a)

The return value of the parse as for B<KQMLRead>; and

=item b)

A string containing the raw text of the parsed message.

=back

The function B<KQMLKeywordify> is a convenience function that takes a
reference to a list of lists as returned by the parsing functions, and
returns a reference to a hash with the following elements:

=over

=item a)

The element B<list> contains the original list reference.

=item b)

The element B<verb> contains the first word from the toplevel list;

=item c)

For each keyword/value pair in the top level of the list, there is an
element indexed by the B<keyword> (downcased) whose value is the value.
Keywords are tokens starting with a colon and the index includes the colon.
Keyword arguments must precede any other arguments.

=item d)

The element B<rest> contains any arguments not consumed by the previous
two catgeories (ie., non-keyword arguments). The value of the element is
a reference to the list of remaining arguments.

=back

The function B<KQMLUnkeywordify> is almost the inverse of
B<KQMLKeywordify>, except that it discards the B<list> element. It takes
a reference to a hash (like one returned by B<KQMLKeywordify>) and
returns a reference to a list of lists containing:

=over

=item a)

The value of the B<verb> element of the hash.

=item b)

The list of key/value pairs in the hash (except for B<list>, B<verb>, and B<rest>).

=item c)

The elements of the B<rest> element of the hash.

=back

This interface is provisional and subject to change.

The function B<KQMLAtomIsString> returns true iff its KQML atom argument
represents a KQML string. If that function returns true, you may use the
function B<KQMLStringAtomAsPerlString> to convert the atom to a native Perl
string (removing delimiting quotes and interpreting escape sequences).

The function B<KQMLAsString> is a convenience function that takes a
reference to a list of lists as returned by the parsing functions, and
returns a string. The list is printed in standard Lisp-like parenthesized
notation. You could make this acceptable to Perl by changing the
parentheses to square brackets if you wanted to B<eval> it back.

=head1 DATATYPES

If you don't know how to handle a list of lists, the following description
may help and you should probably look at the L<perllol> documentation.
First you will want to test the return value from the KQML functions to
see if it is indeed a reference to a list of lists (indicating success).
Use the B<ref> function for this. If it returns true, dereferencing
the value will give you the toplevel list. Each element of the list will
be either a scalar or a reference to a sublist, and again B<ref>
will tell you which it is.

Here is an example of a subroutine that formats the list of lists into a
string and returns it (this is effectively B<KQMLAsString>):

    sub list_as_string {
	my($listref) = @_;
	my $str = '(';
	my $item;
	foreach $item (@$listref) {
	    if (ref($item)) {
		$str .= list_as_string($item);
	    } else {
		$str .= $item;
	    }
	    $str .= ' ';
	}
	if ($str ne '(') {
	    chop($str);
	}
	$str .= ')';
    }

And here's how you might use it:

    $perf = KQMLRead(\*STDIN, \$text);
    print "text=$text\n";
    if (ref($perf)) {
	print "perf=" . list_as_string($perf) . "\n";
    } else {
	print "error=$perf\n";
    }

=head1 SEE ALSO

L<perlmod>

=head1 BUGS

Probably.

=head1 AUTHOR

George Ferguson, ferguson@cs.rochester.edu.

=cut

#	#	#	#	#	#	#	#	#	#
#
# Declarations
#

# KQML spec special chars
my $SpecialChars = '<>=+-*/&^~_@$%:.!?';

# The KQML spec isn't clear about case-sensitivity. Although I would
# expect that it should be case-sensitive, I suspect that it probably
# isn't, since Lisp isn't. Anyway, making this be ("\l$a" eq "\l$b")
# the parser case-insensitive; making this ($a eq $b) makes it case-
# sensitive.
sub STREQ {
    my($a,$b) = @_;
    "\l$a" eq "\lb";
}

# These are the same as in the C library
my $KQML_ERROR_BAD_CONTEXT = -1;
my $KQML_ERROR_BAD_START = -3;
my $KQML_ERROR_BAD_HASH = -4;
my $KQML_ERROR_READ_INTERRUPTED = -6;
my $KQML_ERROR_READ_ERROR = -101;

# States of the finite-state parser
my $START = 0;
my $NORMAL = 1;
my $QUOTED = 2;
my $BSLASH = 3;
my $HASH = 4;
my $HASHED = 5;
my $DONE = 6;
my $ERROR = 99;

################################################
#
# Parsing routines
#

sub DEBUG {
    my($msg) = @_;
    print STDERR "$0: $msg\n";
}

sub newContext {
    my %context = (state => $START, text => '', hashnum => 0);
    return \%context;
}

sub resetContext {
    my($self) = @_;
    #DEBUG("resetContext: resetting context");
    $self->{'state'} = $START;
    $self->{'text'} = '';
    $self->{'hashnum'} = 0;
}

sub initContext {
    my($self) = @_;
    #DEBUG("initContext: initializing context");
    $self->{'expr'} = [];
    $self->{'stack'} = [ $self->{'expr'} ];
}

sub emptyContext {
    my($self) = @_;
    my $result = ($#{$self->{'stack'}} < 0);
    #DEBUG("emptyContext: returning " . ($result ? 'true' : 'false'));
    $result;
}

# Adds a sublist to the current list being filled and makes it current
sub pushContext {
    my($self) = @_;
    my $stackref = $self->{'stack'};
    my $currentref = $stackref->[$#{$stackref}];
    my $newref = [];
    # Add sublist to current list
    push(@$currentref, $newref);
    # Push onto context stack
    push(@$stackref, $newref);
    #DEBUG("pushContext: depth now " . ($#{$stackref}+1));
}

# Pops to the previous list being filled
sub popContext {
    my($self) = @_;
    my $stackref = $self->{'stack'};
    pop(@$stackref);
    #DEBUG("popContext: depth now " . ($#{$stackref}+1));
}

# Sets the current element to the empty string (different from undef)
sub startElement {
    my($self, $ch) = @_;
    $self->{'element'} = '';
    #DEBUG("startElement: element=\"\"");
}

# Adds a character to the current element (token/word/string)
sub addCharToElement {
    my($self, $ch) = @_;
    if (!defined($self->{'element'})) {
	$self->{'element'} = '';
    }
    $self->{'element'} .= $ch;
    #DEBUG("addCharToElement: element=\"" . $self->{'element'} . "\"");
}

# Adds the current element to the current list being filled and resets element
sub addElementToContext {
    my($self) = @_;
    my $stackref = $self->{'stack'};
    my $currentref = $stackref->[$#{$stackref}];
    # Add element to current list
    if (defined($self->{'element'})) {
	#DEBUG("addElementToContext: element=\"" . $self->{'element'} . "\"");
	push(@$currentref, $self->{'element'});
	#DEBUG("addElementToContext: length now " . ($#{$currentref}+1));
	undef($self->{'element'});
    }
}

sub KQMLParseChar {
    my($ch, $context) = @_;
    #DEBUG("KQMLParseChar: ch=$ch");
    # Sanity check
    if (!ref($context)) {
	#DEBUG("KQMLParseChar: done (error: bad context)");
	return $KQML_ERROR_BAD_CONTEXT;
    }
    # If we are in DONE state, reset the context
    if ($context->{'state'} == $DONE) {
	resetContext($context);
    }
    # Save character in text buffer
    $context->{'text'} .= $ch;
    # Adjust state based on character
    if ($context->{'state'} == $START || $context->{'state'} == $ERROR) {
	# Waiting for open paren
	if ($ch eq '(') {
	    # Open paren: Start expression parsing
	    initContext($context);
	    $context->{'state'} = $NORMAL;
	} elsif ($ch =~ /\s/ && $context->{'state'} == $START) {
	    # Whitespace, but only skip leading spaces
	    chop($context->{'text'});
	} else {
	    # Something else -> error
	    # Subtle: backup so garbage is not here later
	    chop($context->{'text'});
	    #DEBUG("KQMLParseChar: done (error: bad start)");
	    return $KQML_ERROR_BAD_START;
	}
    } elsif ($context->{'state'} == $NORMAL) {
	# Parsing when not in any string
	if ($ch eq '(') {
	    # Open paren
	    pushContext($context);
	} elsif ($ch eq ')') {
	    # Close paren
	    addElementToContext($context);
	    popContext($context);
	} elsif ($ch eq '"') {
	    # Start quoted string
	    $context->{'state'} = $QUOTED;
	    addCharToElement($context, $ch);
	} elsif ($ch eq '#' && !defined($context->{element})) {
	    # Start hashed string
	    $context->{'state'} = $HASH;
	    $context->{'hashnum'} = 0;
	} elsif ($ch eq " " || $ch eq "\t" || $ch eq "\n" || $ch eq "\f") {
	    # Whitespace
	    addElementToContext($context);
	} else {
	    # Any other char (technically should check $SpecialChars)
	    addCharToElement($context, $ch);
	}
    } elsif ($context->{'state'} == $QUOTED) {
	# Inside quoted string
	if ($ch eq '\\') {
	    # Backslash escapes next
	    $context->{'state'} = $BSLASH;
	} elsif ($ch eq '"') {
	    # Quote ends string
	    $context->{'state'} = $NORMAL;
	    addCharToElement($context, $ch);
	    addElementToContext($context);
	} else {
	    # Any other char
	    addCharToElement($context, $ch);
	}
    } elsif ($context->{'state'} == $BSLASH) {
	# After backslash... back to quoted string
	$context->{'state'} = $QUOTED;
	addCharToElement($context, '\\');
	addCharToElement($context, $ch);
    } elsif ($context->{'state'} == $HASH) {
	# Number follows hash sign
	if ($ch =~ /\d/) {
	    $context->{'hashnum'} =
	      $context->{'hashnum'} * 10 + ord($ch) - ord('a');
	} elsif ($ch eq '"') {
	    # Quote ends number
	    if ($context->{'hashnum'} > 0) {
		$context->{'state'} = $HASHED;
	    } else {
		$context->{'state'} = $NORMAL;
	    }
	} else {
	    # Otherwise error
	    # Could be state=ERROR here, but that interferes with the way
	    # we handle leading errors (inter-message garbage) above. So
	    # instead we'll go to state NORMAL and just sort of plug along.
	    # Hard to say what the right thing to do is here... Eventually
	    # we'll try to parse and just fail.
	    $context->{'state'} = $NORMAL;
	    # Killing the textbuf here is a grotty hack, but it saves
	    # repeatedly printing the same garbage later.
	    $context->{'text'} = '';
	    #DEBUG("KQMLParseChar: done (error: bad hash)");
	    return $KQML_ERROR_BAD_HASH;
	}
    } elsif ($context->{'state'} == $HASHED) {
	# Inside hashed string
	addCharToElement($context, $ch);
	$context->{'hashnum'} -= 1;
	if ($context->{'hashnum'} <= 0) {
	    addElementToContext($context);
	    $context->{'state'} = $NORMAL;
	}
    }
    # If after all this we have balanced parens, we are done
    if ($context->{'state'} != $START && emptyContext($context)) {
	#DEBUG("KQMLParseChar: done (perf complete)");
	$context->{'state'} = $DONE;
	return $context->{'expr'};
    }
    # Otherwise we're still processing
    #DEBUG("KQMLParseChar: done (continuing)");
    return undef;
}

#
# This routine reads a single char from the given filehandle reference
# and parses it.
# Returns: reference to performative if successful
#	   undef if parse is not complete
#          0 on EOF
#          <0 on error
# If the parse was not completed, calling this function again will
# continue it (hence the $context argument).
#
sub KQMLReadAndParse1 {
    my($handle, $context) = @_;
    my $ch = '';
    my $n = 0;
    # Read one char
    if (($n = read($handle, $ch, 1)) != 1) {
	# Read failed, why?
	if ($n < 0) {
	    # Error
	    if ($! =~ /interrupted/i) {
		return $KQML_ERROR_READ_INTERRUPTED;
	    } else {
	       return $KQML_ERROR_READ_ERROR;
	    }
	} else {
	    # EOF
	    return 0;
	}
    }
    # Read char ok so go parse it
    return KQMLParseChar($ch, $context);
}

#
# This routine reads a performative from the given filehandle reference.
# Returns: reference to performative if successful
#          0 on EOF
#          <0 on error
# If the second argument is given it should be a reference to a scalar
# into which the text of the message will be saved.
#
sub KQMLRead {
    my($handle, $textref) = @_;
    # New parsing context
    my $context = newContext();
    # Keep trying to read until completed or error
    my $ret;
    do {
	$ret = KQMLReadAndParse1($handle, $context);
    } while (!defined($ret));
    # Save message text if asked to do so
    if (defined($textref)) {
	$$textref = $context->{'text'};
    }
    # Ok, we're done
    return $ret;
}

#
# This reads exactly one char, parses it, and returns as for
# KQMLReadAndParse1. It also fills in $$textref if given as for KQMLRead.
#
{ my %contexts;
sub KQMLRead1 {
    my($handle, $textref) = @_;
    my $fd = fileno($handle);
    # Create context for this fd if needed
    if (!defined($contexts{$fd})) {
	$contexts{$fd} = newContext();
    }
    # Go read and parse one char
    my $ret = KQMLReadAndParse1($handle, $contexts{$fd});
    # Save message text if asked to do so
    if (defined($textref)) {
	$$textref = $contexts{$fd}->{'text'};
    }
    # Ok, we're done
    return $ret;
}
} # end static vars

# Used with Perl/Tk
sub original_KQMLInputCB {
    my($handle, $callback) = @_;
    my($ret, $text);
    $ret = KQMLRead1($handle, \$text);
    if (defined($ret) && defined($callback)) {
	&{$callback}($ret, $text);
    }
}

# Used with Perl/Tk
{ my %contexts;
  # From <sys/filio.h> and <sys/ioccom.h> for FIONREAD ioctl
  my $IOCPARM_MASK = 0xff;  # parameters must be < 256 bytes
  my $IOC_OUT = 0x40000000; # copy out parameters
  my %sizeof = ('int', 4);
  sub _IOR {
      my($x, $y, $t) = @_;
      ($IOC_OUT | (($sizeof{$t} & $IOCPARM_MASK) << 16) | (ord($x)<<8) | $y);
  }
  my $FIONREAD = _IOR('f', 127, 'int'); # get # bytes to read
  # Utility function to handle how ioctl works from perl
  sub num_ready {
      my($handle) = @_;
      # ioctl reads into a char buffer
      my $nready = '';
      if (ioctl($handle, $FIONREAD, $nready) < 0) {
	  return -1;
      }
      # which we want to convert to an int
      unpack('i', $nready);
  }

sub KQMLInputCB {
    my($handle, $callback) = @_;
    my $fd = fileno($handle);
    #DEBUG("KQMLInputCB: fd=$fd");
    # Create context for this fd if needed
    if (!defined($contexts{$fd})) {
	$contexts{$fd} = newContext();
    }
    my $context = $contexts{$fd};
    # See what's ready for reading
    my $nready = num_ready($handle);
    #DEBUG("KQMLInputCB: ioctl said $nready");
    # Nothing ready?
    if ($nready <= 0) {
	# EOF
	#DEBUG("KQMLInputCB: EOF!");
	if (defined($callback)) {
	    &{$callback}(0, '');
	}
    } else {
	# Else loop to get whatever's ready now
	while ($nready > 0) {
	    #DEBUG("KQMLInputCB: reading $nready bytes...");
	    my $line = '';
	    read($handle, $line, $nready);
	    #DEBUG("KQMLInputCB: line=$line");
	    my $len = length($line);
	    my $i;
	    # For each character...
	    for ($i=0; $i < $len; $i++) {
		# Parse it
		my $ret = KQMLParseChar(substr($line, $i, 1), $context);
		# And if the parse is not continuing
		if (defined($ret)) {
		    # Then call the callback with the return value
		    if (defined($callback)) {
			&{$callback}($ret, $line);
		    }
		    # And reset this context
		    resetContext($context);
		}
	    }
	    $nready = num_ready($handle);
	}
    }
    #DEBUG("KQMLInputCB: done");
}
} # end static vars

#
# This will try to read a peformative from the given string. It won't
# work on perfs that span multiple strings, nor will it tell you if part
# of the string belongs to the next perf. Sorry.
# It will return undef if the parse was incomplete at the end of the string.
#
sub KQMLReadFromString {
    my($input) = @_;
    # New parsing context
    my $context = newContext();
    # Go parse each character until done or out of characters
    my($i, $ret);
    for ($i=0; $i < length($input); $i++) {
	$ret = KQMLParseChar(substr($input, $i, 1), $context);
	last if (defined($ret) && ref($ret));
    }
    # Ok, we're done
    return $ret;
}

sub KQMLKeywordify {
    my($listref) = @_;
    # Sanity checks
    if (!defined($listref) || !ref($listref)) {
	return undef;
    }
    # Copy list
    my @list = @$listref;
    # Create hash
    my %hash = ();
    # Save original list here
    $hash{'list'} = $listref;
    # Set hash from args
    $hash{'verb'} = shift @list;
    my @rest = ();
    while (@list) {
	my $key = shift @list;
	if (!ref($key) && $key =~ /^:/) {
	    # Lowercase keys!
	    $hash{lc($key)} = shift @list;
	} else {
	    push(@rest, $key);
	}
    }
    $hash{'rest'} = \@rest;
    # Return reference to hash
    \%hash;
}

sub KQMLUnkeywordify {
    my $hashref = shift;
    # Sanity checks
    if (!defined($hashref) || !ref($hashref)) {
        return undef;
    }
    # Copy hash
    my %hash = %$hashref;
    # Get verb and rest (but discard original list)
    my ($verb, $rest) = @hash{qw(verb rest)};
    delete @hash{qw(verb rest list)};
    # Return list reconstructed using possibly new values in hash 
    [$verb, %hash, @$rest];
}

sub KQMLAsString {
    my($listref) = @_;
    # Sanity checks
    if (!defined($listref)) {
	return "";
    }
    if (!ref($listref)) {
	return "$listref";
    }
    # Walk the list
    my $str = '(';
    my $item;
    foreach $item (@$listref) {
	if (ref($item)) {
	    $str .= KQMLAsString($item);
	} else {
	    $str .= $item;
	}
	$str .= ' ';
    }
    if ($str ne '(') {
	chop($str);
    }
    $str .= ')';
    # Return the string
    return $str;
}

# KQMLAtomIsString($str) => true iff $str is a string representing a quoted
# string from KQML
sub KQMLAtomIsString {
    my ($str) = @_;
    # The following could conceivably allow things like this:
    # "\"foo\" bar \"baz\"", which is actually two strings with a symbol in
    # between, but we don't produce those so it shouldn't be a problem.
    $str =~ /^".*"$/s;
}

# KQMLStringAtomAsPerlString($str) => the unadorned perl string corresponding to the KQML string $str (KQMLAtomIsString($str) must be true)
sub KQMLStringAtomAsPerlString {
    my ($str) = @_;
    # strip quotes
    $str =~ s/^"//;
    $str =~ s/"$//;
    # convert escape sequences
    # NOTE: I'm not sure these are necessary or correct WRT the KQML spec,
    # but they're common enough so I put them in.
    my %escapes = ('t' => "\t", 'n' => "\n");
    $str =~ s/\\(.)/$escapes{$1} || $1/gse;
    return $str;
}

no strict;

# Return ok!
1;

__END__

#
# Test program for KQML.pm
#
use KQML;
no strict;

$input = '(foo bar (baz biz) buzz () last)';
print "input=$input\n";
$perf = KQMLReadFromString($input);
if (ref($perf)) {
    print "perf=" . KQMLAsString($perf) . "\n";
} else {
    print "error=$perf\n";
}

$| = 1;
while (1) {
    print "Enter a performative: ";
    $perf = KQMLRead(\*STDIN, \$text);
    print "text=$text\n";
    if (ref($perf)) {
	print "perf=" . KQMLAsString($perf) . "\n";
	%hash = %{KQMLKeywordify($perf)};
	print " list=" . KQMLAsString($hash{'list'}) . "\n";
	print " verb=" . $hash{'verb'} . "\n";
	foreach $key (keys %hash) {
	    next if ($key !~ /^:/);
	    if (ref($hash{$key})) {
		print " $key=" . KQMLAsString($hash{$key}) . "\n";
	    } else {
		print " $key=" . $hash{$key} . "\n";
	    }
	}
	print " rest=" . join(' ', @{$hash{'rest'}}) . "\n";
    } elsif ($perf == 0) {
	exit(0);
    } else {
	print "error=$perf\n";
    }
}

# Local Variables:
# mode: perl
# End:
