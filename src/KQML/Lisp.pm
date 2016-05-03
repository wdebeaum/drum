#!/usr/grads/bin/perl5 -w
#
# Lisp.pm: Perl5 module for handling Lisp s-expressions
#
# George Ferguson, ferguson@cs.rochester.edu, 15 Apr 1998
# Time-stamp: <Fri Jul 10 17:44:53 EDT 1998 ferguson>
#
# To view the documentation contained in this file, do:
#	pod2man Lisp.pm | nroff -man | less
#
# There is a test program at the end of this file, after the END marker.
#
# Note that this representation loses the distinction between items that
# came in as Lisp strings, and those that were simply tokens. They're
# both stored as Perl strings (lists are stored as references, and
# quotations aren't handled at all, as far as I can tell). Anyway, only
# in LispAsString do we try to handle strings: if the item to be output
# has any funny characters (like spaces) in it, then it is wrapped in
# quotes, otherwise it is simply output.
#
# TODO: Quotations. They and strings are handled properly in the Java
# version of this library, but at a significant cost in comlexity.
#

package Lisp;

require 5.001;
require Exporter;
@ISA = qw(Exporter);
$VERSION = "1.00"; $VERSION = "1.00"; # Twice to shut up -w warning
@EXPORT    = qw(LispRead LispRead1 LispReadFromString LispAsString LispAsPerl);
@EXPORT_OK = qw(LispRead LispRead1 LispReadFromString LispAsString LispAsPerl);

use strict;

=head1 NAME

Lisp - Read and parse Lisp s-expressions

=head1 SYNOPSIS

 use Lisp;

 $lolref = LispRead(\*FILEHANDLE, \$text);

 $lolref = LispRead1(\*FILEHANDLE, \$text);

 $lolref = LispReadFromString($input);

=head1 DESCRIPTION

B<LispRead> tries to read a Lisp s-expression from the given filehandle
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

B<LispRead1> reads a single character from the given filehandle and uses it
in an ongoing parse of a Lisp s-expression. That is, successive calls will
read successive characters and try to parse them together. It returns as
for B<LispRead>, or B<undef> if the parse is not yet complete. Multiple
parses can be ongoing on different filehandles simultaneously.

B<LispReadFromString> tries to parse an s-expression out of the given
string. It returns as for B<LispRead> (with 0 meaning end of string
rather than EOF), and returns B<undef> if the parse was incomplete at
the end of the string. The optional second argument should be a
reference to a scalar into which remaining text in the string will be
stored.

B<LispAsString> is a convenience function that takes a
reference to a list of lists as returned by the parsing functions, and
returns a string. The list is printed in standard Lisp-like parenthesized
notation. You could make this acceptable to Perl by changing the
parentheses to square brackets if you wanted to B<eval> it back.

=head1 DATATYPES

If you don't know how to handle a list of lists, the following description
may help and you should probably look at the L<perllol> documentation.
First you will want to test the return value from the Lisp functions to
see if it is indeed a reference to a list of lists (indicating success).
Use the B<ref> function for this. If it returns true, dereferencing
the value will give you the toplevel list. Each element of the list will
be either a scalar or a reference to a sublist, and again B<ref>
will tell you which it is.

Here is an example of a subroutine that formats the list of lists into a
string and returns it (this is effectively B<LispAsString>):

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

    $perf = LispRead(\*STDIN, \$text);
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

# Lisp spec special chars
my $SpecialChars = '<>=+-*/&^~_@$%:.!?#';
my $SpecialCharsPattern = $SpecialChars;
$SpecialCharsPattern =~ s/(.)/\\$1/g;

# The Lisp is usually case-insensitive. Making this be ("\l$a" eq "\l$b")
# makes the parser case-insensitive; making this ($a eq $b) makes it case-
# sensitive.
sub STREQ {
    my($a,$b) = @_;
    "\l$a" eq "\lb";
}

# These are the same as in the C library
my $Lisp_ERROR_BAD_CONTEXT = -1;
my $Lisp_ERROR_BAD_START = -3;
my $Lisp_ERROR_READ_INTERRUPTED = -6;
my $Lisp_ERROR_READ_ERROR = -101;

# States of the finite-state parser
my $START = 0;
my $NORMAL = 1;
my $QUOTED = 2;
my $BSLASH = 3;
my $COMMENT = 4;
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

sub LispParseChar {
    my($ch, $context) = @_;
    #DEBUG("LispParseChar: ch=$ch");
    # Sanity check
    if (!ref($context)) {
	#DEBUG("LispParseChar: done (error: bad context)");
	return $Lisp_ERROR_BAD_CONTEXT;
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
	} elsif ($ch eq ';') {
	    # Start comment before start of sexp
	    $context->{'prior_state'} = $context->{'state'};
	    $context->{'state'} = $COMMENT;
	} elsif ($ch =~ /\s/ && $context->{'state'} == $START) {
	    # Whitespace, but only skip leading spaces
	    chop($context->{'text'});
	} else {
	    # Something else -> error
	    # Subtle: backup so garbage is not here later
	    chop($context->{'text'});
	    #DEBUG("LispParseChar: done (error: bad start)");
	    return $Lisp_ERROR_BAD_START;
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
	} elsif ($ch eq ';') {
	    # Start comment
	    $context->{'prior_state'} = $context->{'state'};
	    $context->{'state'} = $COMMENT;
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
	    addElementToContext($context);
	} else {
	    # Any other char
	    addCharToElement($context, $ch);
	}
    } elsif ($context->{'state'} == $BSLASH) {
	# After backslash... back to quoted string
	$context->{'state'} = $QUOTED;
	addCharToElement($context, $ch);
    } elsif ($context->{'state'} == $COMMENT) {
	# Comment to end of line
	if ($ch eq "\n") {
	    # At end of line, return to START or NORMAL
	    $context->{'state'} = $context->{'prior_state'};
	}
    }
    # If after all this we have balanced parens, we are done
    if ($context->{'state'} == $NORMAL && emptyContext($context)) {
	#DEBUG("LispParseChar: done (perf complete)");
	$context->{'state'} = $DONE;
	return $context->{'expr'};
    }
    # Otherwise we're still processing
    #DEBUG("LispParseChar: done (continuing)");
    return undef;
}

#
# This routine reads a single char from the given filehandle reference
# and parses it.
# Returns: reference to s-expression if successful
#	   undef if parse is not complete
#          0 on EOF
#          <0 on error
# If the parse was not completed, calling this function again will
# continue it (hence the $context argument).
#
sub LispReadAndParse1 {
    my($handle, $context) = @_;
    my $ch = '';
    my $n = 0;
    # Read one char
    if (($n = read($handle, $ch, 1)) != 1) {
	# Read failed, why?
	if ($n < 0) {
	    # Error
	    if ($! =~ /interrupted/i) {
		return $Lisp_ERROR_READ_INTERRUPTED;
	    } else {
	       return $Lisp_ERROR_READ_ERROR;
	    }
	} else {
	    # EOF
	    return 0;
	}
    }
    # Read char ok so go parse it
    return LispParseChar($ch, $context);
}

#
# This routine reads an s-expression from the given filehandle reference.
# Returns: reference to s-expression if successful
#          0 on EOF
#          <0 on error
# If the second argument is given it should be a reference to a scalar
# into which the text of the message will be saved.
#
sub LispRead {
    my($handle, $textref) = @_;
    # New parsing context
    my $context = newContext();
    # Keep trying to read until completed or error
    my $ret;
    do {
	$ret = LispReadAndParse1($handle, $context);
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
# LispReadAndParse1. It also fills in $$textref if given as for LispRead.
#
{ my %contexts;
sub LispRead1 {
    my($handle, $textref) = @_;
    my $fd = fileno($handle);
    # Create context for this fd if needed
    if (!defined($contexts{$fd})) {
	$contexts{$fd} = newContext();
    }
    # Go read and parse one char
    my $ret = LispReadAndParse1($handle, $contexts{$fd});
    # Save message text if asked to do so
    if (defined($textref)) {
	$$textref = $contexts{$fd}->{'text'};
    }
    # Ok, we're done
    return $ret;
}
} # end static vars

#
# This will try to read a peformative from the given string. It returns
# a lolref if an s-expression could be read, undef if the parse was
# incomplete at the end of the string, 0 if the end of the string was
# reached betweeen s-expressions, and a numeric error otherwise (just like
# LispRead). The optional second argument is a reference to a scalar into
# which remaining text in the string will be stored.
#
sub LispReadFromString {
    my($input,$input_ret) = @_;
    # New parsing context
    my $context = newContext();
    # Go parse each character until done or out of characters
    my($i, $ret);
    my $len = length($input);
    for ($i=0; $i < $len; $i++) {
	$ret = LispParseChar(substr($input, $i, 1), $context);
	last if (defined($ret));
    }
    # If we're at the end of the string outside any sexp
    if ($i == $len && ($context->{'state'} == $START ||
		       $context->{'state'} == $COMMENT)) {
	# Then that is like EOF
	$ret = 0;
    }
    # Save rest of string, if requested
    if (ref($input_ret)) {
	$$input_ret = ($i < $len) ? substr($input, $i+1) : "";
    }
    # Ok, we're done
    return $ret;
}

sub LispAsString {
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
	    $str .= LispAsString($item);
	} elsif ($item =~ /$SpecialCharsPattern/o) {
	    $str .= "\"$item\"";
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

sub LispAsPerl {
    # Just like LispAsPerl, but this readable by Perl
    # (square brackets, commas, quoted atoms)
    my($listref) = @_;
    # Sanity checks
    if (!defined($listref)) {
	return "";
    }
    if (!ref($listref)) {
	return "$listref";
    }
    # Walk the list
    my $str = '[';
    my $item;
    foreach $item (@$listref) {
	if (ref($item)) {
	    $str .= LispAsPerl($item);
	} else {
	    $str .= "'$item'";
	}
	$str .= ',';
    }
    if ($str ne '[') {
	chop($str);
    }
    $str .= ']';
    # Return the string
    return $str;
}

#
# Useful lisp-like functions that operate on lolref's
#
sub nth {
    my($l, $n) = @_;
    $l->[$n];
}
sub car {
    my($l) = @_;
    $l->[0];
}
sub cdr {
    my($l) = @_;
    my @new = @$l; # This might not be very efficient (too much copying)
    shift @new;
    \@new;
}
sub cons {
    my($x,$l) = @_;
    if (ref($l)) {
	my @new = @$l; # ditto
	unshift @new, $x;
	\@new;
    } else {
	[$x, $l];
    }
}
sub append {
    my($l1,$l2) = @_;
    [@$l1, @$l2];
}
sub get_keyword_arg {
    my($l, $key) = @_;
    my $len = $#{@$l};
    my $i;
    for ($i=0; $i <= $len; $i++) {
	if ($l->[$i] =~ /$key/i) {
	    return $l->[$i+1];
	}
    }
    return undef;
}

no strict;

# Return ok!
1;

__END__

#
# Test program for Lisp.pm
#
use Lisp;
no strict;

print "Test reading from string...\n";
$input = '(foo bar (baz biz) buzz () last)';
print "input=$input\n";
$sexp = LispReadFromString($input);
if (ref($sexp)) {
    print "sexp=" . LispAsString($sexp) . "\n";
} else {
    print "error=$sexp\n";
}

print "Test reading multiple from string with comment...\n";
$input = <<_EOF_;
(foo bar (baz biz) buzz () last)
(foo bar
  (baz biz) buzz () last)
(foo bar
   (baz ; test comment
    biz) buzz ()
    last)
_EOF_
while (1) {
    print "input=$input\n";
    $sexp = LispReadFromString($input, \$input);
    if (ref($sexp)) {
	print "sexp=" . LispAsString($sexp) . "\n";
    } elsif ($sexp == 0) {
	last;
    } else {
	print "error=$sexp\n";
    }
}

$| = 1;
while (1) {
    print "Enter an s-expression: ";
    $sexp = LispRead(\*STDIN, \$text);
    print "text=$text\n";
    if (ref($sexp)) {
	print "sexp=" . LispAsString($sexp) . "\n";
	print "content=" . LispAsString(get_keyword_arg($sexp, ':content')) . "\n";
    } elsif ($sexp == 0) {
	last;
    } else {
	print "error=$sexp\n";
    }
}

# Local Variables:
# mode: perl
# End:
