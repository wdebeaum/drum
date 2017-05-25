package StringParser;

require 5.10.1;
$VERSION = "1.00"; $VERSION = "1.00"; # Twice to shut up -w warning
require Exporter;
our @ISA = qw(Exporter);
@EXPORT = qw(parseString keywordify removePrefix);

use util::Log;
local $util::Log::Caller_Info = 0;
local $util::Log::Quiet = 1;

use strict;
use warnings FATAL => 'all';

my $STRING_PARSER_ERROR_BAD_CONTEXT = -1;
my $STRING_PARSER_ERROR_BAD_START = -3;
my $STRING_PARSER_ERROR_BAD_HASH = -4;

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

sub newContext {
    my %context = (state => $START, text => '', hashnum => 0);
    return \%context;
}

sub resetContext {
    my($self) = @_;
    DEBUG (10, "resetContext: resetting context");
    $self->{'state'} = $START;
    $self->{'text'} = '';
    $self->{'hashnum'} = 0;
}

sub initContext {
    my($self) = @_;
    DEBUG (10, "initContext: initializing context");
    $self->{'expr'} = [];
    $self->{'stack'} = [ $self->{'expr'} ];
}

sub emptyContext {
    my($self) = @_;
    my $result = ($#{$self->{'stack'}} < 0);
    DEBUG (10, "emptyContext: returning " . ($result ? 'true' : 'false'));
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
    DEBUG (10, "pushContext: depth now " . ($#{$stackref}+1));
}

# Pops to the previous list being filled
sub popContext {
    my($self) = @_;
    my $stackref = $self->{'stack'};
    pop(@$stackref);
    DEBUG (10, "popContext: depth now " . ($#{$stackref}+1));
}

# Sets the current element to the empty string (different from undef)
sub startElement {
    my($self, $ch) = @_;
    $self->{'element'} = '';
    DEBUG (10, "startElement: element=\"\"");
}

# Adds a character to the current element (token/word/string)
sub addCharToElement {
    my($self, $ch) = @_;
    if (!defined($self->{'element'})) {
        $self->{'element'} = '';
    }
    $self->{'element'} .= $ch;
    DEBUG (10, "addCharToElement: element=\"%s\"" , $self->{'element'});
}

# Adds the current element to the current list being filled and resets element
sub addElementToContext {
    my($self) = @_;
    my $stackref = $self->{'stack'};
    my $currentref = $stackref->[$#{$stackref}];
    # Add element to current list
    if (defined($self->{'element'})) {
        DEBUG (10, "addElementToContext: element=\"%s\"" , $self->{'element'});
        push(@$currentref, $self->{'element'});
        DEBUG (10, "addElementToContext: length now " . ($#{$currentref}+1));
        undef($self->{'element'});
    }
}

sub parseChar {
    my($ch, $context) = @_;
    DEBUG (10, "parseChar: ch=%s", $ch);
    # Sanity check
    if (!ref($context)) {
        DEBUG(2, "parseChar: done (error: bad context)");
        return $STRING_PARSER_ERROR_BAD_CONTEXT;
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
            DEBUG(2, "parseChar: done (error: bad start)");
            return $STRING_PARSER_ERROR_BAD_START;
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
            DEBUG(2, "parseChar: done (error: bad hash)");
            return $STRING_PARSER_ERROR_BAD_HASH;
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
        DEBUG (10, "parseChar: done (perf complete)");
        $context->{'state'} = $DONE;
        return $context->{'expr'};
    }
    # Otherwise we're still processing
    DEBUG (10, "parseChar: done (continuing)");
    return undef;
}

sub parseString {
    my($input) = @_;

    # New parsing context
    my $context = newContext();

    # Go parse each character until done or out of characters
    my($i, $ret);
    for ($i=0; $i < length($input); $i++) {
        $ret = parseChar(substr($input, $i, 1), $context);
        last if (defined($ret) && ref($ret));
    }

    if (defined($ret) && !ref($ret))
    {
        return $ret;
    }

    \@$ret;
}


sub keywordify {
    my($listref) = @_;
    # Sanity checks
    if (!defined($listref) || !ref($listref)) {
        return undef;
    }
    # Copy list
    my @list = @$listref;
    # Create hash
    my %hash = ();
    # Set hash from args
    my $key = shift @list;
    if (!ref($key) && $key =~ /^:/) {
        # Lowercase keys!
        $hash{lc($key)} = shift @list;
    } else {
        $hash{'type'} = $key;
    }
    my @rest = ();
    while (@list) {
        $key = shift @list;
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


sub removePrefix {
    my ($original, $prefix) = @_;
    if (!defined($original) or !defined($prefix))
    {
        return $original;
    }

    if ((length $prefix) > (length $original))
    {
        return $original;
    }

    if (substr($original, 0, (length $prefix)) eq $prefix)
    {
        return substr($original, (length $prefix));
    }

    return $original;
}

1;