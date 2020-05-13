#!/usr/bin/perl

# TripsModule.pm - base class for TRIPS modules written in Perl
# based on TripsModule.java, trips-agent.lisp, and existing code from
# individual Perl modules
# William de Beaumont
# 2008-09-16

package TripsModule;
use Data::Dumper;
use KQML::KQML;
use IO::Socket;
use IO::Handle;

use strict 'vars';

sub new
{
    my $class = shift;
    my $self = {argv => [@_],
    		usage => "usage: $0 [-debug boolean] [-connect boolean|HOST:PORT] [-name ModuleName]",
		in => \*STDIN,
		out => \*STDOUT,
		pending_messages => [],
		last_reply_ID => 0,
		debug => 0,
		name => "",
		'connect' => 1,
		host => 'localhost',
		port => 6200
		};
    bless $self, $class;
}

sub init
{
    my $self = shift;
    $self->handle_common_parameters();
    if ($self->{connect}) {
      $self->connect();
    } else {
      STDOUT->autoflush(1);
    }
    # because apparently passing -CSD to perl doesn't do the trick
    binmode $self->{in}, ':utf8';
    binmode $self->{out}, ':utf8';
    $self->register() if ($self->{name} ne '');
}

# send a message (as a string or a KQML LoLref)
sub send_msg
{
    my ($self, $msg) = @_;
    $msg = KQML::KQMLAsString($msg) unless (ref($msg) eq '');
    print STDERR "sending: $msg\n" if ($self->{debug});
    my $fh = $self->{out};
    print $fh "$msg\n";
}

# TODO messages in three forms:
# - string
# - LoLref (output of KQMLRead...)
# - hashref (output of KQMLKeywordify)

# send a message in reply to a given message we received
sub reply_to_msg
{
    my ($self, $msg, $reply) = @_;
    $reply = KQML::KQMLReadFromString($reply) if (ref($reply) eq '');
    push @$reply, ':in-reply-to', $msg->{':reply-with'}
      if (defined($msg->{':reply-with'}));
    push @$reply, ':receiver', $msg->{':sender'}
      if (defined($msg->{':sender'}));
    $self->send_msg($reply);
}

# send a message with a :reply-with argument, and wait for the reply, deferring
# any other messages we get
# return the :content of the reply
sub send_and_wait
{
    my ($self, $msg) = @_;
    $msg = KQML::KQMLReadFromString($msg) if (ref($msg) eq '');
    my $reply_ID = lc($self->{name}) . (++($self->{last_reply_ID}));
    push @$msg, ':reply-with', $reply_ID;
    $self->send_msg($msg);
    print STDERR "waiting for reply with ID $reply_ID\n"
      if ($self->{debug});
    my $in_msg;
    while (($in_msg = $self->receive_message()) and
	   lc($in_msg->{':in-reply-to'}) ne $reply_ID)
    {
	push @{$self->{pending_messages}}, $in_msg;
    }
    print STDERR Data::Dumper->Dump([$in_msg], ['reply'])
      if ($self->{debug});
    # return message content. if called in list context, also return the full
    # message as the second item
    return (wantarray() ? ($in_msg->{':content'}, $in_msg) : $in_msg->{':content'});
}

#
# Parse command line
#
sub boolean_opt {
    my($opt,$str) = @_;
    if ($str =~ /(t)|(true)|(yes)|1/i) {
        return 1;
    } elsif ($str =~ /(nil)|(false)|(no)|0/i) {
        return 0;
    } else {
        die("$0: invalid boolean for option $opt: $str\n");
    }
}

sub handle_common_parameters
{
    my $self = shift;
    my @argv = @{$self->{argv}};
    my @unused_argv = ();
    while (@argv) {
	my $opt = shift @argv;
	if ($opt eq '-help') {
	    print "$self->{usage}\n";
	    exit 1;
	} elsif ($opt eq '-debug') {
	    $self->{debug} = boolean_opt($opt, shift @argv);
	} elsif ($opt eq '-connect') {
	    my $value = shift @argv;
	    if ($value =~ /^((t)|(true)|(yes))$/i) {
		$self->{connect} = 1;
	    } elsif ($value =~ /^((nil)|(false)|(no))$/i) {
		$self->{connect} = 0;
	    } elsif ($value =~ /^([^:]+):(\d+)$/) {
		$self->{connect} = 1;
		$self->{host} = $1;
		$self->{port} = $2;
	    } else {
		die "$0: bad argument for -connect: $value\n";
	    }
	} elsif ($opt eq '-name')
	{
	    $self->{name} = shift @argv;
	} else
	{
	    push @unused_argv, $opt;
	}
    }
    @{$self->{argv}} = @unused_argv;
}

#
# Set up the connection to the TRIPS facilitator (or stdin/stdout)
#
sub connect
{
    my $self = shift;
    my $sock = IO::Socket::INET->new( Proto => "tcp",
				      PeerAddr => $self->{host},
				      PeerPort => $self->{port},
	);
    die "$0: can't connect to facilitator at " . $self->{host} . ":" . $self->{port} . ": $!\n" unless ($sock);
    $sock->autoflush(1);
    $self->{in} = $self->{out} = $sock;
}

# Introduce ourselves
sub register
{
    my $self = shift;
    $self->send_msg("(register :name $self->{name})");
}

sub ready
{
    $_[0]->send_msg("(tell :content (module-status ready))");
}

#
# Process incoming messages
#
sub run
{
    my $self = shift;
    $self->init();
    $self->ready();
    $self->dispatch_messages();
}

# receive a message from the input
sub receive_message
{
    my $self = shift;
    my $msglist = KQML::KQMLRead($self->{in});
    if (ref($msglist) eq 'ARRAY')
    {
        my $msg = KQML::KQMLKeywordify($msglist);
	print STDERR "receiving: " . Data::Dumper->Dump([$msg],['msg'])
	    if ($self->{debug});
	return $msg;
    } else
    {
	print STDERR "got bogus value from KQMLRead: " . Data::Dumper->Dump([$msglist],['msglist'])
	    if ($self->{debug});
	return undef;
    }
}

# get the next message we should process (could have been deferred)
sub get_next_message
{
    my $self = shift;
    if (@{$self->{pending_messages}})
    {
	my $msg = shift @{$self->{pending_messages}};
	print STDERR "processing deferred message: " . Data::Dumper->Dump([$msg], ['msg'])
	    if ($self->{debug});
	return $msg;
    } else
    {
	return $self->receive_message();
    }
}

# keep getting messages and dispatching them to the appropriate receive_*
# function
sub dispatch_messages
{
    my $self = shift;
    my $msg = undef;
    while ($msg = $self->get_next_message())
    {
	my $verb = lc($msg->{verb});
	$verb =~ s/-/_/g;
	eval
	{ no strict 'refs'; # prevent chaos if strictures change at the top
	  if (grep { $_ eq $verb }
		   qw(eos error sorry ready next rest discard unregister))
	  { # the KQML performative has no :content arg
	      &{ref($self) . "::receive_$verb"}($self, $msg);
	  } else # there are lots of these, so I'm not bothering to list them
	  {
	      my $content = KQML::KQMLKeywordify($msg->{':content'});
	      &{ref($self) . "::receive_$verb"}($self, $msg, $content);
	  }
	  return 1;
	} || do
	{
	  my $error = $@;
	  chomp $error;
	  $error =~ s/\\/\\\\/g;
	  $error =~ s/"/\\"/g;
	  $self->send_msg("(error :comment \"$error\")");
	}
    }
}

# sorry messages are ignored by default
sub receive_sorry
{
    my ($self, $msg, $content) = @_;
    1;
}

# error messages are ignored by default
sub receive_error
{
    my ($self, $msg, $content) = @_;
    1;
}

1;
