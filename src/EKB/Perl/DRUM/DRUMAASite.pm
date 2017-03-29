package DRUM::DRUMAASite;

require 5.10.1;
$VERSION = "1.00"; $VERSION = "1.00"; # Twice to shut up -w warning
require Exporter;
our @ISA = qw(Exporter);
@EXPORT = qw(parseDRUMAASites
             getName
             getCode
             getPosition
    );

use StringParser;
use util::Log;
local $AKRL::Log::Caller_Info = 0;

use strict;
use warnings FATAL => 'all';

sub new
{
    my $class = shift;
    my $data = shift;
    my $self = {};
    if (defined($data) && ref($data) eq "HASH")
    {
        $self->{NAME} = $data->{":name"};
        $self->{CODE} = $data->{":letter"};
        $self->{POSITION} = $data->{":index"};
    }
    bless $self, $class;
    return $self;
}

sub parseDRUMAASites
{
    my ($drumAASitesList) = @_;
    my @results = ();

    if (!defined($drumAASitesList) || ref($drumAASitesList) ne "ARRAY")
    {
        return \@results;
    }

    $drumAASitesList = @$drumAASitesList[0];

    if (!defined($drumAASitesList) || ref($drumAASitesList) ne "ARRAY")
    {
        return \@results;
    }

    # see if this is an array of AKRL Objects or just one
    my $DRUMAASite;
    if (@$drumAASitesList > 0)
    {
        foreach my $item (@$drumAASitesList)
        {
            if (ref($item) eq "ARRAY" && @$item[0] eq "AA-SITE")
            {
                $DRUMAASite = new DRUM::DRUMAASite (keywordify($item));
                push @results, $DRUMAASite;
            }
        }
    }

    return \@results;
}

# Object Methods

# returns a string containing the name or undef
sub getName
{
    my $self = shift;
    return $self->{NAME};
}

# returns a string containing the code or undef
sub getCode
{
    my $self = shift;
    return $self->{CODE};
}

# returns a scalar (int) containing the score or undef
sub getPosition
{
    my $self = shift;
    return $self->{POSITION};
}

1;