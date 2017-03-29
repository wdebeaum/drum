package DRUM::DRUMMatch;

require 5.10.1;
$VERSION = "1.00"; $VERSION = "1.00"; # Twice to shut up -w warning
require Exporter;
our @ISA = qw(Exporter);
@EXPORT = qw(parseDRUMMatches);

use StringParser;
use util::Log;
use Data::Dumper;
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
        $self->{SCORE} = $data->{":score"};
        $self->{MATCHED} = $data->{":matched"};
        $self->{STATUS} = $data->{":status"};
    }
    bless $self, $class;
    return $self;
}

sub parseDRUMMatches
{
    my ($drumMatches) = @_;
    my @results = ();

    if (!defined($drumMatches) || ref($drumMatches) ne "ARRAY")
    {
        return \@results;
    }

    my $DRUMMatch;
    if (@$drumMatches > 0)
    {
        foreach my $item (@$drumMatches)
        {
            if (ref($item) eq "ARRAY" && @$item[0] eq "MATCH")
            {
                $DRUMMatch = new DRUM::DRUMMatch (keywordify($item));
                push @results, $DRUMMatch;
            }
        }
    }

    return \@results;
}

# Object Methods

# returns a scalar (float) containing the score or undef
sub getScore
{
    my $self = shift;
    return $self->{SCORE};
}

# returns a string containing the matched text or undef
sub getMatched
{
    my $self = shift;
    return $self->{MATCHED};
}

# returns a string containing the status or undef
sub getStatus
{
    my $self = shift;
    return $self->{STATUS};
}


sub toString
{
    my $self = shift;

    my $result = "MATCH - ";

    my $tmp = $self->getScore();
    if (defined($tmp))
    {
        $result = $result . " SCORE:$tmp";
    }
    $tmp = $self->getMatched();
    if (defined($tmp))
    {
        $result = $result . " MATCHED:$tmp";
    }
    $tmp = $self->getStatus();
    if (defined($tmp))
    {
        $result = $result . " Status:$tmp";
    }

    return $result;
}

1;