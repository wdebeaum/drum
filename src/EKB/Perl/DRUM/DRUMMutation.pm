package DRUM::DRUMMutation;

require 5.10.1;
$VERSION = "1.00"; $VERSION = "1.00"; # Twice to shut up -w warning
require Exporter;
our @ISA = qw(Exporter);
@EXPORT = qw(parseDRUMMutations
             getType
             getPosition
             getFromName
             getFromCode
             getToName
             getToCode
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
        $self->{TYPE} = $data->{":type"};
        $self->{POSITION} = $data->{":aa-index"};
        my $toFrom = $data->{":old"};
        if (defined ($toFrom) and ref($toFrom) eq "ARRAY")
        {
            $toFrom = keywordify($toFrom);
            $self->{FROM_NAME} = $toFrom->{":name"};
            $self->{FROM_CODE} = $toFrom->{":letter"};
        }
        $toFrom = $data->{":new"};
        if (defined ($toFrom) and ref($toFrom) eq "ARRAY")
        {
            $toFrom = keywordify($toFrom);
            $self->{TO_NAME} = $toFrom->{":name"};
            $self->{TO_CODE} = $toFrom->{":letter"};
        }
    }
    bless $self, $class;
    return $self;
}

sub parseDRUMMutations
{
    my ($drumMutationsList) = @_;
    my @results = ();

    if (!defined($drumMutationsList) || ref($drumMutationsList) ne "ARRAY")
    {
        return \@results;
    }

    $drumMutationsList = @$drumMutationsList[0];

    if (!defined($drumMutationsList) || ref($drumMutationsList) ne "ARRAY")
    {
        return \@results;
    }

    # see if this is an array of AKRL Objects or just one
    my $DRUMMutation;
    if (@$drumMutationsList > 0)
    {
        foreach my $item (@$drumMutationsList)
        {
            if (ref($item) eq "ARRAY" && @$item[0] eq "MUTATION")
            {
                $DRUMMutation = new DRUM::DRUMMutation (keywordify($item));
                push @results, $DRUMMutation;
            }
        }
    }

    return \@results;
}

# Object Methods

# returns a string containing the type or undef
sub getType
{
    my $self = shift;
    return $self->{TYPE};
}

# returns a scalar (int) containing the score or undef
sub getPosition
{
    my $self = shift;
    return $self->{POSITION};
}

# returns a string containing the from name or undef
sub getFromName
{
    my $self = shift;
    return $self->{FROM_NAME};
}

# returns a string containing the from code or undef
sub getFromCode
{
    my $self = shift;
    return $self->{FROM_CODE};
}

# returns a string containing the to name or undef
sub getToName
{
    my $self = shift;
    return $self->{TO_NAME};
}

# returns a string containing the to code or undef
sub getToCode
{
    my $self = shift;
    return $self->{TO_CODE};
}

1;