package DRUM::DRUMTerm;

require 5.10.1;
$VERSION = "1.00"; $VERSION = "1.00"; # Twice to shut up -w warning
require Exporter;
our @ISA = qw(Exporter);
@EXPORT = qw(parseDRUMTerms
             getID
             getName
             getDBXRefs
             getScore
             getOntTypes
             getSpecies
             getMatches
             toString
    );

use StringParser;
use DRUM::DRUMMatch;
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
        $self->{ID} = $data->{":id"};
        $self->{NAME} = $data->{":name"};
        $self->{SCORE} = $data->{":score"};
        $self->{DBXREFS} = $data->{":dbxrefs"};
        $self->{SPECIES} = $data->{":species"};
        $self->{ONT_TYPES} = $data->{":ont-types"};
        if (defined ($data->{":matches"}))
        {
            $self->{MATCHES} = parseDRUMMatches( $data->{":matches"});
        }
    }
    bless $self, $class;
    return $self;
}

sub parseDRUMTerms
{
    my ($drumTermsList) = @_;
    my @results = ();

    if (!defined($drumTermsList) || ref($drumTermsList) ne "ARRAY")
    {
        return \@results;
    }

    $drumTermsList = @$drumTermsList[0];

    if (!defined($drumTermsList) || ref($drumTermsList) ne "ARRAY")
    {
        return \@results;
    }

    # see if this is an array of AKRL Objects or just one
    my $DRUMTerm;
    if (@$drumTermsList > 0)
    {
        foreach my $item (@$drumTermsList)
        {
            if (ref($item) eq "ARRAY" && @$item[0] eq "TERM")
            {
                $DRUMTerm = new DRUM::DRUMTerm (keywordify($item));
                push @results, $DRUMTerm;
            }
        }
    }

    return \@results;
}

# Object Methods

# returns a string containing the id or undef
sub getID
{
    my $self = shift;
    return $self->{ID};
}

# returns a string containing the name or undef
sub getName
{
    my $self = shift;
    return $self->{NAME};
}

# returns a scalar (float) containing the score or undef
sub getScore
{
    my $self = shift;
    return $self->{SCORE};
}

# returns a list of strings which are Data Base Cross References or undef
sub getDBXRefs
{
    my $self = shift;
    return $self->{DBXREFS};
}

# returns a string containing the species or undef
sub getSpecies
{
    my $self = shift;
    return $self->{SPECIES};
}

# returns a list of strings of Ontology Types or undef
sub getOntTypes
{
    my $self = shift;
    return $self->{ONT_TYPES};
}

# returns a list of Match Objects or undef
sub getMatches
{
    my $self = shift;
    return $self->{MATCHES};
}

sub toString
{
    my $self = shift;

    my $result = "TERM - ";

    my $tmp = $self->getID();
    if (defined($tmp))
    {
        $result = $result . " ID:$tmp";
    }
    $tmp = $self->getName();
    if (defined($tmp))
    {
        $result = $result . " NAME:$tmp";
    }
    $tmp = $self->getScore();
    if (defined($tmp))
    {
        $result = $result . " SCORE:$tmp";
    }

    $tmp = $self->getDBXRefs();
    if (defined($tmp) && ref($tmp) eq "ARRAY" && @$tmp > 0)
    {
        $result = $result . " DBXRefs:(" . @$tmp[0];
        for (my $i = 1; $i < @$tmp; $i++)
        {
            $result = $result . ", " . @$tmp[$i];
        }
        $result = $result . ")";
    }

    $tmp = $self->getSpecies();
    if (defined($tmp) && ref($tmp) eq "ARRAY" && @$tmp > 0)
    {
        $result = $result . " SPECIES:(" . @$tmp[0];
        for (my $i = 1; $i < @$tmp; $i++)
        {
            $result = $result . ", " . @$tmp[$i];
        }
        $result = $result . ")";
    }

    $tmp = $self->getOntTypes();
    if (defined($tmp) && ref($tmp) eq "ARRAY" && @$tmp > 0)
    {
        $result = $result . " ONT-TYPES:(" . @$tmp[0];
        for (my $i = 1; $i < @$tmp; $i++)
        {
            $result = $result . ", " . @$tmp[$i];
        }
        $result = $result . ")";
    }

    $result = $result . "\n";


    $tmp = $self->getMatches();
    if (defined($tmp) && ref($tmp) eq "ARRAY" && @$tmp > 0)
    {
        $result = $result . "    MATCHES: (\n";
        foreach my $match (@$tmp)
        {
            if ($self->getScore() eq $match->getScore())
            {
                $result = $result . "             * " . $match->toString() . "\n";
            }
            else
            {
                $result = $result . "               " . $match->toString() . "\n";
            }
        }
        $result = $result . "             )";
    }

    return $result;
}

1;