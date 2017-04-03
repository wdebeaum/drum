package AKRL::AKRLList;

require 5.10.1;
$VERSION = "1.00"; $VERSION = "1.00"; # Twice to shut up -w warning
require Exporter;
our @ISA = qw(Exporter);
@EXPORT = qw(parseAKRL);

use StringParser;
use AKRL::AKRLTerm;
use DRUM::DRUMTerm;
use DRUM::DRUMMatch;
use util::Log;
local $util::Log::Caller_Info = 0;

use strict;
use warnings FATAL => 'all';

sub new
{
    my $class = shift;
    my $self = {};
    $self->{_akrlObjectHash} = shift;
    bless $self, $class;
    return $self;
}

sub parseAKRL
{
    my ($list) = @_;
    my %results = ();
    unless (defined($list) && ref($list) eq "ARRAY")
    {
        $list = parseString($list);
    }

    # check for parsing error
    if (defined ($list) and !ref($list))
    {
        return $list;
    }

    my $size = @$list;

    # see if this is an array of AKRL Objects or just one
    my $AKRLObject = 0;
    if ($size > 0 and ref(@$list[0]) eq 'ARRAY')
    {
        INFO ("Parsing $size AKRL Objects\n");
        foreach my $item (@$list)
        {
            $AKRLObject = new AKRL::AKRLTerm ($item);
            if (!defined($AKRLObject) or !ref($AKRLObject))
            {
                # Return the error code
                return $AKRLObject;
            }

            $results{$AKRLObject->getToken} = $AKRLObject;
        }
    }
    else
    {
        INFO ("Parsing one AKRL Object\n");

        $AKRLObject = new AKRL::AKRLTerm ($list);
        if (!defined($AKRLObject) or !ref($AKRLObject))
        {
            # Return the error code
            return $AKRLObject;
        }
        else
        {
            $results{$AKRLObject->getToken} = $AKRLObject;
        }
    }

    new AKRL::AKRLList(\%results);
}

# Object Methods

sub getObjectWithToken
{
    my($self, $token) = @_;
    if (!defined($self->{_akrlObjectHash}) or !defined($token) or ref($self->{_akrlObjectHash}) ne 'HASH')
    {
        return undef;
    }

    my $result = $self->{_akrlObjectHash}->{$token};
    if (!defined ($result) and substr($token, 0, 5) ne "ONT::")
    {
        $result = $self->{_akrlObjectHash}->{"ONT::".$token};
    }
    return $result;
}

sub getTokens
{
    my($self) = @_;
    if (!defined($self->{_akrlObjectHash}) or ref($self->{_akrlObjectHash}) ne 'HASH')
    {
        return undef;
    }

    my @keys = keys %{ $self->{_akrlObjectHash} };
    \@keys;
}

sub RemovePrefix
{
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

sub printTree
{
    my ($self, $token, $processed, $tabs) = @_;

    if (!defined($self->{_akrlObjectHash}) or !defined($token) or ref($self->{_akrlObjectHash}) ne 'HASH')
    {
        return;
    }

    if (!defined ($processed))
    {
        my %newProcessed = ();
        $processed = \%newProcessed;
    }
    elsif (defined ($processed->{$token}))
    {
#        print ($tabs . " - ALREADY PROCESSED.\n");
        return;
    }
    $processed->{$token} = 1;

    if (!defined ($tabs))
    {
        $tabs = '';
    }

    my $object = $self->{_akrlObjectHash}->{$token};
    if (!defined($object))
    {
        return undef;
    }

    my $indicator = $object->getIndicator;

    if ($indicator eq "ONT::RELN")
    {
        print ($tabs . "Event: " . RemovePrefix($token, "ONT::") . "\n");
    }
    elsif ($indicator eq "ONT::A")
    {
        print ($tabs . "Term:  " . RemovePrefix($token, "ONT::") . "\n");
    }

    $tabs = $tabs . "   ";
    for my $key ($object->getKeys)
    {
        # skip the main keys
        if (substr($key, 0, 1) ne ":")
        {
            next;
        }

        my $values = $object->getValuesAsArrayForKey($key);
        foreach my $value (@$values)
        {
#            print ($tabs . "- $key : $value\n");
            $self->printTree($value, $processed, $tabs);
        }
    }

    return 0;
}

sub printAKRLList {
    my ($self) = @_;

    my $indicator = '';
    my $token = '';
    my $type = '';
    my $rule = '';
    my $drum = '';

    foreach my $key ($self->getTokens)
    {
        my $item = $self->getObjectWithToken($key);
        $indicator = $item->getIndicator;
        $token = $item->getToken;
        if (defined $token)
        {
            $token = removePrefix($token, "ONT::");
        }
        $type = $item->getInstanceOf;
        $rule = $item->getValuesAsStringForKey (":RULE");
        $drum = $item->getValueForKey(":drum");

        if (!defined($indicator))
        {
            next;
        }

        if ($indicator eq "ONT::RELN")
        {
            print ("Event: ");
        }
        elsif ($indicator eq "ONT::A")
        {
            #            my $terms = getDrumTerms($item);
            #            print ("  " . (@$terms) . " DRUM TERMS::\n");
            #            printObject($terms);
            print ("Term:  ");
        }
        else
        {
            print ("UNKNOWN:  ");
        }
        print ("$indicator token:$key id:$token type:$type".(defined($rule) ? " rule:".$rule : "").(defined($drum) ? " drum:".$drum : "")."\n");
        #        $self->printTree($key);

        if (defined ($drum))
        {
            my $drumTerms = parseDRUMTerms($drum);
            if (!defined $drumTerms)
            {
                print ("  * No Drum Terms.\n");
            }
            else
            {
                my @terms = @{$drumTerms};
                foreach my $term (@terms)
                {
                    print ("  DRUM-".$term->toString()."\n");
                }
            }
        }
    }
}

1;
