# ARKL.pm
#
# Time-stamp: <Thr Mar  9 14:00:00 CST 2017 rcarff>
#
# Author: Roger Carff <rcarff@ihmc.us>, 1 Mar 2017
#

#----------------------------------------------------------------
# Description:
# Package for manipulating AKRLs.

#----------------------------------------------------------------
# History
# 2017/03/01 v1.0   rcarff
# - Created, to parse ARKL strings and turn them into ARKL objects
# 2017/03/06 v1.1   rcarff
# - Added code to export the AKRL as EKB

package AKRL::AKRLTerm;

=head1 NAME

AKRL - Perl module for handling B<A>bstract B<K>nowledge B<R>epresentation B<L>anguage (AKRL) formated strings.

=head1 DESCRIPTION

AKRL (Abstract Knowledge Representation Language) as it Relates to BOB ASSERTION's

The Context of an ASSERTION from BOB contains an array of AKRL objects with the following format:

=over

- Indicator, Token and Attribute value pairs. Whitespace separates each item
  and it's surrounded by parentheses.

(Indicator Token :Attrib value :Attrib value...)

=back

- The Indicator is one of the following text strings:

=over 1

	ONT::A     - Indefinite reference
	ONT::THE   - Definite reference
	ONT::KIND  - Complex type expression
	ONT::RELN  - Relational entity, such as actions, events, and properties
	ONT::QUANT - Quantified expression

=back

- The Token is a text string indicating the symbol being defined.

- The Attribute is a text string that always starts with a ':'.

- The Value can be one of the following:

=over

	- A number

	- A Text String

	- Attribute value pairs with an optional Verb (KQML) bounded by parentheses.

		([Verb] :Attrib value :Attrib value...)

	- A list of values separated by whitespace

		value value value

=back

=cut

require 5.10.1;
$VERSION = "1.1"; $VERSION = "1.1"; # Twice to shut up -w warning
#require Exporter;
#our @ISA = qw(Exporter);
#@EXPORT = qw(parseAKRL);

use EKB;
use util::Log;
use DRUM::DRUMTerm;
use StringParser;
local $util::Log::Caller_Info = 0;

use strict;
use warnings FATAL => 'all';

my $AKRL_ERROR_MISSING_INDICATOR = -10;
my $AKRL_ERROR_MISSING_TOKEN = -11;
my $AKRL_ERROR_MISSING_INSTANCE_OF = -12;
my $AKRL_ERROR_INVALID_KEY = -13;
my $AKRL_ERROR_MISSING_VALUE = -14;

# Internal method used to convert an AKRL Array of Strings into a hashtable of key->value pairs.
# The first two Array values are assumed to be the 'indicator' and the 'token'
sub keywordifyAKRL {
    my($listref) = @_;

    # Sanity checks
    if (!defined($listref) || !ref($listref)) {
        return undef;
    }

    # Copy list
    my @list = @$listref;

    # Create hash
    my %hash = ();

    # see if the first value can be the indicator type
    unless (@list and !ref($list[0]) and substr($list[0], 0, 1) ne ":")
    {
        # Complain that this is not valid AKRL Format!!! The first item in the list should be a string.
        DEBUG (1, "Invalid AKRL Indicator: " . (@list ? "Empty List!!" : $list[0]) . "\n");
        return $AKRL_ERROR_MISSING_INDICATOR;
    }

    $hash{indicator} = shift @list;

    # see if the second value is the indicator's token
    unless (@list and !ref($list[0]) and substr($list[0], 0, 1) ne ":")
    {
        # Complain that this is not valid AKRL Format!!! The second item in the list should be the token.
        DEBUG (1, "Invalid AKRL Token: " . (@list ? "No Token Value!!" : $list[0]) . "\n");
        return $AKRL_ERROR_MISSING_TOKEN;
    }

    $hash{token} = shift @list;

    # Set hash from args
    while (@list) {
        my $key = shift @list;
        unless (!ref($key) and substr($key, 0, 1) eq ":")
        {
            # Complain that this is not valid AKRL Format!!! Expecting Attribute string starting with ':'.
            DEBUG (1, "Invalid AKRL Key: $key\n");
            return $AKRL_ERROR_INVALID_KEY;
        }
        $key = lc($key);

        unless (@list)
        {
            # Complain that this is not valid AKRL Format!!! The value is missing.
            DEBUG (1, "Missing Value for key: $key\n");
            return $AKRL_ERROR_MISSING_VALUE;
        }
        my $value = shift @list;
        if (!defined($value))
        {
            # Complain that this is not valid AKRL Format!!! The value is missing.
            DEBUG (1, "Missing Value for key: $key\n");
            return $AKRL_ERROR_MISSING_VALUE;
        }

        # see if there is already a value for this key
        my $akrlArray = $hash{$key};
        if (!defined ($akrlArray))
        {
            @$akrlArray = ();
            $hash{$key} = $akrlArray;
        }
        push(@$akrlArray, $value);
    }

    # verify that there is an :instance-of key.
    if (!defined ($hash{':instance-of'}))
    {
        # Complain that this is not valid AKRL Format!!! The :instance-of slot is missing.
        DEBUG (1, "Missing Value for ':instance-of'\n");
        return $AKRL_ERROR_MISSING_INSTANCE_OF;
    }

    # Return reference to hash
    \%hash;
}

=head1 CONSTRUCTOR

=head2 new ($AKRLKeyValueList)

Creates a new AKRLList.  The $AKRLKeyValueList must be a list of strings and
references to lists of strings.  The first two strings in the main list must
be the indicator and the token.  All other strings must be in pairs with the
first string being a key which starts with a ':' character and the value being
either a string or a list.

=cut

sub new
{
    my $class = shift;
    my $AKRLKeyValueList = shift;
    my $self = {};
    $self->{_hashref} = keywordifyAKRL($AKRLKeyValueList);

    if (!defined($self->{_hashref}) or !ref($self->{_hashref}))
    {
        return $self->{_hashref}
    }
    bless $self, $class;
    return $self;
}

=head1 GETTERS

=head2 getValuesAsArrayForKey($key)

Returns all possible values for the given $key in an array.

If no value exists for the given $key then undef is returned.

=cut

sub getValuesAsArrayForKey
{
    my($self, $key) = @_;
    if (!defined($self->{_hashref}) or !defined($key) or ref($self->{_hashref}) ne 'HASH')
    {
        return undef;
    }

    my $value = $self->{_hashref}->{lc($key)};
    if (!defined($value))
    {
        return undef;
    }

    if (ref($value) eq 'ARRAY')
    {
        return $value;
    }

    return [$value];
}

=head2 getValuesAsStringForKey($key, $separator)

Returns the values for the given $key as a string separated by the given
$separator or ',' if no separator is specified.

If no value exists for the given $key then undef is returned.

=cut

sub getValuesAsStringForKey
{
    my($self, $key, $separator) = @_;
    if (!defined($self->{_hashref}) or !defined($key) or ref($self->{_hashref}) ne 'HASH')
    {
        return undef;
    }

    my $value = $self->{_hashref}->{lc($key)};
    if (!defined($value))
    {
        return undef;
    }

    if (ref($value) ne 'ARRAY')
    {
        return $value;
    }

    if (!defined($separator))
    {
        $separator = ",";
    }

    return join($separator, @$value);
}

=head2 getNumValuesForKey($key)

Returns the number of values associated with the given $key or undef if none.

=cut

sub getNumValuesForKey
{
    my($self, $key) = @_;
    if (!defined($self->{_hashref}) or !defined($key) or ref($self->{_hashref}) ne 'HASH')
    {
        return undef;
    }

    my $value = $self->{_hashref}->{lc($key)};
    if (!defined($value))
    {
        return undef;
    }

    if (ref($value) ne 'ARRAY')
    {
        return 1;
    }

    return @$value;
}

=head2 getKeys()

Returns a list of keys for this AKRL object which have values.

=cut

sub getKeys
{
    my($self) = @_;
    if (!defined($self->{_hashref}) or ref($self->{_hashref}) ne 'HASH')
    {
        return undef;
    }

    keys $self->{_hashref};
}

=head2 getValueForKey($key, $index)

Returns the value for $key at $index or the first value if $index is not
specified.  If $index is out of bounds of the array or no value exists
for the specified $key then undef is returned.

=cut

sub getValueForKey
{
    my($self, $key, $index) = @_;
    if (!defined($self->{_hashref}) or !defined($key) or ref($self->{_hashref}) ne 'HASH')
    {
        return undef;
    }

    my $value = $self->{_hashref}->{lc($key)};
    if (!defined($value))
    {
        return undef;
    }

    if (!defined($index))
    {
        $index = 0;
    }

    if (ref($value) ne 'ARRAY')
    {
        if ($index == 0)
        {
            return $value;
        }
        else
        {
            return undef;
        }
    }

    if ($index < 0 || $index >= @$value)
    {
        return undef;
    }

    return @$value[$index];
}

=head2 getIndicator()

Returns the indicator for this AKRL object.

=cut

sub getIndicator
{
    my($self) = @_;
    return $self->getValueForKey("indicator");
}

=head2 getToken()

Returns the token for this AKRL object.

=cut

sub getToken
{
    my($self) = @_;
    return $self->getValueForKey("token");
}

=head2 getInstanceOf()

Returns the Instance Of value for this AKRL object.

=cut

sub getInstanceOf
{
    my($self) = @_;
    return $self->getValueForKey(":instance-of");
}

# Helper Methods

# nomalizeId ($id)
#
# Given a string replaces any '::' with ':' and remvoes all '|' characters
#
sub normalizeId {
    my ($id) = @_;
    my $normalizedId = $id;
    if (defined($normalizedId))
    {
        $normalizedId =~ s/::/:/g;
        $normalizedId =~ s/\|//g;
    }
    return $normalizedId;
}

# toEKB Methods

# createEKBEvent ($akrl, $akrlList)
#
# Returns the given $akrl object as an EKB Event.
#
sub createEKBEvent
{
    my ($akrl, $akrlList) = @_;

    if (!defined ($akrl))
    {
        return 0;
    }

    # get the id of the event && trim the prefix
    my $tmpString = $akrl->getToken();
    if (defined $tmpString)
    {
        $tmpString = removePrefix($tmpString, "ONT::");
    }

    # create a new assertion
    my $event = EKB::make_node("EVENT", {
            id   => $tmpString,
            rule => $akrl->getValuesAsStringForKey (":RULE")
        });

    # Add the type
    $tmpString = $akrl->getInstanceOf();
    if (defined ($tmpString))
    {
        EKB::add_child($event, EKB::make_slot_node("type", $tmpString));
    }

    # Add the force
    $tmpString = $akrl->getValueForKey(":FORCE");
    if (defined ($tmpString))
    {
        EKB::add_child($event, EKB::make_slot_node("force", $tmpString));
    }

    # Add the Agent
    $tmpString = $akrl->getValueForKey (":AGENT");
    if (defined ($tmpString))
    {
        my $child = EKB::make_node("arg1",
            { id => removePrefix($tmpString, "ONT::"),
                role => ":AGENT"
            });
        EKB::add_child($event, $child);
        my $refAKRLObj = $akrlList->getObjectWithToken($tmpString);
        if (defined ($refAKRLObj))
        {
            EKB::add_child($child, EKB::make_slot_node("type", $refAKRLObj->getInstanceOf()));
        }
    }

    # Add the Affected
    $tmpString = $akrl->getValueForKey (":AFFECTED");
    if (defined ($tmpString))
    {
        my $child = EKB::make_node("arg2",
            { id => removePrefix($tmpString, "ONT::"),
                role => ":AFFECTED"
            });
        EKB::add_child($event, $child);
        my $refAKRLObj = $akrlList->getObjectWithToken($tmpString);
        if (defined ($refAKRLObj))
        {
            EKB::add_child($child, EKB::make_slot_node("type", $refAKRLObj->getInstanceOf()));
        }
    }

    return $event;
}

# createEKBTerm ($akrl, $ekb, $akrlList)
#
# Returns the given $akrl object as an EKB Term.
#
sub createEKBTerm
{
    my ($akrl, $akrlList) = @_;

    if (!defined($akrl))
    {
        return;
    }

    # process the DRUM information
    my $drumTerms = parseDRUMTerms($akrl->getValueForKey(":drum"));
    my $dbid = undef;
    my $drumTermsNode = undef;
    my $tmpString = undef;
    if (defined($drumTerms) && $drumTerms > 0)
    {
        $drumTermsNode = EKB::make_node("drum-terms");
        my $count = 0;
        foreach my $drumTerm (@{$drumTerms})
        {
            $count += 1;
            # append id to the dbid string
            my $id = $drumTerm->getID;
            if (defined ($id))
            {
                #normalize the id before appending to dbid string
                $id = normalizeId($id);
                if (defined($dbid))
                {
                    $dbid = $dbid . '|' . $id;
                }
                else
                {
                    $dbid = $id;
                }
            }

            # find the matched-name from the list of matches
            my $matches = $drumTerm->getMatches();
            my $matchedName = undef;
            if (defined ($matches) && ref($matches) eq "ARRAY" && $matches > 0)
            {
                foreach my $match (@$matches)
                {
                    if ($match->getScore() == $drumTerm->getScore())
                    {
                        $matchedName = $match->getMatched();
                        if (defined ($matchedName))
                        {
                            $matchedName =~ s/\"//g;
                        }
                        last;
                    }
                }
            }
            my $name = $drumTerm->getName();
            if (defined($name))
            {
                $name =~ s/\"//g;
            }

            my $drumTermNode = make_node("drum-term",
                {  dbid => $id,
                    'match-score' => $drumTerm->getScore(),
                    'matched-name' => $matchedName,
                    name => $name
                });
            # Add the Ont Types
            my $ontTypes = $drumTerm->getOntTypes();
            if (defined ($ontTypes) && ref($ontTypes) eq "ARRAY" && $ontTypes > 0)
            {
                my $types = EKB::make_node("types");
                foreach my $ontType (@$ontTypes)
                {
                    EKB::add_child($types, EKB::make_slot_node("type", $ontType));
                }
                EKB::add_child($drumTermNode, $types)
            }
            # Add any DBXRefs
            my $dbxRefs = $drumTerm->getDBXRefs();
            if (defined ($dbxRefs) && ref($dbxRefs) eq "ARRAY" && $dbxRefs > 0)
            {
                my $xrefs = EKB::make_node("xrefs");
                foreach my $xref (@$dbxRefs)
                {
                    if (defined($xref))
                    {
                        EKB::add_child($xrefs, EKB::make_slot_node("xref", normalizeId($xref)));
                    }
                }
                EKB::add_child($drumTermNode, $xrefs)
            }
            # Add any Species
            $tmpString = $drumTerm->getSpecies();
            if (defined($tmpString))
            {
                EKB::add_child($drumTermNode, EKB::make_slot_node("species", $tmpString));
            }
            EKB::add_child($drumTermsNode, $drumTermNode);
        }
        if ($count <= 0)
        {
            $drumTermsNode = undef;
        }
    }

    # get the id of the term and trim the prefix
    $tmpString = $akrl->getToken();
    if (defined $tmpString)
    {
        $tmpString = removePrefix($tmpString, "ONT::");
    }

    # create a new assertion
    my $term = EKB::make_node("TERM", {
            id   => $tmpString,
            dbid => $dbid,
            rule => $akrl->getValuesAsStringForKey (":RULE")
        });

    # Add the type
    $tmpString = $akrl->getInstanceOf();
    if (defined ($tmpString))
    {
        EKB::add_child($term, EKB::make_slot_node("type", $tmpString));
    }

    # Add the name
    $tmpString = $akrl->getValueForKey(":NAME");
    if (defined ($tmpString))
    {
        $tmpString = removePrefix($tmpString, "W::");
        EKB::add_child($term, EKB::make_slot_node("name", $tmpString));
    }

    # Add any Features
    $tmpString = $akrl->getValueForKey(":INEVENT");
    if (defined($tmpString))
    {
        $tmpString = removePrefix($tmpString, "ONT::");
        my $features = EKB::make_node("features");
        my $inevent = EKB::make_node("inevent");
        my $event = EKB::make_node("event", {id => $tmpString});
        EKB::add_child($inevent, $event);
        EKB::add_child($features, $inevent);
        EKB::add_child($term, $features);
    }

    # Add any drum terms
    if (defined ($drumTermsNode))
    {
        EKB::add_child($term, $drumTermsNode);
    }

    return $term;
}

=head2 toEKB($akrlList)

Returns this AKRL object in EKB form.

=cut

sub toEKB
{
    my ($self, $akrlList) = @_;

    if (!defined ($self))
    {
        return undef;
    }

    my $indicator = $self->getIndicator();
    my $ekbObject = undef;
    if ($indicator eq "ONT::RELN")
    {
        $ekbObject = createEKBEvent($self, $akrlList);
    }
    elsif ($indicator eq "ONT::A")
    {
        $ekbObject = createEKBTerm($self, $akrlList);
    }

    return $ekbObject;
}

1;

=head1 AUTHOR

Roger Carff E<lt>rcarff@ihmc.usE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2017 by Roger Carff E<lt>rcarff@ihmc.usE<gt>

This module is free software. You can redistribute it and/or modify it under
the terms of the Artistic License 2.0.

This program is distributed in the hope that it will be useful, but without
any warranty; without even the implied warranty of merchantability or fitness
for a particular purpose.

=cut
