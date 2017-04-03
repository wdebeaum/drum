# ARKL2EKB.pm
#
# Time-stamp: <Tue Mar 28 17:38:08 CDT 2017 lgalescu>
#
# Author: Roger Carff <rcarff@ihmc.us>, 9 Mar 2017
#

#----------------------------------------------------------------
# Description:
# Package for converting AKRL objects to EKB.

#----------------------------------------------------------------
# History
# 2017/03/09 v1.0   rcarff
# - Created, to convert ARKL objects into EKB.

package AKRL2EKB;

=head1 NAME

AKRL2EKB - Perl module for handling the conversion of AKRL to EKB.

=head1 DESCRIPTION

Functions to convert AKRL to EKB

=cut

require 5.10.1;
$VERSION = "1.0"; $VERSION = "1.0"; # Twice to shut up -w warning
require Exporter;
our @ISA = qw(Exporter);
@EXPORT = qw(normalizeId
             createEKBEvent
             createEKBTerm
             createEKBAssertion
             toEKB
            );

use EKB;
use AKRL::AKRLTerm;
use AKRL::AKRLList;
use DRUM::DRUMTerm;
use DRUM::DRUMAASite;
use DRUM::DRUMMutation;
use StringParser;
use util::Log;
use Data::Dumper;
local $util::Log::Caller_Info = 0;

use strict;
use warnings FATAL => 'all';

my $AKRL_ERROR_MISSING_INDICATOR = -10;
my $AKRL_ERROR_MISSING_TOKEN = -11;
my $AKRL_ERROR_MISSING_INSTANCE_OF = -12;
my $AKRL_ERROR_INVALID_KEY = -13;
my $AKRL_ERROR_MISSING_VALUE = -14;

# Helper Methods

=head2 normalizeId($id)

Returns a copy of the $id string after replacing any '::' with ':' and
removing all '|' characters.

=cut

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

sub uniq {
    keys %{{ map { $_ => 1 } @_ }};
}

# toEKB Methods

sub modify_assertion {
    my ($attribute, $value, $ekb, $node) = @_;
    if (defined ($value))
    {
        $ekb->modify_assertion ($node, (EKB::make_slot_node($attribute, $value)));
        return 1;
    }
    return undef;
}

sub add_arg {
    my ($arg, $value, $ekb, $node, $akrlList) = @_;
    if (defined ($value))
    {
        my $argNode = $ekb->add_arg($node, $arg, removePrefix($value, "ONT::"));

        my $refAKRLObj = $akrlList->getObjectWithToken($value);
        if (defined ($refAKRLObj))
        {
            # RECUSIVE CALL HERE TO ADD THE TERMS. ALSO DO NOT WORRY ABOUT THE TYPE HERE
            addEKBAssertion($ekb, $refAKRLObj, $akrlList);
        }

        return $argNode;
    }
    return undef;
}

sub add_child_with_id {
    my ($child, $id, $ekb, $node, $akrlList) = @_;
    if (defined ($id))
    {
        my $childNode = EKB::make_node($child,
            { id => removePrefix($id, "ONT::")
            });
        EKB::add_child($node, $childNode);

        my $refAKRLObj = $akrlList->getObjectWithToken($id);
        if (defined ($refAKRLObj))
        {
            # RECUSIVE CALL HERE TO ADD THE TERMS. ALSO DO NOT WORRY ABOUT THE TYPE HERE
            addEKBAssertion($ekb, $refAKRLObj, $akrlList);
        }
        return $childNode;
    }
    return undef;
}

sub add_feature {
    my ($feature, $id, $ekb, $node, $akrlList) = @_;
    if (defined ($id))
    {
        my $featureNode = $ekb->add_feature($node, $feature => { id => removePrefix($id, "ONT::") });

        my $refAKRLObj = $akrlList->getObjectWithToken($id);
        if (defined ($refAKRLObj))
        {
            # RECUSIVE CALL HERE TO ADD THE TERMS. ALSO DO NOT WORRY ABOUT THE TYPE HERE
            addEKBAssertion($ekb, $refAKRLObj, $akrlList);
        }

        return $featureNode;
    }
    return undef;
}

=head2 createEKBEvent($ekb, $akrl, $akrlList)

Returns the given $arkl object as an EKB 'EVENT' node.

=cut

sub createEKBEvent
{
    my ($ekb, $akrl, $akrlList) = @_;

    if (!defined ($akrl))
    {
        return 0;
    }

    if (!defined ($ekb))
    {
        return 1;
    }

    # get the id of the event && trim the prefix
    my $tmpString = $akrl->getToken();
    if (defined $tmpString)
    {
        $tmpString = removePrefix($tmpString, "ONT::");
    }

    # if an assertion with this id already exists, then return it.
    my $event = $ekb->get_assertion($tmpString);
    if (defined($event))
    {
        return $event;
    }

    my $instanceOf = $akrl->getInstanceOf();
    if (!defined ($instanceOf))
    {
        return 2;
    }

    # create a new assertion based on the instanceOf.
    my $eventElementName = "EVENT";
    if ($instanceOf eq "ONT::LEARN")
    {
        $eventElementName = "EPI";
    }

    $event = $ekb->make_assertion ($eventElementName, {
        id   => $tmpString,
        rule => $akrl->getValuesAsStringForKey (":RULE")
    });

    $ekb->add_assertion($event);

    # Add the type
    modify_assertion("type", $akrl->getInstanceOf(), $ekb, $event);

    # Check for negation
    modify_assertion("negation", $akrl->getValueForKey(":NEGATION"), $ekb, $event);

    # Add the force
    modify_assertion("force", $akrl->getValueForKey(":FORCE"), $ekb, $event);

    # Add the Agent
    add_arg(":AGENT", $akrl->getValueForKey (":AGENT"), $ekb, $event, $akrlList);

    # Add the Affected
    add_arg(":AFFECTED", $akrl->getValueForKey (":AFFECTED"), $ekb, $event, $akrlList);

    # Add the Formal
    add_arg(":FORMAL", $akrl->getValueForKey (":FORMAL"), $ekb, $event, $akrlList);

    # Check for Sites
    add_child_with_id ("site", $akrl->getValueForKey (":SITE"), $ekb, $event, $akrlList);

    # Check for Cell Location
    my $locNode = add_child_with_id("location", $akrl->getValueForKey (":LOC"), $ekb, $event, $akrlList);
#    if (defined($locNode))
#    {
#        $tmpString = $akrl->getValueForKey (":LOCMOD");
#        if (defined ($tmpString))
#        {
#            if (ref ($tmpString) eq "ARRAY")
#            {
#                shift @$tmpString;
#                $tmpString = shift @$tmpString;
#            }
#
#            EKB::set_attribute($child, "mod", removePrefix($tmpString, "ONT::"));
#        }
#    }

    # Check for Modifiers (:MODN)
    $tmpString = $akrl->getValuesAsArrayForKey(":MODN");
    if (defined($tmpString) and ref($tmpString) eq "ARRAY" and scalar(@$tmpString) > 0)
    {
        my $modsNode = make_node("mods");
        my $foundOne = undef;
        foreach my $mod (@$tmpString)
        {
            if (ref($mod) eq "ARRAY" and scalar(@$mod) > 2)
            {
                $foundOne = "true";
                my $modNode = EKB::make_node ("mod", (
                        EKB::make_slot_node("type", @$mod[1]),
                        EKB::make_slot_node("value", removePrefix(@$mod[2], "W::"))
                    ));
                EKB::add_child($modsNode, $modNode);
            }
        }
        if (defined($foundOne))
        {
            EKB::add_child($event, $modsNode);
        }
    }

    # Check for Modality (:MODALITY)
    $tmpString = $akrl->getValueForKey(":MODALITY");
    if (defined($tmpString) and ref($tmpString) eq "ARRAY" and scalar(@$tmpString) > 2)
    {
        modify_assertion("modality", @$tmpString[2], $ekb, $event);

        # id = $akrl->getToken();
        my $modId = removePrefix($akrl->getToken(), "ONT::V");

        my $modalityNode = $ekb->make_assertion ("MODALITY", {
                id   => 'X'.$modId,
                rule => "-MODALITY"
            });
        modify_assertion("type", @$tmpString[1], $ekb, $modalityNode);
        add_arg(":EVENT", "V".$modId, $ekb, $modalityNode, $akrlList);
        $ekb->add_assertion($modalityNode);
    }

    # Check for From Location
    add_child_with_id ("from-location", $akrl->getValueForKey (":FROM"), $ekb, $event, $akrlList);

    # Check for To Location
    my $toLoc = add_child_with_id ("to-location", $akrl->getValueForKey (":TO"), $ekb, $event, $akrlList);
    if (defined($toLoc))
    {
        # Add the Result
        add_arg(":RES", $akrl->getValueForKey (":TO"), $ekb, $event, $akrlList);
    }

    # process the DRUM Terms
    my $drumTerms = parseDRUMTerms($akrl->getValueForKey(":drum"));
    my $drumTermsNode = undef;
    if (defined($drumTerms) && scalar(@$drumTerms) > 0)
    {
	$drumTermsNode = EKB::make_node("drum-terms");
        foreach my $drumTerm (@{$drumTerms})
        {
            # append id to the dbid string
            my $id = $drumTerm->getID;

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
                        $xref = normalizeId($xref);
                        EKB::add_child($xrefs, EKB::make_slot_node("xref", $xref));
                    }
                }
                EKB::add_child($drumTermNode, $xrefs)
            }
            # Add any Species
            $tmpString = $drumTerm->getSpecies();
            if (defined($tmpString))
            {
                EKB::add_child($drumTermNode, EKB::make_slot_node("species", trimLeadingAndTrailingQuotes($tmpString)));
            }
            EKB::add_child($drumTermsNode, $drumTermNode);
	}
	$ekb->modify_assertion($event, $drumTermsNode);
    }
    
    return $event;
}

sub trimLeadingAndTrailingQuotes
{
    my ($string) = @_;
    if (defined($string))
    {
        $string =~ s/^\s+|\s+$//g;
        $string =~ s/^"|"$//g;
    }
    return $string;
}

sub addMutationToFromChildNode
{
    my ($mutationNode, $type, $name, $code) = @_;
    if (!defined($mutationNode) or !defined($type))
    {
        return;
    }

    if (!defined ($name) and !defined($code))
    {
        return;
    }
    my $aaType = EKB::make_node($type);
    my $aa = EKB::make_node("aa");
    if (defined ($name))
    {
        EKB::add_child($aa, EKB::make_slot_node("name", trimLeadingAndTrailingQuotes($name)));
    }
    if (defined ($code))
    {
        EKB::add_child($aa, EKB::make_slot_node("code", trimLeadingAndTrailingQuotes($code)));
    }
    EKB::add_child($aaType, $aa);
    EKB::add_child($mutationNode, $aaType);
}

=head2 createEKBTerm($ekb, $akrl, $akrlList)

Returns the given $arkl object as an EKB 'TERM' node.

=cut

sub createEKBTerm
{
    my ($ekb, $akrl, $akrlList) = @_;

    if (!defined($akrl))
    {
        return 0;
    }

    if (!defined ($ekb))
    {
        return 1;
    }

    # get the id of the term and trim the prefix
    my $tmpString = $akrl->getToken();
    if (defined $tmpString)
    {
        $tmpString = removePrefix($tmpString, "ONT::");
    }

    INFO ("Processing AKRL Term Element with id: $tmpString");

    # if an assertion with this id already exists, then return it.
    my $term = $ekb->get_assertion($tmpString);
    if (defined($term))
    {
        INFO (" - An AKRL Term with the id: $tmpString, has already been processed.");
        return $term;
    }

    # figure out if this is an Aggregate, Complex, or Regular Term
    my $seqIds = $akrl->getValueForKey (":SEQUENCE");
    if (!defined($seqIds))
    {
        $seqIds = $akrl->getValueForKey (":M-SEQUENCE");
    }
    if (defined($seqIds) and ref($seqIds) eq "ARRAY")
    {
        my @cleanIds = map { removePrefix($_, "ONT::") } @$seqIds;

        my $operator = $akrl->getValueForKey(":OPERATOR");
        if (defined ($operator))
        {
            # This is an Aggregate Term
            $term = $ekb->make_aggregate_term (\@cleanIds, removePrefix($operator, "ONT::"),{
                    id   => $tmpString,
                    rule => $akrl->getValuesAsStringForKey (":RULE")
                });
        }
        else
        {
            # This will be handled as a Complex Term
            $term = $ekb->make_complex_term (\@cleanIds, {
                    id   => $tmpString,
                    rule => $akrl->getValuesAsStringForKey (":RULE")
                });
        }

        foreach my $seqId (@$seqIds)
        {
            my $refAKRLObj = $akrlList->getObjectWithToken($seqId);
            if (defined ($refAKRLObj))
            {
                # RECUSIVE CALL HERE TO ADD THE TERMS. ALSO DO NOT WORRY ABOUT THE TYPE HERE
                addEKBAssertion($ekb, $refAKRLObj, $akrlList);
            }
        }
    }
    else
    {
        INFO (" - Creating a new EKB 'TERM' assertion.");
        # create a new assertion
        $term = $ekb->make_assertion("TERM", {
                id   => $tmpString,
                rule => $akrl->getValuesAsStringForKey (":RULE")
            });
    }
    $ekb->add_assertion($term);

    # Add the type
    $tmpString = $akrl->getValueForKey(":ELEMENT-TYPE");
    if (!defined($tmpString))
    {
        $tmpString = $akrl->getInstanceOf();
    }
    modify_assertion("type", $tmpString, $ekb, $term);

    # Add the name
    modify_assertion("name", removePrefix($akrl->getValueForKey(":NAME"), "W::"), $ekb, $term);

    # Add any Features
    my $inevents = $akrl->getValuesAsArrayForKey(":INEVENT");
    if (defined ($inevents) and ref ($inevents) eq "ARRAY" and scalar(@$inevents) > 0)
    {
        my $inevent = $ekb->add_feature ($term, "inevent");
        foreach my $eventId (@$inevents)
        {
            add_child_with_id("event", $eventId, $ekb, $inevent, $akrlList);
        }
    }

    # Add any Mutations
    add_feature("mutation", $akrl->getValueForKey(":MUTATION"), $ekb, $term, $akrlList);

    # process any DRUM Mutation Info
    my $drumMutations = parseDRUMMutations($akrl->getValueForKey(":drum"));
    if (defined ($drumMutations) && $drumMutations > 0)
    {
        foreach my $mutation (@{$drumMutations})
        {
            my $mutationNode = EKB::make_node("mutation");
            if (defined($mutation->getType()))
            {
                EKB::add_child($mutationNode, EKB::make_slot_node("type", $mutation->getType()));
            }
            if (defined($mutation->getPosition))
            {
                EKB::add_child($mutationNode, EKB::make_slot_node("pos", $mutation->getPosition()));
            }
            addMutationToFromChildNode ($mutationNode, "aa-from", $mutation->getFromName(), $mutation->getFromCode());
            addMutationToFromChildNode ($mutationNode, "aa-to", $mutation->getToName(), $mutation->getToCode());
            if (EKB::get_children_by_name_regex($mutationNode, "") > 0)
            {
                EKB::add_child($term, $mutationNode);
            }
        }
    }

    # process any DRUM AA-Sites
    my $drumAASites = parseDRUMAASites($akrl->getValueForKey(":drum"));
    if (defined ($drumAASites) && $drumAASites > 0)
    {
        foreach my $aasite (@{$drumAASites})
        {
            my $aasiteNode = EKB::make_node("site");
            $tmpString = $aasite->getName();
            if (defined($tmpString))
            {
                EKB::add_child($aasiteNode, EKB::make_slot_node("name", trimLeadingAndTrailingQuotes($tmpString)));
            }
            $tmpString = $aasite->getCode();
            if (defined($tmpString))
            {
                EKB::add_child($aasiteNode, EKB::make_slot_node("code", trimLeadingAndTrailingQuotes($tmpString)));
            }
            $tmpString = $aasite->getPosition();
            if (defined($tmpString))
            {
                EKB::add_child($aasiteNode, EKB::make_slot_node("pos", $tmpString));
            }
            my $features = EKB::make_node("features");
            EKB::add_child($features, $aasiteNode);
            EKB::add_child($term, $features);
        }
    }

    # process the DRUM Terms
    my $drumTerms = parseDRUMTerms($akrl->getValueForKey(":drum"));
    my @dbid = ();
    my $drumTermsNode = undef;
    if (defined($drumTerms) && scalar(@$drumTerms) > 0)
    {
        $drumTermsNode = EKB::make_node("drum-terms");
        foreach my $drumTerm (@{$drumTerms})
        {
            # append id to the dbid string
            my $id = $drumTerm->getID;
            if (defined ($id))
            {
                #normalize the id before appending to dbid array
                push (@dbid, normalizeId($id));
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
                        $xref = normalizeId($xref);
                        #normalize the id before appending to dbid array
                        push (@dbid, $xref);

                        EKB::add_child($xrefs, EKB::make_slot_node("xref", $xref));
                    }
                }
                EKB::add_child($drumTermNode, $xrefs)
            }
            # Add any Species
            $tmpString = $drumTerm->getSpecies();
            if (defined($tmpString))
            {
                EKB::add_child($drumTermNode, EKB::make_slot_node("species", trimLeadingAndTrailingQuotes($tmpString)));
            }
            EKB::add_child($drumTermsNode, $drumTermNode);
        }
        @dbid = uniq(@dbid);
        if (scalar(@dbid) > 0)
        {
#            EKB::add_child($term, $drumTermsNode);
            $ekb->modify_assertion($term, ({dbid => join("|", @dbid)}, $drumTermsNode));

        }
    }

    return $term;
}

=head2 addEKBAssertion($ekb, $akrl, $akrlList)

Creates an appropriate EKB assertion (EVENT or TERM) from the $akrl and adds it to
the provided $ekb.  All children of the $akrl are also added to the $ekb and the
assertion is returned.

=cut

sub addEKBAssertion
{
    my ($ekb, $akrl, $akrlList) = @_;

    if (!defined ($akrl))
    {
        return undef;
    }

    my $indicator = $akrl->getIndicator();
    my $ekbTerm = undef;
    if ($indicator eq "ONT::RELN")
    {
        $ekbTerm = createEKBEvent($ekb, $akrl, $akrlList);
    }
    elsif ($indicator eq "ONT::A" or $indicator eq "ONT::THE")
    {
        $ekbTerm = createEKBTerm($ekb, $akrl, $akrlList);
    }

    return $ekbTerm;
}

=head2 toEKB($akrlList)

Returns an EKB object containing all the AKRL items in the $akrlList as ekb assertions.

=cut

sub toEKB
{
    my ($akrlList, $ids) = @_;

    if (!defined ($akrlList) or !ref($akrlList))
    {
        return undef;
    }

    if (!defined ($ids) or (ref ($ids) ne "ARRAY") or scalar(@$ids) <= 0)
    {
        $ids = undef;
        my $tokens = $akrlList->getTokens();
        if (defined ($tokens) and ref($tokens) eq "ARRAY")
        {
            $ids = $tokens;
        }
    }

    if (!defined ($ids))
    {
        return undef;
    }

    my $ekb = EKB->new;
    my $ekbObject = undef;
    foreach my $key (@$ids)
    {
        INFO ("Processing AKRL TERM with ID: $key");
        $ekbObject = addEKBAssertion($ekb, ($akrlList->getObjectWithToken($key)), $akrlList);
    }

    return $ekb;
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
