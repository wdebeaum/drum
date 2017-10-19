# ARKL2EKB.pm
#
# Time-stamp: <Fri Sep 15 22:23:44 CDT 2017 lgalescu>
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
use Ont::BioEntities;
use Ont::BioEvents;
use Ont::GenericTerms;
use StringParser;
use util::Log;
use Data::Dumper;
local $util::Log::Caller_Info = 1;

use strict;
use warnings FATAL => 'all';

my $AKRL_ERROR_MISSING_INDICATOR = -10;
my $AKRL_ERROR_MISSING_TOKEN = -11;
my $AKRL_ERROR_MISSING_INSTANCE_OF = -12;
my $AKRL_ERROR_INVALID_KEY = -13;
my $AKRL_ERROR_MISSING_VALUE = -14;

my $ont_events = Ont::BioEvents->new();
my $ont_bioents = Ont::BioEntities->new();
my $ont_genericTerms = Ont::GenericTerms->new();

=head2 Processed Event Role Arguments

eventRoleArguments is an array which contains all AKRL properties which will
be turned into EKB Event arg's.

=cut

my @eventRoleArguments = (":AGENT",
			  ":AGENT1",
			  ":AFFECTED",
			  ":AFFECTED1",
			  ":FACTOR",
			  ":OUTCOME",
			  ":AFFECTED-RESULT",
			  ":NEUTRAL",
			  ":NEUTRAL1",
			  ":NEUTRAL2",
			  ":FORMAL",
			  ":EXPERIENCER",
			  ":RES"
			 );

=head2 Event Modifiers

A list of Event Modifiers to process.  These are turned into child slot
nodes of the parent event.

=cut

my @eventModifiers = ("NEGATION", "POLARITY", "FORCE");

=head2 Poly Modifiers

A mapping of Poly Modifiers to process.  These are turned into child nodes
of the parent event or term and contain child slot nodes for type and value.
The mapping is from AKRL attribute to EKB node name.

=cut

my %polyModifiers = (":DEGREE" => "degree",
                     ":FREQUENCY" => "frequency",
                     ":MODN" => "mod",
                     ":MODA" => "mod");

=head2 Event Features

A mapping of Event Features to process.  These are turned into child nodes
of the parent event.  The mapping is from AKRL attribute to EKB node name.

=cut

my %eventFeatures = (":SITE" => "site",
                     ":LOC" => "location",
                     ":FROM" => "from-location",
                     ":TO" => "to-location",
                     ":CELL-LINE" => "cell-line",
                     ":EPI" => "epistemic-modality");

=head2 Event Feature Children

A mapping of Event Feature children to process.  These are turned into child nodes
of the parent event's feature child node  The mapping is from AKRL attribute
to EKB node name.

=cut

my %eventFeatureChildren = (":INEVENT" => "inevent");      # Can have multiple and remove package prefix

=head2 Term Features

A mapping of Term Features to process.  These are turned into child nodes
of the parent term's feature child node.  The mapping is from AKRL attribute
to EKB feature name.

=cut

my %termFeatures = (":ACTIVE" => "active",          # Only one and remove package prefix
                    ":LOC" => "location",           # Can have multiple and remove package prefix
                    ":SITE" => "site",              # Only one and remove package prefix
                    ":CELL-LINE" => "cell-line",    # Only one and remove package prefix
                    ":INEVENT" => "inevent",        # Can have multiple and remove package prefix
                    ":MUTATION" => "mutation");     # Can have multiple, If Boolean or not Ont Var then text, else ID attribute.

# Helper Methods

=head2 normalizeId($id)

Returns a copy of the $id string after replacing any '::' with ':' and
removing all '|' characters.

=cut

sub normalizeId
{
    my ($id) = @_;
    my @parsed = split(/:+/, $id);

    my $normalizedId = "";

    if (scalar(@parsed) == 2)
    {
        my $part2 = $parsed[1];
        $part2 =~ s/\|//g;
        $normalizedId = $parsed[0] . ":" . $part2;
        return $normalizedId;
    }
    @parsed = split(/_/, $id);
    if (scalar(@parsed) == 2)
    {
        my $part2 = $parsed[1];
        $part2 =~ s/\|//g;
        $normalizedId = $parsed[0] . ":" . $part2;
        return $normalizedId;
    }

    WARN("Unrecognized dbid format: $id");
    return $id;
}

=head2 removePackage ($word)

Removes the package prefix from the provided word and returns it.

=cut

sub removePackage
{
    my ($word) = @_;
    $word =~ s/^[A-Za-z]+:://g;
    return $word;
}

=head2 normalizeOnt($word)

Normalized the ontology in the provided word by first removing the package
and then converting any special character encodings back to the special
character.

=cut

sub normalizeOnt
{
    my ($word) = @_;
    if (!defined ($word))
    {
        return $word;
    }
    $word = removePackage($word);
    $word =~ s/-PUNC-SLASH-/\//g;
    $word =~ s/-PUNC-MINUS-/-/g;
    $word =~ s/-PUNC-EN-DASH-/-/g;
    $word =~ s/-PUNC-PERIOD-/./g;
    $word =~ s/-PUNC-PERIOD/./g;
    $word =~ s/-PUNC-COMMA-/,/g;
    $word =~ s/-START-PAREN-/(/g;
    $word =~ s/START-PAREN-/(/g;
    $word =~ s/-END-PAREN-/)/g;
    $word =~ s/-END-PAREN/)/g;
    $word =~ s/ /_/g;;
    return $word;
}

=head2 uniq(@_)

Given a list of objects, this will return a list which contains only the
unique objects in the list.

=cut

sub uniq
{
    keys %{{ map { $_ => 1 } @_ }};
}

# toEKB Methods

=head2 add_slot_node ($attribute, $value, $ekb, $node)

Helper method which adds a slot node with the given attribute and value to
the specified node, but only if the value is defined.

=cut

sub add_slot_node
{
    my ($attribute, $value, $ekb, $node) = @_;
    if (defined ($value))
    {
        $ekb->modify_assertion ($node, make_slot_node($attribute, $value));
        return 1;
    }
    return undef;
}

=head2 add_arg ($role, $value, $ekb, $node, $akrlList)

Helper method to add an arg to the specified node with the given role and
value if the value is defined.  Also, if the value it defined then try to
recursively process it and another ID in the provided AKRL List.

=cut

sub add_arg
{
    my ($role, $value, $ekb, $node, $akrlList) = @_;
    if (defined ($value))
    {
        my $argNode = $ekb->add_arg($node, $role, removePackage($value));
        addEKBAssertion($value, $ekb, $akrlList);
        return $argNode;
    }
    return undef;
}

=head2 add_child_with_id_attribute ($child, $id, $ekb, $node, $akrlList)

Helper method which adds a child node with an attribute id to the provided node,
if the id is defined.  Also, if the id is defined, it is recursively processed
as another AKRL object.

=cut

sub add_child_with_id_attribute
{
    my ($child, $id, $ekb, $node, $akrlList) = @_;
    if (defined ($id))
    {
        my $res = addEKBAssertion($id, $ekb, $akrlList);
        if (!defined ($res))
        {
            return undef;
        }
        my $childNode = make_node($child, { id => removePackage($id) });
#        $ekb->modify_assertion($node, $childNode);
        EKB::add_child($node, $childNode);
        return $childNode;
    }
    return undef;
}

=head2 add_poly_modifiers ($node, $ekb, $akrl)

Helper method to add the poly modifiers contained in the AKRL object as ekb
to the provided node.

=cut

sub add_poly_modifiers
{
    my ($node, $ekb, $akrl) = @_;
    # Check for PolyModifiers
    my $modsNode = make_node("mods");
    my $foundOne = undef;
    foreach my $polyModifier (keys(%polyModifiers))
    {
        my $value = $akrl->getValuesAsArrayForKey($polyModifier);
        if (defined($value) and ref($value) eq "ARRAY" and scalar(@$value) > 0)
        {
            foreach my $mod (@$value)
            {
                if (ref($mod) eq "ARRAY" and scalar(@$mod) > 2)
                {
                    $foundOne = "true";
                    my $modNode = make_node ($polyModifiers{$polyModifier}, (
                            make_slot_node("type", @$mod[1]),
                            make_slot_node("value", removePackage(@$mod[2], "W::"))
                        ));
                    $modsNode->appendChild($modNode);
                }
            }
        }
    }
    if (defined($foundOne))
    {
        $ekb->modify_assertion($node, $modsNode)
    }
}

=head2 add_feature_smart ($feature, $value, $ekb, $node, $akrlList)

Helper method to add a feature to the provided node.  The value is checked to
see how the feature should be added.  Boolean and non-Ontology Vars are added
as slot nodes and Ontology vars are added as features with id's which are
recursively processeds as new AKRL objects.

=cut

sub add_feature_smart
{
    my ($feature, $value, $ekb, $node, $akrlList) = @_;

    my $featureAdded = undef;

    if (defined ($value))
    {
        if (ref($value) eq 'ARRAY')
        {
            foreach my $val (@$value)
            {
                $featureAdded = add_feature_smart($feature, $val, $ekb, $node, $akrlList);
            }
        }
        elsif (ref($value) eq '')
        {
            if ($value =~ /(?i)\AONT::(TRUE|FALSE)\z/ or !($value =~ /(?i)\AONT::[VX][0-9]+\z/))
            { # Boolean or Not an Ont Var
                $featureAdded = $ekb->add_feature($node, $feature => removePackage($value));
            }
            else # Is an Ont Var
            {
                $featureAdded = $ekb->add_feature($node, $feature => { id => removePackage($value) });
                addEKBAssertion($value, $ekb, $akrlList);
            }
        }
    }
    return $featureAdded;
}

=head2 trimLeadingAndTrailingQuotes ($string)

The provided string is stripped of any leading and trailing quotes and
then returned.

=cut

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
        return -1;
    }

    if (!defined ($name) and !defined($code))
    {
        return -2;
    }
    my $aaType = make_node($type);
    my $aa = make_node("aa");
    if (defined ($name))
    {
        $aa->appendChild(make_slot_node("name", trimLeadingAndTrailingQuotes($name)));
    }
    if (defined ($code))
    {
        $aa->appendChild(make_slot_node("code", trimLeadingAndTrailingQuotes($code)));
    }
    $aaType->appendChild($aa);
    $mutationNode->appendChild($aaType);

    return 0;
}

sub processDRUMMutationInfo
{
    my ($node, $ekb, $drumMutations) = @_;
    if (!defined($drumMutations) || scalar($drumMutations) <= 0)
    {
        return 0;
    }
    if (!defined ($ekb))
    {
        return 1;
    }
    if (!defined ($node))
    {
        return 2;
    }

    # process each DRUM Mutation Info
    foreach my $mutation (@{$drumMutations})
    {
        my $mutationNode = make_node("mutation");
        if (defined($mutation->getType()))
        {
            $mutationNode->appendChild(make_slot_node("type", $mutation->getType()));
        }
        if (defined($mutation->getPosition))
        {
            $mutationNode->appendChild(make_slot_node("pos", $mutation->getPosition()));
        }
        my $retFrom = addMutationToFromChildNode ($mutationNode, "aa-from", $mutation->getFromName(), $mutation->getFromCode());
        my $retTo =   addMutationToFromChildNode ($mutationNode, "aa-to", $mutation->getToName(), $mutation->getToCode());
        if (($retFrom >= 0) || ($retTo >= 0))
        {
            $node->appendChild($mutationNode);
        }
    }

    return 0;
}

sub processDRUMAASites
{
    my ($node, $ekb, $drumAASites) = @_;
    if (!defined($drumAASites) || scalar($drumAASites) <= 0)
    {
        return 0;
    }
    if (!defined ($ekb))
    {
        return 1;
    }
    if (!defined ($node))
    {
        return 2;
    }

    my $tmpString = "";

    # process any DRUM AA-Sites
    foreach my $aasite (@{$drumAASites})
    {
        my @siteInfo = ();
        $tmpString = $aasite->getName();
        if (defined($tmpString))
        {
            push (@siteInfo, make_slot_node("name", trimLeadingAndTrailingQuotes($tmpString)));
        }
        $tmpString = $aasite->getCode();
        if (defined($tmpString))
        {
            push (@siteInfo, make_slot_node("code", trimLeadingAndTrailingQuotes($tmpString)));
        }
        $tmpString = $aasite->getPosition();
        if (defined($tmpString))
        {
            push (@siteInfo, make_slot_node("pos", $tmpString));
        }
        my $aasiteNode = $ekb->add_feature ($node, "site", @siteInfo);
    }

    return 0;
}

sub processDRUMTerms
{
    my ($node, $ekb, $drumTerms) = @_;

    if (!defined($drumTerms) || scalar(@$drumTerms) <= 0)
    {
        return 0;
    }

    my @dbid = ();
    my $drumTermsNode = undef;
    my $tmpString = undef;

    $drumTermsNode = make_node("drum-terms");
    foreach my $drumTerm (@{$drumTerms})
    {
        # append id to the dbid string
        my $id = $drumTerm->getID;
        if (defined ($id))
        {
            #normalize the id
            $id = normalizeId($id);
            push (@dbid, $id);
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
            my $types = make_node("types");
            foreach my $ontType (@$ontTypes)
            {
                $types->appendChild(make_slot_node("type", $ontType));
            }
            $drumTermNode->appendChild($types)
        }
        # Add any DBXRefs
        my $dbxRefs = $drumTerm->getDBXRefs();
        if (defined ($dbxRefs) && ref($dbxRefs) eq "ARRAY" && $dbxRefs > 0)
        {
            my $xrefs = make_node("xrefs");
            foreach my $xref (@$dbxRefs)
            {
                if (defined($xref))
                {
                    $xref = normalizeId($xref);
                    #normalize the id before appending to dbid array
                    push (@dbid, $xref);
                    $xrefs->appendChild(make_node("xref", {dbid => $xref}));
                }
            }
            $drumTermNode->appendChild($xrefs)
        }
        # Add any Species
        $tmpString = $drumTerm->getSpecies();
        if (defined($tmpString))
        {
            $drumTermNode->appendChild(make_slot_node("species", trimLeadingAndTrailingQuotes($tmpString)));
        }
        $drumTermsNode->appendChild($drumTermNode);
    }
    @dbid = uniq(@dbid);

    if (scalar(@dbid) > 0)
    {
        $ekb->modify_assertion($node, $drumTermsNode);
    }

    return \@dbid;
}

=head2 createEKBEvent($ekb, $akrl, $akrlList)

Returns the given $arkl object as an EKB 'EVENT' node.

=cut

sub createEKB_Event
{
    my ($ekb, $akrl, $akrlList) = @_;

    if (!defined ($akrl))
    {
        return 10;
    }

    # make sure the instanceOf is in the list of acceptable Ontologies
    my $instanceOf = $akrl->getInstanceOf();
    if (!defined ($instanceOf))
    {
        return 11;
    }

    if (!$ont_events->has($instanceOf) and $instanceOf ne "ONT::SEQUENCE")
    {
        WARN("Ignoring event with type: $instanceOf");
        return 12;
    }

    return createEKB_RELN("EVENT", $ekb, $akrl, $akrlList);
}

=head2 createEKB_EPI($ekb, $akrl, $akrlList)

Returns the given $arkl object as an EKB 'EPI' node.

=cut

sub createEKB_EPI
{
    my ($ekb, $akrl, $akrlList) = @_;

    if (!defined ($akrl))
    {
        return 10;
    }

    # make sure the instanceOf is in the list of acceptable Ontologies
    my $instanceOf = $akrl->getInstanceOf();
    if (!defined ($instanceOf))
    {
        return 11;
    }

    return createEKB_RELN("EPI", $ekb, $akrl, $akrlList);
}

=head2 createEKB_CC($ekb, $akrl, $akrlList)

Returns the given $arkl object as an EKB 'CC' node.

=cut

sub createEKB_CC
{
    my ($ekb, $akrl, $akrlList) = @_;

    if (!defined ($akrl))
    {
        return 10;
    }

    # make sure the instanceOf is in the list of acceptable Ontologies
    my $instanceOf = $akrl->getInstanceOf();
    if (!defined ($instanceOf))
    {
        return 11;
    }

    return createEKB_RELN("CC", $ekb, $akrl, $akrlList);
}

=head2 createEKBReln($type, $ekb, $akrl, $akrlList)

Returns the given $arkl object as an EKB 'EVENT' node.

=cut

sub createEKB_RELN
{
    my ($eventElementName, $ekb, $akrl, $akrlList) = @_;

    if (!defined($eventElementName))
    {
        $eventElementName = "EVENT";
    }

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
        $tmpString = removePackage($tmpString);
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

    # figure out if this is an Aggregate, Complex, or Regular Term
    my $seqIds = $akrl->getValueForKey (":SEQUENCE");
    if (!defined($seqIds))
    {
        $seqIds = $akrl->getValueForKey (":M-SEQUENCE");
    }
    if (defined($seqIds) and ref($seqIds) eq "ARRAY")
    {
        my @cleanIds = map { removePackage($_) } @$seqIds;

        my $operator = $akrl->getValueForKey(":OPERATOR");
        if (defined ($operator))
        {
            # This is a conjoined Event
            $event = $ekb->make_conjoined_event (
                            \@cleanIds,
                            removePackage($operator),
                            {
                                id   => $tmpString,
                                rule => $akrl->getValuesAsStringForKey (":RULE")
                            });
        }

        foreach my $seqId (@$seqIds)
        {
            addEKBAssertion($seqId, $ekb, $akrlList);
        }
    }

    if (!defined($event))
    {
        # create a new assertion based on the instanceOf.
        $event = $ekb->make_assertion ($eventElementName, {
                id   => $tmpString,
                rule => $akrl->getValuesAsStringForKey (":RULE")
            });
    }

    $ekb->add_assertion($event);

    # Add the type
    add_slot_node("type", $instanceOf, $ekb, $event);

    # Add the modifiers
    foreach my $modifier (@eventModifiers)
    {
        add_slot_node(lc($modifier), $akrl->getValueForKey(":".$modifier), $ekb, $event);
    }

    # Check for Modality (:MODALITY)
    $tmpString = $akrl->getValueForKey(":MODALITY");
    if (defined($tmpString))
    {
        if (ref($tmpString) eq "ARRAY" and scalar(@$tmpString) > 2)
        {
            add_slot_node("modality", @$tmpString[2], $ekb, $event);

            # id = $akrl->getToken();
            my $modId = removePrefix($akrl->getToken(), "ONT::V");

            my $modalityNode = $ekb->make_assertion ("MODALITY", {
                    id   => 'X'.$modId,
                    rule => "-MODALITY"
                });
            add_slot_node("type", @$tmpString[1], $ekb, $modalityNode);
            add_arg(":EVENT", "V".$modId, $ekb, $modalityNode, $akrlList);
            $ekb->add_assertion($modalityNode);
        }
        else
        {
            add_slot_node ("modality", $tmpString, $ekb, $event);
        }
    }

    # Add Mods
    add_poly_modifiers($event, $ekb, $akrl);


    # Add Event Feature Children
    foreach my $feature (keys(%eventFeatureChildren))
    {
        add_feature_smart($eventFeatureChildren{$feature}, $akrl->getValueForKey($feature), $ekb, $event, $akrlList)
    }

    # Add event role arguments
    foreach my $role (@eventRoleArguments)
    {
        add_arg($role, $akrl->getValueForKey ($role), $ekb, $event, $akrlList);
    }

    # Add Other Event Features
    foreach my $feature (keys(%eventFeatures))
    {
        add_child_with_id_attribute ($eventFeatures{$feature}, $akrl->getValueForKey ($feature), $ekb, $event, $akrlList);
    }

    # process the DRUM Terms
    processDRUMTerms($event, $ekb, parseDRUMTerms($akrl->getValueForKey(":drum")));

    return $event;
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
        $tmpString = removePackage($tmpString);
    }

    DEBUG (10, "Processing AKRL Term Element with id: $tmpString");

    # if an assertion with this id already exists, then return it.
    my $term = $ekb->get_assertion($tmpString);
    if (defined($term))
    {
        DEBUG (10, " - An AKRL Term with the id: $tmpString, has already been processed.");
        return $term;
    }

    # Add the type
    my $instanceOf = $akrl->getValueForKey(":ELEMENT-TYPE");
    if (!defined($instanceOf))
    {
        $instanceOf = $akrl->getInstanceOf();
    }

    if (!$ont_bioents->has($instanceOf) and !$ont_genericTerms->has($instanceOf))
    {
        WARN("Ignoring term with type: $instanceOf");
        return 3;
    }

    # figure out if this is an Aggregate, Complex, or Regular Term
    my $seqIds = $akrl->getValueForKey (":SEQUENCE");
    if (!defined($seqIds))
    {
        $seqIds = $akrl->getValueForKey (":M-SEQUENCE");
    }
    if (defined($seqIds) and ref($seqIds) eq "ARRAY")
    {
        my @cleanIds = map { removePackage($_) } @$seqIds;

        my $operator = $akrl->getValueForKey(":OPERATOR");
        if (defined ($operator))
        {
            # This is an Aggregate Term
            $term = $ekb->make_aggregate_term (\@cleanIds, removePackage($operator),{
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
            addEKBAssertion($seqId, $ekb, $akrlList);
        }
    }
    else
    {
        DEBUG (10, " - Creating a new EKB 'TERM' assertion.");
        # create a new assertion
        $term = $ekb->make_assertion("TERM", {
                id   => $tmpString,
                rule => $akrl->getValuesAsStringForKey (":RULE")
            });
    }
    $ekb->add_assertion($term);

    # Add the type
    add_slot_node("type", $instanceOf, $ekb, $term);

    # Add the name
    add_slot_node("name", normalizeOnt($akrl->getValueForKey(":NAME")), $ekb, $term);

    # Add the Quantity
    add_child_with_id_attribute ('of', $akrl->getValueForKey(":ENTITY"), $ekb, $term, $akrlList);

    # Add Mods
    add_poly_modifiers($term, $ekb, $akrl);

    # Add Term Features
    foreach my $feature (keys(%termFeatures))
    {
        add_feature_smart($termFeatures{$feature}, $akrl->getValueForKey($feature), $ekb, $term, $akrlList)
    }

    # Add assoc-with terms.
    my $assocWith = $akrl->getValuesAsArrayForKey(":ASSOC-WITH");
    if (defined ($assocWith))
    {
        foreach my $val (@$assocWith)
        {
            add_child_with_id_attribute ('assoc-with', $val, $ekb, $term, $akrlList);
        }
    }

    # process any DRUM AA Sites features
    processDRUMAASites($term, $ekb, parseDRUMAASites($akrl->getValueForKey(":drum")));

    # process any DRUM Mutation Info
    processDRUMMutationInfo($term, $ekb, parseDRUMMutations($akrl->getValueForKey(":drum")));

    # process the DRUM Terms
    my $dbids = processDRUMTerms ($term, $ekb, parseDRUMTerms($akrl->getValueForKey(":drum")));
    if (defined($dbids) and ref($dbids) eq "ARRAY")
    {
        $ekb->modify_assertion($term, ({ dbid => join("|", @$dbids) }));
    }

    return $term;
}

=head2 addEKBAssertion($akrlId, $ekb, $akrlList)

Creates an appropriate EKB assertion (EVENT or TERM) from the $akrl and adds it to
the provided $ekb.  All children of the $akrl are also added to the $ekb and the
assertion is returned.

=cut

sub addEKBAssertion
{
    my ($akrlId, $ekb, $akrlList, $isTopLevel) = @_;

    if (!defined ($akrlId))
    {
        return undef;
    }

    if (ref($akrlId) eq "ARRAY")
    {
        my @idList = @$akrlId;
        foreach my $id (@idList)
        {
            addEKBAssertion($id, $ekb, $akrlList);
        }
        return 1;
    }

    my $akrl = $akrlList->getObjectWithToken($akrlId);

    if (!defined ($akrl))
    {
        return undef;
    }

    my $indicator = $akrl->getIndicator();
    my $ekbTerm = undef;
    if ($indicator eq "ONT::EVENT")
    {
        $ekbTerm = createEKB_Event($ekb, $akrl, $akrlList);
    }
    elsif ($indicator eq "ONT::EPI")
    {
        $ekbTerm = createEKB_EPI($ekb, $akrl, $akrlList);
    }
    elsif ($indicator eq "ONT::CC")
    {
        $ekbTerm = createEKB_CC($ekb, $akrl, $akrlList);
    }
    elsif ($indicator eq "ONT::TERM")
    {
        $ekbTerm = createEKBTerm($ekb, $akrl, $akrlList);
    }
    elsif (!defined($isTopLevel) or $isTopLevel ne 'true')
    {
        if ($indicator eq "ONT::RELN")
        {
            $ekbTerm = createEKB_Event($ekb, $akrl, $akrlList);
        }
        elsif ($indicator eq "ONT::A" or $indicator eq "ONT::THE")
        {
            $ekbTerm = createEKBTerm($ekb, $akrl, $akrlList);
        }
    }
    else
    {
	WARN("AKRL term not mappable to an EKB assertion: $indicator");
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

    INFO ("toEKB - Asked to process these ids: " . join(", ", @$ids));
    my $ekb = EKB->new;
    my $ekbObject = undef;
    foreach my $key (@$ids)
    {
        DEBUG (10, "Processing AKRL TERM with ID: $key");
        addEKBAssertion($key, $ekb, $akrlList, 'true');
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
