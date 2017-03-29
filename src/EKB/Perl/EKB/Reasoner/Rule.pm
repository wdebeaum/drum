# Rule.pm
#
# Time-stamp: <Mon Feb  6 12:49:43 CST 2017 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 30 May 2016
#

#----------------------------------------------------------------
# Description:
# Class for definining rules for EKB::Reasoner.

#----------------------------------------------------------------
# History
# 2016/05/30 v0.0	lgalescu
# - Started.
# 2016/06/04 v1.0	lgalescu
# - Working version.

package EKB::Reasoner::Rule;

$VERSION = '1.0';

use strict 'vars';
use warnings;

use Data::Dumper;
use List::Util qw(any);
use XML::LibXML;

use util::Log;

# rules
# { name => string
#   constraints => [ list of Xpath expressions ],
#   repeat => 0|1,
#   rule => &func, # applies function to XML node
# }

# NB:
# constraint XPath expressions are relative to the element, eg:
# EVENT[type="ONT::ACTIVATE" and arg[role=":AFFECTED"]]
# the idea is that the constraints would make a quick selection, but
# the rule may impose finer-grained constraints

sub new {
    my $class = shift;
    my ($reasoner, $def) = @_;
    my $valid = eval { $reasoner->isa("EKB::Reasoner")
		       &&
		       (ref($def) eq 'HASH') };
    if (!$valid) {
        ERROR "Failed to create %s object%s.", $class, ($@ ? " ($@)" : "");
        return undef;
    }
    my $self = {
		reasoner => $reasoner,
		name => $def->{name} // "",
		constraints => $def->{constraints} // [],
		# whether the rule can be tried on repeatedly
		repeat => $def->{repeat} // 0,
		# function that executes the rule;
		# has 3 args: rule, ekb, element
		# returns 0 for failure, 1 for success
		handler => $def->{handler},
	       };
    bless $self, $class;
}

## Getters (partial)
# no setters: rule is defined in full using new()

sub name {
    my $self = shift;
    return $self->{name};
}

sub reasoner {
    my $self = shift;
    return $self->{reasoner};
}

sub constraints {
    my $self = shift;
    return $self->{constraints};
}

sub repeat {
    my $self = shift;
    return $self->{repeat};
}

# select applicable elements from a list 
sub select {
    my $self = shift;
    my @elements = @_;
    DEBUG 3, "checking rule applicability to %d elements",
      scalar(@elements);
    my @result = grep { $self->is_applicable_to($_) } @elements;
    DEBUG 2, "rule \"%s\" applies to %d out of %d elements",
      $self->name(), scalar(@result), scalar(@elements);
    return @result;
}

# check if rule is applicable to an element
sub is_applicable_to {
    my $self = shift;
    my $element = shift;
    DEBUG 3, "checking applicability to: %s", $element;
    return 1 if scalar(@{ $self->{constraints} }) == 0;
    my $result =
      any { $element->exists("self::" . $_) } @{ $self->{constraints} };
    DEBUG 3, "=> %d", $result;
    return $result;
}

# apply rule (to an element in an EKB)
# the result should be the number of times the rule was applied
sub apply {
    my $self = shift;
    my $result = $self->{handler}->($self, @_);
    $result =~ m{\A\d+\Z}
      or ERROR "Handler for %s does not return 0 or 1!", $self->name();
    return $result;
}

1;
