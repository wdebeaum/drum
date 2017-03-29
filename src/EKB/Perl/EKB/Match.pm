# Match.pm
#
# Time-stamp: <Fri Feb  3 15:02:28 CST 2017 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>,  3 May 2016
#

#----------------------------------------------------------------
# Description:
# 

#----------------------------------------------------------------
# History
# 2016/05/03 v0.1	lgalescu
# - Created, based on prior scripts.
# 2016/05/11 v0.2	lgalescu
# - Some additions.
# 2016/05/26 v1.0	lgalescu

package EKB::Match;

$VERSION = '1.0';
  
use strict 'vars';
use warnings;

use Data::Dumper;
use List::Util qw(any none all first);
use XML::LibXML;

use util::Log;


require Exporter;

our @ISA = qw(Exporter);

our @EXPORT = qw(
                 match_node 
		 match_node_any
		 match_node_op
		 match_attributes match_attribute match_attribute_op
		 match_structure 
		 match_text
		 OP_NOT OP_AND OP_OR
               );

# logical operators used in graph patterns
use constant { 
    OP_NOT => "!",
    OP_AND => "and",
    OP_OR => "or",
  };

# match node with attributes, children, etc.
# returns 1 iff node matches both the attributes and the structure
#       
# examples:
# &match_node($n,
#             { AX => { "a1" => STRING,		# done
#                       "a2" => REGEX,		# done
#                       "a3" => undef,		# done
#                       "a4" => [OP, EXPR,...], # done
#                      },	
#               SX => { "xp1" => STRING,	# done
#                       "xp2" => { ... },	# done
#                       "xp3" => {},		# done
#                       "xp3" => [OP, EXPR...],	# done
#                      } })
sub match_node {
  my ($node, $to_match) = @_;

  unless (ref($to_match) eq 'HASH') {
      FATAL "a node must be matched against a hash! got: %s", $to_match;
  }
  return 1 unless $to_match; # empty hashes match any node
  DEBUG(3, "match_node(%s, %s)", $node, Dumper($to_match));
  return
    &match_attributes($node, $to_match->{AX})
    && 
    &match_structure($node, $to_match->{SX});
}

# match a set of attribute-value pairs
# returns: 1 iff all attributes exist and have the given value
sub match_attributes {
  my ($node, $to_match) = @_;
  DEBUG(3, "%s: (%s, %s)", "match_attributes", $node, Dumper($to_match));
  return 1 unless (ref($to_match) eq 'HASH');
  foreach my $attr (keys %$to_match) {
    my $result = &match_attribute($node, $attr, $to_match->{$attr});
    return 0 unless $result;
  }
  DEBUG(3, "%s: => %s", "match_attributes", 1);
  return 1;
}

# match the value of an attribute of a node
# returns: 1 iff attribute exists and matches the given value
sub match_attribute {
    my ($node, $attr, $value) = @_;
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Indent = 0;
    DEBUG(3, "%s: (%s, %s, %s)",
	  "match_attribute", $node, $attr, Dumper($value));
    if (! $node->hasAttribute($attr)) {
	return !(defined $value);
    }
    my $attrValue = $node->getAttribute($attr);
    my $result;
    if (! defined $value) {
	return 0;
    } elsif (ref($value) eq "Regexp") {
	$result = ($attrValue =~ $value);
    } elsif (ref($value) eq 'ARRAY') {
	return &match_attribute_op($node, $attr, @$value);
    } elsif (ref(\$value) eq "SCALAR") { # string eq is used for numbers, too
	$result = ($attrValue eq $value);
    } else {
	local $Data::Dumper::Terse = 1;
	local $Data::Dumper::Indent = 0;
	FATAL("Unexpected expression in match_attribute: %s (ref: %s, value: %s)",
	      $value, ref($value), Dumper($value));
    }
    DEBUG(3, "%s(%s,%s) => %s",
	  "match_attribute", $attrValue, Dumper($value), $result);
    return $result;
}

# structural match
# param: $node -- a node
# param: $struct -- a structure expression to match
# result: 1 iff there is a structural match
# n.b.: a node will always match an empty structure!
sub match_structure {
    my ($node, $struct) = @_;
  
    return 1 unless (ref($struct) eq 'HASH');
    foreach my $path (keys %$struct) {
	my $value = $struct->{$path};
	if (ref($value) eq 'HASH') {
	    return 0
	      if none {&match_node($_, $value)} $node->findnodes($path);
	} elsif (ref($value) eq 'ARRAY') {
	    return 0
	      unless &match_node_op($node, $path, @$value);
	} else {
	    return 0
	      unless &match_text($node, $path, $value);
	}
    }
    return 1;
}


# match a node that has a descendant matching a logical expression
# param: $node -- a node
# param: $path -- a path, relative to $node, identifying descendant(s)
# param: $op -- operator, from a closed set (OP_NOT, OP_AND, OP_OR)
# param: @values -- expression(s) to match;
#        N.B.: only one value for OP_NOT, any number for the other operators
#        operators don't work on structures yet!
# result: 1 iff OP(map{&match_node($node/$path, $_)} @values)
sub match_node_op {
  my ($node, $path, $op, @values) = @_;

  if ($op eq OP_NOT) {
    my $value = $values[0];
    return ! &match_node($node, { SX => { $path => $value } }); 
  } elsif ($op eq OP_AND) {
    foreach my $value (@values) {
      # short-circuit
      return 0 unless &match_node($node, { SX => { $path => $value } });
    }
    return 1;
  } elsif ($op eq OP_OR) {
    foreach my $value (@values) {
      # short-circuit
      return 1 if &match_node($node, { SX => { $path => $value } });
    }
    return 0;
  } else {
    FATAL("Unrecognized operator: $op");
  }
}

# match attribute value using logical expression
# param: $node -- a node
# param: $attr -- an attribute name
# param: $op -- operator, from a closed set (OP_NOT, OP_AND, OP_OR)
# param: @values -- expression(s) to match;
#        N.B.: only one value for OP_NOT, any number for the other operators
# result: 1 iff OP(map{&match_attribute($node, $attr, $_)} @values)
sub match_attribute_op {
  my ($node, $attr, $op, @values) = @_;

  my $result;
  if ($op eq OP_NOT) {
    DEBUG(3, "%s(%s, %s, %s, %s)", 'match_attribute_op', $node, $attr, $op, Dumper(\@values));
    $result = none { &match_attribute($node, $attr, $_) } @values;
    DEBUG(3, "%s(%s, %s, %s, %s): %s", 'match_attribute_op', $node, $attr, $op, Dumper(\@values), $result);
  } elsif ($op eq OP_AND) {
    $result = all { &match_attribute($node, $attr, $_) } @values;
  } elsif ($op eq OP_OR) {
    $result = any { &match_attribute($node, $attr, $_) } @values;
  } else {
    FATAL("Unrecognized operator: $op");
  }
  return $result;
}

# match value of text descendant
# returns: 1 iff text descendant exists and has the given value
sub match_text {
  my ($node, $path, $value) = @_;
  my @matchingNodes = $node->findnodes($path);
  DEBUG(3, "%s: %s[%s=%s]", "match_text", $node, $path, $value);
  return 0 unless scalar(@matchingNodes) > 0;
  foreach my $descendant (@matchingNodes) {
    my $textContent = $descendant->textContent;
    if ($textContent eq $value) {
      return 1;
    }
  }
  return 0; 
}

1;
