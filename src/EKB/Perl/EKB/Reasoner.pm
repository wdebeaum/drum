# Reasoner.pm
#
# Time-stamp: <Tue Apr 18 17:29:19 CDT 2017 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>, 22 May 2016
#

#----------------------------------------------------------------
# Description:
# Inference engine for EKBs.

#----------------------------------------------------------------
# History
# 2016/05/22 v0.0	lgalescu
# - Started.
# 2016/05/31 v1.0	lgalescu
# - First version working.
# 2016/06/04 v1.1	lgalescu
# - Some optimizations.
# 2016/06/10 v1.2	lgalescu
# - Fixed dependency handling.

#----------------------------------------------------------------
# TODO
# - optimization: should pre-compute element-rule matches and keep track
#   of what has happened
# - optimization + logic: should get control over EKB changes, to be able
#   to apply rules to those elements as well

package EKB::Reasoner;

$VERSION = '1.2';

use strict 'vars';
use warnings;

use Data::Dumper;
use XML::LibXML;
use List::Util qw(any first min max);

use EKB;
use EKB::Match;
use EKB::Reasoner::Rule;
use util::Log;

sub new {
  my $class = shift;
  my %options = @_;
  my $self = {
	      ekb => shift,
	      rules => [],
	      # options
	      options => {},
	      # log for which rules applies to what elements
	      log => {},
	     };
  bless $self, $class;
  $self->_init(%options);
  return $self;
}

## Getters/Setters

# get/set options in bulk
sub options {
  my $self = shift;
  if (@_) {
    my %opts = @_;
    while (my ($opt, $val) = each(%opts)) {
      $self->option($opt, $val);
    }
  }
  my @opts = %{ $self->{options} }
    if $self->{options};
  return @opts;
}

# get/set individual option
sub option {
  my $self = shift;
  my ($option, $value) = @_;
  if (defined $value) {
    $self->{options}{$option} = $value;
  }
  return $self->{options}{$option};
}

sub ekb {
  my $self = shift;
  return $self->{ekb};
}

sub rules {
  my $self = shift;
  return $self->{rules};
}

sub rule_names {
  my $self = shift;
  return map { $_->name() } @{ $self->{rules} };
}

## Initialization
sub _init {
  my $self = shift;
  if (@_) {
    $self->options(@_);
  }
  return;
}

sub run {
  my $self = shift;
  foreach my $rule (@{ $self->rules() }) {
    $self->apply_rule($rule);
  }
  $self->info_rule_counts();
}

# get rule by name
sub get_rule {
  my $self = shift;
  my $r_name = shift;
  return first { $_->name() eq $r_name } @{ $self->rules() }
}

# apply rule
sub apply_rule {
  my $self = shift;
  my ($rule, @ids) = @_;
  my $r_name = $rule->name;
  my $ekb = $self->ekb;

  DEBUG 2, "Trying to apply rule \"%s\" to (%s)", $r_name, join(" ", @ids);

 REPEAT:
  # select assertions
  my @assertions =
    @ids
    ? map { $ekb->get_assertion($_) } @ids
    : $ekb->get_assertions();
  DEBUG 2, "assertions for given ids: %s", join("\n", @assertions)
    if @ids;
  my @selected = $rule->select(@assertions);
  my @selectIds = map { $_->getAttribute('id') } @selected;

  DEBUG 2, "... %d assertions [%s]", scalar(@selectIds), "@selectIds";
    
  my $count1 = 0;
  # apply rule
  foreach my $elem (@selected) {
    my $id = $elem->getAttribute('id');
    next if $self->has_rule_applied($r_name, $id);
    my $result = $rule->apply($ekb, $elem);
    if ($result) {
      $self->log_rule_application($rule->name(), $id);
      $count1 += $result;
    }

  }
  DEBUG 2, "Rule \"%s\" applied %d times.", $r_name, $count1;
	
  goto REPEAT
    if ($rule->{repeat} && ($count1 > 0));

  return $count1;
}

# log rule application
# rule: a rule name
# ekb: big ekb structure
# id: id of an ekb item
sub log_rule_application {
  my $self = shift;
  my ($r_name, $id) = @_;
  $self->{log}{$r_name}{$id} += 1;
  INFO "Applied rule %s to %s", $r_name, $id;
}

# check if rule applied already
# rule: a rule name
# ekb: big ekb structure
# id: id of an ekb item
sub has_rule_applied {
  my $self = shift;
  my ($r_name, $id) = @_;
  return exists $self->{log}{$r_name}{$id};
}

# rule dependency
# $id: id of an ekb item
# @r_names: a list of rule names
sub dependsOn {
  my $self = shift;
  my ($id, @r_names) = @_;
  my $result  = 1;
  unless (@r_names) {
    @r_names = $self->rule_names();
  }
  foreach my $r_name (@r_names) {
    INFO "Checking dependency: %s(%s)", $r_name, $id;
    next if $self->has_rule_applied($r_name, $id);
    my $rule = $self->get_rule($r_name);
    unless ($rule) {
      ERROR "Rule not found: $r_name";
      $result = 0;
    }
    $self->apply_rule($rule, $id);
  }
  return $result;
}

# get counts for how many times each rule was applied
# ekb: big ekb structure
sub info_rule_counts {
  my $self = shift;

  DEBUG(3, "%s", Dumper($self->{log}));
  
  for my $rule (@{ $self->rules() }) {
    my $r_name = $rule->name();
    my $count = 0;
    map { $count += $self->{log}{$r_name}{$_} }
      (keys %{ $self->{log}{$r_name} });
    INFO("Rule %s applied %d times.", $r_name, $count)
      if $count;
  }
}


## Rules


1;
