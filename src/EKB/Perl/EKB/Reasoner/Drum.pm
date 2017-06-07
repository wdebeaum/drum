# Drum.pm
#
# Time-stamp: <Mon May 29 14:10:39 CDT 2017 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>,  1 Jun 2016
# $Id: Drum.pm,v 1.12 2017/05/29 19:19:08 lgalescu Exp $
#

#----------------------------------------------------------------
# Description:
# Reasoner for making inferences from text-extracted EKBs.

#----------------------------------------------------------------
# History:
# 2016/06/01 v0.0	lgalescu
# - Started. Importing things from previous scripts.
# 2016/06/04 v1.0	lgalescu
# - Working version.
# 2016/06/05 v1.1	lgalescu
# - More rules.
# 2016/06/06 v1.2	lgalescu
# - Lots of cleaning up.
# 2016/06/07 v1.3	lgalescu
# - Added one more rule.
# 2016/06/08 v1.4	lgalescu
# - More cleanup rules. Some structural changes. Added PTM rule.
# 2016/06/09 v1.4.1	lgalescu
# - Various fixes.
# 2016/06/10 v1.5	lgalescu
# - Several new rules. Fixed dependencies.
# 2016/06/11 v1.6	lgalescu
# - Several new rules. 
# 2016/06/13 v1.6.1	lgalescu
# - Reworked several rules to use new EKB interface. Much cleaner now.
# 2016/07/31 v1.7	lgalescu
# - Added rules for BIND with aggregate agent (EKR:BindXandY1, EKR:BindXandY2).
# 2017/02/03 v1.7.1	lgalescu
# - adapted to work as part of EKB TRIPS component
# 2017/03/04 v1.7.2	lgalescu
# - Updated following refactoring in EKB.pm.
# 2017/03/11 v1.7.3	lgalescu
# - Updated following refactoring in EKB.pm.
# 2017/03/19 v1.7.4	lgalescu
# - Updated following refactoring in EKB.pm.
# 2017/03/19 v1.7.5	lgalescu
# - Fixed version required for List::Util.
# 2017/04/18 v1.8.0	lgalescu
# - Added option for adding (or not) binding features.
# - Made all options false by default (the stuff they add is easily inferrable
#   and it makes graphs difficult to read)
# 2017/04/18 v1.9.0	lgalescu
# - Added a couple of rules for "complex forming" and one for removing useless
#   PRODUCE events.
# 2017/04/21 v1.9.1	lgalescu
# - Minor fixes.
# 2017/04/21 v1.10.0	lgalescu
# - Updated to current style of the (still evolving) EKB interface.
# - Bug fix.
# - Now checking inevent references in pseudoargs, as well.
# 2017/04/?? v1.11.0	lgalescu
# - Consolidated addition of binding features into new rule: AddBindingFeatures.
# - Synced w/ EKB.pm (much, much cleaner code now).
# - Many small style and efficiency improvements.
# 2017/05/26 v1.12.0	lgalescu
# - updated to conform to new representation for 'inevent' features.
# - fixed one bug (highly improbable the situation ever occurred).
# - added new rules (EKR::dePTM, EKR:AddComplexName)

#----------------------------------------------------------------
# Usage:
# 

package EKB::Reasoner::Drum;

$VERSION = '1.12.0';

use strict 'vars';
use warnings;

use Data::Dumper;
use XML::LibXML;
use List::Util 1.45 qw(all any first min max uniq);

use EKB;
use EKB::Match;
use Ont::BioEntities;
use Ont::BioEvents;
use EKB::Reasoner::Rule;
use EKB::Reasoner;
use util::Log;

our @ISA = qw(EKB::Reasoner);

my @rules;

my $ont_events = Ont::BioEvents->new();
my $ont_bioents = Ont::BioEntities->new();

sub new {
  my $class = shift;
  my $ekb = shift;
  my %options = @_;
  my $self = {
	      ekb => $ekb,
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

sub _init {
  my $self = shift;
  $self->default_options();
  $self->SUPER::_init(@_); # pass options up

  push @{ $self->{rules} },
    map { EKB::Reasoner::Rule->new($self, $_) } @rules;
}

sub default_options {
  my $self = shift;
  $self->options(
		 add_amount_changes => 0,
		 add_binding_features => 0,
		 add_binding_features_rule => 0,
		);
}

## Rules

# note: this is an ordered list!
@rules =
  (
   {
    ## remove dangling references on relation arguments
    # < X:EVENT()|CC()|EPI()|MODALITY()
    # < X/arg[@id=id]
    # < ! *[@id=id]
    # > - X/arg/@id
    name => "EKR:RemoveDanglingReferences",
    constraints => [],
    handler => sub  {
      my ($rule, $ekb, $r) = @_;

      return 0 unless is_relation($r);

      my $r_id = $r->getAttribute('id');

      my @args = assertion_args($r);
      my $count = 0;
      foreach my $arg (@args) {
	my $a_id = $arg->getAttribute('id');
	next if $ekb->get_assertion($a_id);
	INFO "Rule %s matches %s (arg: %s)",
	  $rule->name, $r_id, $a_id;
	$arg->removeAttribute('id');
	$count++;
      }

      $count;
    }
   },
   
   {
    ## delete CC relations that don't have two arguments and lack referrers
    # < C:CC(!arg[2])
    # < ! *(*:C)
    # > -C
    # WARNING: removes assertions
    name => "EKR:RemoveIncompleteCC",
    constraints => ['CC[not(arg[2])]'],
    handler => sub {
      my ($rule, $ekb, $c) = @_;

      my $c_id = $c->getAttribute('id');

      # < C:CC(!arg[2])
      my @args = assertion_args($c);
      return 0 if (scalar(@args) >= 2);

      # < ! *(*:C)
      my @referrers =
	grep { $_->getAttribute('id') ne $c_id }
	$ekb->find_referrers($c_id);
      return 0 unless (scalar(@referrers) == 0);

      INFO("Rule %s matches %s",
	   $rule->name(), $c_id);
	
      # > -C
      $c->parentNode->removeChild($c);

      1;
    }
   },

   {
    ## delete empty relations (no arguments of any kind) with no referrers
    # < X:reln()
    # < ! *(*:X)
    # > - X
    # WARNING: removes assertions
    name => "EKR:RemoveBareReln",
    constraints => [],
    repeat => 1,
    handler => sub  {
      my ($rule, $ekb, $r) = @_;

      return 0 unless is_relation($r);

      my $r_id = $r->getAttribute('id');

      # < X:reln()
      my @args = ( assertion_args($r), assertion_xargs($r) );
      return 0 unless (scalar(@args) == 0);
	
      # < ! *(*:X)
      my @referrers =
	grep { $_->getAttribute('id') ne $r_id }
	$ekb->find_referrers($r_id);
      return 0 unless (scalar(@referrers) == 0);

      # > - X
      INFO("Rule %s matches %s",
	   $rule->name, $r_id);
	
      $ekb->remove_assertion($r);

      1;
    }
   },
   
   {
    ## delete ONT::PRODUCE events that duplicate :RES arguments of other events
    # < E: ont::produce(:affected-result Y)
    # < E1: *(:res|:result|:affected-result Y)
    # < ! *(:* E)
    # > - E
    name => "EKR:RemoveProduce",
    constraints => ['EVENT[type[.="ONT::PRODUCE"] and not(arg1) and arg2[@role=":AFFECTED-RESULT"]]'],
    handler => sub {
      my ($rule, $ekb, $e) = @_;
      my $e_id = $e->getAttribute('id');

      # < E :affected-result Y
      my $y_id = $e->findvalue('arg2/@id');

      # < E1 * :res|:result:affected-result Y
      my @referrers =
	grep { assertion_args($_, '[@role=":RES" or @role=":RESULT" or @role=":AFFECTED-RESULT"]') }
	grep { $_->getAttribute('id') ne $e_id }
	$ekb->find_referrers($y_id, 'EVENT');
      return 0 if (scalar(@referrers) == 0);

      # < ! * :<any> E
      $ekb->find_referrers($e_id)
	&& return 0;
      
      INFO "Rule %s matches %s (ref in: %s)",
	$rule->name(), $e_id, join(",", map {$_->getAttribute('id')} @referrers );
	
      # > - E
      $ekb->remove_assertion($e);

      1;
    }
   },
   
   {
    ## fix ANTIBODY: CELL-PART => PROTEIN
    # TERM[@id=$t_id and type=ONT::CELL-PART and name=ANTIBODY]
    # =>
    # mod: type($t_id)=ONT::PROTEIN
    name => "EKR:FixAntibody",
    constraints => ['TERM[type[.="ONT::CELL-PART"] and name[.="ANTIBODY"]]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;
	
      my $t_id = $t->getAttribute('id');
	
      INFO("Rule %s matches term %s",
	   $rule->name(), $t_id);
    
      set_slot_value($t, 'type', "ONT::PROTEIN");
	
      1;
    }
   },

   {
    ## name unnamed complexes using component names
    name => "EKR:AddComplexName",
    constraints => ['TERM[type[.="ONT::MACROMOLECULAR-COMPLEX"] and components and not(name)]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;
      my @comps =
	map { $ekb->get_assertion($_, "TERM") }
	map { $_->value } $t->findnodes('components/component/@id');;
      $ekb->modify_assertion($t,
			     make_slot_node( name => make_complex_name(@comps) ));
      1;
    }
   },

   {
    ## Remove dangling 'inevent' features (event doesn't exist or it doesn't
    ## have an arg pointing to the term)
    # < TERM[@id=$t_id and //inevent[@id=$e]]
    #   ! *[@id=$e and arg*[@id=$t_id]]
    # > - TERM//inevent[@id=$e]
    name => "EKR:FixDanglingInevent",
    constraints => ['TERM[features/inevent]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;
	
      my $t_id = $t->getAttribute('id');
      
      my @e_ids = map { $_->value } $t->findnodes('features/inevent/@id');
      my $count = 0;
      foreach my $e_id (@e_ids) {
	my $e = $ekb->get_assertion($e_id);
	my @args = ( assertion_args($e), assertion_xargs($e) );
	next if any { $_->getAttribute('id') eq $t_id } @args;

	INFO "Rule %s matches term %s (inevent: %s)",
	  $rule->name(), $t_id, $e_id;

	remove_elements($t, 'features/inevent[@id="'.$e_id.'"]');
	    
	$count++;
      }
      $count;
    }
   },

   {
    ## when <mod><type>ONT::NEG</type><value>NOT</value></mod>
    ## flip <negation>
    # < E[mods/mod[type=ONT::NEG and value=NOT]]
    # > - E/mods/mod[type=ONT::NEG and value=NOT]
    # > ~ E[negation=flip()]
    name => 'EKR:ModNegNot',
    constraints => ['*/mods/mod[type[.="ONT::NEG"] and value[.="NOT"]]'],
    handler => sub {
      my ($rule, $ekb, $r) = @_;

      return 0 unless is_relation($r);

      my $r_id = $r->getAttribute('id');

      INFO("Rule %s matches %s",
	   $rule->name, $r_id);

      # > ~ E[negation=flip()]
      my $neg = get_slot_value($r, 'negation');
      if (defined $neg) {
	if ($neg eq '+') {
	  $r->set_slot_value('negation', '-');
	} else {
	  $r->set_slot_value('negation', '+');
	}
      } else {
	$r->addChild(make_slot_node( negation => '+' ));
      }

      # > - E:/mods/mod[type=ONT::NEG and value=NOT]
      remove_elements($r, 'self::*/mods/mod[type[.="ONT::NEG"] and value[.="NOT"]]');

      1;
    }
   },
   
   {
    ## sequence term disambiguation; only for GENE-PROTEIN
    # TERM[type=ONT::GENE-PROTEIN
    #  and components[component[type=ONT::PROTEIN]
    #             and component[type<<ONT::GENE-PROTEIN] ... ]
    # =>
    # mod: TERM/type=ONT::PROTEIN
    # mod: TERM//component/type=ONT::PROTEIN
    # WARNING: adds EKB cross-refs
    name => "EKR:FixProteinTypeSeq",
    constraints => ['TERM[type[.="ONT::GENE-PROTEIN"] and components]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;
	
      # get term id
      my $t_id = $t->getAttribute('id');
      my $t_type = get_slot_value($t, 'type');

      my @comp_ids = map { $_->value } $t->findnodes('components/component/@id');
      DEBUG 3, "comp_ids: %s", "@comp_ids";
      my @comp_terms = map { $ekb->get_assertion($_, "TERM") } @comp_ids;
      DEBUG 4, "comp_terms: %s", "@comp_terms";

      all { defined($_) } @comp_terms
	or do { DEBUG 1, "At least one component of $t_id is not defined!";
		return 0 };
    
      my @comp_types = uniq map { get_slot_value($_, 'type') } @comp_terms;
      DEBUG 3, "comp_types: %s", "@comp_types";
    
      # we must have at least one PROTEIN
      scalar(grep { $_ eq "ONT::PROTEIN" } @comp_types) > 0
	or return 0;
      # we must have at least one non-PROTEIN
      scalar(@comp_types) > 1
	or return 0;

      INFO("Rule %s matches term %s [type=%s, components: %s]",
	   $rule->name, $t_id, $t_type, "@comp_ids");
    
      foreach my $comp_term (@comp_terms) {
	my $comp_type = get_slot_value($comp_term, 'type');
	next if ($comp_type eq "ONT::PROTEIN");
	set_slot_value($comp_term, 'type', "ONT::PROTEIN");

	INFO "changed type of %s: %s => %s",
	  $comp_term->getAttribute('id'), $comp_type, "ONT::PROTEIN";
      }
      set_slot_value($t, 'type', "ONT::PROTEIN");

      1;
    }
   },

   {
    ## sequence term disambiguation; only for PROTEIN
    # a sequence whose components have a common DBID are synonyms
    # robust: at least two proteins have the same DBID
    # TERM[components[component[@id=$id1]
    #             and component[@id=$id2 and @dbid=$dbid1] ... ]
    # TERM[@id=$id1 and @dbid=$dbid1]
    # TERM[@id=$id2 and @dbid=$dbid1] ...
    # =>
    # mod: components[@type="ONT::EQUAL"]
    # mod: add @equals=$id1 to all TERMs 2..n
    # WARNING: adds EKB cross-refs
    name => "EKR:SequenceTermEquiv",
    constraints => ['TERM[type[.="ONT::PROTEIN"] and components]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;
	
      # get term id &c
      my $t_id = $t->getAttribute('id');
      my $t_type = get_slot_value($t, 'type');
      my $t_text = get_slot_value($t, 'text');

      my ($comp_arg) = $t->findnodes('components');
      my @comp_ids = map { $_->value } $comp_arg->findnodes('component/@id');
      my @comp_terms = map { $ekb->get_assertion($_, "TERM") } @comp_ids;
      my @comp_dbids = map { $_->getAttribute('dbid') } @comp_terms;

      WARN "sequence term: <%s, %s, %s>: comp_ids: (%s) comp_dbids: (%s)",
	$t_id, $t_type, $t_text, "@comp_ids", "@comp_dbids";

      my $rep_id = check_protein_equivs(@comp_terms);
      return 0 unless $rep_id;

      INFO("Rule %s matches term %s (representative: %s)",
	   $rule->name(), $t_id, $rep_id);

      # add $rep_id dbid to this
      my $rep_term = $ekb->get_assertion($rep_id, "TERM");
      my $rep_dbid = $rep_term->getAttribute('dbid');

      if ($t->getAttribute('dbid')) {
	WARN "Cannot add dbid to sequence term; it already has one: %s",
	  $t->getAttribute('dbid');
      } else {
	$t->setAttribute('dbid', $rep_dbid);
      }
    
      # make all other members equivalent to $rep_id
      foreach my $ot (@comp_terms) {
	next if $ot->getAttribute('id') eq $rep_id;
	my $eq_feat =
	  make_node("equals", { id => $rep_id,
				provenance => $rule->name() } );
	$ot->addChild($eq_feat);
      }

      # add info to $comp_arg
      $comp_arg->setAttribute('type', "ONT::EQUAL");
    
      # update rule attribute
      append_to_attribute($t, 'rule', $rule->name());

      1;
    }
   },

   {
    ## sequence term disambiguation
    # limitations: only for GENE-PROTEIN
    #              for most events, except ONT::BREAK
    # found in text: PTM, MODULATE
    # TODO: not finished
    # TERM[components/component[not(@type="ONT::EQUAL")]]
    # =>
    # mod: TERM[aggregate[@operator="UNK"]/member] # highly likely AND
    #
    name => "EKR:SequenceTerm",
    constraints => ['TERM[components]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;
      my $t_id = $t->getAttribute('id');
	
      $rule->reasoner()->dependsOn($t_id, "EKR:SequenceTermEquiv");

      # T:[type=ONT::GENE-PROTEIN]
      $ont_bioents->is_a(get_slot_value($t, 'type'), "ONT::GENE-PROTEIN")
	or return 0;

      # must have some component, no assigned type (eg, ONT::EQUAL)
      match_node($t, { SX => { 'components' 
			       => { AX => { 'type' => undef } }
			     } })
	or return 0;

      my @referrers = $ekb->find_referrers($t_id, "EVENT");
      {
	my @ref_info =
	  map { $_->getAttribute('id') => get_slot_value($_, 'type') }
	  @referrers;
	DEBUG 3,  "%s : referrers: %s", $rule, "@ref_info";
      }

      # check that any referrers are of the desired types
      # FIXME: we probably should check that $t_id is an arg, but, whatever
      my @ref_types = map { get_slot_value($_, 'type') } @referrers;
      my $ref_test =
	any {
	  my $t=$_;
	  any { $ont_events->is_a($_, $t) } @ref_types
	}
	("ONT::PTM", "ONT::MODULATE", "ONT::EXPRESS", "ONT::TRANSCRIBE",
	 "ONT::TRANSLATE", "ONT::TRANSLOCATE");
      return 0 unless $ref_test;
    
      my ($comp_feat) = $t->findnodes('components');
      my @comps = $t->findnodes('components/component');
      my @comp_ids = map { $_->getAttribute('id') } @comps;

      INFO "Rule %s matches term %s (comps: %s)",
	$rule->name(), $t_id, "@comp_ids";

      $comp_feat->setNodeName('aggregate');
      $comp_feat->setAttribute('operator', "AND");
      map { $_->setNodeName('member') } @comps;

      1;
    }
   },
   
   {
    ## obtain localization event from term w/ location info
    # < TERM[@id=$t_id and features/location/@id=$l_id]
    # > EVENT[type=ONT::LOCALIZATION
    #         pred=ONT::BE-AT-LOC
    #         arg[@id=$t_id]
    #         location[@id=$l_id]]
    # WARNING: adds assertions
    name => "EKR:LocateTerm",
    constraints => ['TERM[features/location]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;
	
      my $t_id = $t->getAttribute('id');

      # get location(s)
      my @loc_ids = map {$_->value} $t->findnodes('features/location/@id');

      my @loc_terms =
	grep { defined $_ } map { $ekb->get_assertion($_, "TERM") } @loc_ids;
		     
      my $start = min ( $t->getAttribute('start'),
			map { $_->getAttribute('start') }
			@loc_terms );
      my $end = max ( $t->getAttribute('end'),
		      map { $_->getAttribute('end') }
		      @loc_terms );

      # TODO: perhaps, instead of making multiple events, i should make
      # an aggregate location?? but then why shouldn't the original term have
      # a single location feature pointing to the aggregate??
      foreach my $loc_term (@loc_terms) {
	my $l_id = $loc_term->getAttribute('id');
	$ekb->add_assertion_r( 'EVENT',
			       refs => [$t_id],
			       start => $start,
			       end => $end,
			       rule => $rule->name,
			       slots => [ make_slot_node(type => 'ONT::LOCALIZATION'),
					  make_predicate('ONT::BE-AT-LOC'),
					  make_arg(':NEUTRAL' => $t_id),
					  make_node("location", { id => $l_id }) ] );
      }
		     
      1;
    }
   },

   {
    ## distribute :EPI info in aggregate event assertions to member events
    # EVENT[type=S-CONJOINED and 
    #       aggregate/member[@id=$eid] and 
    #       epistemic-modality[@id=$mid]]
    # EVENT[@id=$id]
    # => mod: EVENT[@id=$id and epistemic-modality[@id=$mid]]
    # WARNING: adds EKB cross-refs
    name => "EKR:EpiConjoinedEvent",
    constraints => ['EVENT[aggregate and epistemic-modality]'],
    handler => sub {
      my ($rule, $ekb, $e) = @_;

      my $e_id = $e->getAttribute('id');
	
      # match aggregate
      match_node($e, { SX => { 'type' => [OP_OR,
					  "ONT::S-CONJOINED",
					  "ONT::VP-CONJOINED"],
			       'aggregate' => {},
			       'epistemic-modality' => {} } })
	or return 0;

      # get :epi node(s)
      my @epis = $e->findnodes('epistemic-modality');
    
      # get aggregate member ids
      my @m_ids = map { $_->value } $e->findnodes('aggregate/member/@id');
      return 0 unless @m_ids;

      INFO("Rule %s matches event %s w/ members (%s)", 
	   $rule->name(), $e_id, "@m_ids");

      foreach my $m_id (@m_ids) {
	my $m_e = $ekb->get_assertion($m_id, "EVENT");
	DEBUG(1, "%s", $m_e);
	next unless $m_e;
	my ($pred) = $m_e->findnodes('predicate');
	foreach my $epi (@epis) {
	  my $c_epi = $epi->cloneNode(1);
	  $m_e->insertBefore($c_epi, $pred);
	  DEBUG(1, "%s", $m_e);
	}
      }

      1;
    }
   },

   {
    ## Y cells express X
    # < E:express(agent:Y, affected-result:X)
    # < X:gene-protein
    # < Y:cell
    # > E/location/IN-LOC:Y
    # - E/agent
    name => "EKR:ExpressByCell",
    constraints => ['EVENT[type[.="ONT::EXPRESS"] and arg1[@role=":AGENT"] and arg2[@role=":AFFECTED-RESULT"]]'],
    handler => sub {
      my ($rule, $ekb, $e) = @_;
      my $e_id = $e->getAttribute('id');

      # < Y:cell
      my ($y_arg) = $e->findnodes('arg1');
      my $y_id = $y_arg->getAttribute('id');
      my $y_term = $ekb->get_assertion($y_id, "TERM")
	or return 0;
      match_node($y_term, { SX => { 'type' => "ONT::CELL" } })
	or return 0;
	
      # < X:gene-protein
      my $x_id = $e->findvalue('arg2/@id');
      my $x_term = $ekb->get_assertion($x_id, "TERM")
	or return 0;
      my $x_type = get_slot_value($x_term, 'type');
      $ont_bioents->is_a($x_type, "ONT::GENE-PROTEIN")
	or return 0;

      INFO "Rule %s matches event %s (arg1: %s, arg2: %s)",
	$rule->name(), $e_id, $y_id, $x_id;

      # > E/location/IN-LOC:Y
      my $loc = make_node("location",
			  { id => $y_id,
			    mod => "IN-LOC"});
      $e->addChild($loc);

      # - E/agent
      $e->removeChild($y_arg);

      1;
    }
   },

   {
    ## Y cells express -
    # < E:express(affected:Y)
    # < ! E/arg1
    # < Y:cell
    # > E/location/IN-LOC:Y
    # WARNING: adds EKB cross-refs
    name => "EKR:ExpressByCellNoResult",
    constraints => ['EVENT[type[.="ONT::EXPRESS"] and not(arg1) and arg2[@role=":AFFECTED"]]'],
    handler => sub {
      my ($rule, $ekb, $e) = @_;
      my $e_id = $e->getAttribute('id');

      # < Y:cell
      my ($y_arg) = $e->findnodes('arg2');
      my $y_id = $y_arg->getAttribute('id');
      my $y_term = $ekb->get_assertion($y_id, "TERM")
	or return 0;
      match_node($y_term, { SX => { 'type' => "ONT::CELL" } })
	or return 0;
	
      INFO "Rule %s matches event %s (arg2: %s)",
	$rule->name(), $e_id, $y_id;

      # > E/location/IN-LOC:Y
      my $loc = make_node("location",
			  { id => $y_id,
			    mod => "IN-LOC"});
      $e->addChild($loc);

      # - E/agent
      $e->removeChild($y_arg);

      1;
    }
   },

   {
    ## delete terms not referenced elsewhere
    # < T:TERM(id=$t_id)
    # < ! *[...id=$t_id...]
    # > - T
    # WARNING: removes assertions
    name => 'EKR:RemoveOrphanTerm',
    constraints => ['TERM'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;

      return 0 unless ($rule->reasoner()->option('remove_orphans'));

      my $t_id = $t->getAttribute('id');

      # < ! *(...id=X...)
      my @referrers = $ekb->find_referrers($t_id);
      return 0 unless (scalar(@referrers) == 0);
  
      INFO("Rule %s matches term %s",
	   $rule->name(), $t_id);
      
      $ekb->remove_assertion($t);

      1;
    }
   },

   {
    ## eg, unphosphorylated X [<does-something>]
    ## => X[not-ptm:PHOSPHORYLATION]
    # < X[//inevent/@id=E]
    # < X[type(is_a)ONT::MOLECULAR-PART]
    # < E[affected:X]
    # < E[type(is_a)ONT::PTM]
    # < ! E[negation]
    # < E[mods/mod/type=ONT::MANNER-UNDO]
    # > ~ X[type=s/ONT::GENE/ONT::PROTEIN/]
    # > + X/not-features/ptm[@id=E]
    # > - X//inevent[@id=E]
    name => 'EKR:InEventUnPTM',
    constraints => ['TERM[features/inevent]'],
    handler => sub {
      my ($rule, $ekb, $x) = @_;
      my $x_id = $x->getAttribute('id');

      # < X[type(is_a)ONT::MOLECULAR-PART]
      my $x_type = get_slot_value($x, 'type');
      $ont_bioents->is_a($x_type, 'ONT::MOLECULAR-PART')
	or return 0;

      # < X[//inevent/@id=E]
      my @inevents =
	grep {
	  # < E[type(is_a)ONT::PTM]
	  $ont_events->is_a(get_slot_value($_, 'type'), 'ONT::PTM')
	  &&
	  # < ! E[negation]
	  # < E[mods/mod/type=ONT::MANNER-UNDO]
	  # < E[affected:X]
	  match_node($_, { SX => { 'negation'
				   => [OP_NOT, '+'],
				   'mods/mod'
				   => { SX => { 'type' => 'ONT::MANNER-UNDO',
						'value' => 'UN-' } },
				   'arg2'
				   => { AX => { 'role' => ':AFFECTED',
						'id' => $x_id } }
				 } })
	}
	map { $ekb->get_assertion($_) }
	map { $_->value } $x->findnodes('features/inevent/@id');
      return 0 unless @inevents;

      my $count = 0;
      foreach my $e (@inevents) {
	my $e_id = $e->getAttribute('id');
	my $e_type = get_slot_value($e, 'type');

	INFO "Rule %s matches term %s[t=%s] (affected-by: %s[t=%s])", 
	  $rule->name, $x_id, $x_type, $e_id, $e_type;

	# > ~ X[type=s/ONT::GENE/ONT::PROTEIN/]
	set_slot_value($x, 'type', 'ONT::PROTEIN')
	  if $ont_bioents->is_a('ONT::GENE', $x_type);

	# > - X//inevent[@id=E]
	remove_elements($x, 'features/inevent[@id="'.$e_id.'"]');

	# > + X/not-features/ptm[@id=E]
	$ekb->add_notfeature($x, 'ptm' => { type => $e_type,
					    event => $e_id });

	$count++;
      }

      $count;
    }
   },

   {
    ## X not bound to Y
    ## => X[not-bound-to:Y]
    # < X:*[inevent[@id=E]]
    # < X/type is_a ONT::MOLECULAR-PART
    # < E:bind(affected:X,affected1:Y,negation:+)
    # < Y/type is_a <bio_entity>
    # < ! E/arg*[result:*]
    # > - X//inevent[@id=E]
    # > + X/not-features/bound-to[Y]
    # > + Y/not-features/bound-to[X]
    # NB: we don't remove E, even when it's got no other referrers, since it
    # may hold other info (eg, binding site, location)
    name => 'EKR:InEventNotBind',
    constraints => ['TERM[features/inevent]'],
    handler => sub {
      my ($rule, $ekb, $x) = @_;
      my $x_id = $x->getAttribute('id');

      # < X/type is_a ONT::MOLECULAR-PART
      my $x_type = get_slot_value($x, 'type');
      $ont_bioents->is_a($x_type, 'ONT::MOLECULAR-PART')
	or return 0;

      # < X:*[inevent]
      my @inevents =
	grep {
	  # < E/type is_a ONT::BIND 
	  $ont_events->is_a(get_slot_value($_, 'type'), 'ONT::BIND')
	    &&
	    # < E:bind(r1:X,r2:Y,negation:+)
	    # < r1=AFFECTED && r2=AFFECTED1
	    match_node($_, { SX => { 'negation' 
				     => '+',
				     'arg1' 
				     => { AX => { 'role' => ':AFFECTED',
						  'id' => $x_id  } },
				     'arg2' 
				     => { AX => { 'role' => ':AFFECTED1'} }
				   } })
	    &&
	    # < ! E/arg*[result:*]
	    ! assertion_args($_, '[@role=":RESULT"]')
	  }
	map { $ekb->get_assertion($_) }
	map { $_->value }
	$x->findnodes('features/inevent/@id');	
      return 0 unless @inevents;

      DEBUG 2, "%s: got %d inevents: (%s)",
	$rule->name, scalar(@inevents),
	join(',', map {$_->getAttribute('id')} @inevents);
	  
      my $count = 0;
      foreach my $e (@inevents) {
	my $e_id = $e->getAttribute('id');
	my $e_type = get_slot_value($e, 'type');

	# < Y/type is_a <bio_entity>
	my $y_id = $e->findvalue('arg2/@id');
	my $y = $ekb->get_assertion($y_id, "TERM")
	  or next;
	my $y_type = get_slot_value($y, 'type');
	$ont_bioents->has($y_type)
	  or next;

	INFO "Rule %s matches term %s[t=%s] in %s[t=%s](*, %s[t=%s])", 
	  $rule->name, $x_id, $x_type, $e_id, $e_type, $y_id, $y_type;

	# > - X//inevent[@id=E]
	remove_elements($x, 'features/inevent[@id="'.$e_id.'"]');

	# > + X/not-features/bound-to[Y]
	$ekb->add_notfeature($x, 'bound-to' => { id => $y_id,
						 event => $e_id });
	# > + Y/not-features/bound-to[X] 
	$ekb->add_notfeature($y, 'bound-to' => { id => $x_id,
						 event => $e_id });
	
	$count++;
      }
      
      $count;
    }
   },
   
   {
    ## phosphorylated X [by Y] [<does-something>]
    ## => non-phosphorylated X is phosphorylated [by Y] to phospho-X
    ##    phospho-X [<does-something>]
    # < X[//inevent/@id=E]
    # < X[type(is_a)ONT::MOLECULAR-PART]
    # < E[affected:X]
    # < E[type(is_a)ONT::PTM]
    # < ! E[mods/mod/type=ONT::MANNER-UNDO]
    # < ! E[negation)
    # < ! E[result]
    # > ~ X[type=s/ONT::GENE/ONT::PROTEIN/]
    # > + X[features/ptm[@id=E]]
    # > + X1=X[not-features/ptm[@id=E]]
    # > ~ E[result:X]
    # > + E[affected:X1]
    # > - X//inevent[@id=E]
    name => 'EKR:InEventPTM',
    constraints => ['TERM[features/inevent]'],
    handler => sub {
      my ($rule, $ekb, $x) = @_;
      my $x_id = $x->getAttribute('id');

      # < X[type(is_a)ONT::MOLECULAR-PART]
      my $x_type = get_slot_value($x, 'type');
      $ont_bioents->is_a($x_type, 'ONT::MOLECULAR-PART')
	or return 0;

      # < X[//inevent/@id=E]
      my @inevents =
	grep {
	  # < E[type(is_a)ONT::PTM]
	  $ont_events->is_a(get_slot_value($_, 'type'), 'ONT::PTM')
	  &&
	  # < ! E[mods/mod/type=ONT::MANNER-UNDO]
	  # < ! E[negation)
	  # < E[affected:X]
	  match_node($_, { SX => { 'negation' 
				   => [OP_NOT, '+'],
				   'mods/mod/type' 
				   => [OP_NOT, 'ONT::MANNER-UNDO'],
				   'arg2' 
				   => { AX => { 'role' => ':AFFECTED',
						'id' => $x_id } }
				 } })
	  &&
	  # < ! E[result]
	  ! assertion_args($_, '[@role=":RESULT"]')
	}
	map { $ekb->get_assertion($_) }
	map { $_->value } $x->findnodes('features/inevent/@id');	
      return 0 unless @inevents;

      my $count = 0;
      foreach my $e (@inevents) {
	my $e_id = $e->getAttribute('id');
	my $e_type = get_slot_value($e, 'type');

	INFO "Rule %s matches term %s[t=%s] (affected-by: %s[t=%s])", 
	  $rule->name, $x_id, $x_type, $e_id, $e_type;

	# > ~ X[type=s/ONT::GENE/ONT::PROTEIN/]
	set_slot_value($x, 'type', 'ONT::PROTEIN')
	  if $ont_bioents->is_a('ONT::GENE', $x_type);

	# > - X//inevent[@id=E]
	remove_elements($x, 'features/inevent[@id="'.$e_id.'"]');

	# > + X1=X
	my $x1 = $ekb->clone_assertion($x, {rule => $rule->name});
	my $x1_id = $x1->getAttribute('id');
	$ekb->add_assertion($x1);

	# > + X[features/ptm[@id=E]]
	$ekb->add_feature($x, 'ptm' => { type => $e_type,
					 event => $e_id });
	# > + X1[not-features/ptm[@id=E]]
	$ekb->add_notfeature($x1, 'ptm' => { type => $e_type,
					     event => $e_id });
	# > ~ E[affected:X1]
	my ($x_arg) = assertion_args($e, '[@role=":AFFECTED"]');
	set_attribute($x_arg, 'id' => $x1_id);
	# > + E[result:X]
	$ekb->add_arg($e, ':RESULT' => $x_id);

	$count++;
      }
	
      $count;
    }
   },

   {
    ## X bound/E to Y */E2; X binding/E to Y */E2
    ## => unbound X binds to unbound Y to form X/Y complex
    # < X:*[inevent[@id=E]]
    # < X/type is_a ONT::MOLECULAR-PART
    # < E:bind(r1:X,r2:Y,!negation)
    # < r1=AGENT && r2=AFFECTED || r1=AFFECTED && r2=AFFECTED1
    # < Y/type is_a <bio_entity>
    # < ! E/arg*[result:*]
    # < ! E/mods/mod/type[ONT::MANNER-UNDO]
    # ? & dependsOn ^(X//inevent/@id[.!=E])
    # ? & dependsOn ^(Y//inevent/@id[.!=E])
    # > - X//inevent[@id=E]
    # > + X1=X
    # > + Y1=Y
    # > + X1/not-features/bound-to[Y]
    # > + Y/not-features/bound-to[X1] 
    # > + X/features/bound-to[Y1]
    # > + Y1/features/bound-to[X]
    # > + Z:MACROMOLECULAR-COMPLEX(components:(X,Y1))
    # > ~ E:bind(r1:Y,r2:X1,result:Z)
    # > + E1:increase(affected:Z)
    # > + C1:cause(factor:E,outcome:E1)
    # > + E2:decrease(affected:X1)
    # > + C2:cause(factor:E,outcome:E2)
    # > + E3:decrease(affected:Y)
    # > + C3:cause(factor:E,outcome:E3)
    # TODO: maybe merge with InEventBind2?
    name => "EKR:InEventBind",
    constraints => ['TERM[features/inevent]'], # TODO: needs to be more specific!
    handler => sub {
      my ($rule, $ekb, $x) = @_;
      my $x_id = $x->getAttribute('id');

      # < X/type is_a ONT::MOLECULAR-PART
      my $x_type = get_slot_value($x, 'type');
      $ont_bioents->is_a($x_type, 'ONT::MOLECULAR-PART')
	or return 0;

      # < X:*[inevent]
      my @inevents =
	grep {
	  defined($_)
	  &&
	  # < E/type is_a ONT::BIND 
	  $ont_events->is_a(get_slot_value($_, 'type'), 'ONT::BIND')
	  &&
	  # < E:bind(r1:X,!negation:+)
	  # < ! E/mods/mod/type[ONT::MANNER-UNDO]
	  # < r1=AGENT && r2=AFFECTED || r1=AFFECTED && r2=AFFECTED1
	  match_node($_, { SX => { 'negation' 
				   => [OP_NOT, '+'],
				   'mods/mod/type' 
				   => [OP_NOT, 'ONT::MANNER-UNDO']
				 } })
	  &&
	  ( match_node($_, { SX => { 'arg1' 
				     => { AX => { 'role' => ':AGENT',
						  'id' => $x_id  } },
				     'arg2' 
				     => { AX => { 'role' => ':AFFECTED' } }
				   } })
	    ||
	    match_node($_, { SX => { 'arg1' 
				     => { AX => { 'role' => ':AFFECTED',
						  'id' => $x_id  } },
				     'arg2' 
				     => { AX => { 'role' => ':AFFECTED1' } }
				   } })
	  )
	  &&
	  # < ! E/arg*[result:*]
	  ! assertion_args($_, '[@role=":RESULT"]')
	}
	map { $ekb->get_assertion($_, 'EVENT') }
	map { $_->value } $x->findnodes('features/inevent/@id');	
      return 0 unless @inevents;

      DEBUG 2, "%s: got %d inevents: (%s)",
	$rule->name, scalar(@inevents),
	join(',', map {$_->getAttribute('id')} @inevents);

      my $count = 0;
      foreach my $e (@inevents) {
	my $e_id = $e->getAttribute('id');
	my $e_type = get_slot_value($e, 'type');

	# < Y/type is_a <bio_entity>
	my $y_id = $e->findvalue('arg2/@id');
	my $y = $ekb->get_assertion($y_id, "TERM")
	  or next;
	my $y_type = get_slot_value($y, 'type');
	$ont_bioents->has($y_type)
	  or next;

	INFO "Rule %s matches term %s[t=%s] in %s[t=%s](*, %s[t=%s])", 
	  $rule->name, $x_id, $x_type, $e_id, $e_type, $y_id, $y_type;

	# > - X//inevent[@id=E]
	remove_elements($x, 'features/inevent[@id="'.$e_id.'"]');

	# > + X1=X
	# NB: since X participates in E, we cannot break it up if it's an aggregate
	# TODO: clone deeply, if isolated
	my $x1 = $ekb->clone_assertion($x, {rule => $rule->name});
	$ekb->add_assertion($x1);
	my $x1_id = $x1->getAttribute('id');

	# > + Y1=Y
	my @y_members = $ekb->find_members_rec($y_id, ["AND", "BOTH"]);
	my @y_comp_ids =
	  map { $_->getAttribute('id') }
	  map { $ekb->add_assertion($_) }
	  map { $ekb->clone_assertion($_, {rule => $rule->name}) } @y_members;

	if ($rule->reasoner()->option('add_binding_features')) {
	  # > + X1/not-features/bound-to[Y]
	  $ekb->add_notfeature($x1, 'bound-to' => { id => $y_id,
						    event => $e_id,
						    rule => $rule->name() });
	  # > + Y/not-features/bound-to[X1] 
	  $ekb->add_notfeature($y, 'bound-to' => { id => $x1_id,
						   event => $e_id,
						   rule => $rule->name() });
	}
	
	# > + Z:MACROMOLECULAR-COMPLEX(components:(X,Y1))
	my $z_name = make_complex_name($x, @y_members);
	my $z_id =
	  $ekb->infer_assertion( "TERM",
				 { refid => "$e_id",
				   rule => $rule->name},
				 make_slot_nodes(type => 'ONT::MACROMOLECULAR-COMPLEX',
						 name => $z_name),
				 make_components($x_id, @y_comp_ids) );

	# > ~ E:bind(r1:Y,r2:X1,result:Z)
	set_attribute(get_child_node($e, "arg1"), 'id' => $x1_id);
	$ekb->add_arg($e, ':RESULT' => $z_id);

	if ($rule->reasoner()->option('add_amount_changes')) {
	  # > + E1:increase(affected:Z)
	  my $e1_id =
	    $ekb->increases($z_id, {refid => "$e_id", rule => $rule->name});
	  # > + C1:cause(factor:E,outcome:E1)
	  $ekb->causes($e_id, $e1_id, {refid => "$e_id", rule => $rule->name});
	  # > + E2:decrease(affected:X1)
	  my $e2_id =
	    $ekb->decreases($x1_id, {refid => "$e_id", rule => $rule->name});
	  # > + C2:cause(factor:E,outcome:E2)
	  $ekb->causes($e_id, $e2_id, {refid => "$e_id", rule => $rule->name});
	  # > + E3:decrease(affected:Y)
	  my $e3_id =
	    $ekb->decreases($y_id, {refid => "$e_id", rule => $rule->name});
	  # > + C3:cause(factor:E,outcome:E3)
	  $ekb->causes($e_id, $e3_id, {refid => "$e_id", rule => $rule->name});
	}

	$count++;
      }

      $count;
    }
   },

   {
    ## Y-bound X 
    ## => unbound X binds to unbound Y to form X/Y complex
    # < X:*[inevent[@id=E]]
    # < X/type is_a ONT::MOLECULAR-PART
    # < E:bind(r1:Y,r2:X,!negation)
    # < r1=AGENT && r2=AFFECTED 
    # < Y/type is_a <bio_entity>
    # < ! E/arg*[result:*]
    # < ! E/mods/mod/type[ONT::MANNER-UNDO]
    # ? & dependsOn ^(X//inevent/@id[.!=E])
    # ? & dependsOn ^(Y//inevent/@id[.!=E])
    # > - X//inevent[@id=E]
    # > + X1=X
    # > + Y1=Y
    # > + X1/not-features/bound-to[Y]
    # > + Y/not-features/bound-to[X1] 
    # > + X/features/bound-to[Y1]
    # > + Y1/features/bound-to[X]
    # > + Z:MACROMOLECULAR-COMPLEX(components:(X,Y1))
    # > ~ E:bind(r1:Y,r2:X1,result:Z)
    # > + E1:increase(affected:Z)
    # > + C1:cause(factor:E,outcome:E1)
    # > + E2:decrease(affected:X1)
    # > + C2:cause(factor:E,outcome:E2)
    # > + E3:decrease(affected:Y)
    # > + C3:cause(factor:E,outcome:E3)
    name => "EKR:InEventBind2",
    constraints => ['TERM[features/inevent]'], # TODO: needs to be more specific!
    handler => sub {
      my ($rule, $ekb, $x) = @_;
      my $x_id = $x->getAttribute('id');

      # < X/type is_a ONT::MOLECULAR-PART
      my $x_type = get_slot_value($x, 'type');
      $ont_bioents->is_a($x_type, 'ONT::MOLECULAR-PART')
	or return 0;

      # < X:*[inevent]
      my @inevents =
	grep {
	  # < E/type is_a ONT::BIND 
	  $ont_events->is_a(get_slot_value($_, 'type'), 'ONT::BIND')
	    &&
	    # < E:bind(r1:X,!negation:+)
	    # < ! E/mods/mod/type[ONT::MANNER-UNDO]
	    # < r1=AGENT && r2=AFFECTED || r1=AFFECTED && r2=AFFECTED1
	    match_node($_, { SX => { 'negation' 
				     => [OP_NOT, '+'],
				     'mods/mod/type' 
				     => [OP_NOT, 'ONT::MANNER-UNDO'],
				     'arg1' 
				     => { AX => { 'role' => ':AGENT'} },
				     'arg2' 
				     => { AX => { 'role' => ':AFFECTED',
						  'id' => $x_id  } }
				   } })
	    &&
	    # < ! E/arg*[result:*]
	    ! assertion_args($_, '[@role=":RESULT"]')
	  }
	map { $ekb->get_assertion($_) }
	map { $_->value }
	$x->findnodes('features/inevent/@id');	
      return 0 unless @inevents;

      DEBUG 2, "%s: got %d inevents: (%s)",
	$rule->name, scalar(@inevents),
	join(',', map {$_->getAttribute('id')} @inevents);

      my $count = 0;
      foreach my $e (@inevents) {
	my $e_id = $e->getAttribute('id');
	my $e_type = get_slot_value($e, 'type');

	# < Y/type is_a <bio_entity>
	my $y_id = $e->findvalue('arg1/@id');
	my $y = $ekb->get_assertion($y_id, "TERM")
	  or next;
	my $y_type = get_slot_value($y, 'type');
	$ont_bioents->has($y_type)
	  or next;

	INFO "Rule %s matches term %s[t=%s] in %s[t=%s](*, %s[t=%s])", 
	  $rule->name, $x_id, $x_type, $e_id, $e_type, $y_id, $y_type;

	# > - X//inevent[@id=E]
	remove_elements($x, 'features/inevent[@id="'.$e_id.'"]');

	# > + X1=X
	# NB: since X participates in E, we cannot break it up if it's an aggregate
	# TODO: clone deeply, if isolated
	my $x1 = $ekb->clone_assertion($x, {rule => $rule->name});
	my $x1_id = $x1->getAttribute('id');
	$ekb->add_assertion($x1);
	# > ~ E:bind(r2:X1)
	set_attribute(get_child_node($e, "arg2"), 'id' => $x1_id);

	# > + Y1=Y
	my @y_members = $ekb->find_members_rec($y_id, ["AND", "BOTH"]);
	my @y_comp_ids =
	  map { $_->getAttribute('id') }
	  map { $ekb->add_assertion($_) }
	  map { $ekb->clone_assertion($_, {rule => $rule->name}) } @y_members;

	if ($rule->reasoner()->option('add_binding_features')) {
	  # > + X1/not-features/bound-to[Y]
	  $ekb->add_notfeature($x1, 'bound-to' => { id => $y_id,
						    event => $e_id,
						    rule => $rule->name() });
	  # > + Y/not-features/bound-to[X1] 
	  $ekb->add_notfeature($y, 'bound-to' => { id => $x1_id,
						   event => $e_id,
						   rule => $rule->name() });
	}
	
	# > + Z:MACROMOLECULAR-COMPLEX(components:(X,Y1))
	my $z_name = make_complex_name($x, @y_members);
	my $z_id =
	  $ekb->infer_assertion( "TERM",
				 { refid => "$e_id",
				   rule => $rule->name},
				 make_slot_nodes(type => 'ONT::MACROMOLECULAR-COMPLEX',
						 name => $z_name),
				 make_components($x_id, @y_comp_ids) );

	# > + E:bind(result:Z)
	$ekb->add_arg($e, ':RESULT' => $z_id);

	if ($rule->reasoner()->option('add_amount_changes')) {
	  # > + E1:increase(affected:Z)
	  my $e1_id =
	    $ekb->increases($z_id, {refid => "$e_id", rule => $rule->name});
	  # > + C1:cause(factor:E,outcome:E1)
	  $ekb->causes($e_id, $e1_id, {refid => "$e_id", rule => $rule->name});
	  # > + E2:decrease(affected:X1)
	  my $e2_id =
	    $ekb->decreases($x1_id, {refid => "$e_id", rule => $rule->name});
	  # > + C2:cause(factor:E,outcome:E2)
	  $ekb->causes($e_id, $e2_id, {refid => "$e_id", rule => $rule->name});
	  # > + E3:decrease(affected:Y)
	  my $e3_id =
	    $ekb->decreases($y_id, {refid => "$e_id", rule => $rule->name});
	  # > + C3:cause(factor:E,outcome:E3)
	  $ekb->causes($e_id, $e3_id, {refid => "$e_id", rule => $rule->name});
	}

	$count++;
      }
	
      $count;
    }
   },
   
   {
    ## activated X
    # < X/inevent[@id=E]
    # < X/type is_a ONT::MOLECULAR-PART
    # < E:activate(affected:X,!negation)
    # < ! E/arg*[result:*]
    # > - X//inevent[@id=E]
    # > + X1=X
    # > + X1//active[FALSE]
    # > + X//active[TRUE]
    # > + E:*(affected:X1, result:X)
    # > + E1:increase(affected:X) 
    # > + C1:cause(factor:E,outcome:E1)
    # > + E2:decrease(affected:X1)
    # > + C2:cause(factor:E,outcome:E2)
    name => "EKR:IneventActivate",
    constraints => ['TERM[features/inevent]'],
    handler => sub {
      my ($rule, $ekb, $x) = @_;
      my $x_id = $x->getAttribute('id');

      # < X/type is_a ONT::MOLECULAR-PART
      my $x_type = get_slot_value($x, 'type');
      $ont_bioents->is_a($x_type, 'ONT::MOLECULAR-PART')
	or return 0;

      # < X:*[inevent]
      my @inevents =
	grep {
	  # < E:activate(affected:X,!negation)
	  $ont_events->is_a(get_slot_value($_, 'type'), 'ONT::ACTIVATE')
	    &&
	    match_node($_, { SX => { 'negation' 
				     => [OP_NOT, '+'],
				     'arg2' 
				     => { AX => { 'role' => ':AFFECTED',
						  'id' => $x_id  } }
				   } })
	    &&
	    # < ! E/arg*[result:*]
	    ! assertion_args($_, '[@role=":RESULT"]')
	  }
	map { $ekb->get_assertion($_) }
	map { $_->value }
	$x->findnodes('features/inevent/@id');	
      return 0 unless @inevents;

      my $count = 0;
      foreach my $e (@inevents) {
	my $e_id = $e->getAttribute('id');
	my $e_type = get_slot_value($e, 'type');

	INFO "Rule %s matches term %s[t=%s] in %s/[t=%s]", 
	  $rule->name, $x_id, $x_type, $e_id, $e_type;

	# > - X//inevent[@id=E]
	remove_elements($x, 'features/inevent[@id="'.$e_id.'"]');

	# > + X1=X[active[FALSE]]
	my $x1 = $ekb->clone_assertion($x, {rule => $rule->name});
	my $x1_id = $x1->getAttribute('id');
	$ekb->add_feature($x1, 'active' => 'FALSE');
	$ekb->add_assertion($x1);

	# > ~ X//active[TRUE]
	$ekb->add_feature($x, 'active' => 'TRUE');

	# > + E:*(affected:X1, result:X)
	set_attribute(get_child_node($e, "arg2"), 'id' => $x1_id);
	$ekb->add_arg($e, ':RESULT' => $x_id);

	if ($rule->reasoner()->option('add_amount_changes')) {
	  # > + E1:increase(affected:X) 
	  my $e1_id =
	    $ekb->increases($x_id, {refid => "$e_id", rule => $rule->name});
	  # > + C1:cause(factor:E,outcome:E1)
	  $ekb->causes($e_id, $e1_id, {refid => "$e_id", rule => $rule->name});
	  # > + E2:decrease(affected:X1)
	  my $e2_id =
	    $ekb->decreases($x1_id, {refid => "$e_id", rule => $rule->name});
	  # > + C2:cause(factor:E,outcome:E2)
	  $ekb->causes($e_id, $e2_id, {refid => "$e_id", rule => $rule->name});
	}

	$count++;
      }
	
      $count;
    }
   },
   
   {## X1 and X2 form a complex, the complex formed by X1 and X2
    ## =>
    ## X1 and X2 bind into X1/X2 complex
    # < E ont::produce :agent X :affected-result Y
    # < X ont::molecular-part :aggregate (X1 X2 ...)
    # < Y ont::macromolecular-complex
    # <? Y :inevent E
    # > E ont::bind :agent X :result Y
    # > X1' = X1, ...
    # > Y ont::macromolecular-complex :components (X1' X2' ...)
    # - Y :inevent E
    name => "EKR:ComplexFormation1",
    constraints => ['EVENT[type[.="ONT::PRODUCE"] and not(negation) and arg1[@role=":AGENT"] and arg2[@role=":AFFECTED-RESULT"]]'],
    handler => sub {
      my ($rule, $ekb, $e) = @_;
      my $e_id = $e->getAttribute('id');

      # < X ont::molecular-part 
      my $x_id = $e->findvalue('arg1/@id');
      my $x = $ekb->get_assertion($x_id, "TERM")
	or return 0;
      my $x_type = get_slot_value($x, 'type');
      $ont_bioents->is_a($x_type, 'ONT::MOLECULAR-PART')
	or return 0;
      # < X :aggregate (X1 X2 ...)
      my @members =
	grep { $_->getAttribute('id') ne $x_id }
	$ekb->find_members_rec($x_id, ["AND", "BOTH"]);
      return 0 unless @members; # if T is a member, it is the only one!
      my @member_ids = map { $_->getAttribute('id') } @members;

      # < Y ont::macromolecular-complex
      my $y_id = $e->findvalue('arg2/@id');
      my $y = $ekb->get_assertion($y_id, "TERM")
	or return 0;
      my $y_type = get_slot_value($y, 'type');
      $ont_bioents->is_a($y_type, 'ONT::MACROMOLECULAR-COMPLEX')
	or return 0;
      # < ! E :result 
      assertion_args($e, '[@role=":RESULT"]')
	and return 0;

      INFO "Rule %s matches event %s (agent:%s, affected-result:%s)", 
	$rule->name, $e_id, $x_id, $y_id;

      # > E ont::bind :result Y
      set_attribute(get_child_node($e, 'arg2'), 'role' => ":RESULT");
      set_slot_value($e, 'type' => "ONT::BIND");
      append_to_attribute($e, 'rule', $rule->name);
      
      # > X1' = X1, ...
      my @comp_ids =
	map { $_->getAttribute('id') }
	map { $ekb->add_assertion($_) }
	map { $ekb->clone_assertion($_, {rule => $rule->name}) } @members;

      # > Y ont::macromolecular-complex :components (X1' X2' ...)
      if (@comp_ids) {
	my $y_name = make_complex_name(@members);
	$ekb->modify_assertion($y,
			       make_slot_node( name => $y_name ),
			       make_components(@comp_ids));
      }

      # - Y :inevent E
      remove_elements($y, 'features/inevent[@id="'.$e_id.'"]');
	
      1;
      }
   },

   {## X1 and X2 bind to form a complex
    ## =>
    ## X1 and X2 bind into X1/X2 complex
    # < E[type=ont::bind][agent:X][res:Y]
    # < ! E[result]
    # < X[type=ont::molecular-part][aggregate:(X1 X2 ...)]
    # < Y[type=ont::macromolecular-complex]
    # > E[result:Y]
    # > X1' = X1, ...
    # > Y[type=ont::macromolecular-complex][components:(X1' X2' ...)]
    name => "EKR:ComplexFormation2",
    constraints => ['EVENT[type[.="ONT::BIND"] and not(negation) and not(mods/mod/type[.="ONT::MANNER-UNDO"]) and arg1[@role=":AGENT"] and arg2[@role=":RES"]]'],
    handler => sub {
      my ($rule, $ekb, $e) = @_;
      my $e_id = $e->getAttribute('id');

      # < E[res:Y]
      my ($arg_res) = assertion_args($e, '[@role=":RES"]');
      # < ! E[result]
      assertion_args($e, '[@role=":RESULT"]')
	and return 0;
	
      # < X[type=ont::molecular-part] 
      my $x_id = $e->findvalue('arg1/@id');
      my $x = $ekb->get_assertion($x_id, "TERM")
	or return 0;
      my $x_type = get_slot_value($x, 'type');
      $ont_bioents->is_a($x_type, 'ONT::MOLECULAR-PART')
	or return 0;
      # < X[aggregate:(X1 X2 ...)]
      my @members =
	grep { $_->getAttribute('id') ne $x_id }
	$ekb->find_members_rec($x_id, ["AND", "BOTH"]);
      return 0 unless @members; # if T is a member, it is the only one!
      my @member_ids = map { $_->getAttribute('id') } @members;

      # < Y[type=ont::macromolecular-complex]
      my $y_id = $arg_res->getAttribute('id');
      my $y = $ekb->get_assertion($y_id, "TERM")
	or return 0;
      my $y_type = get_slot_value($y, 'type');
      $ont_bioents->is_a($y_type, 'ONT::MACROMOLECULAR-COMPLEX')
	or return 0;

      INFO "Rule %s matches event %s (agent:%s, affected-result:%s)", 
	$rule->name, $e_id, $x_id, $y_id;

      # > E[result:Y]
      set_attribute($arg_res, 'role' => ":RESULT");
      append_to_attribute($e, 'rule', $rule->name);
      
      # > X1' = X1, ...
      my @comp_ids =
	map { $_->getAttribute('id') }
	map { $ekb->add_assertion($_) }
	map { $ekb->clone_assertion($_, {rule => $rule->name}) } @members;

      # > Y[type=ont::macromolecular-complex][components:(X1' X2' ...)]
      if (@comp_ids) {
	my $y_name = make_complex_name(@members);
	$ekb->modify_assertion($y,
			       make_slot_node( name => $y_name ),
			       make_components(@comp_ids));
      }

      1;
      }
   },
   
   {
    ## X1 and X2 interact; interaction between X1 and X2
    ## => X1 and X2 bind to form X1/X2 complex
    # < E:BIND(agent:X, !affected, !site)
    # < ! E/mods/mod/type[ONT::MANNER-UNDO]
    # < X aggregate[AND(X1,X2,...)]
    # < X type[. is_a <bio_entity>]
    # & dependsOn ^(X1//inevent/@id) ...
    # > + E:BIND(affected:X1,affected:X2,...)
    # > + X1'=X1, X2'=X2, ...
    # > + X1'/features/bound-to(X2',...), ...
    # > + Z:MACROMOLECULAR-COMPLEX(components:(X1',X2',...))
    # > ~ X1/not-features/bound-to(X2,...), ...
    # > - E/agent:T
    # > + E/affected:X1,...
    # > + E1:increase(affected:Z)
    # > + C1:cause(factor:E,outcome:E1)
    # > + E2_1:decrease(affected:X1), ...
    # > + C2_1:cause(factor:E,outcome:E2_1), ...
    name => "EKR:BindXandY1",
    constraints => ['EVENT[type[.="ONT::BIND"] and not(negation) and arg1 and not(arg2) and not(site)]'],
    handler => sub  {
      my ($rule, $ekb, $e) = @_;

      my $e_id = $e->getAttribute('id');

      # < E:BIND(agent:Y, !affected)
      # < ! E/mods/mod/type[ONT::MANNER-UNDO]
      match_node($e, { SX => { 'mods/mod/type' 
			       => [OP_NOT, 'ONT::MANNER-UNDO'],
			       'arg1' 
			       => { AX => { 'role' => ":AGENT" } },
			     } })
	or return 0;

      # < ! arg*(result:*)
      assertion_args($e, '[@role=":RESULT"]')
	and return 0;

      # < T/type is_a <bio_entity>
      my $t_arg = get_child_node($e, 'arg1');
      my $t_id = $t_arg->getAttribute('id');
      my $t = $ekb->get_assertion($t_id, "TERM")
	or return 0;
      my $t_type = get_slot_value($t, 'type');
      $ont_bioents->has($t_type)
	or return 0;

      # < T:TERM[aggregate[AND(X1,X2,...)]]
      my @members =
	grep { $_->getAttribute('id') ne $t_id }
	$ekb->find_members_rec($t_id, ["AND", "BOTH"]);
      return 0 unless @members; # if T is a member, it is the only one!
      my @member_ids = map { $_->getAttribute('id') } @members;

      INFO "Rule %s matches event %s (affected:%s[t=%s])", 
	$rule->name, $e_id, $t_id, $t_type;

      # & dependsOn ^(X1//inevent/@id) ...
      map { $rule->reasoner()->dependsOn($_) }
	map { $_->getAttribute('id') }
	map { $_->findnodes('features/inevent') } @members;

      my @comp_ids =
	map { $_->getAttribute('id') }
	map { $ekb->add_assertion($_) }
	map { $ekb->clone_assertion($_, {rule => $rule->name}) } @members;
      DEBUG 1, "Got %d components (out of %d members)",
	scalar(@comp_ids), scalar(@member_ids);

      if (0) { # LG 2017/04/19 -- disabled 
	# > - E/agent:T
	$e->removeChild($t_arg);
	# FIXME: this ought to be handled automatically, but needs
	# change in Reasoner control mechanism
	$rule->reasoner()->dependsOn($t_id, "EKR:RemoveOrphanTerm");
      }
      
      # > + Z:MACROMOLECULAR-COMPLEX(components:(X1,Y1))
      my $z_name = make_complex_name(@members);
      my $z_id =
	$ekb->infer_assertion( "TERM",
			       { refid => "$e_id",
				 rule => $rule->name},
			       make_slot_nodes(type => 'ONT::MACROMOLECULAR-COMPLEX',
					       name => $z_name),
			       make_components(@comp_ids) );
      $ekb->add_arg($e, ':RESULT' => $z_id);

      if ($rule->reasoner()->option('add_amount_changes')) {
	# add: E1:increase(affected:Z)
	my $e1_id =
	  $ekb->increases($z_id, {refid => "$e_id", rule => $rule->name});
	# add: C1:cause(factor:E,outcome:E1)
	$ekb->causes($e_id, $e1_id, {refid => "$e_id", rule => $rule->name});

	foreach my $x1_id (@member_ids) {
	  # add: E2:decrease(affected:X1)
	  my $e2_id =
	    $ekb->decreases($x1_id, {refid => "$e_id", rule => $rule->name});
	  # add: C2:cause(factor:E,outcome:E2)
	  $ekb->causes($e_id, $e2_id, {refid => "$e_id", rule => $rule->name});
	}
      }

      1;
    }
   },

   {
    ## X1 and X2 associate; association between X1 and X2
    ## => X1 and X2 bind into X1/X2 complex
    # < E:BIND(affected:T, !agent, !site)
    # < ! E/mods/mod/type[ONT::MANNER-UNDO]
    # < T:TERM[aggregate[AND(X1,X2,...)]]
    # < T/type is_a <bio_entity>
    # & dependsOn ^(X21//inevent/@id) ...
    # > + E:BIND(affected:X1,affected:X2,...)
    # > + X1'=X1, X2'=X2, ...
    # > + X1'/features/bound-to(X2',...), ...
    # > + Z:MACROMOLECULAR-COMPLEX(components:(X1',X2',...))
    # > ~ X1/not-features/bound-to(X2,...), ...
    # > - E/agent:T
    # > + E/affected:X1,...
    # > + E1:increase(affected:Z)
    # > + C1:cause(factor:E,outcome:E1)
    # > + E2_1:decrease(affected:X1), ...
    # > + C2_1:cause(factor:E,outcome:E2_1), ...
    name => "EKR:BindXandY2",
    constraints => ['EVENT[type[.="ONT::BIND"] and not(negation) and arg2 and not(arg1) and not(site)]'],
    handler => sub  {
      my ($rule, $ekb, $e) = @_;

      my $e_id = $e->getAttribute('id');

      # < E:BIND(affected:Y, !agent)
      # < ! E/mods/mod/type[ONT::MANNER-UNDO]
      match_node($e, { SX => { 'mods/mod/type' 
			       => [OP_NOT, 'ONT::MANNER-UNDO'],
			       'arg2' 
			       => { AX => { 'role' => ":AFFECTED" } },
			     } })
	or return 0;

      # < ! arg*(result:*)
      assertion_args($e, '[@role=":RESULT"]')
	and return 0;

      # < T/type is_a <bio_entity>
      my $t_arg = get_child_node($e, 'arg2');
      my $t_id = $t_arg->getAttribute('id');
      my $t = $ekb->get_assertion($t_id, "TERM")
	or return 0;
      my $t_type = get_slot_value($t, 'type');
      $ont_bioents->has($t_type)
	or return 0;

      # < T:TERM[aggregate[AND(X1,X2,...)]]
      my @members =
	grep { $_->getAttribute('id') ne $t_id }
	$ekb->find_members_rec($t_id, ["AND", "BOTH"]);
      return 0 unless @members; # if T is a member, it is the only one!
      my @member_ids = map { $_->getAttribute('id') } @members;

      INFO "Rule %s matches event %s (affected:%s[t=%s])", 
	$rule->name, $e_id, $t_id, $t_type;

      # & dependsOn ^(X1//inevent/@id) ...
      map { $rule->reasoner()->dependsOn($_) }
	uniq map { $_->getAttribute('id') }
	map { $_->findnodes('features/inevent') } @members;

      my @comp_ids =
	map { $_->getAttribute('id') }
	map { $ekb->add_assertion($_) }
	map { $ekb->clone_assertion($_, {rule => $rule->name}) } @members;
      DEBUG 1, "Got %d components (out of %d members)",
	scalar(@comp_ids), scalar(@member_ids);

      if (0) { # LG 2017/04/19 -- disabled 
	# > - E/agent:T
	$e->removeChild($t_arg);
	# FIXME: this ought to be handled automatically, but needs
	# change in Reasoner control mecanism
	$rule->reasoner()->dependsOn($t_id, "EKR:RemoveOrphanTerm");
      }
      
      # > + Z:MACROMOLECULAR-COMPLEX(components:(X1,Y1))
      my $z_name = make_complex_name(@members);

      my $z_id =
	$ekb->infer_assertion( "TERM",
			       { refid => "$e_id",
				 rule => $rule->name},
			       make_slot_nodes(type => 'ONT::MACROMOLECULAR-COMPLEX',
					       name => $z_name),
			       make_components(@comp_ids) );

      $ekb->add_arg($e, ':RESULT' => $z_id);

      if ($rule->reasoner()->option('add_amount_changes')) {
	# add: E1:increase(affected:Z)
	my $e1_id =
	  $ekb->increases($z_id, {refid => "$e_id", rule => $rule->name});
	# add: C1:cause(factor:E,outcome:E1)
	$ekb->causes($e_id, $e1_id, {refid => "$e_id", rule => $rule->name});

	foreach my $x1_id (@member_ids) {
	  # add: E2:decrease(affected:X1)
	  my $e2_id =
	    $ekb->decreases($x1_id, {refid => "$e_id", rule => $rule->name});
	  # add: C2:cause(factor:E,outcome:E2)
	  $ekb->causes($e_id, $e2_id, {refid => "$e_id", rule => $rule->name});
	}
      }

      1;
    }
   },
   
   {
    ## X binds to Y, X is bound to Y
    ## => X binds Y into X/Y complex
    # < E:bind(r1:X,r2:Y,!negation)
    # < ! E/mods/mod/type[ONT::MANNER-UNDO]
    # < r1=AGENT && r2=AFFECTED || r1=AFFECTED && r2=AFFECTED1
    # < X/type is_a <bio_entity>
    # < Y/type is_a <bio_entity>
    # < ! arg*(result:*)
    # < ! X//inevent[@id=E]
    # < ! Y//inevent[@id=E]
    # & dependsOn ^(X//inevent/@id)
    # & dependsOn ^(Y//inevent/@id)
    # > + X1=X
    # > + Y1=Y
    # > + X1/features/bound-to(Y1)
    # > + Y1/features/bound-to(X1)
    # > + Z:MACROMOLECULAR-COMPLEX(components:(X1,Y1))
    # > ~ E:bind(agent:X,affected:Y,result:Z)
    # > ~ X/not-features/bound-to(Y)
    # > ~ Y/not-features/bound-to(X) 
    # > + E1:increase(affected:Z)
    # > + C1:cause(factor:E,outcome:E1)
    # > + E2:decrease(affected:X)
    # > + C2:cause(factor:E,outcome:E2)
    # > + E3:decrease(affected:Y)
    # > + C3:cause(factor:E,outcome:E3)
    name => "EKR:BindXtoY",
    constraints => ['EVENT[type[.="ONT::BIND"] and not(negation) and arg1 and arg2]'],
    handler => sub  {
      my ($rule, $ekb, $e) = @_;

      my $e_id = $e->getAttribute('id');

      # < ! E/mods/mod/type[ONT::MANNER-UNDO]
      # < r1=AGENT && r2=AFFECTED || r1=AFFECTED && r2=AFFECTED1
      match_node($e, { SX => { 'mods/mod/type' 
			       => [OP_NOT, 'ONT::MANNER-UNDO'],
			       'arg1' 
			       => { AX => { 'role' => [OP_OR,
						       ":AGENT",
						       ":AFFECTED"] } },
			       'arg2' 
			       => { AX => { 'role' => [OP_OR,
						       ":AFFECTED",
						       ":AFFECTED1"] } }
			     } })
	or return 0;

      # < ! arg*(result:*)
      assertion_args($e, '[@role=":RESULT"]')
	and return 0;

      # < X/type is_a <bio_entity>
      my $x_id = $e->findvalue('arg1/@id');
      my $x = $ekb->get_assertion($x_id, "TERM")
	or return 0;
      my $x_name = get_slot_value($x, 'name') // "";
      my $x_type = get_slot_value($x, 'type');
      $ont_bioents->has($x_type)
	or return 0;
      # < ! X//inevent[@id=E]
      $x->findnodes('features/inevent[@id="'.$e_id.'"]')
	and return 0;

      # < Y/type is_a <bio_entity>
      my $y_id = $e->findvalue('arg2/@id');
      my $y = $ekb->get_assertion($y_id, "TERM")
	or return 0;
      my $y_name = get_slot_value($y, "name") // "";
      my $y_type = get_slot_value($y, 'type');
      $ont_bioents->has($y_type)
	or return 0;
      # < ! Y//inevent[@id=E]
      $y->findnodes('features/inevent[@id="'.$e_id.'"]')
	and return 0;
  
      INFO "Rule %s matches event %s w/ args (%s[n=%s], %s[n=%s])", 
	$rule->name, $e_id, $x_id, $x_name, $y_id, $y_name;

      # & dependsOn ^(X//inevent/@id)
      map { $rule->reasoner()->dependsOn($_) }
	map { $_->value } $x->findnodes('features/inevent/@id');	
	  
      # & dependsOn ^(Y//inevent/@id)
      map { $rule->reasoner()->dependsOn($_) }
	map { $_->value } $y->findnodes('features/inevent/@id');
      
      if ($rule->reasoner()->option('add_binding_features')) {
	# > ~ X[not-features/bound-to:Y]
	$ekb->add_notfeature($x, 'bound-to' => { id => $y_id,
						 event => $e_id,
						 rule => $rule->name() });
	# > ~ Y[not-features/bound-to:X] 
	$ekb->add_notfeature($y, 'bound-to' => { id => $x_id,
						 event => $e_id,
						 rule => $rule->name() });
      }

      # add: X1=X
      my $x1 = $ekb->clone_assertion($x, {rule => $rule->name});
      my $x1_id = $x1->getAttribute('id');
      # remove inevent E feature, if there
      #remove_elements($x1, 'features/inevent[@id="'.$e_id.'"]');
      $ekb->add_assertion($x1);
      
      # > + Y1=Y
      my $y1 = $ekb->clone_assertion($y, {rule => $rule->name});
      my $y1_id = $y1->getAttribute('id');
      # remove inevent E feature, if there
      #remove_elements($y1, 'features/inevent[@id="'.$e_id.'"]');
      $ekb->add_assertion($y1);
      
      # > + Z:MACROMOLECULAR-COMPLEX(components:(X1,Y1))
      my $z_name = make_complex_name($x, $y);
      my $z_id =
	$ekb->infer_assertion( "TERM",
			       { refid => "$e_id",
				 rule => $rule->name},
			       make_slot_nodes(type => 'ONT::MACROMOLECULAR-COMPLEX',
					       name => $z_name),
			       make_components($x1_id, $y1_id) );

      # mod: E:bind(agent:X,affected:Y,result:Z)
      $ekb->add_arg($e, ':RESULT' => $z_id);

      if ($rule->reasoner()->option('add_amount_changes')) {
	# add: E1:increase(affected:Z)
	my $e1_id =
	  $ekb->increases($z_id, {refid => "$e_id", rule => $rule->name});
	# add: C1:cause(factor:E,outcome:E1)
	$ekb->causes($e_id, $e1_id, {refid => "$e_id", rule => $rule->name});
	# add: E2:decrease(affected:X)
	my $e2_id =
	  $ekb->decreases($x_id, {refid => "$e_id", rule => $rule->name});
	# add: C2:cause(factor:E,outcome:E2)
	$ekb->causes($e_id, $e2_id, {refid => "$e_id", rule => $rule->name});
	# add: E3:decrease(affected:Y)
	my $e3_id =
	  $ekb->decreases($y_id, {refid => "$e_id", rule => $rule->name});
	# add: C3:cause(factor:E,outcome:E3)
	$ekb->causes($e_id, $e3_id, {refid => "$e_id", rule => $rule->name});
      }

      1;
    }
   },

   {
    ## dePTMs: eg, X is dephosphorylated [by Y] 
    ## => phosphorylated X is dephosphorylated [by Y] to unphosphorylated X
    # < E[type(is_a)ONT::PTM]
    # < E[mods/mod/type=ONT::MANNER-UNDO]
    # < ! E[negation]
    # < ! E[result:*]
    # < E[affected:X]
    # < X[type(is_a)ONT::MOLECULAR-PART]
    # < ! X[//inevent/@id=E]
    # & dependsOn X//inevent/@id
    # > ~ X[type=s/ONT::GENE/ONT::PROTEIN/]
    # > + X[features/ptm[@id=E]]
    # > + X1=X[not-features/ptm[@id=E]]
    # > + E[result:X1]
    # > + E1[type=ont::increase][affected:X1] 
    # > + C1[type=ont::cause][factor:E,outcome:E1]
    # > + E2[type=ont::decrease][affected:X]
    # > + C2[type=ont::cause][factor:E,outcome:E2]
    name => 'EKR:dePTM',
    constraints => ['EVENT[not(negation) and mods/mod/type[.="ONT::MANNER-UNDO"] and arg2[@role=":AFFECTED"]]'],
    handler => sub {
      my ($rule, $ekb, $e) = @_;
      my $e_id = $e->getAttribute('id');
      # < E[type(is_a)ONT::PTM]
      my $e_type = get_slot_value($e, 'type');
      $ont_events->is_a($e_type, "ONT::PTM")
	or return 0;
      # < ! E[result:*]
      assertion_args($e, '[@role=":RESULT"]')
	and return 0;
      # < E[affected:X]
      my ($arg_x) = assertion_args($e, '[@role=":AFFECTED"]');
      my $x_id = $arg_x->getAttribute('id');
      my $x = $ekb->get_assertion($x_id, 'TERM')
	or return 0;
      # < X[type(is_a)ONT::MOLECULAR-PART]
      my $x_type = get_slot_value($x, 'type');
      $ont_bioents->is_a($x_type, 'ONT::MOLECULAR-PART')
	or return 0;
      # < ! X[//inevent/@id=E]
      $x->findnodes('features/inevent[@id="'.$e_id.'"]')
	and return 0;
	
      INFO "Rule %s matches event %s (affected:%s[t=%s])", 
	$rule->name, $e_id, $x_id, $x_type;

      # & dependsOn X//inevent/@id
      map { $rule->reasoner()->dependsOn($_) }
	map { $_->value } $x->findnodes('features/inevent/@id');	

      # > ~ X[type=s/ONT::GENE/ONT::PROTEIN/]
      set_slot_value($x, 'type' => 'ONT::PROTEIN')
	if $ont_bioents->is_a('ONT::GENE', $x_type);
      # > + X1=X
      my $x1 = $ekb->clone_assertion($x, {rule => $rule->name});
      $ekb->add_assertion($x1);
      my $x1_id = $x1->getAttribute('id');
      # > + X[features/ptm[@id=E]]
      $ekb->add_feature($x, 'ptm' => { type => $e_type,
				       event => $e_id });
      # > + X1[not-features/ptm[@id=E]]
      $ekb->add_notfeature($x1, 'ptm' => { type => $e_type,
					   event => $e_id });
      # > + E[result:X1]
      $ekb->add_arg($e, ':RESULT' => $x1_id);

      if ($rule->reasoner()->option('add_amount_changes')) {
	# > + E1:increase(affected:X1) 
	my $e1_id = 
	  $ekb->increases($x1_id, {refid => "$e_id", rule => $rule->name});
	# > + C1:cause(factor:E,outcome:E1)
	$ekb->causes($e_id, $e1_id, {refid => "$e_id", rule => $rule->name});
	# > + E2:decrease(affected:X)
	my $e2_id =
	  $ekb->decreases($x_id, {refid => "$e_id", rule => $rule->name});
	# > + C2:cause(factor:E,outcome:E2)
	$ekb->causes($e_id, $e2_id, {refid => "$e_id", rule => $rule->name});
      }

      1;
    }
   },

   {
    ## PTMs: eg, X is phosphorylated [by Y] 
    ## => non-phosphorylated X is phosphorylated [by Y] to phospho-X
    # < E[type(is_a)ONT::PTM]
    # < ! E[negation]
    # < ! E[mods/mod/type=ONT::MANNER-UNDO]
    # < ! E[result:*]
    # < E[affected:X]
    # < X[type(is_a)ONT::MOLECULAR-PART]
    # < ! X[//inevent/@id=E]
    # & dependsOn X//inevent/@id
    # > ~ X[type=s/ONT::GENE/ONT::PROTEIN/]
    # > + X[not-features/ptm[@id=E]]
    # > + X1=X[features/ptm[@id=E]]
    # > + E[result:X1]
    # > + E1[type=ont::increase][affected:X1] 
    # > + C1[type=ont::cause][factor:E,outcome:E1]
    # > + E2[type=ont::decrease][affected:X]
    # > + C2[type=ont::cause][factor:E,outcome:E2]
    name => 'EKR:PTM',
    constraints => ['EVENT[not(negation) and not(mods/mod/type[.="ONT::MANNER-UNDO"]) and arg2[@role=":AFFECTED"]]'],
    handler => sub {
      my ($rule, $ekb, $e) = @_;
      my $e_id = $e->getAttribute('id');
      # < E[type(is_a)ONT::PTM]
      my $e_type = get_slot_value($e, 'type');
      $ont_events->is_a($e_type, "ONT::PTM")
	or return 0;
      # < ! E[result:*]
      assertion_args($e, '[@role=":RESULT"]')
	and return 0;
      # < E[affected:X]
      my ($arg_x) = assertion_args($e, '[@role=":AFFECTED"]');
      my $x_id = $arg_x->getAttribute('id');
      my $x = $ekb->get_assertion($x_id, 'TERM')
	or return 0;
      # < X[type(is_a)ONT::MOLECULAR-PART]
      my $x_type = get_slot_value($x, 'type');
      $ont_bioents->is_a($x_type, 'ONT::MOLECULAR-PART')
	or return 0;
      # < ! X[//inevent/@id=E]
      $x->findnodes('features/inevent[@id="'.$e_id.'"]')
	and return 0;
	
      INFO "Rule %s matches event %s (affected:%s[t=%s])", 
	$rule->name, $e_id, $x_id, $x_type;

      # & dependsOn X//inevent/@id
      map { $rule->reasoner()->dependsOn($_) }
	map { $_->value } $x->findnodes('features/inevent/@id');	

      # > ~ X[type=s/ONT::GENE/ONT::PROTEIN/]
      set_slot_value($x, 'type' => 'ONT::PROTEIN')
	if $ont_bioents->is_a('ONT::GENE', $x_type);
      # > + X1=X
      my $x1 = $ekb->clone_assertion($x, {rule => $rule->name});
      $ekb->add_assertion($x1);
      my $x1_id = $x1->getAttribute('id');
      # > + X[not-features/ptm[@id=E]]
      $ekb->add_notfeature($x, 'ptm' => { type => $e_type,
					  event => $e_id });
      # > + X1[features/ptm[@id=E]]
      $ekb->add_feature($x1, 'ptm' => { type => $e_type,
					event => $e_id });
      # > + E[result:X1]
      $ekb->add_arg($e, ':RESULT' => $x1_id);

      if ($rule->reasoner()->option('add_amount_changes')) {
	# > + E1:increase(affected:X1) 
	my $e1_id = 
	  $ekb->increases($x1_id, {refid => "$e_id", rule => $rule->name});
	# > + C1:cause(factor:E,outcome:E1)
	$ekb->causes($e_id, $e1_id, {refid => "$e_id", rule => $rule->name});
	# > + E2:decrease(affected:X)
	my $e2_id =
	  $ekb->decreases($x_id, {refid => "$e_id", rule => $rule->name});
	# > + C2:cause(factor:E,outcome:E2)
	$ekb->causes($e_id, $e2_id, {refid => "$e_id", rule => $rule->name});
      }

      1;
    }
   },

   {
    ## X {is} activated/E [by Y]
    ## => inactive X {is} activated/E [by Y] to active X
    ## X must be a physical entity (not a process, incl pathway!)
    # < E:activate(affected:X)
    # < ! E/negation
    # < ! E/arg*[result:*]
    # < X/type is_a <bio_entity>
    # < ! X//inevent[@id=E]
    # & dependsOn ^(X//inevent/@id)
    # > + X//active[FALSE]
    # > + X1=X
    # > + X1//active[TRUE]
    # > + E:*(result:X1)
    # > + E1:increase(affected:X1) 
    # > + C1:cause(factor:E,outcome:E1)
    # > + E2:decrease(affected:X)
    # > + C2:cause(factor:E,outcome:E2)
    name => "EKR:ActivateX",
    constraints => ['EVENT[type[.="ONT::ACTIVATE"] and not(negation) and arg2[@role=":AFFECTED"]]'],
    handler => sub {
      my ($rule, $ekb, $e) = @_;
      my $e_id = $e->getAttribute('id');
    
      # < X/type is_a <bio_entity>
      my $x_id = $e->findvalue('arg2/@id');
      my $x = $ekb->get_assertion($x_id, "TERM")
	or return 0;
      my $x_type = get_slot_value($x, 'type');
      $ont_bioents->is_a($x_type, 'ONT::MOLECULAR-PART')
	or return 0;
      # < ! E/arg*[result:*]
      assertion_args($e, '[@role=":RESULT"]')
	and return 0;
      # < ! X//inevent[@id=E]
      $x->findnodes('features/inevent[@id="'.$e_id.'"]')
	and return 0;

      INFO "Rule %s matches event %s (affected:%s[t=%s])", 
	$rule->name, $e_id, $x_id, $x_type;

      # check if $x is :affected by other events (? WHY ?)
      # my @affectedBy =
      #   grep { ($_->getAttribute('id') ne $e_id)
      # 	 &&
      # 	 match_node($_,
      # 		    { SX => { 'arg2' 
      # 			      =>  { AX => { 'id' => $x_id,
      # 					    'role' => ":AFFECTED" } },
      # 			    } }) }
      #   $ekb->get_assertions("EVENT");
      # WARN "%s is also affected by: (%s)",
      #   $x_id, join(" ", map {$_->getAttribute('id')} @affectedBy);

      # & dependsOn ^(X//inevent/@id)
      map { $rule->reasoner()->dependsOn($_) }
	map { $_->value } $x->findnodes('features/inevent/@id');	

      # > + X1=X
      my $x1 = $ekb->clone_assertion($x, {rule => $rule->name});
      my $x1_id = $x1->getAttribute('id');
      $ekb->add_assertion($x1);
      # > + X1:X[active=TRUE]
      $ekb->add_feature($x1, 'active' => 'TRUE');
      # > ~ X => X[active=FALSE]
      $ekb->add_feature($x, 'active' => 'FALSE');

      # > + E:*(result:X1)
      $ekb->add_arg($e, ':RESULT' => $x1_id);

      if ($rule->reasoner()->option('add_amount_changes')) {
	# > + E1:increase(affected:X1) 
	my $e1_id =
	  $ekb->increases($x1_id, {refid => "$e_id", rule => $rule->name});
	# > + C1:cause(factor:E,outcome:E1)
	$ekb->causes($e_id, $e1_id, {refid => "$e_id", rule => $rule->name});
	# > + E2:decrease(affected:X)
	my $e2_id =
	  $ekb->decreases($x_id, {refid => "$e_id", rule => $rule->name});
	# > + C2:cause(factor:E,outcome:E2)
	$ekb->causes($e_id, $e2_id, {refid => "$e_id", rule => $rule->name});
      }
      
      1;
    }
   },

   {
    ## X {is} deactivated/E [by Y]
    ## => active X {is} deactivated/E [by Y] to inactive X
    ## X must be a physical entity (not a process, incl pathway!)
    # < E:deactivate(affected:X)
    # < ! E/negation
    # < ! E/arg*[result:*]
    # < X/type is_a <bio_entity>
    # < ! X//inevent[@id=E]
    # & dependsOn ^(X//inevent/@id[.!=E])
    # > + X1:X
    # > + X//active[TRUE]
    # > + X1//active[FALSE]
    # > + E:*(result:X1)
    # > + E1:increase(affected:X1) 
    # > + C1:cause(factor:E,outcome:E1)
    # > + E2:decrease(affected:X)
    # > + C2:cause(factor:E,outcome:E2)
    name => "EKR:DeactivateX",
    constraints => ['EVENT[type[.="ONT::DEACTIVATE"] and not(negation) and arg2[@role=":AFFECTED"]]'],
    handler => sub {
      my ($rule, $ekb, $e) = @_;
      my $e_id = $e->getAttribute('id');
    
      # < X/type is_a <bio_entity>
      my $x_id = $e->findvalue('arg2/@id');
      my $x = $ekb->get_assertion($x_id, "TERM")
	or return 0;
      my $x_type = get_slot_value($x, 'type');
      $ont_bioents->is_a($x_type, 'ONT::MOLECULAR-PART')
	or return 0;
      # < ! E/arg*[result:*]
      assertion_args($e, '[@role=":RESULT"]')
	and return 0;
      # < ! X//inevent[@id=E]
      $x->findnodes('features/inevent[@id="'.$e_id.'"]')
	and return 0;

      INFO "Rule %s matches event %s (affected:%s[t=%s])", 
	$rule->name, $e_id, $x_id, $x_type;

      # check if $x is :affected by other events (? WHY ?)
      # my @affectedBy =
      #   grep { ($_->getAttribute('id') ne $e_id)
      # 	 &&
      # 	 match_node($_,
      # 		    { SX => { 'arg2' 
      # 			      =>  { AX => { 'id' => $x_id,
      # 					    'role' => ":AFFECTED" } },
      # 			    } }) }
      #   $ekb->get_assertions("EVENT");
      # WARN "%s is also affected by: (%s)",
      #   $x_id, join(" ", map {$_->getAttribute('id')} @affectedBy);

      # & dependsOn ^(X//inevent/@id)
      map { $rule->reasoner()->dependsOn($_) }
	map { $_->value } $x->findnodes('features/inevent/@id');	

      # > + X1=X
      my $x1 = $ekb->clone_assertion($x, {rule => $rule->name});
      my $x1_id = $x1->getAttribute('id');
      $ekb->add_assertion($x1);
      # > + X1/features/active[FALSE]
      $ekb->add_feature($x1, 'active' => 'FALSE');
      # > + X/features/active[TRUE]
      $ekb->add_feature($x, 'active' => 'TRUE');

      # > + E:*(result:X1)
      $ekb->add_arg($e, ':RESULT' => $x1_id);

      if ($rule->reasoner()->option('add_amount_changes')) {
	# > + E1:increase(affected:X1) 
	my $e1_id =
	  $ekb->increases($x1_id, {refid => "$e_id", rule => $rule->name});
	# > + C1:cause(factor:E,outcome:E1)
	$ekb->causes($e_id, $e1_id, {refid => "$e_id", rule => $rule->name});
	# > + E2:decrease(affected:X)
	my $e2_id =
	  $ekb->decreases($x_id, {refid => "$e_id", rule => $rule->name});
	# > + C2:cause(factor:E,outcome:E2)
	$ekb->causes($e_id, $e2_id, {refid => "$e_id", rule => $rule->name});
      }
      
      1;
    }
   },

   ## TODO: increase/decrease in ptm results in increase/decrease of the modified protein

   {# complex components are bound to one another
    # NB: this information can be readily inferrable by any reasoner from the EKB,
    # therefore we guard this rule using an option, so that, by default, it is not
    # executed
    name => "EKR:AddBindingFeatures",
    constraints => ['TERM[type[.="ONT::MACROMOLECULAR-COMPLEX"] and components]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;

      return 0 unless $rule->reasoner()->option('add_binding_features_rule');

      my $t_id = $t->getAttribute('id');

      my @comp_terms =
	grep { $ont_bioents->is_a(get_slot_value($_, 'type'), 'ONT::MOLECULAR-PART') }
	map { $ekb->get_assertion($_, "TERM") }
	map { $_->value } $t->findnodes('components/component/@id');
      DEBUG 2, "comp_terms: %s", "@comp_terms";

      my @comp_ids = map { $_->getAttribute('id') } @comp_terms;
      DEBUG 2, "comp_ids: %s", "@comp_ids";

      scalar(@comp_ids) > 1
	or return 0;

      INFO "Rule %s matches term %s (comps: %s)",
	$rule->name(), $t_id, join(", ", @comp_ids);

      foreach my $x1_id (@comp_ids) {
	foreach my $x2_id (@comp_ids) {
	  next if $x2_id eq $x1_id;
	  my ($x2) = grep { ($_->getAttribute('id') eq $x2_id) } @comp_terms;
	  DEBUG 2, "Adding bound-to feature to: %s", $x2;
	  $ekb->add_feature($x2, 'bound-to' => { id => $x1_id, rule => $rule->name() });
	}
      }
      
      1;
    }    
   },
   
  );

1;
