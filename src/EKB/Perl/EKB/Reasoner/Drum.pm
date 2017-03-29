# Drum.pm
#
# Time-stamp: <Mon Mar 20 21:10:25 CDT 2017 lgalescu>
#
# Author: Lucian Galescu <lgalescu@ihmc.us>,  1 Jun 2016
# $Id: Drum.pm,v 1.4 2017/03/21 04:16:05 lgalescu Exp $
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

#----------------------------------------------------------------
# Usage:
# 

package EKB::Reasoner::Drum;

$VERSION = '1.7.3';

use strict 'vars';
use warnings;

use Data::Dumper;
use XML::LibXML;
use List::Util qw(all any first min max uniq);

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
		 add_amount_changes => 1,
		 add_complex => 0,
		);
}

## Rules

@rules =
  (
   {
    ## remove missing references on relation arguments
    # < X:EVENT()|CC()|EPI()|MODALITY()
    # < X/arg[@id=id]
    # < ! *[@id=id]
    # > - X/arg/@id
    name => "EKR:RemoveDanglingReferences",
    constraints => [],
    handler => sub  {
      my ($rule, $ekb, $r) = @_;

      $ekb->is_relation($r)
	or return 0;

      my $r_id = $r->getAttribute('id');

      my @args = get_children_by_name_regex($r, qr{^arg});
      my $count = 0;
      foreach my $arg (@args) {
	my $a_id = $arg->getAttribute('id');
	next if $ekb->get_assertion($a_id);
	INFO "Rule %s matches %s (arg: %s)",
	  $rule->name, $r_id, $a_id;
	$arg->removeAttribute('id');
	$count++;
      }
      return $count;
    }
   },
   
   {
    ## delete empty relations (no arguments of any kind) with no referrers
    # < X:EVENT()|CC()|EPI()|MODALITY()
    # > - X
    # WARNING: removes assertions
    name => "EKR:RemoveBareReln",
    constraints => [],
    repeat => 1,
    handler => sub  {
      my ($rule, $ekb, $r) = @_;

      $ekb->is_relation($r)
	or return 0;

      my $r_id = $r->getAttribute('id');

      # check for args
      my @args = get_children_by_name_regex($r, qr{^arg|site|location|from-location|to-location|cell-line});
      return 0 unless (scalar(@args) == 0);
	
      # check for referrers (excluding self-reference)
      my @referrers =
	grep {$_->getAttribute('id') ne $r_id }
	$ekb->find_referrers($r_id);
      return 0 unless (scalar(@referrers) == 0);

      INFO("Rule %s matches %s",
	   $rule->name, $r_id);
	
      $ekb->remove_assertion($r);

      return 1;
    }
   },
   
   {
    ## delete CC relations that don't have two arguments
    # < C:CC(arg?)
    # > -C
    # WARNING: removes assertions
    name => "EKR:RemoveIncompleteCC",
    constraints => ['CC[not(arg[2])]'],
    handler => sub {
      my ($rule, $ekb, $c) = @_;

      my $c_id = $c->getAttribute('id');

      # check for args
      my @args = get_children_by_name_regex($c, qr{^arg});
      return 0 if (scalar(@args) >= 2);

      # check for referrers (excluding self-reference)
      my @referrers =
	grep {$_->getAttribute('id') ne $c_id }
	$ekb->find_referrers($c_id);
      return 0 unless (scalar(@referrers) == 0);

      INFO("Rule %s matches %s",
	   $rule->name(), $c_id);
	
      $c->parentNode->removeChild($c);

      return 1;
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
	
      return 1;
    }
   },

   {
    ## Remove dangling :inevent features (event doesn't exist or it doesn't
    ## have an arg pointing to the term)
    # < TERM[@id=$t_id and features/inevent/event[@id=$e]]
    #   ! *[@id=$e and arg*[@id=$t_id]]
    # > - TERM/features/inevent/event[@id=$e]
    name => "EKR:FixDanglingInevent",
    constraints => ['TERM[features/inevent/event]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;
	
      my $t_id = $t->getAttribute('id');
      
      my @e_ids =
	map { $_->value }
	$t->findnodes('features/inevent/event/@id');
      my $count = 0;
      foreach my $e_id (@e_ids) {
	my $e = $ekb->get_assertion($e_id);
	my @args = $e ? get_children_by_name_regex($e, qr{^arg}) : ();
	next if any { $_->getAttribute('id') eq $t_id } @args;

	INFO "Rule %s matches term %s (inevent: %s)",
	  $rule->name(), $t_id, $e_id;

	remove_elements($t, 'features/inevent/event[@id="'.$e_id.'"]');
	    
	$count++;
      }
      return $count;
    }
   },

   {
    ## when <mod><type>ONT::NEG</type><value>NOT</value></mod>
    ## flip <negation>
    # < E:*[mods/mod[type[.="ONT::NEG"] and value[.="NOT"]]]
    # > - E:/mods/mod[type[.="ONT::NEG"] and value[.="NOT"]]
    # > ~ E:negation[flip()]
    name => 'EKR:ModNegNot',
    constraints => ['*/mods/mod[type[.="ONT::NEG"] and value[.="NOT"]]'],
    handler => sub {
      my ($rule, $ekb, $r) = @_;

      $ekb->is_relation($r)
	or return 0;

      my $r_id = $r->getAttribute('id');

      INFO("Rule %s matches %s",
	   $rule->name, $r_id);

      # > ~ E:negation[flip()]
      my $neg = get_slot_value($r, 'negation');
      if (defined $neg) {
	if ($neg eq '+') {
	  $r->get_slot_value('negation', '+');
	} else {
	  $r->get_slot_value('negation', '-');
	}
      } else {
	$r->addChild(make_slot_node( negation => '+' ));
      }

      # > - E:/mods/mod[type[.="ONT::NEG"] and value[.="NOT"]]
      remove_elements($r, 'self::*/mods/mod[type[.="ONT::NEG"] and value[.="NOT"]]');

      return 1;
    }
   },
   
   {
    ## sequence term disambiguation; only for GENE-PROTEIN
    # TERM[type=ONT::GENE-PROTEIN
    #  and components[component[type=ONT::PROTEIN]
    #             and component[type<<ONT::GENE-PROTEIN] ... ]
    # =>
    # mod: TERM/type=ONT::PROTEIN
    # mod: TERM///component/type=ONT::PROTEIN
    # WARNING: adds EKB cross-refs
    name => "EKR:FixProteinTypeSeq",
    constraints => ['TERM[type[.="ONT::GENE-PROTEIN"] and components]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;
	
      # get term id
      my $t_id = $t->getAttribute('id');
      my $t_type = get_slot_value($t, 'type');

      my @comp_ids =
	map { $_->getAttribute('id') }
	$t->findnodes('components/component');

      DEBUG 3, "comp_ids: %s", "@comp_ids";

      my @comp_terms =
	map { $ekb->get_assertion($_, "TERM") }
	@comp_ids;

      DEBUG 4, "comp_terms: %s", "@comp_terms";

      all { defined($_) } @comp_terms
	or ( DEBUG(1, "At least one component of $t_id is not defined!")
	     and return 0);
    
      my @comp_types =
	uniq
	map { get_slot_value($_, 'type') }
	@comp_terms;

      DEBUG 3, "comp_types: %s", "@comp_types";
    
      # we must have at least one PROTEIN
      scalar(grep { $_ eq "ONT::PROTEIN" } @comp_types) > 0
	or return 0;

      # we must have at least one non-PROTEIN
      scalar(@comp_types) > 1
	or return 0;

      INFO("Rule %s matches term %s [type=%s, components: ]",
	   $rule->name, $t_id, $t_type, "@comp_ids");
    
      foreach my $comp_term (@comp_terms) {
	my $comp_type = get_slot_value($comp_term, 'type');
	next if ($comp_type eq "ONT::PROTEIN");
	set_slot_value($comp_term, 'type', "ONT::PROTEIN");

	INFO "changed type of %s: %s => %s",
	  $comp_term->getAttribute('id'), $comp_type, "ONT::PROTEIN";
      }
      set_slot_value($t, 'type', "ONT::PROTEIN");

      return 1;
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
      my @comp_ids =
	map { $_->getAttribute('id') }
	$comp_arg->findnodes('component');
      my @comp_terms =
	map { $ekb->get_assertion($_, "TERM") }
	@comp_ids;
      my @comp_dbids =
	map { $_->getAttribute('dbid') }
	@comp_terms;

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
      my $t_rule = $t->getAttribute('rule');
      $t->setAttribute('rule', $t_rule . "," . $rule->name());

      return 1;
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
    constraints => ['TERM[type[.="ONT::PROTEIN"] and components]'],
    handler => sub {
      my ($rule, $ekb, $t) = @_;
	
      # get term id
      my $t_id = $t->getAttribute('id');

      DEBUG 3, "%s: %s", $rule->name, $t;

      $rule->reasoner()->dependsOn($t_id, "EKR:SequenceTermEquiv");

      # must have some component
      match_node($t, { SX => { 'type' 
			       => [OP_OR,
				   "ONT::PROTEIN",
				   "ONT::GENE-PROTEIN"],
			       'components' 
			       => { AX => { 'type' => undef } }
			     } })
	or return 0;
      
      my $t_type = get_slot_value($t, 'type');
      my $t_text = get_slot_value($t, 'text');

      my @referrers = $ekb->find_referrers($t_id, "EVENT");
      {
	my @ref_info =
	  map { $_->getAttribute('id') => get_slot_value($_, 'type') }
	  @referrers;
	DEBUG 3,  "%s : referrers: %s", $rule, "@ref_info";
      }

      # check that any referrers are of the desired types
      # FIXME: we probably should check that $t_id is an arg, but, whatever
      my @ref_types =
	map { get_slot_value($_, 'type') }
	@referrers;
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

      return 1;
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
      my @locs = $t->findnodes('features/location');

      my @loc_terms =
	grep { defined $_ }
	map { $ekb->get_assertion($_, "TERM") }
	map { $_->getAttribute('id') }
	@locs;
		     
      my $start = min ( $t->getAttribute('start'),
			map { $_->getAttribute('start') }
			@loc_terms );
      my $end = max ( $t->getAttribute('end'),
		      map { $_->getAttribute('end') }
		      @loc_terms );

      # my $pid = $t->getAttribute('paragraph');
      # my $e_text = $ekb->get_span_text($pid, $start, $end);

      # TODO: perhaps, instead of making multiple events, i should make
      # an aggregate location?? but then why shouldn't the original term have
      # a single location feature pointing to the aggregate??
      foreach my $loc_term (@loc_terms) {
	my $l_id = $loc_term->getAttribute('id');
	$ekb->add_event_r( refs => [$t_id],
			   start => $start,
			   end => $end,
			   rule => $rule->name,
			   slots => [ make_slot_node(type => 'ONT::LOCALIZATION'),
				      make_predicate('ONT::BE-AT-LOC'),
				      make_arg(':NEUTRAL' => $t_id),
				      make_node("location",
						{ id => $l_id }) ] );
      }
		     
      return 1;
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
      my @mIds =
	map { $_->getAttribute('id') }
	$e->findnodes('aggregate/member');
      return 0 unless @mIds;

      INFO("Rule %s matches event %s w/ members (%s)", 
	   $rule->name(), $e_id, "@mIds");

      foreach my $mId (@mIds) {
	my $mEvent = $ekb->get_assertion($mId, "EVENT");
	DEBUG(1, "%s", $mEvent);
	next unless $mEvent;
	my ($pred) = $mEvent->findnodes('predicate');
	foreach my $epi (@epis) {
	  my $epiClone = $epi->cloneNode(1);
	  $mEvent->insertBefore($epiClone, $pred);
	  DEBUG(1, "%s", $mEvent);
	}
      }

      return 1;
    }
   },

   {
    ## Y cells express X
    # < E:express(agent:Y, affected-result:X)
    # < X:gene-protein
    # < Y:cell
    # > E/location/IN-LOC:Y
    # - E/agent
    # ?? X probably should be a PROTEIN; maybe a GENE would work too?
    name => "EKR:FixExpressByCell",
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

      return 1;
    }
   },

   {
    ## Y cells express -
    # < E:express(affected:Y)
    # < ! E/arg1
    # < Y:cell
    # > E/location/IN-LOC:Y
    # WARNING: adds EKB cross-refs
    name => "EKR:FixExpressByCellNoResult",
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

      return 1;
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

      return 1;
    }
   },

   {
    ## eg, unphosphorylated X [<does-something>]
    ## => X[not-ptm:PHOSPHORYLATION]
    # < X:*[inevent/id=E]
    # < X/type is_a ONT::MOLECULAR-PART
    # < E:*(affected:X)
    # < E/type is_a ONT::PTM 
    # < ! E/negation
    # < E/mods/mod/type[ONT::MANNER-UNDO]
    # < ! X//inevent/event[id=E]
    # > ?~ X/type[ONT::PROTEIN]
    # > + X/not-features/ptm[id=E]
    # > - X//inevent/event[id=E]
    name => 'EKR:InEventUnPTM',
    constraints => ['TERM[features/inevent]'],
    handler => sub {
      my ($rule, $ekb, $x) = @_;
      my $x_id = $x->getAttribute('id');

      # < X/type is_a ONT::MOLECULAR-PART
      my $x_type = get_slot_value($x, 'type');
      $ont_bioents->is_a($x_type, 'ONT::MOLECULAR-PART')
	or return 0;

      # < X:*[inevent/id=E]
      my @inevents =
	grep {
	  # < E/type is_a ONT::PTM 
	  $ont_events->is_a(get_slot_value($_, 'type'), 'ONT::PTM')
	    &&
	    # < ! E/negation
	    # < E/mods/mod/type[ONT::MANNER-UNDO]
	    # < E:*(affected:X)
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
	map { $_->getAttribute('id') }
	$x->findnodes('features/inevent/event');	

      unless (@inevents) {
	return 0;
      }

      my $count = 0;
      foreach my $e (@inevents) {
	my $e_id = $e->getAttribute('id');
	my $e_type = get_slot_value($e, 'type');

	INFO "Rule %s matches term %s/%s (affected-by: %s/%s)", 
	  $rule->name, $x_id, $x_type, $e_id, $e_type;

	# > ?~ X/type[.=ONT::PROTEIN]
	set_slot_value($x, 'type', 'ONT::PROTEIN')
	  if $ont_bioents->is_a('ONT::GENE', $x_type);

	# > - X//inevent/event[id=E]
	remove_elements($x, 'features/inevent/event[@id="'.$e_id.'"]');

	# > + X/not-features/ptm[id=E]
	$ekb->add_notfeature($x,'ptm' => { type => $e_type,
					   event => $e_id });
	    
	$count++;
      }
	
      return $count;
    }
   },

   {
    ## X not bound to Y
    ## => X[not-bound-to:Y]
    # < X:*[inevent/id=E]
    # < X/type is_a ONT::MOLECULAR-PART
    # < E:bind(affected:X,affected1:Y,negation:+)
    # < Y/type is_a <bio_entity>
    # < ! E/arg*[result:*]
    # > - X//inevent/event[id=E]
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
	    ! get_child_node_regex_attr($_, qr{^arg}, { role => ':RESULT' } )
	  }
	map { $ekb->get_assertion($_) }
	map { $_->getAttribute('id') }
	$x->findnodes('features/inevent/event');	

      unless (@inevents) {
	return 0;
      }

      DEBUG 2, "%s: got %d inevents: (%s)",
	$rule->name, scalar(@inevents),
	join(',', map {$_->getAttribute('id')} @inevents);
	  
      my $count = 0;
      foreach my $e (@inevents) {
	my $e_id = $e->getAttribute('id');
	my $e_type = get_slot_value($e, 'type');

	# < Y/type is_a <bio_entity>
	my $y_arg = get_child_node($e, 'arg2');
	my $y_id = $y_arg->getAttribute('id');
	my $y = $ekb->get_assertion($y_id, "TERM")
	  or next;
	my $y_type = get_slot_value($y, 'type');
	$ont_bioents->has($y_type)
	  or next;

	INFO "Rule %s matches term %s/%s in %s/%s(*, %s/%s)", 
	  $rule->name, $x_id, $x_type, $e_id, $e_type, $y_id, $y_type;

	# > - X//inevent/event[E]
	remove_elements($x, 'features/inevent/event[@id="'.$e_id.'"]');

	# > + X/not-features/bound-to[Y]
	$ekb->add_notfeature($x, 'bound-to' => { id => $y_id,
						 event => $e_id });
	# > + Y/not-features/bound-to[X] 
	$ekb->add_notfeature($y, 'bound-to' => { id => $x_id,
						 event => $e_id });
	
	$count++;
      }
      
      return $count;
    }
   },
   
   {
    ## phosphorylated X [by Y] [<does-something>]
    ## => non-phosphorylated X is phosphorylated [by Y] to phospho-X
    ##    phospho-X [<does-something>]
    # < X:*[inevent/id=E]
    # < X/type is_a ONT::MOLECULAR-PART
    # < E:*(affected:X,!negation)
    # < E/type is_a ONT::PTM 
    # < ! E/mods/mod/type[ONT::MANNER-UNDO]
    # < ! E/arg*[result:*]
    # > ?~ X/type[ONT::PROTEIN]
    # > + X/features/ptm[id=E]
    # > + X1=X[not-features/ptm[id=E]]
    # > ~ E:*(result:X)
    # > + E:*(affected:X1)
    # > - X//inevent/event[id=E]
    name => 'EKR:InEventPTM',
    constraints => ['TERM[features/inevent]'],
    handler => sub {
      my ($rule, $ekb, $x) = @_;
      my $x_id = $x->getAttribute('id');

      # < X/type is_a ONT::MOLECULAR-PART
      my $x_type = get_slot_value($x, 'type');
      $ont_bioents->is_a($x_type, 'ONT::MOLECULAR-PART')
	or return 0;

      # < X:*[inevent/id=E]
      my @inevents =
	grep {
	  # < E/type is_a ONT::PTM 
	  $ont_events->is_a(get_slot_value($_, 'type'), 'ONT::PTM')
	    &&
	    # < ! E/mods/mod/type[ONT::MANNER-UNDO]
	    # < E:*(affected:X, !negation)
	    match_node($_, { SX => { 'negation' 
				     => [OP_NOT, '+'],
				     'mods/mod/type' 
				     => [OP_NOT, 'ONT::MANNER-UNDO'],
				     'arg2' 
				     => { AX => { 'role' => ':AFFECTED',
						  'id' => $x_id } }
				   } })
	    &&
	    # < ! E/arg*[result:*]
	    ! get_child_node_regex_attr($_, qr{^arg}, { role => ':RESULT' } )
	  }
	map { $ekb->get_assertion($_) }
	map { $_->getAttribute('id') }
	$x->findnodes('features/inevent/event');	

      unless (@inevents) {
	return 0;
      }

      my $count = 0;
      foreach my $e (@inevents) {
	my $e_id = $e->getAttribute('id');
	my $e_type = get_slot_value($e, 'type');

	INFO "Rule %s matches term %s/%s (affected-by: %s/%s)", 
	  $rule->name, $x_id, $x_type, $e_id, $e_type;

	# > ?~ X/type[.=ONT::PROTEIN]
	set_slot_value($x, 'type', 'ONT::PROTEIN')
	  if $ont_bioents->is_a('ONT::GENE', $x_type);

	# > - X//inevent/event[id=E]
	remove_elements($x, 'features/inevent/event[@id="'.$e_id.'"]');

	# > + X1=X
	my $x1 = $ekb->clone_assertion($x, {rule => $rule->name});
	my $x1_id = $x1->getAttribute('id');
	$ekb->add_assertion($x1);

	# > + X/features/ptm[id=E]
	$ekb->add_feature($x, 'ptm' => { type => $e_type,
					 event => $e_id });
	# > + X1/not-features/ptm[id=E]
	$ekb->add_notfeature($x1, 'ptm' => { type => $e_type,
					     event => $e_id });
	# > ~ E:*(result:X)
	my $x_arg = get_child_node($e, 'arg2');
	$x_arg->setAttribute('role', ':RESULT');

	# > + E:*(affected:X1)
	$ekb->add_arg($e, ':AFFECTED' => $x1_id);

	$count++;
      }
	
      return $count;
    }
   },

   {
    ## X bound/E to Y 
    ## => unbound X binds to unbound Y to form X/Y complex
    # < X:*[inevent/id=E]
    # < X/type is_a ONT::MOLECULAR-PART
    # < E:bind(r1:X,r2:Y,!negation)
    # < r1=AGENT && r2=AFFECTED || r1=AFFECTED && r2=AFFECTED1
    # < Y/type is_a <bio_entity>
    # < ! E/arg*[result:*]
    # < ! E/mods/mod/type[ONT::MANNER-UNDO]
    # ? & dependsOn ^(X//inevent/event/id[!E])
    # ? & dependsOn ^(Y//inevent/event/id[!E])
    # > - X//inevent/event[id=E]
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
    name => "EKR:InEventBind",
    constraints => ['TERM[features/inevent]'],
    handler => sub {
      my ($rule, $ekb, $x) = @_;
      my $x_id = $x->getAttribute('id');

      # < X/type is_a ONT::MOLECULAR-PART
      my $x_type = get_slot_value($x, 'type');
      $ont_bioents->is_a($x_type, 'ONT::MOLECULAR-PART')
	or return 0;
      my $x_name = get_slot_value($x, "name") // "";

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
				     => { AX => { 'role' => [OP_OR,
							     ':AGENT',
							     ':AFFECTED'],
						  'id' => $x_id  } },
				     'arg2' 
				     => { AX => { 'role' => [OP_OR,
							     ':AFFECTED',
							     ':AFFECTED1']} }
				   } })
	    &&
	    # < ! E/arg*[result:*]
	    ! get_child_node_regex_attr($_, qr{^arg}, { role => ':RESULT' } )
	  }
	map { $ekb->get_assertion($_) }
	map { $_->getAttribute('id') }
	$x->findnodes('features/inevent/event');	

      unless (@inevents) {
	return 0;
      }

      DEBUG 2, "%s: got %d inevents: (%s)",
	$rule->name, scalar(@inevents),
	join(',', map {$_->getAttribute('id')} @inevents);
	  
      my $uttid = $x->getAttribute('uttnum');

      my $count = 0;
      foreach my $e (@inevents) {
	my $e_id = $e->getAttribute('id');
	my $e_type = get_slot_value($e, 'type');

	# < Y/type is_a <bio_entity>
	my $y_arg = get_child_node($e, 'arg2');
	my $y_id = $y_arg->getAttribute('id');
	my $y = $ekb->get_assertion($y_id, "TERM")
	  or next;
	my $y_name = get_slot_value($y, "name") // "";
	my $y_type = get_slot_value($y, 'type');
	$ont_bioents->has($y_type)
	  or next;

	INFO "Rule %s matches term %s/%s in %s/%s(*, %s/%s)", 
	  $rule->name, $x_id, $x_type, $e_id, $e_type, $y_id, $y_type;

	# > - X//inevent/event[E]
	remove_elements($x, 'features/inevent/event[@id="'.$e_id.'"]');

	# > + X1=X
	my $x1 = $ekb->clone_assertion($x, {rule => $rule->name});
	my $x1_id = $x1->getAttribute('id');
	$ekb->add_assertion($x1);

	# > + Y1=Y
	my $y1 = $ekb->clone_assertion($y, {rule => $rule->name});
	my $y1_id = $y1->getAttribute('id');
	$ekb->add_assertion($y1);

	# > + X1/not-features/bound-to[Y]
	$ekb->add_notfeature($x1, 'bound-to' => { id => $y_id,
						  event => $e_id });
	# > + Y/not-features/bound-to[X1] 
	$ekb->add_notfeature($y, 'bound-to' => { id => $x1_id,
						 event => $e_id });
	# > + X/features/bound-to[Y1]
	$ekb->add_feature($x, 'bound-to' => { id => $y1_id,
					      event => $e_id });
	# > + Y1/features/bound-to[X]
	$ekb->add_feature($y1, 'bound-to' => { id => $x_id,
					       event => $e_id });

	# > + Z:MACROMOLECULAR-COMPLEX(components:(X,Y1))
	my $z_name =
	  ($x_name && $y_name)
	  ? $x_name . "/" . $y_name 
	  : "COMPLEX";
	my $z_id =
	  $ekb->add_term_r( refs => [ $e_id, $x_id, $y1_id ],
			    rule => $rule->name,
			    slots => [ make_slot_nodes(type => 'ONT::MACROMOLECULAR-COMPLEX',
						       name => $z_name),
				       make_node("components",
						 make_node("component",
							   { id => $x_id }),
						 make_node("component",
							   { id => $y1_id })) ]
			  );

	# > ~ E:bind(r1:Y,r2:X1,result:Z)
	my $x_arg = get_child_node($e, "arg1");
	$x_arg->setAttribute('id', $x1_id);
	$ekb->add_arg($e, ':RESULT' => $z_id);

	if ($rule->reasoner()->option('add_amount_changes')) {
	  
	  # > + E1:increase(affected:Z)
	  my $e1_id =
	    $ekb->add_event_r( refs => [$e_id],
			       rule => $rule->name,
			       slots => [ make_slot_node(type => 'ONT::INCREASE'), 
					  make_arg(':AFFECTED' => $z_id) ] );
	  # > + C1:cause(factor:E,outcome:E1)
	  $ekb->add_cause_r( refs => [$e_id],
			     rule => $rule->name,
			     slots => [ make_arg(':FACTOR' => $e_id),
					make_arg(':OUTCOME' => $e1_id) ] );
	  # > + E2:decrease(affected:X1)
	  my $e2_id =
	    $ekb->add_event_r( refs => [$e_id],
			       rule => $rule->name,
			       slots => [ make_slot_node(type => 'ONT::DECREASE'), 
					  make_arg(':AFFECTED' => $x1_id) ] );
	  # > + C2:cause(factor:E,outcome:E2)
	  $ekb->add_cause_r( refs => [$e_id],
			     rule => $rule->name,
			     slots => [ make_arg(':FACTOR' => $e_id),
					make_arg(':OUTCOME' => $e2_id) ] );
	  # > + E3:decrease(affected:Y)
	  my $e3_id =
	    $ekb->add_event_r( refs => [$e_id],
			       rule => $rule->name,
			       slots => [ make_slot_node(type => "ONT::DECREASE"), 
					  make_arg(':AFFECTED' => $y_id) ] );
	  # > + C3:cause(factor:E,outcome:E3)
	  $ekb->add_cause_r( refs => [$e_id],
			     rule => $rule->name,
			     slots => [ make_arg(':FACTOR' => $e_id),
					make_arg(':OUTCOME' => $e3_id) ] );
	}

	$count++;
      }

      return $count;
    }
   },

   {
    ## Y-bound X 
    ## => unbound X binds to unbound Y to form X/Y complex
    # < X:*[inevent/id=E]
    # < X/type is_a ONT::MOLECULAR-PART
    # < E:bind(r1:Y,r2:X,!negation)
    # < r1=AGENT && r2=AFFECTED 
    # < Y/type is_a <bio_entity>
    # < ! E/arg*[result:*]
    # < ! E/mods/mod/type[ONT::MANNER-UNDO]
    # ? & dependsOn ^(X//inevent/event/id[!E])
    # ? & dependsOn ^(Y//inevent/event/id[!E])
    # > - X//inevent/event[id=E]
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
    constraints => ['TERM[features/inevent]'],
    handler => sub {
      my ($rule, $ekb, $x) = @_;
      my $x_id = $x->getAttribute('id');

      # < X/type is_a ONT::MOLECULAR-PART
      my $x_type = get_slot_value($x, 'type');
      $ont_bioents->is_a($x_type, 'ONT::MOLECULAR-PART')
	or return 0;
      my $x_name = get_slot_value($x, "name") // "";

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
				     'arg2' 
				     => { AX => { 'role' => ':AFFECTED',
						  'id' => $x_id  } },
				     'arg1' 
				     => { AX => { 'role' => ':AGENT'} }
				   } })
	    &&
	    # < ! E/arg*[result:*]
	    ! get_child_node_regex_attr($_, qr{^arg}, { role => ':RESULT' } )
	  }
	map { $ekb->get_assertion($_) }
	map { $_->getAttribute('id') }
	$x->findnodes('features/inevent/event');	

      unless (@inevents) {
	return 0;
      }

      DEBUG 2, "%s: got %d inevents: (%s)",
	$rule->name, scalar(@inevents),
	join(',', map {$_->getAttribute('id')} @inevents);
	  
      my $uttid = $x->getAttribute('uttnum');

      my $count = 0;
      foreach my $e (@inevents) {
	my $e_id = $e->getAttribute('id');
	my $e_type = get_slot_value($e, 'type');

	# < Y/type is_a <bio_entity>
	my $y_arg = get_child_node($e, 'arg1');
	my $y_id = $y_arg->getAttribute('id');
	my $y = $ekb->get_assertion($y_id, "TERM")
	  or next;
	my $y_name = get_slot_value($y, "name") // "";
	my $y_type = get_slot_value($y, 'type');
	$ont_bioents->has($y_type)
	  or next;

	INFO "Rule %s matches term %s/%s in %s/%s(*, %s/%s)", 
	  $rule->name, $x_id, $x_type, $e_id, $e_type, $y_id, $y_type;

	# > - X//inevent/event[E]
	remove_elements($x, 'features/inevent/event[@id="'.$e_id.'"]');

	# > + X1=X
	my $x1 = $ekb->clone_assertion($x, {rule => $rule->name});
	my $x1_id = $x1->getAttribute('id');
	$ekb->add_assertion($x1);

	# > + Y1=Y
	my $y1 = $ekb->clone_assertion($y, {rule => $rule->name});
	my $y1_id = $y1->getAttribute('id');
	$ekb->add_assertion($y1);

	# > + X1/not-features/bound-to[Y]
	$ekb->add_notfeature($x1, 'bound-to' => { id => $y_id,
						  event => $e_id });
	# > + Y/not-features/bound-to[X1] 
	$ekb->add_notfeature($y, 'bound-to' => { id => $x1_id,
						 event => $e_id });
	# > + X/features/bound-to[Y1]
	$ekb->add_feature($x, 'bound-to' => { id => $y1_id,
					      event => $e_id });
	# > + Y1/features/bound-to[X]
	$ekb->add_feature($y1, 'bound-to' => { id => $x_id,
					       event => $e_id });

	# > + Z:MACROMOLECULAR-COMPLEX(components:(X,Y1))
	my $z_name =
	  ($x_name && $y_name)
	  ? $x_name . "/" . $y_name 
	  : "COMPLEX";
	my $z_id =
	  $ekb->add_term_r( refs => [ $e_id, $x_id, $y1_id ],
			    rule => $rule->name,
			    slots => [ make_slot_nodes(type => "ONT::MACROMOLECULAR-COMPLEX",
						       name => $z_name),
				       make_node("components",
						 make_node("component",
							   { id => $x_id }),
						 make_node("component",
							   { id => $y1_id })) ]
			  );

	# > ~ E:bind(r1:Y,r2:X1,result:Z)
	my $x_arg = get_child_node($e, "arg2");
	$x_arg->setAttribute('id', $x1_id);
	$ekb->add_arg($e, ':RESULT' => $z_id);

	if ($rule->reasoner()->option('add_amount_changes')) {
	  
	  # > + E1:increase(affected:Z)
	  my $e1_id =
	    $ekb->add_event_r( refs => [$e_id],
			       rule => $rule->name,
			       slots => [ make_slot_node(type => 'ONT::INCREASE'), 
					  make_arg(':AFFECTED' => $z_id) ] );
	  # > + C1:cause(factor:E,outcome:E1)
	  $ekb->add_cause_r( refs => [$e_id],
			     rule => $rule->name,
			     slots => [ make_arg(':FACTOR' => $e_id),
					make_arg(':OUTCOME' => $e1_id) ] );
	  # > + E2:decrease(affected:X1)
	  my $e2_id =
	    $ekb->add_event_r( refs => [$e_id],
			       rule => $rule->name,
			       slots => [ make_slot_node(type => 'ONT::DECREASE'), 
					  make_arg(':AFFECTED' => $x1_id) ] );
	  # > + C2:cause(factor:E,outcome:E2)
	  $ekb->add_cause_r( refs => [$e_id],
			     rule => $rule->name,
			     slots => [ make_arg(':FACTOR' => $e_id),
					make_arg(':OUTCOME' => $e2_id) ] );
	  # > + E3:decrease(affected:Y)
	  my $e3_id =
	    $ekb->add_event_r( refs => [$e_id],
			       rule => $rule->name,
			       slots => [ make_slot_node(type => 'ONT::DECREASE'), 
					  make_arg(':AFFECTED' => $y_id) ] );
	  # > + C3:cause(factor:E,outcome:E3)
	  $ekb->add_cause_r( refs => [$e_id],
			     rule => $rule->name,
			     slots => [ make_arg(':FACTOR' => $e_id),
					make_arg(':OUTCOME' => $e3_id) ] );
	}

	$count++;
      }
	
      return $count;
    }
   },
   
   {
    ## activated X
    # < X/inevent[E]
    # < X/type is_a ONT::MOLECULAR-PART
    # < E:activate(affected:X,!negation)
    # < ! E/arg*[result:*]
    # > - X//inevent/event[E]
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
      my $x_name = get_slot_value($x, "name") // "";

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
	    ! get_child_node_regex_attr($_, qr{^arg}, { role => ':RESULT' } )
	  }
	map { $ekb->get_assertion($_) }
	map { $_->getAttribute('id') }
	$x->findnodes('features/inevent/event');	

      unless (@inevents) {
	return 0;
      }

      my $uttid = $x->getAttribute('uttnum');

      my $count = 0;
      foreach my $e (@inevents) {
	my $e_id = $e->getAttribute('id');
	my $e_type = get_slot_value($e, 'type');

	INFO "Rule %s matches term %s/%s in %s/%s(*)", 
	  $rule->name, $x_id, $x_type, $e_id, $e_type;

	# > - X//inevent/event[E]
	remove_elements($x, 'features/inevent/event[@id="'.$e_id.'"]');

	# > + X1=X[active[FALSE]]
	my $x1 = $ekb->clone_assertion($x, {rule => $rule->name});
	my $x1_id = $x1->getAttribute('id');
	$ekb->add_feature($x1, 'active' => 'FALSE');
	$ekb->add_assertion($x1);

	# > ~ X//active[TRUE]
	$ekb->add_feature($x, 'active' => 'TRUE');

	# > + E:*(affected:X1, result:X)
	my $x_arg = get_child_node($e, 'arg2');
	$x_arg->setAttribute('role', ':RESULT');
	$ekb->add_arg($e, ':AFFECTED' => $x1_id);

	if ($rule->reasoner()->option('add_amount_changes')) {

	  # > + E1:increase(affected:X) 
	  my $e1_id =
	    $ekb->add_event_r( refs => [$e_id],
			       rule => $rule->name,
			       slots => [ make_slot_node(type => 'ONT::INCREASE'), 
					  make_arg(':AFFECTED' => $x_id) ] );
	  # > + C1:cause(factor:E,outcome:E1)
	  $ekb->add_cause_r( refs => [$e_id],
			     rule => $rule->name,
			     slots => [ make_arg(':FACTOR' => $e_id),
					make_arg(':OUTCOME' => $e1_id) ] );
	  # > + E2:decrease(affected:X1)
	  my $e2_id =
	    $ekb->add_event_r( refs => [$e_id],
			       rule => $rule->name,
			       slots => [ make_slot_node(type => 'ONT::DECREASE'), 
					  make_arg(':AFFECTED' => $x1_id) ] );
	  # > + C2:cause(factor:E,outcome:E2)
	  $ekb->add_cause_r( refs => [$e_id],
			     rule => $rule->name,
			     slots => [ make_arg(':FACTOR' => $e_id),
					make_arg(':OUTCOME' => $e2_id) ] );
	}

	$count++;
      }
	
      return $count;
    }
   },
   
   {
    ## eg, X1 and X2 interact; interaction between X1 and X2
    ## => X1 and X2 bind to form X1/X2 complex
    # < E:BIND(agent:T, !affected, !site)
    # < ! E/mods/mod/type[ONT::MANNER-UNDO]
    # < T:TERM[aggregate[AND(X1,X2,...)]]
    # < T/type is_a <bio_entity>
    # & dependsOn ^(X21//inevent/event/id) ...
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
      get_child_node_regex_attr($e, qr{^arg}, { role => ":RESULT" } )
	and return 0;

      # < T/type is_a <bio_entity>
      my $t_arg = get_child_node($e, 'arg1');
      my $t_id = $t_arg->getAttribute('id');
      my $t = $ekb->get_assertion($t_id, "TERM")
	or return 0;
      my $t_name = get_slot_value($t, 'name') // "";
      my $t_type = get_slot_value($t, 'type');
      $ont_bioents->has($t_type)
	or return 0;

      # < Y:TERM[aggregate[AND(X1,X2,...)]]
      match_node($t, { SX => { 'aggregate'
			       => { AX => { 'operator' => "AND" } }
			      } })
	or return 0;

      INFO "Rule %s matches event %s (affected:%s/%s)", 
	$rule->name, $e_id, $t_id, $t_type;

      # & dependsOn ^(X1//inevent/event/id) ...
      map { $rule->reasoner()->dependsOn($_) }
	map { $_->getAttribute('id') }
	map { $_->findnodes('features/inevent/event') }
	map { $ekb->get_assertion($_, "TERM") }
	map { $_->getAttribute('id') }
	$t->findnodes('aggregate/member');
      
      # get aggregate member ids
      my @x_ids =
	map { $_->getAttribute('id') }
	$t->findnodes('aggregate/member');
      return 0 unless @x_ids;

      DEBUG 1, "Got %d members", scalar(@x_ids);

      my @x1_ids = ();
      foreach my $x_id (@x_ids) {
	# > + E/affected:X1,...
	$ekb->add_arg($e, ':AFFECTED' => $x_id);

	# > + X1_n=X_n, ...
	my $x = $ekb->get_assertion($x_id, "TERM")
	  or next; # TODO: maybe infer a term here?
	my $x1 = $ekb->clone_assertion($x, {rule => $rule->name});
	my $x1_id = $x1->getAttribute('id');
	$ekb->add_assertion($x1);
	push @x1_ids, $x1_id;
      }

      # > - E/agent:T
      $e->removeChild($t_arg);
      # FIXME: this ought to be handled automatically, but needs
      # change in Reasoner control mecanism
      $rule->reasoner()->dependsOn($t_id, "EKR:RemoveOrphanTerm");
      
      # > + Z:MACROMOLECULAR-COMPLEX(components:(X1,Y1))
      my @x_names =
	map { get_slot_value($_, "name") }
	map { $ekb->get_assertion($_, "TERM") }
	@x_ids;
      my $z_name =
	(grep { ! defined($_) } @x_names)
	? "COMPLEX"
	: join("/", @x_names);
      # components
      my @comp_nodes
	= map { make_node("component", { id => $_ }) } @x1_ids;

      my $z_id =
      	$ekb->add_term_r( refs => [$e_id, @x1_ids ],
      			  rule => $rule->name,
      			  slots => [ make_slot_nodes(type => 'ONT::MACROMOLECULAR-COMPLEX',
						     name => $z_name),
      				     make_node("components", @comp_nodes) ]
      			);

      $ekb->add_arg($e, ':RESULT' => $z_id);

      if ($rule->reasoner()->option('add_amount_changes')) {
	  
	# add: E1:increase(affected:Z)
	my $e1_id =
	  $ekb->add_event_r( refs => [$e_id],
			     rule => $rule->name,
			     slots => [ make_slot_node(type => 'ONT::INCREASE'), 
					make_arg(':AFFECTED' => $z_id) ] );
	# add: C1:cause(factor:E,outcome:E1)
	$ekb->add_cause_r( refs => [$e_id],
			   rule => $rule->name,
			   slots => [ make_arg(':FACTOR' => $e_id),
				      make_arg(':OUTCOME' => $e1_id) ] );

	foreach my $x1_id (@x1_ids) {
	  # add: E2:decrease(affected:X1)
	  my $e2_id =
	    $ekb->add_event_r( refs => [$e_id],
			       rule => $rule->name,
			       slots => [ make_slot_node(type => 'ONT::DECREASE'), 
					  make_arg(':AFFECTED' => $x1_id) ] );
	  # add: C2:cause(factor:E,outcome:E2)
	  $ekb->add_cause_r( refs => [$e_id],
			     rule => $rule->name,
			     slots => [ make_arg(':FACTOR' => $e_id),
					make_arg(':OUTCOME' => $e2_id) ] );

	}
      }

      return 1;
    }
   },

   {
    ## eg, X1 and X2 associate; association between X1 and X2
    ## => X1 and X2 bind to form X1/X2 complex
    # < E:BIND(affected:T, !agent, !site)
    # < ! E/mods/mod/type[ONT::MANNER-UNDO]
    # < T:TERM[aggregate[AND(X1,X2,...)]]
    # < T/type is_a <bio_entity>
    # & dependsOn ^(X21//inevent/event/id) ...
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
      get_child_node_regex_attr($e, qr{^arg}, { role => ":RESULT" } )
	and return 0;

      # < T/type is_a <bio_entity>
      my $t_arg = get_child_node($e, 'arg2');
      my $t_id = $t_arg->getAttribute('id');
      my $t = $ekb->get_assertion($t_id, "TERM")
	or return 0;
      my $t_name = get_slot_value($t, 'name') // "";
      my $t_type = get_slot_value($t, 'type');
      $ont_bioents->has($t_type)
	or return 0;

      # < Y:TERM[aggregate[AND(X1,X2,...)]]
      match_node($t, { SX => { 'aggregate'
			       => { AX => { 'operator' => "AND" } }
			      } })
	or return 0;

      INFO "Rule %s matches event %s (affected:%s/%s)", 
	$rule->name, $e_id, $t_id, $t_type;

      # & dependsOn ^(X1//inevent/event/id) ...
      map { $rule->reasoner()->dependsOn($_) }
	map { $_->getAttribute('id') }
	map { $_->findnodes('features/inevent/event') }
	map { $ekb->get_assertion($_, "TERM") }
	map { $_->getAttribute('id') }
	$t->findnodes('aggregate/member');
      
      # get aggregate member ids
      my @x_ids =
	map { $_->getAttribute('id') }
	$t->findnodes('aggregate/member');
      return 0 unless @x_ids;

      DEBUG 1, "Got %d members", scalar(@x_ids);

      my @x1_ids = ();
      foreach my $x_id (@x_ids) {
	# > + E/affected:X1,...
	$ekb->add_arg($e, ':AFFECTED' => $x_id);

	# > + X1_n=X_n, ...
	my $x = $ekb->get_assertion($x_id, "TERM")
	  or next; # TODO: maybe infer a term here?
	my $x1 = $ekb->clone_assertion($x, {rule => $rule->name});
	my $x1_id = $x1->getAttribute('id');
	$ekb->add_assertion($x1);
	push @x1_ids, $x1_id;
      }

      # > - E/agent:T
      $e->removeChild($t_arg);
      # FIXME: this ought to be handled automatically, but needs
      # change in Reasoner control mecanism
      $rule->reasoner()->dependsOn($t_id, "EKR:RemoveOrphanTerm");

      # > + Z:MACROMOLECULAR-COMPLEX(components:(X1,Y1))
      my @x_names =
	map { get_slot_value($_, "name") }
	map { $ekb->get_assertion($_, "TERM") }
	@x_ids;
      my $z_name =
	(grep { ! defined($_) } @x_names)
	? "COMPLEX"
	: join("/", @x_names);
      # components
      my @comp_nodes
	= map { make_node("component", { id => $_ }) } @x1_ids;

      my $z_id =
      	$ekb->add_term_r( refs => [$e_id, @x1_ids ],
      			  rule => $rule->name,
      			  slots => [ make_slot_nodes(type => 'ONT::MACROMOLECULAR-COMPLEX',
						     name => $z_name),
      				     make_node("components", @comp_nodes) ]
      			);

      $ekb->add_arg($e, ':RESULT' => $z_id);

      if ($rule->reasoner()->option('add_amount_changes')) {
	  
	# add: E1:increase(affected:Z)
	my $e1_id =
	  $ekb->add_event_r( refs => [$e_id],
			     rule => $rule->name,
			     slots => [ make_slot_node(type => 'ONT::INCREASE'), 
					make_arg(':AFFECTED' => $z_id) ] );
	# add: C1:cause(factor:E,outcome:E1)
	$ekb->add_cause_r( refs => [$e_id],
			   rule => $rule->name,
			   slots => [ make_arg(':FACTOR' => $e_id),
				      make_arg(':OUTCOME' => $e1_id) ] );

	foreach my $x1_id (@x1_ids) {
	  # add: E2:decrease(affected:X1)
	  my $e2_id =
	    $ekb->add_event_r( refs => [$e_id],
			       rule => $rule->name,
			       slots => [ make_slot_node(type => 'ONT::DECREASE'), 
					make_arg(':AFFECTED' => $x1_id) ] );
	  # add: C2:cause(factor:E,outcome:E2)
	  $ekb->add_cause_r( refs => [$e_id],
			     rule => $rule->name,
			     slots => [ make_arg(':FACTOR' => $e_id),
					make_arg(':OUTCOME' => $e2_id) ] );

	}
      }

      return 1;
    }
   },
   
   {
    ## X binds to Y, X is bound to Y
    ## => X not bound to Y and Y not bound to X bind to form an X/Y complex
    # < E:bind(r1:X,r2:Y,!negation)
    # < ! E/mods/mod/type[ONT::MANNER-UNDO]
    # < r1=AGENT && r2=AFFECTED || r1=AFFECTED && r2=AFFECTED1
    # < X/type is_a <bio_entity>
    # < Y/type is_a <bio_entity>
    # < ! arg*(result:*)
    # < ! X//inevent/event[id=E]
    # < ! Y//inevent/event[id=E]
    # & dependsOn ^(X//inevent/event/id)
    # & dependsOn ^(Y//inevent/event/id)
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
      get_child_node_regex_attr($e, qr{^arg}, { role => ":RESULT" } )
	and return 0;

      # < X/type is_a <bio_entity>
      my $x_arg = get_child_node($e, 'arg1');
      my $x_id = $x_arg->getAttribute('id');
      my $x = $ekb->get_assertion($x_id, "TERM")
	or return 0;
      my $x_name = get_slot_value($x, 'name') // "";
      my $x_type = get_slot_value($x, 'type');
      $ont_bioents->has($x_type)
	or return 0;
      # < ! X/features/inevent/event[id=E]
      $x->findnodes('features/inevent/event[@id="'.$e_id.'"]')
	and return 0;

      # < Y/type is_a <bio_entity>
      my $y_arg = get_child_node($e, "arg2");
      my $y_id = $y_arg->getAttribute('id');
      my $y = $ekb->get_assertion($y_id, "TERM")
	or return 0;
      my $y_name = get_slot_value($y, "name") // "";
      my $y_type = get_slot_value($y, 'type');
      $ont_bioents->has($y_type)
	or return 0;
      # < ! Y/features/inevent/event[id=E]
      $y->findnodes('features/inevent/event[@id="'.$e_id.'"]')
	and return 0;
  
      INFO "Rule %s matches event %s w/ args (%s/%s, %s/%s)", 
	$rule->name, $e_id, $x_id, $x_name, $y_id, $y_name;

      # & dependsOn ^(X/features/inevent/event/id)
      map { $rule->reasoner()->dependsOn($_) }
	map { $_->getAttribute('id') }
	$x->findnodes('features/inevent/event');	
	  
      # & dependsOn ^(Y/features/inevent/event/id)
      map { $rule->reasoner()->dependsOn($_) }
	map { $_->getAttribute('id') }
	$y->findnodes('features/inevent/event');
      
      my $z_id; 
      if ($rule->reasoner()->option('add_complex')) {
	# > + Z:MACROMOLECULAR-COMPLEX(components:(X1,Y1))
	$z_id =
	  $ekb->add_complex_r( comps => [ $x_id, $y_id ],
			       refs => [ $e_id ],
			       rule => $rule->name,
			     );
	# > ~ X[not-features/bound-to:Y]
	$ekb->add_notfeature($x, 'bound-to' => { id => $y_id,
						 event => $e_id });
	# > ~ Y[not-features/bound-to:X] 
	$ekb->add_notfeature($y, 'bound-to' => { id => $x_id,
						 event => $e_id });
      } else {
	
      # add: X1=X
      my $x1 = $ekb->clone_assertion($x, {rule => $rule->name});
      my $x1_id = $x1->getAttribute('id');
      # remove inevent E feature, if there
      #remove_elements($x1, 'features/inevent/event[@id="'.$e_id.'"]');
      $ekb->add_assertion($x1);
    
      # > + Y1=Y
      my $y1 = $ekb->clone_assertion($y, {rule => $rule->name});
      my $y1_id = $y1->getAttribute('id');
      # remove inevent E feature, if there
      #remove_elements($y1, 'features/inevent/event[@id="'.$e_id.'"]');
      $ekb->add_assertion($y1);

      # > + X1=X[bound-to:Y1]
      $ekb->add_feature($x1, 'bound-to' => { id => $y1_id,
					     event => $e_id });
      # > + Y1/features/bound-to[id=X1]
      $ekb->add_feature($y1, 'bound-to' => { id => $x1_id,
					     event => $e_id });
      # > ~ X[not-features/bound-to:Y]
      $ekb->add_notfeature($x, 'bound-to' => { id => $y_id,
					       event => $e_id });
      # > ~ Y[not-features/bound-to:X] 
      $ekb->add_notfeature($y, 'bound-to' => { id => $x_id,
					       event => $e_id });

      # > + Z:MACROMOLECULAR-COMPLEX(components:(X1,Y1))
      my $z_name =
	($x_name && $y_name)
	? $x_name . "/" . $y_name 
	: "COMPLEX";
      $z_id =
      	$ekb->add_term_r( refs => [$e_id, $x1_id, $y1_id ],
      			  rule => $rule->name,
      			  slots => [ make_slot_nodes(type => "ONT::MACROMOLECULAR-COMPLEX",
						     name => $z_name),
      				     make_node("components",
      					       make_node("component",
      							 {
      							  id => $x1_id }),
      					       make_node("component",
      							 {
      							  id => $y1_id })) ]
      			);

    }
      
      # mod: E:bind(agent:X,affected:Y,result:Z)
      $ekb->add_arg($e, ':RESULT' => $z_id);

      if ($rule->reasoner()->option('add_amount_changes')) {
	  
	# add: E1:increase(affected:Z)
	my $e1_id =
	  $ekb->add_event_r( refs => [$e_id],
			     rule => $rule->name,
			     slots => [ make_slot_node(type => 'ONT::INCREASE'), 
					make_arg(':AFFECTED' => $z_id) ] );
	# add: C1:cause(factor:E,outcome:E1)
	$ekb->add_cause_r( refs => [$e_id],
			   rule => $rule->name,
			   slots => [ make_arg(':FACTOR' => $e_id),
				      make_arg(':OUTCOME' => $e1_id) ] );
	# add: E2:decrease(affected:X)
	my $e2_id =
	  $ekb->add_event_r( refs => [$e_id],
			     rule => $rule->name,
			     slots => [ make_slot_node(type => 'ONT::DECREASE'), 
					make_arg(':AFFECTED' => $x_id) ] );
	# add: C2:cause(factor:E,outcome:E2)
	$ekb->add_cause_r( refs => [$e_id],
			   rule => $rule->name,
			   slots => [ make_arg(':FACTOR' => $e_id),
				      make_arg(':OUTCOME' => $e2_id) ] );
	# add: E3:decrease(affected:Y)
	my $e3_id =
	  $ekb->add_event_r( refs => [$e_id],
			     rule => $rule->name,
			     slots => [ make_slot_node(type => 'ONT::DECREASE'), 
					make_arg(':AFFECTED' => $y_id) ] );
	# add: C3:cause(factor:E,outcome:E3)
	$ekb->add_cause_r( refs => [$e_id],
			   rule => $rule->name,
			   slots => [ make_arg(':FACTOR' => $e_id),
				      make_arg(':OUTCOME' => $e3_id) ] );
      }

      return 1;
    }
   },

   {
    ## PTMs: eg, X is phosphorylated [by Y] 
    ## => non-phosphorylated X is phosphorylated [by Y] to phospho-X
    # < E:*(affected:X)
    # < E/type is_a ONT::PTM 
    # < ! E/negation
    # < ! E/mods/mod/type[ONT::MANNER-UNDO]
    # < ! E/arg*[result:*]
    # < X/type is_a ONT::MOLECULAR-PART
    # < ! X//inevent/event[id=E]
    # & dependsOn ^(X//inevent/event/id)
    # > ?~ X/type[ONT::PROTEIN]
    # > + X/not-features/ptm[id=E]
    # > + X1=X[features/ptm[id=E]]
    # > + E:*(result:X1)
    # > + E1:increase(affected:X1) 
    # > + C1:cause(factor:E,outcome:E1)
    # > + E2:decrease(affected:X)
    # > + C2:cause(factor:E,outcome:E2)
    name => 'EKR:PTM',
    constraints => ['EVENT[not(negation) and not(mods/mod/type[.="ONT::MANNER-UNDO"]) and arg2[@role=":AFFECTED"]]'],
    handler => sub {
      my ($rule, $ekb, $e) = @_;
      my $e_id = $e->getAttribute('id');
      # < E/type is_a ONT::PTM 
      my $e_type = get_slot_value($e, 'type');
      $ont_events->is_a($e_type, "ONT::PTM")
	or return 0;
      # < ! E/arg*[result:*]
      get_child_node_regex_attr($e, qr{^arg}, { role => ":RESULT" } )
	and return 0;
      # < X/type is_a ONT::MOLECULAR-PART
      my $x_arg = get_child_node($e, "arg2");
      my $x_id = $x_arg->getAttribute('id');
      my $x = $ekb->get_assertion($x_id, 'TERM')
	or return 0;
      my $x_type = get_slot_value($x, 'type');
      $ont_bioents->is_a($x_type, 'ONT::MOLECULAR-PART')
	or return 0;
      # < ! X//inevent/event[id=E]
      $x->findnodes('features/inevent/event[@id="'.$e_id.'"]')
	and return 0;
	
      INFO "Rule %s matches event %s (affected:%s/%s)", 
	$rule->name, $e_id, $x_id, $x_type;

      # & dependsOn ^(X//inevent/event/id)
      map { $rule->reasoner()->dependsOn($_) }
	map { $_->getAttribute('id') }
	$x->findnodes('features/inevent/event');	

      # > ?~ X/type[.=ONT::PROTEIN]
      set_slot_value($x, 'type', 'ONT::PROTEIN')
	if $ont_bioents->is_a('ONT::GENE', $x_type);
      # > + X1=X
      my $x1 = $ekb->clone_assertion($x, {rule => $rule->name});
      my $x1_id = $x1->getAttribute('id');
      $ekb->add_assertion($x1);
      # > + X/not-features/ptm[id=E]
      $ekb->add_notfeature($x, 'ptm' => { type => $e_type,
					  event => $e_id });
      # > + X1/features/ptm[id=E]
      $ekb->add_feature($x1, 'ptm' => { type => $e_type,
					event => $e_id });
      # > + E:*(result:X1)
      $ekb->add_arg($e, ':RESULT' => $x1_id);

      if ($rule->reasoner()->option('add_amount_changes')) {

	# > + E1:increase(affected:X1) 
	my $e1_id =
	  $ekb->add_event_r( refs => [$e_id],
			     rule => $rule->name,
			     slots => [ make_slot_node(type => 'ONT::INCREASE'), 
					make_arg(':AFFECTED' => $x1_id) ] );
	# > + C1:cause(factor:E,outcome:E1)
	$ekb->add_cause_r( refs => [$e_id],
			   rule => $rule->name,
			   slots => [ make_arg(':FACTOR' => $e_id),
				      make_arg(':OUTCOME' => $e1_id) ] );
	# > + E2:decrease(affected:X)
	my $e2_id =
	  $ekb->add_event_r( refs => [$e_id],
			     rule => $rule->name,
			     slots => [ make_slot_node(type => 'ONT::DECREASE'), 
					make_arg(':AFFECTED' => $x_id) ] );
	# > + C2:cause(factor:E,outcome:E2)
	$ekb->add_cause_r( refs => [$e_id],
			   rule => $rule->name,
			   slots => [ make_arg(':FACTOR' => $e_id),
				      make_arg(':OUTCOME' => $e2_id) ] );
      }

      return 1;
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
    # < ! X//inevent/event[E]
    # & dependsOn ^(X/features/inevent/event/id)
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
      get_child_node_regex_attr($e, qr{^arg}, { role => ":RESULT" } )
	and return 0;
      # < ! X/features/inevent/event[id=E]
      $x->findnodes('features/inevent/event[@id="'.$e_id.'"]')
	and return 0;

      INFO "Rule %s matches event %s (affected:%s/%s)", 
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

      # & dependsOn ^(X/features/inevent/event/id)
      map { $rule->reasoner()->dependsOn($_) }
	map { $_->getAttribute('id') }
	$x->findnodes('features/inevent/event');	

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
	  $ekb->add_event_r( refs => [$e_id],
			     rule => $rule->name,
			     slots => [ make_slot_node(type => 'ONT::INCREASE'), 
					make_arg(':AFFECTED' => $x1_id) ] );
	# > + C1:cause(factor:E,outcome:E1)
	$ekb->add_cause_r( refs => [$e_id],
			   rule => $rule->name,
			   slots => [ make_arg(':FACTOR' => $e_id),
				      make_arg(':OUTCOME' => $e1_id) ] );
	# > + E2:decrease(affected:X)
	my $e2_id =
	  $ekb->add_event_r( refs => [$e_id],
			     rule => $rule->name,
			     slots => [ make_slot_node(type => 'ONT::DECREASE'), 
					make_arg(':AFFECTED' => $x_id) ] );
	# > + C2:cause(factor:E,outcome:E2)
	$ekb->add_cause_r( refs => [$e_id],
			   rule => $rule->name,
			   slots => [ make_arg(':FACTOR' => $e_id),
				      make_arg(':OUTCOME' => $e2_id) ] );
      }
      
      return 1;
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
    # < ! X//inevent/event[E]
    # & dependsOn ^(X/features/inevent/event/id[!E])
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
      get_child_node_regex_attr($e, qr{^arg}, { role => ":RESULT" } )
	and return 0;
      # < ! X//inevent/event[E]
      $x->findnodes('features/inevent/event[@id="'.$e_id.'"]')
	and return 0;

      INFO "Rule %s matches event %s (affected:%s/%s)", 
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

      # & dependsOn ^(X/features/inevent/event/id)
      map { $rule->reasoner()->dependsOn($_) }
	map { $_->getAttribute('id') }
	$x->findnodes('features/inevent/event');	

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
	  $ekb->add_event_r( refs => [$e_id],
			     rule => $rule->name,
			     slots => [ make_slot_node(type => 'ONT::INCREASE'), 
					make_arg(':AFFECTED' => $x1_id) ] );
	# > + C1:cause(factor:E,outcome:E1)
	$ekb->add_cause_r( refs => [$e_id],
			   rule => $rule->name,
			   slots => [ make_arg(':FACTOR' => $e_id),
				      make_arg(':OUTCOME' => $e1_id) ] );
	# > + E2:decrease(affected:X)
	my $e2_id =
	  $ekb->add_event_r( refs => [$e_id],
			     rule => $rule->name,
			     slots => [ make_slot_node(type => 'ONT::DECREASE'), 
					make_arg(':AFFECTED' => $x_id) ] );
	# > + C2:cause(factor:E,outcome:E2)
	$ekb->add_cause_r( refs => [$e_id],
			   rule => $rule->name,
			   slots => [ make_arg(':FACTOR' => $e_id),
				      make_arg(':OUTCOME' => $e2_id) ] );
      }
      
      return 1;
    }
   },

   ## increase/decrease in ptm results in increase/decrease of the modified protein
   
  );

1;
