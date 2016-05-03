#!/usr/bin/perl

package TextTagger::Prescriptions;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(tag_prescriptions);

use TextTagger::Util qw(match2tag);

use strict vars;

# This is a useful reference:
# http://en.wikipedia.org/wiki/List_of_abbreviations_used_in_medical_prescriptions
# This too:
# http://www.pharmcatalyst.com/techs/sigcodespub.pharm


# Some general principles that seem to hold for the most part:
# - "q" means "every"
# - Latin abbreviations are usually lower case, especially with dots
# - English abbreviations may be lower or upper case
# - English words may have initial caps
# - If a word has 4 or more letters, it may be abbreviated
# - dots indicating abbreviation may be omitted
# - doctors are crazy ;-)

my $number_re = qr/(?:
  # hopefully after "ten" they start writing numerals :-P
  one | two | three | four | five | six | seven | eight | nine | ten |
  One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten |
  \d+(?:\s+\d+)?\s*\/\s*\d+ | # (mixed) fractions
  \d+(?:\.\d+)? | # integers, decimals
  ss # one half
)/x;

my $range_re = qr/(?: $number_re \s* - \s* $number_re )/x;

my $mass_unit_re = qr/(?:
  (?:k|m|mc|)g | (?:kilo|milli|micro|)grams? |
  oz\.? | ounces?
)/x;

my $fluid_unit_re = qr/(?:
  m[Ll] | (?:milli)?liters? |
  MLs? | # technically this is supposed to be megaliter, but I've seen it for mL
  c\.c\. | cc | # cubic centimeter
  oz\.? | ounces? |
  tbsp\.? | tablespoons? |
  tsp\.? | teaspoons?
)/x;

my $delivery_form_unit_re = qr/(?:
  supp\.? | [Ss]uppository |
  gtt?\.? | [Dd]rops? |
  caps?\.? | [Cc]apsules? |
  tabs?\.? | [Tt]ablets?
)/x;

my $mixture_re = qr/(?:
  sol\.? | [Ss]olution | 
  susp\.? | [Ss]uspension
)/x;

my $unit_re = qr/(?:
  $mass_unit_re |
  $fluid_unit_re |
  $delivery_form_unit_re
)/x;

my $amount_re = qr/(?:
  ad\s+lib\.? | # use as much as one desires
  q\.s\. | (?i:qs) | # a sufficient quantity
  (?: $number_re | $range_re ) \s* $unit_re
# This one doesn't work because it conflicts with the one above (I think)
# |
#  (?: $number_re | $range_re ) \s* $mass_unit_re \s* \/
#    \s* $fluid_unit_re (?: \s* $mixture_re )?
)/x;

my $delivery_place_re = qr/(?:
  p\.o\. | (?i:po) | # orally
  p\.r\. | (?i:pr) | # rectally
  # subcutaneously
  s\.c\. | (?i:sc) | subc\.? | subcut\.? | subq\.? | s\.q\. | (?i:sq) |
  s\.l\. | (?i:sl) | # sublingually
  I[DMPV] | # intra-dermal, -muscular, -peritoneal, -venous
  IVP | # intravenous push
  IVPB # intravenous piggyback
)/x;

=unused
my $delivery_form_re = qr/(?:
  $delivery_form_unit_re |
  $mixture_re |
  syr\.? | [Ss]yrup |
  lin\.? | [Ll]iniment |
  liq\.? |
  lot\.? | [Ll]otion |
  crm?\.? | [Cc]ream |
  nebul\.? | [Ss]pray |
  pulv\.? | [Pp]owder
)/x;
=cut

my $weekday_re = qr/(?:
  [Ss]un(?:day)? |
  [Mm]on(?:day)? |
  [Tt]ue(?:s(?:day)?)? |
  [Ww]ed(?:nesday)? |
  [Tt]hu(?:rs(?:day)?)? |
  [Ff]ri(?:day)? |
  [Ss]at(?:urday)?
)/x;

my $frequency_re = qr/(?:
  q[\.\s]\s*\d*\s*h(?:\.|ours?) | # once every so many hours
  alt\.\s*h\. | alt\s+h | # every other hour
  q\.q\.h\. | (?i:qqh) | # every four hours

  [bqt]\.i\.d\. | (?i:[bqt]id) | # two, three, four times a day
  [bqt]\.d\.s\. | (?i:[bqt]ds) | # two, three, four times a day
  q\.[ao]\.d\. | (?i:q[ao]d) | dieb\.\s*alt\. | dieb\s+alt | # every other day
  q\.d\. | (?i:qd) | # once a day
  o\.p\.d\. | (?i:opd) | # once per day
  e\.o\.d\. | (?i:eod) | # every other day
  q\.[ap]\.m\. | (?i:q[ap]m) | # every day before, after noon
  (?:once|twice)\s+daily |
  
  # on these days of the week
  q\.?\s+$weekday_re(?:\s*,\s+$weekday_re)*(?:(?: \s*, | \s+and | \s*,\s+and )\s+$weekday_re)
)/x;

my $times_re = qr/(?:
  a\.c\. | (?i:ac) | # before meals
  p\.c\. | (?i:pc) | # after meals
  c\.[cf]\. | (?i:c[cf]) | # with food
  (?:q\.)?h\.s\. | (?i:q?hs) | # at beditme
  noct\.? | # at night
  mane # in the morning
)/x;

my $postmod_re = qr/(?:
  e\.m\.p\. | (?i:emp) | # as directed
  u\.d\. | (?i:ud) | ut dict\. | # as directed
  m\.d\.u\. | (?i:mdu) | # to be used as directed
  p\.r\.n\. | (?i:prn) | PRN | # as needed
  s\.o\.s\. | (?i:sos) | si op\.? sit | # if there is a need
  s\.a\. | (?i:sa) # use your judgement
)/x;


=old
# NOTE 1: This does not include the drug name, which should come before the
# match, and be tagged by MetaMap.
# NOTE 2: Sometimes postmods will take an argument (e.g. "prn nausea"), which
# should come after the match, and possibly be tagged by MetaMap.
my $prescription_re = qr/
  \b
  $amount_re \s+
  (?: $delivery_form_re \s+ )?
  (?: $delivery_place_re \s+ )?
  (?: $frequency_re | $times_re |
      $frequency_re \s+ $times_re )
  (?: \s+ $postmod_re )?
  (?!\w) # can't use \b here because things can end with dots
/x;
=cut

# adverbials used in prescriptions
my $prescription_adv_re = qr/(?:
  (?: $delivery_place_re \s+ )?
  (?: $frequency_re | $times_re |
      $frequency_re \s+ $times_re ) |
  $postmod_re
)/x;

sub tag_prescriptions {
  my ($self, $text) = @_;
  my @tags = ();
  while ($text =~
         m/
	   \b
           (?: ($amount_re) | # "adjectives"
	       ($prescription_adv_re) # "adverbs"
	   )
	   (?!\w) # can't use \b here because things can end with dots
	 /xg) {
    push @tags, { type => 'pos',
                  'penn-pos' => [defined($1)? 'JJ' : 'RB'],
		  match2tag()
		};
  }
  return [@tags];
}

push @TextTagger::taggers, {
  name => 'prescriptions',
  tag_function => \&tag_prescriptions,
  output_types => [qw(pos)],
  input_text => 1
};

1;
