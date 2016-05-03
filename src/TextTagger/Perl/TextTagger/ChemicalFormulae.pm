#!/usr/bin/perl

package TextTagger::ChemicalFormulae;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(tag_chemical_formulae is_chemical_element);

use TextTagger::Util qw(match2tag);

use strict vars;

my @periodic_table = qw(
H                                                  He
Li Be                               B  C  N  O  F  Ne
Na Mg                               Al Si P  S  Cl Ar
K  Ca Sc Ti V  Cr Mn Fe Co Ni Cu Zn Ga Ge As Se Br Kr
Rb Sr Y  Zr Nb Mo Tc Ru Rh Pd Ag Cd In Sn Sb Te I  Xe
Cs Ba    Hf Ta W  Re Os Ir Pt Au Hg Tl Pb Bi Po At Rn
Fr Ra    Rf Db Sg Bh Hs Mt Ds Rg Cn Uut Fl Uup Lv Uus Uuo

      La Ce Pr Nd Pm Sm Eu Gd Tb Dy Ho Er Tm Yb Lu
      Ac Th Pa U  Np Pu Am Cm Bk Cf Es Fm Md No Lr
);

# list of elements that are also words or personal names
my @chemical_blacklist = qw(
  He Na Al Ga As Mo In Ta At La Ho Er Pa Am No
);

my $element_re = '(?:' . join('|', @periodic_table) . ')';
my $bond_re = qr/(?: - | = | ::? )/x;
my $chemical_formula_re = qr/
  (?<! \S )
  R? $bond_re? \(?
  $element_re \d{3}
  (?:
    (?:
      (?: $element_re | \) ) \d{3} | \(
    )
    $bond_re?
  )*
  (?! \S )
/x;

sub parens_balanced {
  my $str = shift;
  my $level = 0;
  while ($str =~ /[\(\)]/g) {
    if ($& eq '(') {
      $level++;
    } else {
      $level--;
      return 0 if ($level < 0);
    }
  }
  return ($level == 0);
}

# is the given string a chemical element abbreviation?
# (this isn't used by the tagger, but is used in ../../get-obo-terms.pl)
sub is_chemical_element {
  my $str = shift;
  return (($str =~ /^$element_re$/) ? 1 : 0);
}

sub tag_chemical_formulae {
  my $self = shift;
  my $str = shift;
  my @tags = ();
  while ($str =~ /$chemical_formula_re/g) {
    my $tag = { type => 'sense',
    		'penn-pos' => ['NNP'],
		lftype => ['CHEMICAL'],
                match2tag()
              };
    push @tags, $tag if (
      length($tag->{lex}) > 1 and
      parens_balanced($tag->{lex}) and
      not grep { $_ eq $tag->{lex} } @chemical_blacklist
    );
  }
  return [@tags];
}

push @TextTagger::taggers, {
  name => "chemical_formulae",
  tag_function => \&tag_chemical_formulae,
  output_types => ['sense'],
  input_text => 1
};

1;
