package TextPP::NumHack;

use 5.008001;
use strict;
use warnings;

require Exporter;

our @ISA = qw(Exporter);

our @EXPORT_OK = qw(
	&NumHack
);

our $VERSION = '0.01';


# methods go here.

# may leave behind extra spaces here and there, but later processes ought
# to correct that...

my @ones_oh=("oh","one","two","three","four",
	  "five","six","seven","eight","nine");

sub NumHack {
    my $in = shift;

    # sgml tags
    my $sgml_pattern = "<\/?(art|p|s|name)";

    return $in unless $in =~ /\d/;	# skip lines without numbers
    return $in if $in =~ /^$sgml_pattern/;	# skip SGML

    # probable Zip codes
    $in =~ s/\b(\d{5}-\d{4})\b/&SpellDigits($1)/eg;	# 12345
    $in =~ s/\b(\d{5})\b/&SpellDigits($1)/eg;		# 12345-6789

    # phone numbers
    # [LG 2005/12/07] added "." separator, as in 1.800.463.3339
    $in =~ s=(\b)([1l][-\. ])?\(?([2-9]\d{2})\)?[-\./]? ?(\d{3})[-\.](\d{4})\b=&SpellTel($2,$3,$4,$5)=eg; # 215-555-1212 etc.
    $in =~ s/(\b)(\d{3}-\d{4})\b/&SpellDigits($2)/eg;	# 555-1212
    # [LG 2005/12/07] 800.Go.Crazy, 800 HAVE-FUN
    $in =~ s=(\b)(1[-\. ])?\(?([89]\d{2})\)?[-\. ](\w+)[-\.]((??{"\\w{".(7-length($4)).",".(9-length($4))."}"}))\b=&SpellTel($2,$3,$4,$5)=ge;
    # [LG 2005/12/07] 800.GoCrazy
    $in =~ s=(\b)(1[-\. ])?\(?([89]\d{2})\)?[-\. ](\w{7,9})\b=&SpellTel($2,$3,"",$4)=ge;
    $in =~ s/\b1[-\.]\(?800\)?(\W)/ one - eight hundred $1/go;	# isolated 1-800
    $in =~ s/([Aa]rea code) (\d{3})(\W)/"$1 ".&SpellDigits($2)."$3"/eg;

    return $in;
}

sub SpellDigits
{
    my ($num)=$_[0];
    $num =~ s/(\d)(\D)(\d)/$1 $2 $3/go; # add space around non-digits
    # isolated zeros become "oh", string of them become "zero ..."
    $num =~ s/(00+)/" zero" x length($1)/eg;
    $num =~ s/(\d)/" $ones_oh[$1]"/eg;
    return $num;
}

sub SpellTel
{
    my ($pre,$area,$exch,$rest)=@_;
    my $return = $pre ? " one -" : " ";
    if ($area =~ /(\d)00/o)
    {
	$return .= &SpellDigits($1);
	$return .= " hundred";
    }
    else
    {
	$return .= &SpellDigits($area);
    }
    $return .= " - ";

    $return .= &SpellDigits($exch);
    $return .= " - ";
    $return .= &SpellDigits($rest);

    return $return;
}


1;
__END__


=head1 NAME

TextPP::NumHack - Preprocessor for numproc, potentially specialized for Broadcast News material.

=head1 SYNOPSIS

  use TextPP::NumHack;

  \$new_text = NumHack(\$text);

=head1 DESCRIPTION

From the original Perl script:

# preprocessor for numproc, potentially specialized for Broadcast News material

# tries to patch numproc's problems with:
#	- telephone numbers
#	- zip codes
# for example:
#   1-800-555-1212
#     =>  one - eight hundred -  five five five -  one two one two
#   (215) 555-1212
#     =>  two one five -  five five five -  one two one two
#   212/285-9400
#     =>  two one two -  two eight five -  nine four zero zero
#   1-(800)-CAR-CASH
#     =>  one - eight hundred -CAR-CASH
#   New York, NY 10007
#     =>  New York, NY  one zero zero zero seven
#   Philadelphia, PA 19104-6789
#     =>  Philadelphia, PA  one nine one oh four -  six seven eight nine

=head2 EXPORT

None by default.

=head1 SEE ALSO

=head1 HISTORY

Perl script: # $Id: NumHack.pm,v 1.2 2006/05/08 21:50:11 lgalescu Exp $

2005/12/06 Lucian Galescu, E<lt>lgalescu@ihmc.usE<gt>
Perl module ready.

2005/12/07  Lucian Galescu, E<lt>lgalescu@ihmc.usE<gt>
Added processing of 1.800.GoFedEx, 1.800.463.3339, 800-HAVE-FUN, etc.

2006/01/25 Lucian Galescu, E<lt>lgalescu@ihmc.usE<gt>
Added <name> to allowable SGML tags.

=head1 AUTHOR

Lucian Galescu, E<lt>lgalescu@ihmc.usE<gt>

=head1 COPYRIGHT AND LICENSE

The original Perl script is part of a tool suite that bears a copyright notice 
from MIT. Here is the copyright notice from numproc (referenced above):

###############################################################################
# This software is being provided to you, the LICENSEE, by the Massachusetts  #
# Institute of Technology (M.I.T.) under the following license.  By           #
# obtaining, using and/or copying this software, you agree that you have      #
# read, understood, and will comply with these terms and conditions:          #
#                                                                             #
# Permission to use, copy, modify and distribute, including the right to      #
# grant others the right to distribute at any tier, this software and its     #
# documentation for any purpose and without fee or royalty is hereby granted, #
# provided that you agree to comply with the following copyright notice and   #
# statements, including the disclaimer, and that the same appear on ALL       #
# copies of the software and documentation, including modifications that you  #
# make for internal use or for distribution:                                  #
#                                                                             #
# Copyright 1991-4 by the Massachusetts Institute of Technology.  All rights  #
# reserved.                                                                   #
#                                                                             #
# THIS SOFTWARE IS PROVIDED "AS IS", AND M.I.T. MAKES NO REPRESENTATIONS OR   #
# WARRANTIES, EXPRESS OR IMPLIED.  By way of example, but not limitation,     #
# M.I.T. MAKES NO REPRESENTATIONS OR WARRANTIES OF MERCHANTABILITY OR FITNESS #
# FOR ANY PARTICULAR PURPOSE OR THAT THE USE OF THE LICENSED SOFTWARE OR      #
# DOCUMENTATION WILL NOT INFRINGE ANY THIRD PARTY PATENTS, COPYRIGHTS,        #
# TRADEMARKS OR OTHER RIGHTS.                                                 #
#                                                                             #
# The name of the Massachusetts Institute of Technology or M.I.T. may NOT be  #
# used in advertising or publicity pertaining to distribution of the          #
# software.  Title to copyright in this software and any associated           #
# documentation shall at all times remain with M.I.T., and USER agrees to     #
# preserve same.                                                              #
###############################################################################

=cut
