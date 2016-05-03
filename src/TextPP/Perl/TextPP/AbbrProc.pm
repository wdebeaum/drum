package TextPP::AbbrProc;

=head1 NAME

TextPP::AbbrProc - an abbreviation preprocessor for WSJ.

=head1 SYNOPSIS

  use TextPP::AbbrProc;

  \$new_text = AbbrProc(\$text);

  TextPP::AbbrProc::setverbose(1);

=head1 DESCRIPTION

From original Perl script:

# abbreviation preprocessor for WSJ
# assumes 1 sentence per line
#
# 1. map "x.y." -> "x. y."
# 2. convert Roman numerals with appropriate left context into cardinal no.s
# 3. expand abbreviations and word translations
#	expands remaining Roman numerals into ordinal no.s
# 4. map isolated letters: "x" -> "x."


=head2 EXPORT

None by default.

=head1 BUGS

Lots of "unitialized" warnings -- seemingly references past the end of some 
array; they don't appear to be pernicious, but it is hard to say whether this 
was intended (ie, tests were ommitted for efficiency reasons knowing that the 
behavior would be correct anyway) or the code is just poorly written.

=head1 HISTORY

Perl script: # $Id: AbbrProc.pm,v 1.2 2006/05/08 21:50:11 lgalescu Exp $

             # Minor modifications by David Graff, Linguistic Data Consortium, 
             #   in preparation for publishing on cdrom;  Aug. 11, 1994.

             # Major modifications by Robert MacIntyre, LDC, attempting to 
             #   improve performance (~50% speedup), in preparation of Broadcast
             #   News material, August 1996.

             # Minor modifications by Lucian Galescu, University of Rochester; 
             #   Sep. 26, 2001

             # Minor modifications by Lucian Galescu, IHMC; 2005

2005/12/06 Lucian Galescu, E<lt>lgalescu@ihmc.usE<gt>
Perl module ready.

2006/01/25 Lucian Galescu, E<lt>lgalescu@ihmc.usE<gt>
Added <name> to allowable SGML tags.

=head1 AUTHOR

Lucian Galescu, E<lt>lgalescu@ihmc.usE<gt>

=head1 COPYRIGHT AND LICENSE

From the original script:

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

use 5.008001;
use strict;
#use warnings;

require Exporter;

our @ISA = qw(Exporter);

our @EXPORT_OK = qw(
	&AbbrProc
);

our $VERSION = '0.01';

#----------------------------------------------------------------
# Globals
#
my $vflg = 0;
my $abbrlistFile = '';

my (%abbrev, %romanlc, %trans, %Roman);
my (@input, @output, $field);
my ($appendflg);

# methods go here.

sub setverbose {
    $vflg = shift;
}

sub loadabbrlist {

    my ($x, $y);
    my $n;

    while(<DATA>)
    {	if(/^#/) {next;}	# comment
	chomp;
	if(!$_) {next;}		# blank
	$y=$_;
	s/^(\S+)\s+//;		# extract 1st word
	$x=$1;
	if(!$x) {&perr("no word: $y");}
	if(!$_) {&perr("no value: $y");}

	if($x =~ /^\*r/)		# left context for roman numeral
	{	
	    if(!/^[a-zA-Z]{2,}$/) 
	    { &perr("illegal roman: $x"); }
	    tr/a-z/A-Z/;		# map to UC
	    $romanlc{$_}=1;
	}
	elsif($x =~ /\.$/)			# abbreviations
	{
	    if($x !~ /^[a-zA-Z][a-zA-Z\.]+\.$/) 
	    { &perr("illegal abbreviation: $x"); }
	    $x =~ s/\.$//;
	    $abbrev{$x}=$_;
	    if($x =~ /[a-z]/)
	    {	$x =~ tr/a-z/A-Z/;	#UC version
		tr/a-z/A-Z/;
		$abbrev{$x}=$_;
	    }
	    #if(length($x)>$maxabl) {$maxabl=length($x);}
	}
	else				# translations
	{
	    if($x !~ /^[a-zA-Z\.&\/-]+[a-zA-Z]$/)
	    { &perr("illegal translation: $x"); }
	    $trans{$x}=$_;
	    if($x =~ /[a-z]/)
	    {	$x =~ tr/a-z/A-Z/;	#UC version
		tr/a-z/A-Z/;
		$trans{$x}=$_;
	    }
	    #if(length($x)>$maxtrl) {$maxtrl=length($x);}
	}
	$n++;
    }

    if($vflg) {print STDERR "$n lines read from file\n";}

}

sub prepare {

    &loadabbrlist;
    &setupRoman;
}

sub AbbrProc {
    my $in = shift;
    my $out;

    my ($front, $back, $ptbkflg, $sflag, $x, $xback);

    # sgml tags
    my $sgml_pattern = "<\/?(art|p|s|name)";

    # prepare data structures
    &prepare;

    # split into lines;
    my @lines = split(/\n/, $in);

    my $line;

    foreach $line (@lines) 
    { 
	$_ = $line;

	###########################  abbrevproc ############################

	# pass SGML as is
	if (/^$sgml_pattern/)
	{
	    $out .= $_ . "\n";
	    next;
	}

	s/&/ & /g;			# &
	s=/= / =g;			# /
	s/ - / -- /g;		# save (long) dashes
	s/\b(-+)\b/ $1 /g;		# -, --, etc. in words
	s/([^-\s])(-+)([^-\s])/$1 $2 $3/g;

	if(/_/)
	{
	    &perr2("removing illegal underscores (_) in:\n $_\n");
	    s/_/ /g;
	}

	@input = split(/\s+/);
	@output=();
	for($field=0;$field<=$#input;$field++)
	{
	    $_ = $input[$field];
	    # if($vflg) {print "in: $_\n";}

	    s/^(\W*)//;		# strip front
	    $front=$1;

	    s/(\W*)$//;		# strip back
	    $back=$1;
	    if(/\.?\'[sS]$/)		# possessive
	    {
		s/(\.?\'[sS])$//;
		$back="$1$back";
	    }
	    elsif (/^[A-Z]+s$/)	# eg Cs or Xs
	    {
		s/s$//;
		$back="_s$back";
	    }

	    $ptbkflg = ($back =~ /^\./);

	    #if($vflg) {print "f=$front, m=$_, b=$back\n";}


	    # Roman numerals
	    if(/^[IVX]{1,6}$/ && $front eq "" && $field>0 && ($x=&geto()))
	    {
		$x =~ tr/a-z/A-Z/;	# map lc to UC
		$x =~ s/^\W//;	   # strip initial punct from lc
		if($romanlc{$x})	# left context check
		{
		    if($front) 
		    {
			&pusho($front);
			if($front !~ /[\w]$/) {$appendflg=1;}
		    }

		    if ($x=$Roman{$_})
		    {
			&pusho($x);
		    }
		    else
		    {
			&perr2("illegal roman: $_");
			&pusho($_);
		    }

		    if($back)
		    {
			if($back !~ /^[\w]/) {&appendo($back);}
			else {&pusho($back);}
		    }
		    next;
		}

	    }


	    # St. or St ["Street" vs. "Saint"]
	    if($_ eq "St")
	    {	$back =~ s/^\.//;
		if($front ne "" && $back ne "")
		{	
		    &perr2("Cannot resove St.: $input[$field-1] $input[$field] $input[$field+1]");
		    $x = "Street";	# Wild guess
		}
		elsif($front) { $x="Saint"; }
		elsif($back) { $x="Street"; }
		elsif($input[$field-1] !~ /^[A-Z]/
		      && $input[$field+1] =~ /^[A-Z]/)
		{ $x = "Saint"; }
		elsif($input[$field-1] =~ /^[A-Z]/
		      && $input[$field+1] !~ /^[A-Z]/)
		{ $x = "Street"; }
		elsif(!$back && $input[$field+1] =~ /^[A-Z]/)
		{ $x = "Saint"; }
		elsif(!$back && $input[$field+1] eq '-' &&
		      $input[$field+2] =~ /^[A-Z]/)
		{ $x = "Saint"; }
		else
		{
		    &perr2("Cannot resove St.: $input[$field-1] $input[$field] $input[$field+1]");
		    $x = "Street";	# Wild guess
		}

		if($front) 
		{	
		    &pusho($front);
		    if($front !~ /[\w]$/) {$appendflg=1;}
		}
	
		&pusho($x);

		if($back)
		{	
		    if($back !~ /^[\w]/) {&appendo($back);}
		    else {&pusho($back);}
		}
		next;
	    }

	    # abbreviations (end with .)
	    if($ptbkflg && ($x=$abbrev{$_}))
	    {	
		if($front) 
		{
		    &pusho($front);
		    if($front !~ /[\w]$/) {$appendflg=1;}
		}
	
		&pusho($x);
					
		if($field<$#input || $back =~ /[!?]/)
		{   $back =~ s/^\.//; }	# rm .
		else			# end of sent
		{	
		    $back =~ s/^\.(\'s)/$1./;
		    if($back =~ /\..*\./) # 2 dots
		    { $back=~s/\.([^\.]*)/$1/;}
		}

		if($back)
		{
		    if($back !~ /^[\w]/) { &appendo($back);}
		    else { &pusho($back);}
		}
		next;
				
	    }

	    # translations (do not end with .)
	    # first merge multi-token translations
	    if($input[$field+1] =~ /^[-\/&]$/ && $back eq "")
	    {	$x=$input[$field+2];
		$x =~ s/(\W*)$//;
		$xback=$1;
		if($x =~ /\.?\'[sS]$/)		# possessive
		{    $x =~ s/(\.?\'[sS])$//;
		     $xback="$1$xback";
		}
		elsif ($x =~ /^[A-Z]+s$/)	# eg Cs or Xs
		{    $x =~ s/s$//;
		     $xback="_s$xback";
		}
		if($trans{"$_$input[$field+1]$x"})   # eg. AT&T
		{    $_="$_$input[$field+1]$x";
		     $field+=2;
		     $back=$xback;
		     $ptbkflg = ($back =~ /^\./);
		}
	    }
	    # then see if we have a translation
	    if ($x=$trans{$_})
	    {	if($front)
		{    &pusho($front);
		     if($front !~ /[\w]$/) { $appendflg=1;}
		}
	
		&pusho($x);
					
		if($x =~ /\.$/) { $back =~ s/^\.//; } # only 1 .
		if($back)
		{    if($back !~ /^[\w]/) { &appendo($back);}
		     else { &pusho($back);}
		}
		next;
	    }

	    # eg. Cs, but not As Is Ms Us
	    if(($back =~ /^_s/) && /^[B-HJ-LN-TV-Z]$/)  
	    {	if($front)
		{    &pusho($front);
		     if($front !~ /[\w]$/) { $appendflg=1;}
		}
	
		&pusho("$_.");
	
		if($back)
		{    if($back !~ /^[\w]/) {&appendo($back);}
		     else {&pusho($back);}
		}
		next;
	    }

	    # split x.y.
	    $_ .= '.' if $ptbkflg;	# NOTE THIS CHANGES $_ FOR FUTURE MATCHES
					# but it has no more uses in this loop,
					# so this _should_ be okay.
	    if (/^([a-zA-Z]\.)+([sS]?)$/)
	    {
		$sflag = $2;	# remember if plural (as opposed to a.s.)

		chop if $ptbkflg;	# trim period that we just added

		s/\./. /g;		# x.y. -> x. y.

		s/ ([sS])$/$1/ if $sflag;	# reattach final "s"

		if($front) 
		{   &pusho($front);
		    if($front !~ /[\w]$/) { $appendflg=1;}
		}
	
		&pusho($_);

		if($back)
		{   if($back !~ /^[\w]/) { &appendo($back);}
		    else { &pusho($back);}
		}
		next;
	    }

	    # remaining tokens are passed "as is"
	    # [Below does "&pusho($input[$field]);" but faster, since we avoid
	    # the subroutine call for the most common case.]
	    push(@output,$input[$field]);
	}

	$_=join(" ",@output);

	# if($vflg) {print "ab:\t$_\n";}

	#########################  lettproc  ##################################
	if (/\b[b-zB-HJ-Z]\b/)
	{
	    @output = split(/\s+/);

	    foreach(@output)
	    {
		next unless /^\W*[b-zB-HJ-Z]\W*$/;

		#if($vflg) {print "le: $_\n";}

		# some cases to skip/pre-change.  (Note that backslashing of
		# quotes is for the sake of Emacs, not Perl.)
		next if (/^[\'][nN]$/);		# Spic \'n Span

		s/(^[\`\'][nN])[\`\']$/$1/ && next;	# Rock 'n' Roll: 'n' -> \'n

		s/^[\`\'\"]R[\'\`\"]$/"R"/ && next;	# Toys "R" Us

		next if (/^o\'$/);			# Man o\' War

		# put . at end of remaining single-letter words
		s/^(\W*)([b-zB-HJ-Z])([^.\w]\W*|[^\w.]*)$/$1$2.$3/;
	    }
	
	    $_=join(" ",@output);
	}

	s/\s+/ /g;
	s/^ //;
	s/ $//;

	s/ _//g;	# attach final s for Cs or AFLs
	s/_//g;	# clear _
	s/ - /-/g;

	$out .= $_ . "\n" if $_;
    }

    return $out;
}

sub pusho				# pusho($x): push output
{
    if($appendflg)			# global: used for fronts
    {
	&appendo($_[0]);
    }
    else {
	push(@output,@_);
    }
}

sub appendo				# appendo($x): append to output
{
    $appendflg=0;		
    if($#output < 0) { &perr("appendo: output empty"); }
    $output[$#output] .= $_[0];
}

sub geto				# geto(): get last output
{
    if($#output < 0) { warn ("geto: output empty\n");}
    return $output[$#output];
}

sub perr
{
    warn "abbrevproc: $_[0]\n";
    exit(1);
}

sub perr2
{
    warn "abbrevproc: $_[0]\n";
}

sub setupRoman
{
    $Roman{I}="one";
    $Roman{II}="two";
    $Roman{III}="three";
    $Roman{IV}="four";
    $Roman{V}="five";
    $Roman{VI}="six";
    $Roman{VII}="seven";
    $Roman{VIII}="eight";
    $Roman{IX}="nine";
    $Roman{X}="ten";
    $Roman{XI}="eleven";
    $Roman{XII}="twelve";
    $Roman{XIII}="thirteen";
    $Roman{XIV}="fourteen";
    $Roman{XV}="fifteen";
    $Roman{XVI}="sixteen";
    $Roman{XVII}="seventeen";
    $Roman{XVIII}="eighteen";
    $Roman{XIX}="nineteen";
    $Roman{XX}="twenty";
    $Roman{XXI}="twenty-one";
    $Roman{XXII}="twenty-two";
    $Roman{XXIII}="twenty-three";
    $Roman{XXIV}="twenty-four";
    $Roman{XXV}="twenty-five";
    $Roman{XXVI}="twenty-six";
    $Roman{XXVII}="twenty-seven";
    $Roman{XXVIII}="twenty-eight";
    $Roman{XXIX}="twenty-nine";
    $Roman{XXX}="thirty";
    $Roman{XXXI}="thirty-one";
    $Roman{XXXII}="thirty-two";
    $Roman{XXXIII}="thirty-three";
    $Roman{XXXIV}="thirty-four";
    $Roman{XXXV}="thirty-five";
}


1;

#
# Abbreviations List
#

__DATA__
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

# abbreviation list
# derived from unigram file 29 Aug 91 mods to 17 Sept 91
# x.y. mapped to x. y. in program

# true abbreviations (must end with .)
# if key includes lower case, an upper case version will be created
Adm.	Admiral
Ala.	Alabama
Alex.	Alexander
Apr.	April
Ariz.	Arizona
Ark.	Arkansas
AUG.	AUGUST
Aug.	August
Ave.	Avenue
Avg.	Average
avg.	average
Bancorp.	Bancorp
Bhd.	B. H. D.
Blvd.	Boulevard
Brig.	Brigadeer
Bros.	Brothers
Cal.	Calorie
Ca.	California
Calif.	California
Capt.	Captain
Cie.	Company
Cmdr.	Commander
Co.	Company
co.	Company
Col.	Colonel
Colo.	Colorado
Conn.	Connecticut
Corp.	Corporation
Cos.	Companies
Cpl.	Corporal
Dec.	December
Del.	Delaware
Dept.	Department
Dr.	Doctor
Drs.	Doctors
Feb.	February
Fla.	Florida
Fr.	Friar
Fri.	Friday
Ft.	Fort
Ga.	Georgia
Gen.	General
Gov.	Governor
Ill.	Illinois
Inc.	Incorporated
Ind.	Indiana
Indust.	Industrial
InfoCorp.	InfoCorp
Infocorp.	InfoCorp
Intercorp.	Intercorp
Jan.	January
Jr.	Junior
Jul.	July
Jun.	June
Kan.	Kansas
Ky.	Kentucky
La.	Louisiana
lb.	pound
lbs.	pounds
Lt.	Lieutenant
Ltd.	Limited
Ltda.	Company
Maj.	Major
Mar.	March
Mass.	Massachusetts
MCorp.	M. Corporation
Md.	Maryland
Me.	Maine
# Some Italian company
Me.T.A.	M. E. T. A.
Mfg.	Manufacturing
Mich.	Michigan
Minn.	Minnesota
Miss.	Mississippi
Mo.	Missouri
Mt.	Mountain
Mont.	Montana
#			meaning of mistress has changed + symmetry
#Mr.	Mister
#Mrs.	Mistress
#Ms.	Miz
#Messrs.
#
Neb.	Nebraska
Nev.	Nevada
No.	Number
Nos.	Numbers
Nov.	November
Oct.	October
Okla.	Oklahoma
Ont.	Ontario
Op.	Opus
Ore.	Oregon
Pa.	Pennsylvania
PacifiCorp.	PacifiCorp
Penn.	Pennsylvania
PHLCorp.	P. H. L. Corporation
Ph.D.	P. H. D.
PhD.	P. H. D.
Prof.	Professor
Prop.	Proposition
Pte.	Point
Pty.	Party
Pvt.	Private
Rep.	Representative
Reps.	Representatives
Rev.	Reverend
Sen.	Senator
Sens.	Senators
Sept.	September
Sgt.	Sargent
S.p.A.	Company
Sr.	Senior
#St.	Street or Saint		Context dependent (in abbrevproc)
Ste.	Saint
Tel.	Telephone
Tenn.	Tennessee
Tex.	Texas
Va.	Virginia
Vt.	Vermont
W.Va.	West Virginia
Wash.	Washington
Wis.	Wisconsin
Wyo.	Wyoming
Yr.	Year
etc.	et-cetera
Etc.	Et-cetera
ft.	feet
inc.	incorporated
mfg.	manufacturing
Vol.	Volume
vol.	volume
vs.	versus
mm.	millimeter
mg.	milligram

# left contexts for roman cardinal numerals
# case independent comparisons
*r	Act
*r	Advantage
*r	amendment
*r	angiotensin
*r	Antrim
*r	Appendix
*r	Apple
*r	Arrow
*r	Article
*r	Associates
*r	Astros
*r	Bank
*r	Bowl
*r	Bronco
*r	Busch
*r	CSPAN
*r	Canada
*r	Century
*r	Class
*r	Cleopatra
*r	Concepts
*r	Cop
*r	dBase
*r	database
*r	Delta
*r	Detente
*r	Dundee
*r	Esprit
*r	Explorer
*r	Express
*r	Eyes
*r	Factor
*r	Ford
*r	Freaks
*r	Fund
*r	Funding
*r	Funds
*r	Future
*r	GOD
*r	GSTAR
*r	Gemini
*r	Ghostbusters
*r	Global
*r	Group
*r	Gulfstream
*r	Hybrid
*r	Intelsat
*r	Investment
*r	Investments
*r	Iron
*r	Jets
*r	Journalism
*r	Kong
# 		LaSalle nuclear plant
*r	LaSalle
*r	LaserWriter
*r	Lighthouse
*r	Linen
*r	Mark
*r	Mac
*r	MacDraw
*r	MacProject
*r	Macintosh
*r	Management
*r	Mark
*r	Metro
*r	MicroVAX
*r	Minuteman
*r	Monopoly
*r	Notes
*r	numeral
*r	OPEC
*r	Officer
*r	Overseas
*r	Part
*r	Partners
*r	Pershing
*r	Phantasm
*r	Phase
*r	Phobos
*r	Pioneer
*r	Pirate
*r	Play
*r	Plus
*r	Point
*r	Portable
*r	Quick
*r	Rambo
*r	Ransom
*r	Resorts
*r	SALT
*r	Screen
*r	Series
*r	Stage
*r	Superman
*r	System
*r	TIAA
*r	Titan
*r	Title
*r	Toxic
*r	Trac
*r	Trek
*r	Trident
*r	Trooper
*r	Trust
*r	Ultima
*r	Vatican
*r	Ventures
*r	Volume
*r	WW
*r	War
*r	Weapon
*r	Wespac
*r	Westar
*r	Wrestlemania

# Roman ordinals (I, V, and X not included: too many false hits)
II	the second
III	the third
IV	the fourth
VI	the sixth
VII	the seventh
VIII	the eighth
IX	the ninth
XI	the eleventh
XII	the twelfth
XIII	the thirteenth
XIV	the fourteenth
XV	the fifteenth
XVI	the sixteenth
XVII	the seventeenth
XVIII	the eighteenth
XIX	the nineteenth
XX	the twentieth
XXI	the twenty-first
XXII	the twenty-second
XXIII	the twenty-third
XXIV	the twenty-fourth
XXV	the twenty-fifth

# acronyms (not ending in .) needing translation
# if key includes lower case, an upper case version will be created
# keys can include - / & .
AA	Double A.
AAA	Triple A.
AAI	A. A. I.
AAP	A. A. P.
AAR	A. A. R.
AARP	A. A. R. P.
AAS	A. A. S.
AB	A. B.
ABA	A. B. A.
ABB	A. B. B.
ABC	A. B. C.
ABD	A. B. D.
ABF	A. B. F.
ABI	A. B. I.
ABM	A. B. M.
ABN	A. B. N.
ABS	A. B. S.
ABT	A. B. T.
AC	A. C.
ACA	A. C. A.
ACC	A. C. C.
ACCT	A. C. C. T.
ACEC	A. C. E. C.
ACF	A. C. F.
ACI	A. C. I.
ACLI	A. C. L. I.
ACLU	A. C. L. U.
ACM	A. C. M.
ACO	A. C. O.
ACP	A. C. P.
ACSH	A. C. S. H.
ACTV	A. C. T. V.
ADB	A. D. B.
ADC	A. D. C.
ADI	A. D. I.
ADIA	A. D. I. A.
ADM	A. D. M.
ADN	A. D. N.
ADP	A. D. P.
ADR	A. D. R.
ADT	A. D. T.
ADV	A. D. V.
adv	A. D. V.
AD&P	A. D. & P.
AD/SAT	AD / SAT
AE	A. E.
AEA	A. E. A.
AEC	A. E. C.
AEG	A. E. G.
AEI	A. E. I.
AEL	A. E. L.
AEP	A. E. P.
AER	A. E. R.
AES	A. E. S.
AEU	A. E. U.
AEW	A. E. W.
AFA	A. F. A.
AFC	A. F. C.
AFCO	A. F. C. O.
AFDC	A. F. D. C.
AFG	A. F. G.
AFGE	A. F. G. E.
AFIS	A. F. I. S.
AFL	A. F. L.
AFP	A. F. P.
AFSCME	A. F. S. C. M. E.
AG	A. G.
AGA	A. G. A.
AGB	A. G. B.
AGEF	A. G. E. F.
AGF	A. G. F.
AGI	A. G. I.
AGIP	A. G. I. P.
AGS	A. G. S.
AGT	A. G. T.
AHA	A. H. A.
AHL	A. H. L.
AI	A. I.
AIBD	A. I. B. D.
AIC	A. I. C.
AICPA	A. I. C. P. A.
AIFS	A. I. F. S.
AIG	A. I. G.
AIL	A. I. L.
AIME	A. I. M. E.
AIT	A. I. T.
AIW	A. I. W.
AIX	A. I. X.
AK	A. K.
AKA	A. K. A.
ALC	A. L. C.
ALQ	A. L. Q.
ALR	A. L. R.
AM	A. M.
AMA	A. M. A.
AMC	A. M. C.
AMCA	A. M. C. A.
AMCC	A. M. C. C.
AMD	A. M. D.
AME	A. M. E.
AMF	A. M. F.
AMG	A. M. G.
AMI	A. M. I.
AML	A. M. L.
AMO	A. M. O.
AMP	A. M. P.
AMR	A. M. R.
AMT	A. M. T.
ANB	A. N. B.
ANC	A. N. C.
ANF	A. N. F.
ANMC	A. N. M. C.
ANR	A. N. R.
ANWR	A. N. W. R.
ANZ	A. N. Z.
AO	A. O.
AOC	A. O. C.
AOI	A. O. I.
AOK	A. O. K.
AON	A. O. N.
AP	A. P.
A&P	A. & P.
APA	A. P. A.
APAC	A. P. A. C.
API	A. P. I.
APL	A. P. L.
APMA	A. P. M. A.
APN	A. P. N.
APPWP	A. P. P. W. P.
APR	A. P. R.
APS	A. P. S.
APSAC	A. P. S. A. C.
APV	A. P. V.
APW	A. P. W.
ARA	A. R. A.
ARB	A. R. B.
ARD	A. R. D.
ARX	A. R. X.
ASA	A. S. A.
ASB	A. S. B.
ASC	A. S. C.
ASEA	A. S. E. A.
ASI	A. S. I.
ASPCA	A. S. P. C. A.
AST	A. S. T.
AT	A. T.
ATA	A. T. A.
ATC	A. T. C.
ATF	A. T. F.
ATI	A. T. I.
ATM	A. T. M.
ATN	A. T. N.
ATR	A. T. R.
ATS	A. T. S.
AT&T	A. T. & T.
ATV	A. T. V.
AUS	A. U. S.
AV	A. V.
AVAQ	A. V. A. Q.
AVC	A. V. C.
AVX	A. V. X.
AWA	A. W. A.
AWD	A. W. D.
AWOL	A. W. O. L.
AWSJ	A. W. S. J.
AWT	A. W. T.
AXA	A. X. A.
AXP	A. X. P.
AY	A. Y.
AZL	A. Z. L.
AZP	A. Z. P.
AZT	A. Z. T.
BA	B. A.
Ba	B. a.
BAA	B. A. A.
Baa	B. a. a.
BAC	B. A. C.
BAII	B. A. I. I.
B.A.IT	B. A. IT
BASF	B. A. S. F.
B.A.T	B. A. T.
BB	Double B.
BBA	B. B. A.
BBB	Triple B.
BBC	B. B. C.
BBDO	B. B. D. O.
BBN	B. B. N.
BC	B. C.
BCA	B. C. A.
BCCI	B. C. C. I.
BCE	B. C. E.
BCEAO	B. C. E. A. O.
BCG	B. C. G.
BCI	B. C. I.
BCM	B. C. M.
BCOA	B. C. O. A.
BCS	B. C. S.
BCV	B. C. V.
BCW	B. C. W.
BDC	B. D. C.
BDDP	B. D. D. P.
BDM	B. D. M.
BDO	B. D. O.
BDR	B. D. R.
BEC	B. E. C.
BEI	B. E. I.
BF	B. F.
BFEA	B. F. E. A.
BFS	B. F. S.
BGH	B. G. H.
BGS	B. G. S.
BHC	B. H. C.
Bhd	B. H. D.
BHF	B. H. F.
BHP	B. H. P.
BHS	B. H. S.
BHW	B. H. W.
BI	B. I.
BIA	B. I. A.
BICC	B. I. C. C.
BiiN	B. i. i. N.
BIP	B. I. P.
BIR	B. I. R.
BIS	B. I. S.
BIW	B. I. W.
BJ	B. J.
BJF	B. J. F.
BK	B. K.
BL	B. L.
BLM	B. L. M.
BLS	B. L. S.
BM	B. M.
BMA	B. M. A.
BMC	B. M. C.
BMI	B. M. I.
BMP	B. M. P.
BMW	B. M. W.
BMY	B. M. Y.
BN	B. N.
BNL	B. N. L.
BNP	B. N. P.
BNS	B. N. S.
BNY	B. N. Y.
BOC	B. O. C.
BOJ	B. O. J.
BOT	B. O. T.
BP	B. P.
bpd	B. P. D.
BPB	B. P. B.
BPC	B. P. C.
BPCA	B. P. C. A.
BPCC	B. P. C. C.
BPD	B. P. D.
BPI	B. P. I.
BR	B. R.
BRE	B. R. E.
BRNF	B. R. N. F.
BRT	B. R. T.
BRZ	B. R. Z.
BS	B. S.
BSB	B. S. B.
BSD	B. S. D.
BSE	B. S. E.
BSI	B. S. I.
BSN	B. S. N.
BSO	B. S. O.
BST	B. S. T.
BT	B. T.
BTL	B. T. L.
BTR	B. T. R.
BTU	B. T. U.
BV	B. V.
BVI	B. V. I.
BVL	B. V. L.
BW	B. W.
BWA	B. W. A.
BWAC	B. W. A. C.
BZ	B. Z.
BZW	B. Z. W.
CA	C. A.
Ca	C. a.
CAA	C. A. A.
Caa	C. a. a.
CAAC	C. A. A. C.
CAC	C. A. C.
CACI	C. A. C. I.
CAD	C. A. D.
CAE	C. A. E.
CAID	C. A. I. D.
CAMI	C. A. M. I.
CARU	C. A. R. U.
CATV	C. A. T. V.
CAV	C. A. V.
CAW	C. A. W.
CB	C. B.
CBC	C. B. C.
CBI	C. B. I.
CBN	C. B. N.
CBO	C. B. O.
CBOE	C. B. O. E.
CBOT	C. B. O. T.
CBS	C. B. S.
CBT	C. B. T.
CBW	C. B. W.
CCA	C. C. A.
CCC	C. C. C.
CCD	C. C. D.
CCE	C. C. E.
CCH	C. C. H.
CCK	C. C. K.
CCL	C. C. L.
CCX	C. C. X.
CD	C. D.
CDA	C. D. A.
CDC	C. D. C.
CDF	C. D. F.
CDI	C. D. I.
CDL	C. D. L.
CDS	C. D. S.
CDT	C. D. T.
CDU	C. D. U.
CDW	C. D. W.
CE	C. E.
CEA	C. E. A.
CED	C. E. D.
CEE	C. E. E.
CEI	C. E. I.
CEL	C. E. L.
CEO	C. E. O.
CEP	C. E. P.
CES	C. E. S.
CF	C. F.
CFA	C. F. A.
CFC	C. F. C.
CFM	C. F. M.
CFO	C. F. O.
CFP	C. F. P.
CFS	C. F. S.
CFTC	C. F. T. C.
CFTR	C. F. T. R.
CGB	C. G. B.
CGCT	C. G. C. T.
CGE	C. G. E.
CGM	C. G. M.
CGS	C. G. S.
CGT	C. G. T.
CH	C. H.
CHC	C. H. C.
CHG	C. H. G.
CI	C. I.
CIA	C. I. A.
CIBC	C. I. B. C.
CIC	C. I. C.
CID	C. I. D.
CIE	C. I. E.
CIGS	C. I. G. S.
CIM	C. I. M.
CIO	C. I. O.
CIP	C. I. P.
CIR	C. I. R.
CIS	C. I. S.
CIT	C. I. T.
CJ	C. J.
CJI	C. J. I.
CJM	C. J. M.
CK	C. K.
CL	C. L.
CLC	C. L. C.
CLS	C. L. S.
CLU	C. L. U.
CLX	C. L. X.
CM	C. M.
CMA	C. M. A.
CMB	C. M. B.
CMC	C. M. C.
CME	C. M. E.
CMF	C. M. F.
CMI	C. M. I.
CML	C. M. L.
CMO	C. M. O.
CMQ	C. M. Q.
CMS	C. M. S.
CMV	C. M. V.
CMS	C. M. X.
CN	C. N.
CNA	C. N. A.
CNB	C. N. B.
CNBC	C. N. B. C.
CNCL	C. N. C. L.
CNCP	C. N. C. P.
CNFR	C. N. F. R.
CNG	C. N. G.
CNN	C. N. N.
CNOOC	C. N. O. O. C.
CNW	C. N. W.
Corp	Corporation
CP	C. P.
CPA	C. P. A.
CPAC	C. P. A. C.
CPB	C. P. B.
CPC	C. P. C.
CPE	C. P. E.
CPI	C. P. I.
CPL	C. P. L.
CPM	C. P. M.
CPP	C. P. P.
CPR	C. P. R.
CPSC	C. P. S. C.
CPT	C. P. T.
CQ	C. Q.
CR	C. R.
CRA	C. R. A.
CRB	C. R. B.
CRC	C. R. C.
CRI	C. R. I.
CRL	C. R. L.
CRS	C. R. S.
CRT	C. R. T.
CRTC	C. R. T. C.
CRX	C. R. X.
CS	C. S.
CSA	C. S. A.
CSB	C. S. B.
CSC	C. S. C.
CSF	C. S. F.
CSFB	C. S. F. B.
CSI	C. S. I.
CSIS	C. S. I. S.
CSK	C. S. K.
CSO	C. S. O.
CSR	C. S. R.
CSS	C. S. S.
CST	C. S. T.
CSU	C. S. U.
CSV	C. S. V.
CSX	C. S. X.
CT	C. T.
CTA	C. T. A.
CTB	C. T. B.
CTBS	C. T. B. S.
CTC	C. T. C.
CTG	C. T. G.
CTI	C. T. I.
CTK	C. T. K.
CTM	C. T. M.
CTS	C. T. S.
CTV	C. T. V.
CU	C. U.
CUC	C. U. C.
CVB	C. V. B.
CVG	C. V. G.
CVN	C. V. N.
CVNY	C. V. N. Y.
CVS	C. V. S.
CW	C. W.
CWA	C. W. A.
CWB	C. W. B.
CWT	C. W. T.
CX	C. X.
CXR	C. X. R.
DAF	D. A. F.
DAP	D. A. P.
DAX	D. A. X.
DB	D. B.
DBA	D. B. A.
DBI	D. B. I.
DBL	D. B. L.
DBS	D. B. S.
DC	D. C.
DCCC	D. C. C. C.
DCI	D. C. I.
DCNY	D. C. N. Y.
DD	D. D.
DDA	D. D. A.
DDB	D. D. B.
DDC	D. D. C.
DDG	D. D. G.
DDI	D. D. I.
DDR	D. D. R.
DDT	D. D. T.
DEA	D. E. A.
DEC	D. E. C.
DES	D. E. S.
DFA	D. F. A.
DFC	D. F. C.
DFMO	D. F. M. O.
DFS	D. F. S.
DG	D. G.
DGA	D. G. A.
DGPT	D. G. P. T.
DH	D. H.
DHB	D. H. B.
DHL	D. H. L.
DIA	D. I. A.
DIW	D. I. W.
DJ	D. J.
DJIA	D. J. I. A.
DJP	D. J. P.
DJS	D. J. S.
DKB	D. K. B.
DKM	D. K. M.
DL	D. L.
DLC	D. L. C.
DLJ	D. L. J.
DM	D. M.
DMA	D. M. A.
DMB	D. M. B.
DMC	D. M. C.
DMD	D. M. D.
DME	D. M. E.
DMI	D. M. I.
DMS	D. M. S.
DMW	D. M. W.
DMZ	D. M. Z.
DN	D. N.
DNA	D. N. A.
DNC	D. N. C.
DNX	D. N. X.
DOC	D. O. C.
DOD	D. O. D.
DOE	D. O. E.
DOS	D. O. S.
DOT	D. O. T.
DP	D. P.
DPC	D. P. C.
DPG	D. P. G.
DPL	D. P. L.
DPP	D. P. P.
DPS	D. P. S.
DPT	D. P. T.
Dr	Doctor
DRG	D. R. G.
DRI	D. R. I.
DS	D. S.
DSA	D. S. A.
DSC	D. S. C.
DSL	D. S. L.
DSLT	D. S. L. T.
DSM	D. S. M.
DSP	D. S. P.
DST	D. S. T.
DTC	D. T. C.
DTH	D. T. H.
DTI	D. T. I.
DV	D. V.
DVFA	D. V. F. A.
DWG	D. W. G.
DX	D. X.
DYR	D. Y. R.
EA	E. A.
EAC	E. A. C.
EAL	E. A. L.
EAS	E. A. S.
EB	E. B.
EBDC	E. B. D. C.
EBRD	E. B. R. D.
EBS	E. B. S.
EC	E. C.
ECC	E. C. C.
ECD	E. C. D.
ECI	E. C. I.
ECL	E. C. L.
ECPA	E. C. P. A.
ECU	E. C. U.
EDA	E. D. A.
EDB	E. D. B.
EDC	E. D. C.
EDI	E. D. I.
EDM	E. D. M.
EDP	E. D. P.
EDS	E. D. S.
EDT	E. D. T.
EEC	E. E. C.
EECO	E. E. C. O.
EEI	E. E. I.
EEOC	E. E. O. C.
EEP	E. E. P.
EES	E. E. S.
EESP	E. E. S. P.
EF	E. F.
EFA	E. F. A.
EFC	E. F. C.
EG	E. G.
EGA	E. G. A.
EI	E. I.
EIA	E. I. A.
EIB	E. I. B.
EIC	E. I. C.
EIP	E. I. P.
EITC	E. I. T. C.
EIU	E. I. U.
ELN	E. L. N.
EMC	E. M. C.
EMEA	E. M. E. A.
EMI	E. M. I.
EMS	E. M. S.
EMT	E. M. T.
ENI	E. N. I.
ENSR	E. N. S. R.
EP	E. P.
EPA	E. P. A.
EPLF	E. P. L. F.
EPO	E. M. O.
EPO	E. P. O.
EPRI	E. P. R. I.
ERC	E. R. C.
ERG	E. R. G.
ERIS	E. R. I. S.
ERM	E. R. M.
ERO	E. R. O.
ERS	E. R. S.
ES	E. S.
ESA	E. S. A.
ESB	E. S. B.
ESI	E. S. I.
ESL	E. S. L.
ESOP	E. S. O. P.
ESP	E. S. P.
ESPN	E. S. P. N.
ESS	E. S. S.
EST	E. S. T.
ET	E. T.
ETA	E. T. A.
ETBE	E. T. B. E.
ETS	E. T. S.
EU	E. U.
EUA	E. U. A.
EWE	E. W. E.
EXL	E. X. L.
EXP	E. X. P.
EZ	E. Z.
FA	F. A.
FAA	F. A. A.
FAC	F. A. C.
FADA	F. A. D. A.
FAI	F. A. I.
FAO	F. A. O.
FARC	F. A. R. C.
FAS	F. A. S.
FASB	F. A. S. B.
FAZ	F. A. Z.
FBI	F. B. I.
FBS	F. B. S.
FC	F. C.
FCA	F. C. A.
FCB	F. C. B.
FCC	F. C. C.
FCD	F. C. D.
FCMI	F. C. M. I.
FDA	F. D. A.
FDC	F. D. C.
FDIC	F. D. I. C
FDIC	F. D. I. C.
FDN	F. D. N.
FDP	F. D. P.
FDR	F. D. R.
FEA	F. E. A.
FEC	F. E. C.
FEMA	F. E. M. A.
FERC	F. E. R. C.
FF	F. F.
FFA	F. F. A.
FFB	F. F. B.
FFP	F. F. P.
FGH	F. G. H.
FGIC	F. G. I. C.
FH	F. H.
FHA	F. H. A.
FHAA	F. H. A. A.
FHFB	F. H. F. B.
FHLB	F. H. L. B.
FHLBB	F. H. L. B. B.
FHP	F. H. P.
FIA	F. I. A.
FIAC	F. I. A. C.
FICA	F. I. C. A.
FICO	F. I. C. O.
FIFA	F. I. F. A.
FII	F. I. I.
FIP	F. I. P.
FK	F. K.
FKB	F. K. B.
FKI	F. K. I.
FL	F. L.
FLA	F. L. A.
FLX	F. L. X.
FM	F. M.
FMC	F. M. C.
FMHA	F. M. H. A.
FmHA	F. M. H. A.
FMI	F. M. I.
FMLN	F. M. L. N.
FMR	F. M. R.
FMS	F. M. S.
FN	F. N.
FNN	F. N. N.
FNS	F. N. S.
FOMC	F. O. M. C.
FP	F. P.
FPA	F. P. A.
FPC	F. P. C.
FPCO	F. P. C. O.
FPL	F. P. L.
FR	F. R.
FRA	F. R. A.
FS	F. S.
FSA	F. S. A.
FSB	F. S. B.
FSC	F. S. C.
FSD	F. S. D.
FSIA	F. S. I. A.
FSLIC	F. S. L. I. C.
FSLN	F. S. L. N.
FSX	F. S. X.
FT	F. T.
FTC	F. T. C.
FTS	F. T. S.
FTSE	F. T. S. E.
FX	F. X.
FYI	F. Y. I.
GA	G. A.
GAAP	G. A. A. P.
GAC	G. A. C.
GAF	G. A. F.
GAO	G. A. O.
GASB	G. A. S. B.
GATT	G. A. T. T.
GATX	G. A. T. X.
GB	G. B.
GBL	G. B. L.
GBM	G. B. M.
GBS	G. B. S.
GC	G. C.
GCA	G. C. A.
GCC	G. C. C.
GCI	G. C. I.
GDM	G. D. M.
GDP	G. D. P.
GDR	G. D. R.
GE	G. E.
GEC	G. E. C.
GECC	G. E. C. C.
GF	G. F.
GFI	G. F. I.
GFT	G. F. T.
GGK	G. G. K.
GHF	G. H. F.
GHKM	G. H. K. M.
GHR	G. H. R.
GHS	G. H. S.
GHRF	G. H. R. F.
GI	G. I.
GIA	G. I. A.
GIC	G. I. C.
GIS	G. I. S.
GK	G. K.
GKN	G. K. N.
GL	G. L.
GLCM	G. L. C. M.
GLI	G. L. I.
GM	G. M.
GMA	G. M. A.
GMAC	G. M. A. C.
GMBH	G. M. B. H.
GMC	G. M. C.
GMF	G. M. F.
GMHC	G. M. H. C.
GMN	G. M. N.
GMT	G. M. T.
GMTV	G. M. T. V.
GNB	G. N. B.
GNI	G. N. I.
GNMA	G. N. M. A.
GNP	G. N. P.
GOP	G. O. P.
GP	G. P.
GPA	G. P. A.
GPD	G. P. D.
GPG	G. P. G.
GPO	G. P. O.
GPS	G. P. S.
GPT	G. P. T.
GPU	G. P. U.
GQ	G. Q.
GR	G. R.
GRE	G. R. E.
GRI	G. R. I.
GRU	G. R. U.
GS	G. S.
GSA	G. S. A.
GSD	G. S. D.
GSI	G. S. I.
GSL	G. S. L.
GSP	G. S. P.
GSS	G. S. S.
GST	G. S. T.
GSX	G. S. X.
GT	G. T.
GTA	G. T. A.
GTC	G. T. C.
GTE	G. T. E.
GTECH	G. Tech
GTG	G. T. G.
GTI	G. T. I.
GTS	G. T. S.
GV	G. V.
GW	G. W.
GWC	G. W. C.
GXE	G. X. E.
HBJ	H. B. J.
HBM	H. B. M.
HBO	H. B. O.
HCA	H. C. A.
HCC	H. C. C.
HCI	H. C. I.
HCFA	H. C. F. A.
HCFC	H. C. F. C.
HCS	H. C. S.
HD	H. D.
HDL	H. D. L.
HDM	H. D. M.
HDTV	H. D. T. V.
HEI	H. E. I.
HF	H. F.
HFC	H. F. C.
HG	H. G.
HGTV	H. G. T. V.
HH	H. H.
HHB	H. H. B.
HHS	H. H. S.
HILB	H. I. L. B.
HIV	H. I. V.
HK	H. K.
HKSAR	H. K. S. A. R.
HL	H. L.
HLM	H. L. M.
HLX	H. L. X.
HMA	H. M. A.
HMDA	H. M. D. A.
HMG	H. M. G.
HMO	H. M. O.
HMS	H. M. S.
HMSS	H. M. S. S.
HN	H. N.
HNSX	H. N. S. X.
HNV	H. N. V.
HP	H. P.
HPB	H. P. B.
HQ	H. Q.
HR	H. R.
HRB	H. R. B.
HRE	H. R. E.
HRI	H. R. I.
HRS	H. R. S.
HSA	H. S. A.
HSBC	H. S. B. C.
HSH	H. S. H.
HSST	H. S. S. T.
HSV	H. S. V.
HT	H. T.
HTLV	H. T. L. V.
HWC	H. W. C.
HZN	H. Z. N.
IADB	I. A. D. B.
IAE	I. A. E.
IAEA	I. A. E. A.
IAEC	I. A. E. C.
IAFP	I. A. F. P.
IAM	I. A. M.
IATA	I. A. T. A.
IB	I. B.
IBA	I. B. A.
IBAA	I. B. A. A.
IBC	I. B. C.
IBCA	I. B. C. A.
IBES	I. B. E. S.
IBEW	I. B. E. W.
IBH	I. B. H.
IBI	I. B. I.
IBJ	I. B. J.
IBM	I. B. M.
IBP	I. B. P.
IC	I. C.
ICA	I. C. A.
ICAO	I. C. A. O.
ICBM	I. C. B. M.
ICC	I. C. C.
ICCO	I. C. C. O.
ICEE	I. C. E. E.
ICF	I. C. F.
ICG	I. C. G.
ICH	I. C. H.
ICI	I. C. I.
ICL	I. C. L.
ICM	I. C. M.
ICN	I. C. N.
ICO	I. C. O.
ICRP	I. C. R. P.
ICSL	I. C. S. L.
ID	I. D.
IDA	I. D. A.
IDB	I. D. B.
IDC	I. D. C.
IDD	I. D. D.
IDF	I. D. F.
IDG	I. D. G.
IDI	I. D. I.
IDS	I. D. S.
IEA	I. E. A.
IEC	I. E. C.
IEJW	I. E. J. W.
IFA	I. F. A.
IFAR	I. F. A. R.
IFB	I. F. B.
IFC	I. F. C.
IFE	I. F. E.
IFF	I. F. F.
IFI	I. F. I.
IFO	I. F. O.
IFR	I. F. R.
IFRB	I. F. R. B.
IG	I. G.
IGB	I. G. B.
IgG	I. g. G.
IGI	I. G. I.
IGT	I. G. T.
IGX	I. G. X.
IH	I. H.
IHI	I. H. I.
IIGS	I. I. G. S.
IIS	I. I. S.
IIT	I. I. T.
IJ	I. J.
IKEA	I. K. E. A.
IL	I. L.
ILA	I. L. A.
ILC	I. L. C.
ILGWU	I. L. G. W. U.
ILO	I. L. O.
ILS	I. L. S.
IM	I. M.
IMA	I. M. A.
IMC	I. M. C.
IMD	I. M. D.
IMF	I. M. F.
IMG	I. M. G.
IMI	I. M. I.
IMM	I. M. M.
IMO	I. M. O.
IMS	I. M. S.
IMT	I. M. T.
IMU	I. M. U.
INA	I. N. A.
INB	I. N. B.
Inc	Incorporated
IND	I. N. D.
INF	I. N. F.
ING	I. N. G.
INI	I. N. I.
INPO	I. N. P. O.
INR	I. N. R.
INS	I. N. S.
Intl	International
Intercorp	Intercorporation
IOC	I. O. C.
IOR	I. O. R.
IOS	I. O. S.
IOU	I. O. U.
IP	I. P.
IPC	I. P. C.
IPE	I. P. E.
IPFA	I. P. F. A.
IPM	I. P. M.
IPO	I. P. O.
IPS	I. P. S.
IQ	I. Q.
IRA	I. R. A.
IRI	I. R. I.
IRNA	I. R. N. A.
IROC	I. R. O. C.
IRS	I. R. S.
IRT	I. R. T.
ISC	I. S. C.
ISDN	I. S. D. N.
ISE	I. S. E.
ISI	I. S. I.
ISL	I. S. L.
ISM	I. S. M.
ISO	I. S. O.
ISS	I. S. S.
ITA	I. T. A.
ITC	I. T. C.
ITG	I. T. G.
ITN	I. T. N.
ITT	I. T. T.
ITV	I. T. V.
IU	I. U.
IUD	I. U. D.
IUE	I. U. E.
IUR	I. U. R.
IVF	I. V. F.
IVI	I. V. I.
IVIG	I. V. I. G.
IXL	I. X. L.
IWA	I. W. A.
JAL	J. A. L.
JAMA	J. A. M. A.
JATP	J. A. T. P.
JBA	J. B. A.
JC	J. C.
JCB	J. C. B.
JCP	J. C. P.
JCS	J. C. S.
JCT	J. C. T.
JDS	J. D. S.
JEC	J. E. C.
JFA	J. F. A.
JFK	J. F. K.
JGC	J. G. C.
JHM	J. H. M.
JIT	J. I. T.
JLG	J. L. G.
JMB	J. M. B.
JMR	J. M. R.
JOA	J. O. A.
JP	J. P.
JPL	J. P. L.
JPM	J. P. M.
JR	J. R.
JRA	J. R. A.
JSP	J. S. P.
JT	J. T.
JTL	J. T. L.
JTM	J. T. M.
JTPA	J. T. P. A.
JVC	J. V. C.
JVP	J. V. P.
JWD	J. W. D.
JWP	J. W. P.
JWT	J. W. T.
KAL	K. A. L.
KB	K. B.
KBA	K. B. A.
KBGS	K. B. G. S.
KBS	K. B. S.
KC	K. C.
KCBS	K. C. B. S.
KCP	K. C. P.
KCS	K. C. S.
KCST	K. C. S. T.
KD	K. D.
KDD	K. D. D.
KDI	K. D. I.
KETV	K. E. T. V.
KF	K. F.
KFC	K. F. C.
KFF	K. F. F.
KFW	K. F. W.
KG	K. G.
KGaA	K. G. a. A.
KGB	K. G. B.
KGF	K. G. F.
KGMC	K. G. M. C.
KH	K. H.
KHD	K. H. D.
KHJ	K. H. J.
KIC	K. I. C.
KIO	K. I. O.
KK	K. K.
KKB	K. K. B.
KKR	K. K. R.
KLA	K. L. A.
KLM	K. L. M.
KLP	K. L. P.
KLUC	K. L. U. C.
KMA	K. M. A.
KMET	K. M. E. T.
KMG	K. M. G.
KMS	K. M. S.
KMT	K. M. T.
KMW	K. M. W.
KN	K. N.
KNON	K. N. O. N.
KOP	K. O. P.
KPAX	K. P. A. X.
KPC	K. P. C.
KPFK	K. P. F. K.
KPMG	K. P. M. G.
KPRC	K. P. R. C.
KSI	K. S. I.
KSZ	K. S. Z.
KTF	K. T. F.
KTM	K. T. M.
KTWV	K. T. W. V.
KV	K. V.
KVIL	K. V. I. L.
KW	K. W.
KWU	K. W. U.
KZKC	K. Z. K. C.
LA	L. A.
LB	L. B.
LBJ	L. B. J.
LBO	L. B. O.
LBS	L. B. S.
LCA	L. C. A.
LCD	L. C. D.
LCG	L. C. G.
LCI	L. C. I.
LCP	L. C. P.
LDC	L. D. C.
LDDS	L. D. D. S.
LDI	L. D. I.
LDL	L. D. L.
LDP	L. D. P.
LDS	L. D. S.
LDX	L. D. X.
LFB	L. F. B.
LFC	L. F. C.
LG	L. G.
LGP	L. G. P.
LH	L. H.
LHS	L. H. S.
LHX	L. H. X.
LIC	L. I. C.
LiFeS	L. i. F. e. S.
LIG	L. I. G.
LIN	L. I. N.
LIPA	L. I. P. A.
LISC	L. I. S. C.
LJN	L. J. N.
LL	L. L.
LLC	L. L. C.
LME	L. M. E.
LMT	L. M. T.
LN	L. N.
LNG	L. N. G.
LNR	L. N. R.
LNS	L. N. S.
LOF	L. O. F.
LOR	L. O. R.
LOT	L. O. T.
LP	L. P.
LPC	L. P. C.
LPGA	L. P. G. A.
LPL	L. P. L.
LPP	L. P. P.
LS	L. S.
LSB	L. S. B.
LSC	L. S. C.
LSD	L. S. D.
LSI	L. S. I.
LSU	L. S. U.
LT	L. T.
LTCB	L. T. C. B.
LTD	L. T. D.
LTV	L. T. V.
LTX	L. T. X.
LVI	L. V. I.
LVMH	L. V. M. H.
LX	L. X.
LY	L. Y.
MAI	M. A. I.
MB	M. B.
MBA	M. B. A.
MBAA	M. B. A. A.
MBB	M. B. B.
MBE	M. B. E.
MBF	M. B. F.
MBFR	M. B. F. R.
MBH	M. B. H.
MBI	M. B. I.
MBIA	M. B. I. A.
MBS	M. B. S.
MC	M. C.
MCA	M. C. A.
MCC	M. C. C.
MCCP	M. C. C. P.
MCEG	M. C. E. G.
MCI	M. C. I.
MCM	M. C. M.
MCN	M. C. N.
MCO	M. C. O.
MCP	M. C. P.
MCS	M. C. S.
MD	M. D.
MDA	M. D. A.
MDB	M. D. B.
MDC	M. D. C.
MDI	M. D. I.
MDM	M. D. M.
MDT	M. D. T.
MEBA	M. E. B. A.
MEI	M. E. I.
MEK	M. E. K.
MEM	M. E. M.
MEPC	M. E. P. C.
MFA	M. F. A.
MFI	M. F. I.
MFL	M. F. L.
MFN	M. F. N.
MFS	M. F. S.
MGC	M. G. C.
MGI	M. G. I.
MGM	M. G. M.
MH	M. H.
MHA	M. H. A.
MHC	M. H. C.
MHI	M. H. I.
MHP	M. H. P.
MHQ	M. H. Q.
MI	M. I.
MIA	M. I. A.
MICC	M. I. C. C.
MIGA	M. I. G. A.
MIM	M. I. M.
MIP	M. I. P.
MIPS	M. I. P. S.
MIS	M. I. S.
MIT	M. I. T.
MITI	M. I. T. I.
MK	M. K.
MKI	M. K. I.
ML	M. L.
MLP	M. L. P.
MLPI	M. L. P. I.
MLS	M. L. S.
MLX	M. L. X.
MMAC	M. M. A. C.
MMC	M. M. C.
MMI	M. M. I.
MMPI	M. M. P. I.
MMR	M. M. R.
MMS	M. M. S.
MMWEC	M. M. W. E. C.
MNC	M. N. C.
MNet	M. Net
MNX	M. N. X.
MP	M. P.
MPAA	M. P. A. A.
MPB	M. P. B.
MPLA	M. P. L. A.
MPS	M. P. S.
MPT	M. P. T.
MPTP	M. P. T. P.
MPV	M. P. V.
MRC	M. R. C.
MRCA	M. R. C. A.
MRI	M. R. I.
MRP	M. R. P.
MRTA	M. R. T. A.
MS	M. S.
MSA	M. S. A.
MSHA	M. S. H. A.
MSI	M. S. I.
MSL	M. S. L.
MSM	M. S. M.
MSOE	M. S. O. E.
MSP	M. S. P.
MSRB	M. S. R. B.
MSU	M. S. U.
MSX	M. S. X.
MTA	M. T. A.
MTB	M. T. B.
MTBE	M. T. B. E.
MTech	M. Tech
MTI	M. T. I.
MTM	M. T. M.
MTR	M. T. R.
MTS	M. T. S.
MTU	M. T. U.
MTV	M. T. V.
MV	M. V.
MVP	M. V. P.
MVS	M. V. S.
MX	M. X.
NA	N. A.
NAACP	N. Double A. C. P.
NAC	N. A. C.
NACA	N. A. C. A.
NACM	N. A. C. M.
NAD	N. A. D.
NAEIR	N. A. E. I. R.
NAEP	N. A. E. P.
NAHB	N. A. H. B.
NAIC	N. A. I. C.
NAL	N. A. L.
NALU	N. A. L. U.
NAM	N. A. M.
NAPAP	N. A. P. A. P.
NAPM	N. A. P. M.
NAR	N. A. R.
NARFE	N. A. R. F. E.
NAS	N. A. S.
#NASA	N. A. S. A.
NASD	N. A. S. D.
NASSA	N. A. S. S. A.
NATCA	N. A. T. C. A.
NAV	N. A. V.
NBA	N. B. A.
NBC	N. B. C.
NBD	N. B. D.
NBER	N. B. E. R.
NBI	N. B. I.
NBO	N. B. O.
NBS	N. B. S.
NC	N. C.
NCA	N. C. A.
NCAA	N. C. A. A.
NCB	N. C. B.
NCC	N. C. C.
NCI	N. C. I.
NCIF	N. C. I. F.
NCMS	N. C. M. S.
NCNB	N. C. N. B.
NCR	N. C. R.
NCTA	N. C. T. A.
NDF	N. D. F.
NDI	N. D. I.
NDP	N. D. P.
NEA	N. E. A.
NEC	N. E. C.
NEH	N. E. H.
NEI	N. E. I.
NESB	N. E. S. B.
NETAAC	N. E. T. A. A. C.
NFA	N. F. A.
NFC	N. F. C.
NFIB	N. F. I. B.
NFIC	N. F. I. C.
NFL	N. F. L.
NFPA	N. F. P. A.
NFS	N. F. S.
NFSW	N. F. S. W.
NGL	N. G. L.
NH	N. H.
NHK	N. H. K.
NHL	N. H. L.
NHS	N. H. S.
NHTSA	N. H. T. S. A.
NI	N. I.
NIA	N. I. A.
NIC	N. I. C.
NIDA	N. I. D. A.
NIH	N. I. H.
NIMH	N. I. M. H.
NIOSH	N. I. O. S. H.
NIS	N. I. S.
NJ	N. J.
NKF	N. K. F.
NKK	N. K. K.
NKVD	N. K. V. D.
NL	N. L.
NLD	N. L. D.
NLI	N. L. I.
NLM	N. L. M.
NLO	N. L. O.
NLRB	N. L. R. B.
NM	N. M.
NME	N. M. E.
NMP	N. M. P.
NMS	N. M. S.
NMTBA	N. M. T. B. A.
NMU	N. M. U.
NOAA	N. O. A. A.
NOX	N. O. X.
NPA	N. P. A.
NPC	N. P. C.
NPD	N. P. D.
NPM	N. P. M.
NRA	N. R. A.
NRC	N. R. C.
NRDC	N. R. D. C.
NRECA	N. R. E. C. A.
NRM	N. R. M.
NS	N. S.
NSA	N. S. A.
NSC	N. S. C.
NSF	N. S. F.
NSM	N. S. M.
NSPA	N. S. P. A.
NT	N. T.
NTC	N. T. C.
NTG	N. T. G.
NTIA	N. T. I. A.
NTN	N. T. N.
NTSB	N. T. S. B.
NTT	N. T. T.
NTX	N. T. X.
NUI	N. U. I.
NUM	N. U. M.
NUS	N. U. S.
NV	N. V.
NVF	N. V. F.
NW	N. W.
NWA	N. W. A.
NWQ	N. W. Q.
NX	N. X.
NY	N. Y.
NYC	N. Y. C.
NYCB	N. Y. C. B.
NYCE	N. Y. C. E.
NYFE	N. Y. F. E.
NYSE	N. Y. S. E.
NYT	N. Y. T.
NYU	N. Y. U.
NZI	N. Z. I.
OAG	O. A. G.
OAS	O. A. S.
OASDI	O. A. S. D. I.
OAT	O. A. T.
OCC	O. C. C.
OCE	O. C. E.
OCR	O. C. R.
OCS	O. C. S.
OCU	O. C. U.
ODS	O. D. S.
OEC	O. E. C.
OECD	O. E. C. D.
OED	O. E. D.
OEL	O. E. L.
OEM	O. E. M.
OEX	O. E. X.
OG	O. G.
OIRA	O. I. R. A.
OIS	O. I. S.
OK	O. K.
OKC	O. K. C.
OMB	O. M. B.
OMI	O. M. I.
OMV	O. M. V.
ONG	O. N. G.
OPIC	O. P. I. C.
OPM	O. P. M.
ORI	O. R. I.
ORS	O. R. S.
OS	O. S.
OSF	O. S. F.
OSI	O. S. I.
OSS	O. S. S.
OTA	O. T. A.
OTC	O. T. C.
OTF	O. T. F.
OTN	O. T. N.
OTS	O. T. S.
OTV	O. T. V.
OV	O. V.
PA	P. A.
PAE	P. A. E.
PAK	P. A. K.
PATC	P. A. T. C.
PB	P. B.
PBA	P. B. A.
PBGC	P. B. G. C.
PBHG	P. B. H. G.
PBI	P. B. I.
PBR	P. B. R.
PBS	P. B. S.
PBX	P. B. X.
PC	P. C.
PCA	P. C. A.
PCB	P. C. B.
PCC	P. C. C.
PCE	P. C. E.
PCI	P. C. I.
PCjr	P. C. Junior
PCL	P. C. L.
PCM	P. C. M.
PCMCIA	P. C. M. C. I. A.
PCN	P. C. N.
PCP	P. C. P.
PCR	P. C. R.
PCS	P. C. S.
PCW	P. C. W.
PD	P. D.
PDA	P. D. A.
PDF	P. D. F.
PDI	P. D. I.
PDLA	P. D. L. A.
PDR	P. D. R.
PDT	P. D. T.
PE	P. E.
PECC	P. E. C. C.
PF	P. F.
PFM	P. F. M.
PG	P. G.
PGA	P. G. A.
PGH	P. G. H.
PhD	P. H. D.
Ph.D	P. H. D.
Ph.D.s	P. H. D.s
Ph.Ds	P. H. D.s
PhDs	P. H. D.s
PHH	P. H. H.
PHLCorp	P. H. L. Corporation
PHM	P. H. M.
PHP	P. H. P.
PHPO	P. H. P. O.
PI	P. I.
PIK	P. I. K.
PIP	P. I. P.
PIR	P. I. R.
PIW	P. I. W.
PL	P. L.
PLC	P. L. C.
PLE	P. L. E.
PLM	P. L. M.
PLO	P. L. O.
PM	P. M.
PMA	P. M. A.
PMC	P. M. C.
PMDB	P. M. D. B.
PMI	P. M. I.
PMS	P. M. S.
PMT	P. M. T.
PNB	P. N. B.
PNC	P. N. C.
PNG	P. N. G.
PNM	P. N. M.
PNOC	P. N. O. C.
POW	P. O. W.
PP	P. P.
PPD	P. P. D.
PPG	P. P. G.
PPI	P. P. I.
PPM	P. P. M.
PPO	P. P. O.
PPP	P. P. P.
PQQ	P. Q. Q.
PR	P. R.
PRB	P. R. B.
PRC	P. R. C.
PRD	P. R. D.
PRI	P. R. I.
PRSA	P. R. S. A.
Pvt	Private
PRK	P. R. K.
PRP	P. R. P.
PS	P. S.
PSA	P. S. A.
PSC	P. S. C.
PSE	P. S. E.
PSG	P. S. G.
PSI	P. S. I.
PSNH	P. S. N. H.
PSR	P. S. R.
PST	P. S. T.
PSUM	P. S. U. M.
PT	P. T.
PTA	P. T. A.
PTI	P. T. I.
PTL	P. T. L.
PTT	P. T. T.
PUC	P. U. C.
PV	P. V.
PVC	P. V. C.
PW	P. W.
PWA	P. W. A.
PWS	P. W. S.
PX	P. X.
PYA	P. Y. A.
QB	Q. B.
QDE	Q. D. E.
QE	Q. E.
QFB	Q. F. B.
QMS	Q. M. S.
QO	Q. O.
QVC	Q. V. C.
RAC	R. A. C.
RAF	R. A. F.
RAI	R. A. I.
RB	R. B.
RBC	R. B. C.
RC	R. C.
RCA	R. C. A.
RCI	R. C. I.
RCM	R. C. M.
RD	R. D.
RDF	R. D. F.
RDP	R. D. P.
REIT	R. E. I. T.
RF	R. F.
RFC	R. F. C.
RFD	R. F. D.
RFE	R. F. E.
RFI	R. F. I.
RFTV	R. F. T. V.
RG	R. G.
RHI	R. H. I.
RHM	R. H. M.
RI	R. I.
RJ	R. J.
RJR	R. J. R.
RKO	R. K. O.
RL	R. L.
RLC	R. L. C.
RLI	R. L. I.
RLR	R. L. R.
RMC	R. M. C.
RMI	R. M. I.
RMJ	R. M. J.
RMS	R. M. S.
RMV	R. M. V.
RNA	R. N. A.
RNC	R. N. C.
RO	R. O.
ROA	R. O. A.
ROC	R. O. C.
ROTC	R. O. T. C.
RPA	R. P. A.
RPM	R. P. M.
RREEF	R. R. E. E. F.
RS	R. S.
RSC	R. S. C.
RSCG	R. S. C. G.
RSI	R. S. I.
RSO	R. S. O.
RSV	R. S. V.
RT	R. T.
RTBF	R. T. B. F.
RTC	R. T. C.
RTE	R. T. E.
RTHK	R. T. H. K.
RTL	R. T. L.
RTM	R. T. M.
RTS	R. T. S.
RTZ	R. T. Z.
RU	R. U.
RUC	R. U. C.
RV	R. V.
RWE	R. W. E.
RX	R. X.
SA	S. A.
SAA	S. A. A.
SAB	S. A. B.
SACC	S. A. C. C.
SACP	S. A. C. P.
SAI	S. A. I.
SAL	S. A. L.
SALP	S. A. L. P.
SAO	S. A. O.
SAPC	S. A. P. C.
SAS	S. A. S.
SAT	S. A. T.
SB	S. B.
SBA	S. B. A.
SBC	S. B. C.
SBCI	S. B. C. I.
SBIC	S. B. I. C.
SBIR	S. B. I. R.
SBK	S. B. K.
SBS	S. B. S.
SC	S. C.
SCA	S. C. A.
SCE	S. C. E.
SCEcorp	S. C. E. Corporation
SCI	S. C. I.
SCM	S. C. M.
SD	S. D.
SDA	S. D. A.
SDC	S. D. C.
SDG	S. D. G.
SDI	S. D. I.
SDP	S. D. P.
SDR	S. D. R.
SDRC	S. D. R. C.
SDS	S. D. S.
SE	S. E.
SEC	S. E. C.
SEEQ	S. E. E. Q.
SEI	S. E. I.
SEL	S. E. L.
SEM	S. E. M.
SES	S. E. S.
SF	S. F.
SFC	S. F. C.
SFE	S. F. E.
SFN	S. F. N.
SFO	S. F. O.
SGB	S. G. B.
SGC	S. G. C.
SGI	S. G. I.
SGS	S. G. S.
SH	S. H.
SHL	S. H. L.
SHV	S. H. V.
SI	S. I.
SIA	S. I. A.
SIB	S. I. B.
SIBV	S. I. B. V.
SIPC	S. I. P. C.
SIV	S. I. V.
SJNB	S. J. N. B.
SK	S. K.
SKF	S. K. F.
SKK	S. K. K.
SL	S. L.
SLA	S. L. A.
SLH	S. L. H.
SLM	S. L. M.
SLR	S. L. R.
SMC	S. M. C.
SME	S. M. E.
SMES	S. M. E. S.
SMR	S. M. R.
SMS	S. M. S.
SMU	S. M. U.
SMUD	S. M. U. D.
SNC	S. N. C.
SNCF	S. N. C. F.
SNET	S. N. E. T.
SNIA	S. N. I. A.
SNL	S. N. L.
SNPE	S. N. P. E.
SOES	S. O. E. S.
SOS	S. O. S.
SP	S. P.
SPD	S. P. D.
SPE	S. P. E.
SPEP	S. Pep
SPG	S. P. G.
SPI	S. P. I.
SPS	S. P. S.
SPSF	S. P. S. F.
SPX	S. P. X.
SpA	Company
S.p.A	Company
SQL	S. Q. L.
SR	S. R.
SRI	S. R. I.
SRK	S. R. K.
SRL	S. R. L.
SRO	S. R. O.
SRS	S. R. S.
SS	S. S.
SSA	S. S. A.
SSB	S. S. B.
SSBI	S. S. B. I.
SSC	S. S. C.
SSI	S. S. I.
SSMC	S. S. M. C.
SSN	S. S. N.
SSP	S. S. P.
SST	S. S. T.
STC	S. T. C.
Ste	Saint
STS	S. T. S.
SVP	S. V. P.
SX	S. X.
TA	T. A.
TB	T. B.
TBA	T. B. A.
TBC	T. B. C.
TBF	T. B. F.
TBG	T. B. G.
TBK	T. B. K.
TBN	T. B. N.
TBS	T. B. S.
TBWA	T. B. W. A.
TC	T. C.
TCA	T. C. A.
TCBY	T. C. B. Y.
TCC	T. C. C.
TCF	T. C. F.
TCI	T. C. I.
TCMP	T. C. M. P.
TCP	T. C. P.
TCS	T. C. S.
TCU	T. C. U.
TCW	T. C. W.
TD	T. D.
TDD	T. D. D.
TDK	T. D. K.
TDU	T. D. U.
TE	T. E.
TEC	T. E. C.
TEP	T. E. P.
TF	T. F.
TFBA	T. F. B. A.
TFD	T. F. D.
TFF	T. F. F.
TFR	T. F. R.
TGI	T. G. I.
TGL	T. G. L.
TGWU	T. G. W. U.
THA	T. H. A.
THI	T. H. I.
THT	T. H. T.
TI	T. I.
TIAA	T. I. A. A.
TII	T. I. I.
TIL	T. I. L.
TIMI	T. I. M. I.
TJ	T. J.
TJX	T. J. X.
TKR	T. K. R.
TLC	T. L. C.
TM	T. M.
TMC	T. M. C.
TMI	T. M. I.
TMIC	T. M. I. C.
TMK	T. M. K.
TMOC	T. M. O. C.
TNA	T. N. A.
TNF	T. N. F.
TNM	T. N. M.
TNP	T. N. P.
TNT	T. N. T.
TOA	T. O. A.
TPA	T. P. A.
tPA	t. P. A.
TPF	T. P. F.
TPI	T. P. I.
TPS	T. P. S.
TR	T. R.
TRC	T. R. C.
TRE	T. R. E.
TRO	T. R. O.
TRS	T. R. S.
TRT	T. R. T.
TRW	T. R. W.
TS	T. S.
TSA	T. S. A.
TSB	T. S. B.
TSE	T. S. E.
TSF	T. S. F.
TSI	T. S. I.
TSO	T. S. O.
TSSU	T. S. S. U.
TTAC	T. T. A. C.
TTAPS	T. T. A. P. S.
TU	T. U.
TV	T. V.
TVA	T. V. A.
TVI	T. V. I.
TVS	T. V. S.
TVSM	T. V. S. M.
TVX	T. V. X.
TW	T. W.
TWA	T. W. A.
TX	T. X.
TXI	T. X. I.
TXL	T. X. L.
TXO	T. X. O.
UA	U. A.
UAE	U. A. E.
UAL	U. A. L.
UAP	U. A. P.
UAW	U. A. W.
UBAF	U. B. A. F.
UBS	U. B. S.
UC	U. C.
UCLA	U. C. L. A.
UCLAF	U. C. L. A. F.
UCSD	U. C. S. D.
UCSF	U. C. S. F.
UD	U. D.
UDAG	U. D. A. G.
UDC	U. D. C.
UDF	U. D. F.
UEI	U. E. I.
UFO	U. F. O.
UFT	U. F. T.
UFW	U. F. W.
UGI	U. G. I.
UH	U. H.
UHF	U. H. F.
UHL	U. H. L.
UI	U. I.
UIC	U. I. C.
UIS	U. I. S.
UJA	U. J. A.
UK	U. K.
UKI	U. K. I.
ULI	U. L. I.
UMBC	U. M. B. C.
UMC	U. M. C.
UMNO	U. M. N. O.
UMTA	U. M. T. A.
UMW	U. M. W.
UNAM	U. N. A. M.
UNC	U. N. C.
UNCF	U. N. C. F.
UNDP	U. N. D. P.
UNHCR	U. N. H. C. R.
UNLV	U. N. L. V.
UNR	U. N. R.
UOP	U. O. P.
UPC	U. P. C.
UPI	U. P. I.
UPS	U. P. S.
URS	U. R. S.
URW	U. R. W.
US	U. S.
USA	U. S. A.
U.S.A	U. S. A.
USAA	U. S. A. A.
USACafes	U. S. A. Cafes
USADirect	U. S. A. Direct
USAir	U. S. Air
USC	U. S. C.
USCB	U. S. C. B.
USDA	U. S. D. A.
USF	U. S. F.
USFL	U. S. F. L.
USG	U. S. G.
USH	U. S. H.
USI	U. S. I.
USIA	U. S. I. A.
USLico	U. S. Lico
USLife	U. S. Life
USO	U. S. O.
USOC	U. S. O. C.
Uspci	U. S. P. C. I.
USPS	U. S. P. S.
USS	U. S. S.
USSC	U. S. S. C.
USSR	U. S. S. R.
UST	U. S. T.
USW	U. S. W.
USX	U. S. X.
UT	U. T.
UTA	U. T. A.
UTC	U. T. C.
UTL	U. T. L.
UTU	U. T. U.
UV	U. V.
UX	U. X.
VA	V. A.
VAAP	V. A. A. P.
VAD	V. A. D.
VAN	V. A. N.
VBI	V. B. I.
VC	V. C.
VCI	V. C. I.
VCR	V. C. R.
VCRS	V. C. R. S.
VCS	V. C. S.
VD	V. D.
VDT	V. D. T.
VF	V. F.
VFW	V. F. W.
VG	V. G.
VGA	V. G. A.
VH	V. H.
VHA	V. H. A.
VHF	V. H. F.
VHL	V. H. L.
VHS	V. H. S.
VIP	V. I. P.
VIR	V. I. R.
VISN	V. I. S. N.
VJN	V. J. N.
VLI	V. L. I.
VLSI	V. L. S. I.
VM	V. M.
VMS	V. M. S.
VMX	V. M. X.
VNA	V. N. A.
VNR	V. N. R.
VNU	V. N. U.
VO	V. O.
VOA	V. O. A.
VOR	V. O. R.
VP	V. P.
VPI	V. P. I.
VPT	V. P. T.
VQT	V. Q. T.
VR	V. R.
VRA	V. R. A.
VS	V. S.
VSAT	V. S. A. T.
VSB	V. S. B.
VTC	V. T. C.
VTR	V. T. R.
VTX	V. T. X.
VW	V. W.
VWR	V. W. R.
WABC	W. A. B. C.
WAFA	W. A. F. A.
WASP	W. A. S. P.
WATS	W. A. T. S.
WB	W. B.
WBA	W. B. A.
WBAI	W. B. A. I.
WBBM	W. B. B. M.
WBZ	W. B. Z.
WCBS	W. C. B. S.
WCI	W. C. I.
WCIX	W. C. I. X.
WCK	W. C. K.
WCRS	W. C. R. S.
WCVB	W. C. V. B.
WD	W. D.
WDB	W. D. B.
WEFA	W. E. F. A.
WEG	W. E. G.
WestLB	West L. B.
WEU	W. E. U.
WFAN	W. F. A. N.
WFBQ	W. F. B. Q.
WFC	W. F. C.
WFIA	W. F. I. A.
WFLA	W. F. L. A.
WFRR	W. F. R. R.
WFXT	W. F. X. T.
WGA	W. G. A.
WGBH	W. G. B. H.
WGC	W. G. C.
WGHP	W. G. H. P.
WGMS	W. G. M. S.
WGN	W. G. N.
WHAS	W. H. A. S.
WHBQ	W. H. B. Q.
WIC	W. I. C.
WITI	W. I. T. I.
WJBK	W. J. B. K.
WJW	W. J. W.
WKYS	W. K. Y. S.
WLR	W. L. R.
WM	W. M.
WMAQ	W. M. A. Q.
WMG	W. M. G.
WMMS	W. M. M. S.
WMS	W. M. S.
WNAC	W. N. A. C.
WNBC	W. N. B. C.
WNCN	W. N. C. N.
WNET	W. N. E. T.
WNEW	W. N. E. W.
WNS	W. N. S.
WNW	W. N. W.
WNYC	W. N. Y. C.
WNYW	W. N. Y. W.
WOJB	W. O. J. B.
WOMC	W. O. M. C.
WOR	W. O. R.
WPA	W. P. A.
WPBF	W. P. B. F.
WPGH	W. P. G. H.
WPIX	W. P. I. X.
WPP	W. P. P.
WPPSS	W. P. P. S. S.
WQHT	W. Q. H. T.
WQTV	W. Q. T. V.
WQUE	W. Q. U. E.
WR	W. R.
WRKO	W. R. K. O.
WROR	W. R. O. R.
WSBK	W. S. B. K.
WSCV	W. S. C. V.
WSGP	W. S. G. P.
WSJ	W. S. J.
WSVN	W. S. V. N.
WTBS	W. T. B. S.
WTC	W. T. C.
WTD	W. T. D.
WTI	W. T. I.
WTLV	W. T. L. V.
WTO	W. T. O.
WTTV	W. T. T. V.
WTVJ	W. T. V. J.
WTVT	W. T. V. T.
WTXF	W. T. X. F.
WW	W. W.
WWII	W. W. I. I.
WWL	W. W. L.
WWOR	W. W. O. R.
WXRK	W. X. R. K.
WYLD	W. Y. L. D.
WYNY	W. Y. N. Y.
WZTV	W. Z. T. V.
XA	X. A.
XE	X. E.
XJ	X. J.
XL	X. L.
XMP	X. M. P.
XP	X. P.
XR	X. R.
XT	X. T.
XTC	X. T. C.
XYZ	X. Y. Z.
YMCA	Y. M. C. A.
YSL	Y. S. L.
YTT	Y. T. T.
YWCA	Y. W. C. A.
ZCB	Z. C. B.
ZDF	Z. D. F.
ZMI	Z. M. I.
ZR	Z. R.
ZTS	Z. T. S.
ZX	Z. X.
