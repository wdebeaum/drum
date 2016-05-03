package TextPP::NumProc;

=head1 NAME

TextPP::NumProc - A numerical preprocessor.

=head1 SYNOPSIS

  use TextPP::NumProc;
  
  \$new_text = NumProc(\$text);

  TextPP::AbbrProc::setverbose(1);

=head1 DESCRIPTION

From the original Perl script:

# preprocessor for WSJ
# assumes 1 sentence per line
#
# 1.  expand numerical exceptions: eg. 386
# 2.  do regular numerical expansions

=head2 EXPORT

None by default.

=head1 BUGS

Lots of "unitialized" warnings -- seemingly references past the end of some 
array; they don't appear to be pernicious, but it is hard to say whether this 
was intended (ie, tests were ommitted for efficiency reasons knowing that the 
behavior would be correct anyway) or the code is just poorly written. For 
convenience, 'uninitialized' warnings are turned off.

=head1 HISTORY

Perl script: # $Id: NumProc.pm,v 1.3 2007/07/16 13:51:02 lgalescu Exp $

	     # Minor modifications by David Graff, Linguistic Data Consortium, 
	     #   in preparation for publishing on cdrom;  Aug. 11, 1994.

	     # Minor modifications by Lucian Galescu, University of Rochester; 
             #   Sep. 25, 2001

2005/12/06 Lucian Galescu, E<lt>lgalescu@ihmc.usE<gt>
Perl module ready.

2006/01/25 Lucian Galescu, E<lt>lgalescu@ihmc.usE<gt>
Added <name> to allowable SGML tags.

=head1 AUTHOR

Lucian Galescu, E<lt>lgalescu@ihmc.usE<gt>

=head1 COPYRIGHT AND LICENSE

Original script copyright notice:

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
use warnings;
no warnings 'uninitialized';

require Exporter;

our @ISA = qw(Exporter);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration	use TextPP::NumProc ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.

our @EXPORT_OK = qw(
	&NumProc
);

our $VERSION = '0.01';


#----------------------------------------------------------------
# Globals
#
my $POINT='.POINT';		# orthographic notation for .

	# final s in name indicates plural version, otherwise just add s
my @ones_z=("zero","one","two","three","four",
	    "five","six","seven","eight","nine");
my @ones_oh=("oh","one","two","three","four",
	     "five","six","seven","eight","nine");
my @ten=("","ten","twenty","thirty","forty","fifty",
	 "sixty","seventy","eighty","ninety");
my @teen=("ten","eleven","twelve","thirteen","fourteen","fifteen",
	  "sixteen","seventeen","eighteen","nineteen");
my @mult=("","thousand","million","billion","trillion",
	  "quadrillion","quintillion","sextillion","septillion","octillion");
my @den=("","","half","third","quarter","fifth",
	 "sixth","seventh","eighth","ninth","tenth",
	 "eleventh","twelfth","thirteenth","fourteenth","fifteenth",
	 "sixteenth","seventeenth","eighteenth","nineteenth");
my @largeden=("","first","second","third","fourth","fifth",
	      "sixth","seventh","eighth","ninth","tenth",
	      "eleventh","twelfth","thirteenth","fourteenth","fifteenth",
	      "sixteenth","seventeenth","eighteenth","nineteenth");
my @ordnal=("","first","second","third","fourth","fifth",
	    "sixth","seventh","eighth","ninth","tenth",
	    "eleventh","twelfth","thirteenth","fourteenth","fifteenth","sixteenth");
my @months=("Jan.","Feb.","Mar.","Apr.","Jun.","Jul.","Aug.","Sept.","Oct.",
	    "Nov.","Dec.","January","February","March","April","May","June",
	    "July","August","September","October","November","December");

my (%sing_dollar, %sernowd, %except, %months);

my (@input, @output, $field, $front, $back, $x, $next, $next2, $last, $this);
my ($appendflg, $commanextflg, $leadingzeroflg);

my $vflg = 0;

#----------------------------------------------------------------
# Methods
#
sub setverbose {
    $vflg = shift;
}

sub loadexcp {
    my ($y, $x, $n);

    while(<DATA>)
    {
	if ($vflg) { warn "[DATA]" . $_; }

	if(/^#/) {next;}	# comment
	s/\n//;
	if(!$_) {next;}		# blank
	$y=$_;
	s/^(\S+)\s*//;		# extract 1st word
	$x=$1;
	if($x eq "") {&perr2("__DATA__: no word: $y");}
	if($x =~ /^\$\$/)		# $$word => skip
	{	$x =~ s/^\$*//;
		$sing_dollar{$x}=2;
	}
	elsif($x =~ /^\$/)		# $word => singular right context
	{	$x =~ s/^\$*//;
		$sing_dollar{$x}=1;
	}
	elsif($x =~ /^\*/)
	{	$x =~ s/\**//g;
		if(!$x) {&perr2("__DATA__: no serno word");}
		$sernowd{$x}=1;		# serial no words
	}
	else
	{	if($x !~ /\d/) {&perr2("__DATA__: non-numerical key");}
		if(!$_) {&perr2("__DATA__: no value");}

		$except{$x}=$_;		# translations
	}
	$n++;
    }
    if($vflg) {warn "$n lines read from exceptions file\n";}
}

sub makehash {
    my $i;

    for($i=0;$i<=$#months;$i++)	# make months hash
    {	$_=$months[$i];
	$months{$_}=1;		# mixed case
	tr/a-z/A-Z/;
	$months{$_}=1;		# UC
    }
}

sub prepare {
    # load exceptions
    loadexcp();
    # make hash
    makehash();
}

sub NumProc {
    my $in = shift;
    my $out;

    # prepare data structures
    prepare();

    # sgml tags
    my $sgml_pattern = "<\/?(art|p|s|name)";

    # split into lines;
    my @lines = split(/\n/, $in);

    my $line;

    foreach $line (@lines) 
    {	# removed local($front,$back,$x) to conserve memory RWM 8/96
	$_ = $line;

##############################  exceptproc  ##################################
	s/^\s*//;
	s/\n//o;
	if($vflg) {warn "input:\t$_\n";}
	if(/\d/ && !/^$sgml_pattern/)		# opt and protect sgml
	{	@input = split(/\s+/o);
		@output=();
		for($field=0;$field<=$#input;$field++)	# $field is global
		{	$_=$input[$field];
	
			if(!/\d/)			# only processes numbers
			{	&pusho($input[$field]);		# not processed
				next;
			}
	
			s/^(\W*)//o;		# strip front
			$front=$1;
			if($front =~ /\$$/ || $front =~ /#$/)	# protect money
			{	&pusho($input[$field]);		# not processed
				next;
			}
	
			s/(\W*)$//o;		# strip back
			$back=$1;
	
			if($front =~ /\'$/ && $except{"'$_"})	# eg "'20s"
			{	$front =~ s/\'$//;
				if($front) 
				{	&pusho($front);
					if($front !~ /[\w]$/o) {$appendflg=1;}
				}
	
				&pusho($except{"'$_"});		# translation
					
				if($back)
				{	if($back !~ /^[\w]/o) {&appendo($back);}
					else {&pusho($back);}
				}
			}
			elsif($except{$_})
			{	if($front) 
				{	&pusho($front);
					if($front !~ /[\w]$/o) {$appendflg=1;}
				}
	
				&pusho($except{$_});		# translation
					
				if($back)
				{	if($back !~ /^[\w]/o) {&appendo($back);}
					else {&pusho($back);}
				}
			}
			else {&pusho($input[$field]);}		# not processed
		}
		$_=join(" ",@output);
	}
	s/\s+/ /g;
	s/^ //o;
	s/ $//o;
	if($vflg) {warn "ex:\t$_\n";}

############################  numproc  ########################################
	if(!/^$sgml_pattern/)			# protect sgml, also art
	{	s/(\d+)-(\d+)-(\d+)/$1 - $2 - $3/g;	# eg. 1-2-3
		s/(\d+)x(\d+)/$1 by $2/g;		# eg. 2x4
		s/(\d+)\+(\d+)/$1 plus $2/g;		# eg. 2+2
		s=(\d)-(\d)[/\\](\d)=$1 $2/$3=g;	# e.g. 3-1/2
		s=(\d)\\(\d)=$1/$2=g;			# e.g. 1\2 for 1/2
		s/\$(\d[\d,]*)-\$(\d)/$1 to \$$2/g;	# $ range: eg. $1-$2
		s/\$(\d[\d,]*)-(\d)/$1 to \$$2/g;	# $ range: eg. $1-2
		s/(\d)-(\'?)(\d)/$1 to $2$3/g;		# range: eg. 1-2
		s/%-(\d)/% to $1/g;			# % range: eg. 1%-2%
		s/(\d)=(\d)/$1 equals $2/g;		# equation: x=y
		s/ - / -- /g;				# recode dashes
		s/([^-\d\s])-([^-\d\s])/$1 - $2/g;	# split in-word hyphens
		s/- +-/--/g; s/- +-/--/g;		# close dashes
		s/-{3,}/--/g;				# map dashes to --
		s/--/ -- /g;				# space around --
		s/(\d) +(\d+\/\d)/$1 and $2/g;	      # dig frac -> dig and frac
		s/([a-zA-Z])\//$1 \/ /g;		# text/*
		s/\/([a-zA-Z])/ \/ $1/g;		# */text

		s/([a-zA-Z]\d+)\/(\d+)/$1 \/ $2/g;	# eg. a1/3 -> a1 / 3
		s/(\/\d*)th/$1/ig;			# eg. 1/10th -> 1/10
		s/(\/\d*1)st/$1/ig;			# eg. 1/x1st -> 1/x1
		s/(\/\d*2)nd/$1/ig;			# eg. 1/x2nd -> 1/x2
		s/(\/\d*3)rd/$1/ig;			# eg. 1/x3rd -> 1/x3
		s/(\d+)\/(\d+[a-zA-Z])/$1 \/ $2/g;	# eg. 1/3a -> 1 / 3a
		s/([a-zA-Z])-(19\d\d\D)/$1 - $2/g;  # eg. mid-1990 -> mid - 1990
#		s/([a-zA-Z])-(\d)/$1 $2/g;		# eg. a-1 -> a 1
#		s/(\d)-([a-zA-Z])/$1 $2/g;		# eg. 1-a -> 1 a
		s/([a-zA-Z])-(\d)/$1 - $2/g;		# eg. a-1 -> a - 1
		s/(\d)-([a-zA-Z])/$1 - $2/g;		# eg. 1-a -> 1 - a
	
		# fix common time typo (; for :)
		s/\b([012]?\d);([0-5]\d)\b/$1:$2/g;	# e.g. 11;00 -> 11:00

		if(!/\d:\d\d$/o && !/\d:\d\d\D/o)    # preprocess non-time \d:\d
		{	s/(\d):(\d)/$1 : $2/g;
			s/(\S):(\d)/$1: $2/g;
		}
	}

	if($vflg) {warn "num1:\t$_\n";}

	s/^\s*//;
	if(/\d/ && !/^$sgml_pattern/)		# opt and protect sgml
	{	@input = split(/\s+/o);
		@output=();
		$field = 0;
	wloop:	for(;$field<=$#input;$field++)	# $field is global
		{	if($field>0) {$last=$input[$field-1];}
			else {$last='';}
			if($field<$#input) {$next=$input[$field+1];}
			else {$next='';}
			if($field<$#input-1) {$next2=$input[$field+2];}
			else {$next2='';}
			$this=$input[$field];
			$_=$input[$field];
	
			if(/<[\w\.\/]*>/o && !/<p/o && !/<\/p>/o) # pass only
				{&perr("spurious SGML: $_");}	# <p... and </p>
	
			if(/[0-9]/o && !/<p/o)		# number but not <p
			{	if(/[\$\#]/o)			# money
					{&money($_,$next);}
				elsif(/\d:\d\d$/o || /\d:\d\d\D/o)	# time
					{&printtime($_);}
				elsif(/\d+\/\d+\/\d+/o)		# x/x/x date
					{&printdate($_);}
				elsif((/[a-zA-Z].*\d/ || /\d.*[a-zA-Z]/)
				      && 
				      !(/\dth\W*/i || /1st\W*/i || /2nd\W*/i
					|| /3rd\W*/i
					|| (/\d\'?s\W*/
					    && (! /\d[a-zA-Z]+\d+\'?s\W*$/))))
					{&printserno($_);}	 # serial no
				elsif(/\//o)			# fraction
					{&printfrac($_);}
				elsif(/\d\'-?\d+/o)		# ft inches
					{&printftin($_);}
				else {&printnum($_); }	      # ordinary number
			}
			else {&pusho($_ );}		# non-numeric string
		}
		$_=join(" ",@output);
	}
	s/^/ /o;
	s/$/ /o;
	s/ - /-/g;		# unspace hyphen
	s/%/ % /g;
	s/ {2,}/ /g;
	s/^ //o;
	s/ $//o;

	if($_) {$out .= "$_\n";}
    }

    return $out;
}

sub money				# money($this,$next)
{	$_=$_[0];		# $this
	my($next)=$_[1];
	if($vflg) {warn "money: $_, $next\n";}

	my($unit);
	my($subunit_sing);
	my($subunit_pl);
	my($punct);
	my($plural);
	my($sing);
	my($frac);
	my($front);
	my($back);
	my($x);
	my($y);
	my($z);
	my($i);
	my($j);

	s/\$\.(\d)/\$0.$1/g;	# patch numbers like $.22
	if(/A\$/)				# $ stuff
	{	($front)=/^(.*)A\$/;
		s/A\$//;
		$unit='Australian dollar';
		$subunit_sing='cent';
		$subunit_pl='cents';
	}
	elsif(/C\$/)
	{	($front)=/^(.*)C\$/;
		s/C\$//;
		$unit='Canadian dollar';
		$subunit_sing='cent';
		$subunit_pl='cents';
	}
	elsif(/NZ\$/)
	{	($front)=/^(.*)NZ\$/;
		s/NZ\$//;
		$unit='New Zealand dollar';
		$subunit_sing='cent';
		$subunit_pl='cents';
	}
	elsif(/US\$/)
	{	($front)=/^(.*)US\$/;
		s/US\$//;
		$unit='U S dollar';
		$subunit_sing='cent';
		$subunit_pl='cents';
	}
	elsif(/\$/)
	{	($front)=/^(.*)\$/;
		s/\$//;
		$unit='dollar';
		$subunit_sing='cent';
		$subunit_pl='cents';
	}
	elsif(/#/)				# pound
	{	($front)=/^(.*)#/;
		s/#//;
		$unit='pound';
		$subunit_sing='penny';
		$subunit_pl='pence';
	}
	else {&perr("money: unknown currency");}

	($back)=/(\D*)$/;
	$back =~ s/^s//;	# $40s -> $40

	if($front) 
	{	&pusho($front);			# generally punctuation
		if($front !~ /\w$/) {$appendflg=1;}
	}

	$x=$_;
	if($x =~ /\//)
	{	$x =~ s/^\D*//;
		$x =~ s/\D*$//;
		&printfrac($x);
		&pusho("of a $unit");
		$x="";
		$plural=0;
	}

	$x =~ s/^\D*([\d,]*)\D*.*$/$1/;		# int part of string
	if($x ne "") {&printint($x);}		# print int part (eg. dollars)

	if($next eq "and" && $next2 =~ /\d\/\d/ && $next2 !~ /\/.*\//)
	{	if($unit && $x ne "") {&pusho("and");}	      # frac: eg 4 1/16
		$z=$next2;
		$z =~ s/\D*$//;
		&printfrac($z);
		($punct)=($next2 =~ /(\D*)$/);
		$field+=2;
		&pusho("${unit}s");
	
		if($back) {&perr("money: back and 1 1/3");}
		
		if($punct) {&appendo($punct);}	# punctuation from *illion
		return;
	}

	if($back eq "" && $next =~ /^(thousands?|[a-z]*illions?)(\W*)/i)
	{	&printdecfrac($_);			# multiplier
		&pusho($1);
		$punct=$2;
		$plural=1;			### if adj '', if noun 's'
		$field++;
		$frac=1;
	}
	elsif(/\.\d$/ || /\.\d\D/ || /\.\d{3}/ )	# .d or .ddd+
	{	&printdecfrac($_);
		$plural=1;			# can be either
		$frac=1;
	}
	else
	{	$y=$x;
		$y =~ s/,//g;			# remove commas
		if(int($y)!=1) {$plural=1;}
	}

	if($back eq "" && $input[$field+1] =~ /dollar/i)
	{	$unit="";			# fix "$1 dollar" wsj typo
		$subunit_sing="";
		$subunit_pl="";
		&printdecfrac($_);
		$frac=1;
	}

#print "f=$front, m=$_, b=$back\n";
#foo
	$sing=0;
# it appears that this is the source of many if not all warnings
# warn(__LINE__, ": input[$field+1]=",$input[$field+1],"\n"); 
	if($last =~ /^\W*[aA][nN]?\W*$/) {$sing=1;}	# a $123, an $80
	elsif($input[$field+1] eq "-") {$sing=1;}	# eg. $123-a-day
							# next one is chancy
	elsif($input[$field] !~ /\W$/ && $input[$field+1] !~ /^\W/ &&
		$input[$field+1] =~ /[a-zA-Z]$/ && $input[$field+2] eq "-" &&
		$input[$field+3] =~ /^[a-zA-Z]/) {$sing=1;}	# $ after-tax

	elsif($back eq "" && !$punct) # right contexts with no intervening punct
	{	$j=$field+1;		# includes *ly as a skip
		$z="";
		for($i=0;$i<2;$i++,$j++)	# skip ?
		{	$y=$input[$j];			# strip final punct
			$y =~ s/\W*$//;
			if($y !~ /\w*ly$/i && $sing_dollar{$y}!=2) {last;}
			($y)=($input[$j] =~ /(\W*)$/);	# get final punct
			$z .= $y;			# accumulate
		}
		$y=$input[$j];			# strip final punct
		$y =~ s/\W*$//;
		if($z eq "" && $sing_dollar{$y}==1) {$sing=1;}
	}
		
	if($unit)					# print unit
	{	&pusho($unit);
		if($plural && !$sing) {&appendo("s");}	# just add s for plural
	}

	if(!$frac && /\.\d{2}/)			# .dd	(eg. cents)
	{	$y=$_;
		$y =~ s/^[^\.]*\.([\d]*)\D?.*$/$1/;	# get fractional part
		if($unit && $x ne "") {&pusho("and");}
		&printint($y);
		if($sing || int($y)==1) {&pusho($subunit_sing);}
		else {&pusho($subunit_pl);}
	}

	if($back)				# punctuation from this field
	{	if($punct) {&perr("money: back and punct");}

		if($back =~ /^\w/) {&pusho($back);}
		else {&appendo($back);}
	}
		
	if($punct) {&appendo($punct);}		# punctuation from *illion
}

sub printyear			# &printyear(x)
{	if($vflg) {warn "printyear: $_[0]\n";}
	&printnum($_[0]);		# for now
}

sub printtime			# &printtime(x)
{	if($vflg) {warn "printtime: $_[0]\n";}
	$_=$_[0];
	
	my(@x);
	my($front);
	my($back);

	if(/:{2,}/ || !/\d:\d/) {&perr("printtime: not a time");}

	@x=split(/:/,$_);
	($front)=($x[0] =~ /^(\D*)/);
	$x[0] =~ s/^(\D*)//;
	($back)=($x[1] =~ /(\D*)$/);
	$x[1] =~ s/(\D*)$//;
	
	if($front) 
	{	&pusho($front);			# generally punctuation
		if($front !~ /\w$/) {$appendflg=1;}
	}
	&printint($x[0]);
	if($x[1]==0)
	{	$_=$next;
		if(!/^[aApP]\.?[nM]\.?$/) {&pusho("o'clock");}
	}
	elsif ($x[1]<10)
	{	&pusho("oh");
		&printint($x[1]);
	}
	else {&printint($x[1]);}
	if($back)
	{	if($back =~ /^\w/) {&pusho($back);}
		else {&appendo($back);}		# generally punctuation
	}
}

sub printfrac
{	if($vflg) {warn "printfrac: $_[0]\n";}
	my($x)=$_[0];

	my(@z);			#Perl BUG: lists do not seem to be local
	my($sign);
	my($hun, $ones);
	my($front);
	my($back);

	$x =~ s/^([^\d\.]*)//;		# strip front
	$front=$1;
	if($front =~ /^\+$/)		# get sign
	{	$sign="plus";
		$front =~ s/\+$//;
	}
	if($front =~ /^-$/)
	{	$sign="minus";
		$front =~ s/-$//;
	}

	if($x =~ /\D$/)
	{	($back)=( $x =~ /(\D*)$/ );
		$x =~ s/\D*$//;			# strip back: final . is punct
	}

	@z=split(/\//,$x);
	if($#z !=1) {&perr("printfrac: illegal fraction: $_[0]");}
	if($z[1] <= 1) {&perr("printfrac: den too small: $_[0]");}

	if($front) 
	{	&pusho($front);
		if($front =~ /[a-zA-Z]$/) {&appendo("-");}
		$appendflg=1;
	}

	if($sign) {&pusho($sign);}

	&printint($z[0]);			#numerator
	if($z[1] <= $#den)			# small den from table (<20)
	{	&pusho($den[$z[1]]);
		if($z[0]!=1) {&pluralize;}
	}
	else					#large den
	{	$ones=int($z[1]%100);
		$hun=100*int($z[1]/100);
		if($hun>0) {&printint($hun);}
		if($ones==0) 
		{	&appendo("th");
			if($z[0]!=1) {&pluralize;}
		}
		elsif($ones<=$#largeden)		# <20
		{	&pusho($largeden[$ones]);
			if($z[0]!=1) {&pluralize;};
		}
		else
		{	$x=int($ones%10);
			if(int($ones/10))
			{	&pusho($ten[int($ones/10)]);
				if($x)
				{	&appendo("-");	# eg. twenty-five
					$appendflg=1;
				}
			}
			if($x==0)
			{	&pusho("th");
				if($z[0]!=1) {&pluralize;}
			}
			else
			{	&pusho($largeden[$x]);
				if($z[0]!=1) {&pluralize;}
			}
		}
	}

	if($back)
	{	$x=&geto;	# in case of 1/10th etc ([stndrth]=st nd rd th)
		if($back !~ /^[stndrth]{2}/ || $x !~ /$back$/)
		{	if($back =~ /^[a-zA-Z]/) {&appendo("-");}
			&appendo($back);
		}
	}
}

sub printnum			# printnum(n)
{	if($vflg) {warn "printnum: $_[0]\n";}
	my($x)=$_[0];	# print ordinary numbers

	$leadingzeroflg='';			# global
	my($front);
	my($back);
	my($intpart);
	my($fracpart);
	my($hun);
	my($ones);
	my($comma);
	my($sign);
	my($y);

	$x =~ s/^(\D*)//;		# strip front
	$front=$1;
	if($front =~ /^\.$/ || $front =~ /\W\.$/ ||
		($front =~ /\.$/ && $x =~ /^0/ ))		# leading .
	{	$front =~ s/\.$//;
		$x = "." . $x;
	}
	if($front =~ /^\+$/)		# get sign
	{	$sign="plus";
		$front =~ s/\+$//;
	}
	if($front =~ /^-$/)
	{	$sign="minus";
		$front =~ s/-$//;
	}

	if($x =~ /\D$/)
	{	$back=$x;
		$back =~ s/^[\d\.,]*\d//;
		$x =~ s/\D*$//;			# strip back: final . is punct
	}

	if($x =~ /[^\d\.,]/) {&perr("printnum: $_[0] is not a number");}

	if($x!=0 && $x =~ /^0/ && $x =~ /^\d*$/)	# "oh" numbers
	{	if($front) 
		{	&pusho($front);
			if($front !~ /[a-zA-Z]$/) {$appendflg=1;}
		}

		if($sign) { &pusho($sign); }
	
		while($x ne '')
		{	$x =~ s/^(.)//;
			&pusho($ones_oh[$1]);
		}

		if($back)
		{	if($back =~ /^s$/ || $back =~ /^s\W/)	# back = s
			{	&pluralize;			# eg. 1960s
				$back =~ s/^s//;
			}
			if($back)
			{	if($back =~ /^[a-zA-Z]/) {&pusho($back);}
				else {&appendo($back);}	# back = punct or "'s"
			}
		}
		return;
	}

	if($x =~ /^\d/)			# get integer part
	{	if($x =~ /,/)
		{	$comma=1;
			$x =~ s/,//g;	# strip commas
		}
		$intpart=$x;
		$intpart =~ s/\..*$//;
		if($x =~ /^0/) {$leadingzeroflg=1;}
	}

	if($x =~ /\./)			# get fractional part
	{	$fracpart=$x;
		$fracpart =~ s/^.*\././;
	}

	if($front) 
	{	&pusho($front);
		if($front !~ /[a-zA-Z]$/) {$appendflg=1;}
	}

	if($sign) { &pusho($sign); }

	$ones=int($intpart%100);
	if($comma) {&printint($intpart);}
	elsif(($intpart>=1900 || $intpart>=1100 && $ones==0)
		&& $intpart<2000 && !$fracpart)			#4 digit -> 2+2
	{	$hun=int($intpart/100);
		&printint($hun);
		if($ones>=10) {&printint($ones);}
		elsif($ones>0)
		{	&pusho("oh");
			&printint($ones);
		}
		else {&pusho("hundred");}
	}
	else
	{	&printint($intpart);
		$y=$last;
		$y =~ s/^\W*//;				# thize dates: May 25th
		if(length($intpart)<=2 && $months{$y})
		{	&thize("");
			$back =~ s/[a-z]//g;
		}
	}
	if($fracpart) {&printdecfrac($fracpart);}

	if($back)
	{	if($back =~ /^s$/ || $back =~ /^s\W/)	# back = s
		{	&pluralize;			# eg. 1960s
			$back =~ s/^s//;
		}
		if($back =~ /^st$/ || $back =~ /^st\W/)	# back= st
		{	&thize("st");			# eg. 1st
			$back =~ s/^st//;
		}
		if($back =~ /^nd$/ || $back =~ /^nd\W/)	# back= nd
		{	&thize("nd");			# eg. 2nd
			$back =~ s/^nd//;
		}
		if($back =~ /^rd$/ || $back =~ /^rd\W/)	# back= rd
		{	&thize("rd");			# eg. 3rd
			$back =~ s/^rd//;
		}
		if($back =~ /^th$/ || $back =~ /^th\W/)	# back= th
		{	&thize("th");			# eg. 4th
			$back =~ s/^th//;
		}
		if($back)
		{	if($back =~ /^[a-zA-Z]/) {&pusho($back);}
			else {&appendo($back);}	# back = punct or "'s"
		}
	}
}

sub printdate			# printdate(n):	x/x/x format
{	if($vflg) {warn "printdate: $_[0]\n";}
	my($x)=$_[0];	# print ordinary numbers

	my(@y);
	my($front);
	my($back);

	$x =~ s/^(\D*)//;		# strip front
	$front=$1;

	$x =~ s/(\D*)$//;		# strip back
	$back=$1;

	if($x !~ /^\d{1,2}\/\d{1,2}\/(19|20)?\d{2}$/)
		{&perr("printdate: $_[0] is not a date");}

	@y=split(/\//,$x);
	$y[2] =~ s/^19(\d+)$/$1/;
	
	if($front) 
	{	&pusho($front);
		if($front =~ /[a-zA-Z]$/) {&appendo("-");}
		$appendflg=1;
	}

# L. Galescu 02/06/2005 : modified output for dates
#	&printint($y[0]);
	&pusho($months[10+$y[0]]);
#	&appendo("/");

#	$appendflg=1;
	&printint($y[1]);&thize("");
#	&appendo("/"); 

#	$appendflg=1;
	&printint($y[2]);

	if($back)
	{	if($back =~ /^[a-zA-Z]/) {&appendo("-");}
		&appendo($back);
	}
}

sub printserno			# printserno(n): eg. B1, 3b2, 10W-40
{	if($vflg) {warn "printserno: $_[0]\n";}
	my($x)=$_[0];	# print mixed sequences of dig and let

	my($y);
	my($z);
	my($front);
	my($back);

	$x =~ s/^(\W*)//;		# strip front
	$front=$1;
	if($front) 
	{	&pusho($front);
		if($front !~ /[a-zA-Z]$/) {$appendflg=1;}
	}

	$x =~ s/(\W*)$//;		# strip back
	$back=$1;
	$x =~ s/(\d[a-zA-Z]+\d+)(\'?s)$/$1/  # strip "s" or "'s"
	    && ($back = $2 . $back);

	while($x)
	{	$x =~ s/^(\D*)//;	# strip off non-dig
		$y=$1;
		if($y)
		{	$y =~ s/-//g;	# remove -
			if($y eq "") {}
			elsif($sernowd{$y}) {&pusho($y);}	# word
			else
			{	while($y)			# spell out
				{	if($y =~ /[a-zA-Z]\'s$/)
					{	&pusho($y);
						$y =~ s/[a-zA-Z]\'s*$//;
					}
					elsif($y =~ /[A-Z]s$/)
					{	&pusho($y);
						$y =~ s/[A-Z]s$//;
					}
					else
					{	$y =~ s/^(.\.?)//;
						&pusho($1);
					}
				}
			}
		}		     # (should expand here unless in dictionary)
		$x =~ s/^(\d*)//;	# strip off dig
		$y=$1;
		if($y ne "") { &printdigstr($y); }
	}

	if($back =~ /^s\b/)	# back = s
	{			# eg. 2C60s
	    &pluralize;
	    $back =~ s/^s//;
	}
	if($back)
	{	if($back =~ /^\w/) {&pusho($back);}
		else {&appendo($back);}
	}
	$appendflg=0;
}

sub printdigstr			# printdigstr(x)
{	if($vflg) {warn "printdigstr: $_[0]\n";}
	my($x)=$_[0];

	my(@y);
	my($j);
	my($k);

	if($x =~ /^0/)			# leading zero
	{	while($x ne "")
		{	$x =~ s/^(.)//;
			if($1 !~ /\d/) {&perr("printdigstr: non-digit");}
			&pusho("$ones_z[$1]");
		}
		return;
	}
	if($x =~ /^\d0*$/)		# d, d0, d00, d000, etc
	{	&printint($x);
		return;
	}

	$_=$x;
	@y=();
	for($j=0;$_ ne "";$j++) { $y[$j]=chop($_); }	# j=no digits
	for($k=0;$y[$k]==0;$k++) {}			# k= nr following 0s

	if($j==2)			# 2 dig
	{	&printint($x);
		return;
	}
	if($j==3)
	{	&printint($y[2]);
		if($y[1]==0) {&pusho("oh");}
		&printint("$y[1]$y[0]");
		return;
	}
	if($j==5 && $k<=2)
	{	&printint("$y[4]");
		$j=4;
	}
	if($j==4)
	{	&printint("$y[3]$y[2]");
		if($k==2) {&pusho("hundred");}
		else
		{	if($y[1]==0) {&pusho("oh");}
			&printint("$y[1]$y[0]");
		}
		return;
	}
						# >5 dig: just sequential dig
	for($j--;$j>=0;$j--) {&pusho("$ones_oh[$y[$j]]");}
}

sub printftin			# printftin(n): eg. 6\'-4\"
{	if($vflg) {warn "printftin: $_[0]\n";}
	my($x)=$_[0];	# print mixed sequences of dig and let

	my($y);
	my($front);
	my($back);

	$x =~ s/^(\D*)//;		# strip front
	$front=$1;

	$x =~ s/(\D*)$//;		# strip back
	$back=$1;
	$back =~ s/^\"//;		# remove \"

	if($front) 
	{	&pusho($front);
		if($front !~ /[a-zA-Z]$/) {$appendflg=1;}
	}

	$x =~ s/^([\d\.]*)//;	# strip off dig & .
	$y=$1;
	if(!$y) {&perr("printftin: bad feet");}
	&printnum($y);
	if($y==1) {&appendo("-foot");}
	else {&appendo("-feet");}

	$x =~ s/^\'//;	# strip off \'
	$x =~ s/^-//;	# strip off -
	if(!$x) {&perr("printftin: bad intermed");}

	$x =~ s/^([\d\.]*)//;	# strip off dig & .
	$y=$1;
	if(!$y) {&perr("printftin: bad inches");}
	&printnum($y);
	if($y==1) {&appendo("-inch");}
	else {&appendo("-inches");}

	if($back)
	{	if($back !~ /^[a-zA-Z]/) {&appendo($back);}
		else {&pusho($back);}
	}
}

sub printint			# printint(x)
{	if($vflg) {warn "printint: $_[0]\n";}
	my($x)=$_[0];

	my($comma);
	my($leading_zero);
	my($fractional);
	my(@y);
	my ($j);
	
	$fractional=$x =~ /\.\d/;
	$x =~ s/^\D*([\d,]*)\D*.*$/$1/;	# int part of string
	$leading_zero=$x =~ /^0/;
	$comma=$x =~ /,/;
	$x =~ s/,//g;
	if($x eq "") {return;}

	if($x == 0)
	{	&pusho("zero");
		$leadingzeroflg=1;
		return;
	}
	
	@y=();
	for($j=0;$x;$j++) { $y[$j]=chop($x); }

	if($comma || $fractional || 1)
	{	for($j=3*int($#y/3);$j>=0;$j-=3)
		{	if($y[$j+2]) { &pusho("$ones_z[$y[$j+2]] hundred");}
			if($y[$j+1]==1) { &pusho($teen[$y[$j]]);}
			else
			{	if($y[$j+1]>1)
				{	&pusho($ten[$y[$j+1]]);
					if($y[$j])
					{	&appendo("-");	# twenty-five
						$appendflg=1;
					}
				}
				if($y[$j]>0) { &pusho($ones_z[$y[$j]]);}
			}
			if(int($j/3)>0)
			{	if(int($j/3) > $#mult)
					{ &perr("printint: too big"); }
				&pusho($mult[int($j/3)]);
			}
			$commanextflg=1;
		}
	}
	$commanextflg=0;
}

sub printdecfrac
{	if($vflg) {warn "printdecfrac: $_[0]\n";}
	my($x)=$_[0];
	
	my ($j, @y);

	if($x !~ /\.\d/) {return;}
	$x =~ s/^[^\.]*\.([\d]*)\D?.*$/$1/;		# get fractional part

	&pusho($POINT);
	@y=split(//,$x);
	if($leadingzeroflg)
		{for($j=0;$j<=$#y;$j++) { &pusho($ones_z[$y[$j]]);}}
	else {for($j=0;$j<=$#y;$j++) { &pusho($ones_oh[$y[$j]]);}}
}

sub pluralize		# pluralize(): pluralize last entry on output stack
{	if($vflg) {warn "pluralize: $_[0]\n";}
	my($x);

	$_=&geto;
	if( /st$/ || /nd$/ || /rd$/ || /th$/ || /quarter$/ || /zero$/ || /oh/ ||
		/one$/ || /two$/ || /three$/ || /four$/ || /five$/ ||
		/seven$/ || /eight$/ || /nine$/ ||
		/ten$/ || /eleven$/ || /twelve$/ || /een$/ ||
		/hundred$/ || /thousand$/ || /illion$/ )
	{	&appendo("s");
	}
	elsif (/six$/)
	{	&appendo("es");
	}
	elsif (/half$/)
	{	$x=&popo();
		$x =~ s/f$/ves/;
		&pusho($x);
	}
	elsif (/ty$/)			# fifty etc.
	{	$x=&popo();
		$x =~ s/y$/ies/;
		&pusho($x);
	}
	else {&perr("pluralize: unknown word: $_");}
}

sub thize		# thize(): add th to last entry on output stack
{	if($vflg) {warn "printthize: $_[0]\n";}
	my($y)=$_[0];

	my($x);

	$_=&geto;
	if( /four$/ || /six$/ || /seven$/ || /ten$/ ||
		/eleven$/ || /een$/ || /hundred$/ || /thousand$/ || /illion$/ )
	{	if($y && $y ne "th") {&perr("thize: mismatch: $_ $y\n");} # xth
		&appendo("th");
	}
	elsif( /one$/ )						# 1st
	{	if($y && $y ne "st") {&perr("thize: mismatch: $_ $y\n");}
		$x=&popo();
		$x =~ s/one$/first/;
		&pusho($x);
	}
	elsif( /two$/ )						# 2nd
	{	if($y && $y ne "nd") {&perr("thize: mismatch: $_ $y\n");}
		$x=&popo();
		$x =~ s/two$/second/;
		&pusho($x);
	}
	elsif( /three$/ )					# 3rd
	{	if($y && $y ne "rd") {&perr("thize: mismatch: $_ $y\n");}
		$x=&popo();
		$x =~ s/three$/third/;
		&pusho($x);
	}
	elsif( /five$/ || /twelve$/ )				# 5th, 12th
	{	if($y && $y ne "th") {&perr("thize: mismatch: $_ $y\n");}
		$x=&popo();
		$x =~ s/ve$/fth/;
		&pusho($x);
	}
	elsif(/eight$/)
	{	if($y && $y ne "th") {&perr("thize: mismatch: $_ $y\n");} # 8th
		&appendo("h");
	}
	elsif( /nine$/ )
	{	if($y && $y ne "th") {&perr("thize: mismatch: $_ $y\n");}
		$x=&popo();
		$x =~ s/nine$/ninth/;
		&pusho($x);
	}
	elsif( /ty$/ )
	{	if($y && $y ne "th") {&perr("thize: mismatch: $_ $y\n");}
		$x=&popo();
		$x =~ s/ty$/tieth/;
		&pusho($x);
	}
	else {&perr("thize: unknown word: $_");}
}

sub pusho				# pusho($x): push output
{
    if($commanextflg)		# global: used for commas in printint
    {	$commanextflg=0;		
	&appendo(",");
    }
    if($appendflg)			# global: used for fronts
    {	$appendflg=0;		
	&appendo($_[0]);
    }
    else {push(@output,@_);}
}

sub appendo				# appendo($x): append to output
{	
    $appendflg=0;		
#	if($#output < 0) {&pusho("");}
    if($#output < 0) { &perr("appendo: output empty"); }
    $output[$#output] .= $_[0];
}

sub popo				# popo(): pop last output
{	
    if($#output < 0) { &perr("popo: output empty"); }
    pop(@output);
}

sub geto				# geto(): get last output
{	
    if($#output < 0) { &perr("geto: output empty"); }
    return $output[$#output];
}

sub perr
{	
    warn "numproc: $_[0]\n";
    warn "line number=$.: fields=$last, $this, $next\n";
#	exit(1);

    $appendflg=0;
    $commanextflg=0;
    &pusho($this);
    $field++;		# graceful error recovery
    goto wloop;
}

sub perr2
{	
    warn "numproc: $_[0]\n";
    exit(1);
}


1;

#
# Exceptions
#

__DATA__
# varia
9/11	nine eleven

# technology
10/100/1000	ten one hundred one thousand 
10/100/1000BASE-T	ten one hundred one thousand base t.

#end
