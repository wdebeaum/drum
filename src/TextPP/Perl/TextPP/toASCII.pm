package TextPP::toASCII;

use 5.008001;
use strict;
use warnings;

require Exporter;

our @ISA = qw(Exporter);

our @EXPORT_OK = qw(
 	&toASCII	
);

our $VERSION = '0.01';

#----------------------------------------------------------------
# Global variables
#
my %iso8859_1_table = (
    q{&quot;}	=> q{"},
    q{&amp;}	=> q{&},
    q{&lt;}	=> q{<},
    q{&gt;}	=> q{>},
    q{&tilde;}	=> q{~},
    q{&#128;}	=> q{},		# euro
    q{&#129;}	=> q{},
    q{&#130;}	=> q{'},	# ,
    q{&sbquo;}	=> q{'},	# ,
    q{&#131;}	=> q{},
    q{&#132;}	=> q{"},	# ,,
    q{&dbquo;}	=> q{"},	# ,,
    q{&#133;}	=> q{},
    q{&#134;}	=> q{},
    q{&#135;}	=> q{},
    q{&#136;}	=> q{},
    q{&#137;}	=> q{},		# per million
    q{&permil;}	=> q{},		# per million
    q{&#138;}	=> q{S},	# S^
    q{&#139;}	=> q{},
    q{&#140;}	=> q{OE},
    q{&#141;}	=> q{},
    q{&#142;}	=> q{Z},	# Z^
    q{&#143;}	=> q{},
    q{&#144;}	=> q{},
    q{&#145;}	=> q{`},
    q{&lsquo;}	=> q{`},
    q{&#146;}	=> q{'},
    q{&rsquo;}	=> q{'},
    q{&#147;}	=> q{"},
    q{&ldquo;}	=> q{"},
    q{&#148;}	=> q{"},
    q{&rdquo;}	=> q{"},
    q{&#149;}	=> q{},
    q{&#150;}	=> q{-},
    q{&ndash;}	=> q{-},
    q{&#151;}	=> q{--},
    q{&mdash;}	=> q{--},
    q{&#152;}	=> q{~},
    q{&tilde;}	=> q{~},
    q{&#153;}	=> q{},		# (TM)
    q{&trade;}	=> q{},		# (TM)
    q{&#154;}	=> q{s},	# s^
    q{&#155;}	=> q{},
    q{&#156;}	=> q{oe},
    q{&#157;}	=> q{},
    q{&#158;}	=> q{z},	# z^
    q{&#159;}	=> q{Y},	# Y:
    q{&Yuml;}	=> q{Y},	# Y:
    q{&#160;}	=> q{ },
    q{&nbsp;} 	=> q{ },
    q{&#161;}	=> q{},
    q{&iexcl;}	=> q{},
    q{&#162;}	=> q{},
    q{&cent;} 	=> q{},
    q{&#163;}	=> q{},		# GBP
    q{&pound;}	=> q{},		# GBP
    q{&#164;}	=> q{},		# >o<
    q{&curren;}	=> q{},		# >o<
    q{&#165;}	=> q{},   	# Yen
    q{&yen;}  	=> q{},   	# Yen
    q{&#166;}	=> q{},
    q{&brvbar;}	=> q{},
    q{&#167;}	=> q{},
    q{&sect;} 	=> q{},
    q{&#168;}	=> q{},
    q{&uml;}  	=> q{},
    q{&#169;}	=> q{},		# (C)
    q{&copy;} 	=> q{},		# (C)
    q{&#170;}	=> q{},
    q{&ordf;} 	=> q{},
    q{&#171;}	=> q{},
    q{&laquo;}	=> q{},
    q{&#172;}	=> q{},
    q{&not;}  	=> q{},
    q{&#173;}	=> q{},
    q{&shy;}  	=> q{},
    q{&#174;}	=> q{},
    q{&reg;}  	=> q{},
    q{&#175;}	=> q{},
    q{&macr;} 	=> q{},
    q{&#176;}	=> q{},
    q{&deg;}  	=> q{},
    q{&#177;}	=> q{},
    q{&plusmn;}	=> q{},
    q{&#178;}	=> q{},
    q{&sup2;} 	=> q{},
    q{&#179;}	=> q{},
    q{&sup3;} 	=> q{},
    q{&#180;}	=> q{},
    q{&acute;}	=> q{},
    q{&#181;}	=> q{},
    q{&micro;}	=> q{},
    q{&#182;}	=> q{},
    q{&para;} 	=> q{},
    q{&#183;}	=> q{},
    q{&middot;}	=> q{},
    q{&#184;}	=> q{},
    q{&cedil;}	=> q{},
    q{&#185;}	=> q{},
    q{&sup1;} 	=> q{},
    q{&#186;}	=> q{},
    q{&ordm;} 	=> q{},
    q{&#187;}	=> q{},
    q{&raquo;}	=> q{},
    q{&#188;}	=> q{},
    q{&frac14;}	=> q{},
    q{&#189;}	=> q{},
    q{&frac12;}	=> q{},
    q{&#190;}	=> q{},
    q{&frac34;}	=> q{},
    q{&#191;}	=> q{},
    q{&iquest;}	=> q{},
    q{&#192;}	=> "A",
    q{&Agrave;}	=> "A",
    q{&#193;}	=> "A",
    q{&Aacute;}	=> "A",
    q{&#194;}	=> "A",
    q{&Acirc;}	=> "A",
    q{&#195;}	=> "A",
    q{&Atilde;}	=> "A",
    q{&#196;}	=> "A",
    q{&Auml;} 	=> "A",
    q{&#197;}	=> "A",
    q{&Aring;}	=> "A",
    q{&#198;}	=> "A",
    q{&AElig;}	=> "AE",
    q{&#199;}	=> "C",
    q{&Ccedil;}	=> "C",
    q{&#200;}	=> "E",
    q{&Egrave;}	=> "E",
    q{&#201;}	=> "E",
    q{&Eacute;}	=> "E",
    q{&#202;}	=> "E",
    q{&Ecirc;}	=> "E",
    q{&#203;}	=> "E",
    q{&Euml;} 	=> "E",
    q{&#204;}	=> "I",
    q{&Igrave;}	=> "I",
    q{&#205;}	=> "I",
    q{&Iacute;}	=> "I",
    q{&#206;}	=> "I",
    q{&Icirc;}	=> "I",
    q{&#207;}	=> "I",
    q{&Iuml;} 	=> q{I},
    q{&#208;}	=> q{TH},
    q{&ETH;}  	=> q{TH},
    q{&#209;}	=> q{N},
    q{&Ntilde;}	=> q{N},
    q{&#210;}	=> q{O},
    q{&Ograve;}	=> q{O},
    q{&#211;}	=> q{O},
    q{&Oacute;}	=> q{O},
    q{&#212;}	=> q{O},
    q{&Ocirc;}	=> q{O},
    q{&#213;}	=> q{O},
    q{&Otilde;}	=> q{O},
    q{&#214;}	=> q{O},
    q{&Ouml;} 	=> q{O},
    q{&#215;}	=> q{x},
    q{&times;}	=> q{x},
    q{&#216;}	=> q{OE},
    q{&Oslash;}	=> q{OE},
    q{&#217;}	=> q{U},
    q{&Ugrave;}	=> q{U},
    q{&#218;}	=> q{U},
    q{&Uacute;}	=> q{U},
    q{&#219;}	=> q{U},
    q{&Ucirc;}	=> q{U},
    q{&#220;}	=> q{U},
    q{&Uuml;} 	=> q{U},
    q{&#221;}	=> q{Y},
    q{&Yacute;}	=> q{Y},
    q{&#222;}	=> q{TH},
    q{&THORN;}	=> q{TH},
    q{&#223;}	=> q{SS},
    q{&szlig;}	=> q{SS},
    q{&#224;}	=> q{a},
    q{&agrave;}	=> q{a},
    q{&#225;}	=> q{a},
    q{&aacute;}	=> q{a},
    q{&#226;}	=> q{a},
    q{&acirc;}	=> q{a},
    q{&#227;}	=> q{a},
    q{&atilde;}	=> q{a},
    q{&#228;}	=> q{a},
    q{&auml;} 	=> q{a},
    q{&#229;}	=> q{a},
    q{&aring;}	=> q{a},
    q{&#230;}	=> q{ae},
    q{&aelig;}	=> q{ae},
    q{&#231;}	=> q{c},
    q{&ccedil;}	=> q{c},
    q{&#232;}	=> q{e},
    q{&egrave;}	=> q{e},
    q{&#233;}	=> q{e},
    q{&eacute;}	=> q{e},
    q{&#234;}	=> q{e},
    q{&ecirc;}	=> q{e},
    q{&#235;}	=> q{e},
    q{&euml;} 	=> q{e},
    q{&#236;}	=> q{i},
    q{&igrave;}	=> q{i},
    q{&#237;}	=> q{i},
    q{&iacute;}	=> q{i},
    q{&#238;}	=> q{i},
    q{&icirc;}	=> q{i},
    q{&#239;}	=> q{i},
    q{&iuml;} 	=> q{i},
    q{&#240;}	=> q{th},
    q{&eth;}  	=> q{th},
    q{&#241;}	=> q{n},
    q{&ntilde;}	=> q{n},
    q{&#242;}	=> q{o},
    q{&ograve;}	=> q{o},
    q{&#243;}	=> q{o},
    q{&oacute;}	=> q{o},
    q{&#244;}	=> q{o},
    q{&ocirc;}	=> q{o},
    q{&#245;}	=> q{o},
    q{&otilde;}	=> q{o},
    q{&#246;}	=> q{o},
    q{&ouml;} 	=> q{o},
    q{&#247;}	=> q{},
    q{&divide;}	=> q{},
    q{&#248;}	=> q{oe},
    q{&oslash;}	=> q{oe},
    q{&#249;}	=> q{u},
    q{&ugrave;}	=> q{u},
    q{&#250;}	=> q{u},
    q{&uacute;}	=> q{u},
    q{&#251;}	=> q{u},
    q{&ucirc;}	=> q{u},
    q{&#252;}	=> q{u},
    q{&uuml;} 	=> q{u},
    q{&#253;}	=> q{y},
    q{&yacute;}	=> q{y},
    q{&#254;}	=> q{th},
    q{&thorn;}	=> q{th},
    q{&#255;}	=> q{y},
    q{&yuml;} 	=> q{y},
);


# Methods
sub toASCII {
    my $in = shift;
    my $out = "";

    my $char;
    my ($m, $c, $d, $n);
    my ($pos, $pos_before, $pos_after);
    my $repl;

    # first replace HTML codes
    # a little optimization: 
    # actually, quite a bit faster than the "dumb" version
    while ($in =~ /((&[a-zA-Z]+;)|(&#([0-9]+);))/gos) {
	($m, $c, $d, $n) = ($1, $2, $3, $4);
	$pos_after = pos($in);
	$pos_before = $pos_after - length($m);

	if ((defined $d) && ($n < 128)) {
	    $repl = chr($n);
	} elsif (defined $iso8859_1_table{$c}) {
	    $repl = $iso8859_1_table{$c};
	} else {
	    warn "Unknown character: $m\n";
	    $repl = "";
	}
	substr($in, $pos_before, length($m), $repl);
    }
    # now look for extended ASCII codes
    for ($pos = 0; $pos < length($in); $pos++) {
	$char = substr($in, $pos, 1);
	if (isASCII($char)) {
	    $out .= $char;
	} else {
	    if (! defined($iso8859_1_table{"&#".ord($char).";"})) {
		warn "Xchar:undefined :$char:\n";
	    }
	    $out .= $iso8859_1_table{"&#".ord($char).";"};
	}
    }
    return $out;
}

sub isASCII {
    my $c = shift;
    return (ord($c) < 128);
}

1;
__END__

=head1 NAME

TextPP::toASCII - Perl extension for converting HTML character codes and ISO8859-1 codes to ASCII.

=head1 SYNOPSIS

  use TextPP::toASCII;

  \$new_text = toASCII(\$text);

=head1 DESCRIPTION

B<toASCII> transforms every non-ascii character into its ascii equivalent, if one exist; otherwise, the character is deleted. 

=head2 EXPORT

None by default.

=head1 AUTHOR

Lucian Galescu, E<lt>lgalescu@ihmc.usE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by Lucian Galescu

=cut
