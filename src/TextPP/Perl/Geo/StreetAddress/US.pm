package Geo::StreetAddress::US;

# imported for PLOW by Nate Blaylock 10/16/07
# changes to original (version 0.99)
#   - fix parsing of state names with spaces in them that didn't work
#   - parameterize normalization of results (they were always normalized before)
#   - added code to capture 'unit' (e.g., apt#) and 'fraction' (123 1/2). They were
#       being matched before but not saved

=head1 NAME

Geo::StreetAddress::US - Perl extension for parsing US street addresses

=head1 SYNOPSIS

  use Geo::StreetAddress::US;

  my $hashref = Geo::StreetAddress::US->parse_location(
		"1005 Gravenstein Hwy N, Sebastopol CA 95472" );

  my $hashref = Geo::StreetAddress::US->parse_location(
		"Hollywood & Vine, Los Angeles, CA" );

  my $hashref = Geo::StreetAddress::US->parse_address(
		"1600 Pennsylvania Ave, Washington, DC" );

  my $hashref = Geo::StreetAddress::US->parse_intersection(
		"Mission Street at Valencia Street, San Francisco, CA" );

  my $normal = Geo::StreetAddress::US->normalize_address( \%spec );
      # the parse_* methods call this automatically...

=head1 DESCRIPTION

Geo::StreetAddress::US is a regex-based street address and street intersection
parser for the United States. Its basic goal is to be as forgiving as possible
when parsing user-provided address strings. Geo::StreetAddress::US knows about
directional prefixes and suffixes, fractional building numbers, building units,
grid-based addresses (such as those used in parts of Utah), 5 and 9 digit ZIP
codes, and all of the official USPS abbreviations for street types and state
names.

=head1 RETURN VALUES

Most Geo::StreetAddress::US methods return a reference to a hash containing
address or intersection information as one of their arguments. This
"address specifier" hash may contain any of the following fields for a
given address. If a given field is not present in the address, the 
corresponding key will be set to C<undef> in the hash.

=head2 ADDRESS SPECIFIER

=over 4

=item number

House or street number.

=item fraction

Fraction of a house or street number (e.g., in "123 1/2 Maple St." the fraction
will be "1/2")

=item prefix

Directional prefix for the street, such as N, NE, E, etc.  A given prefix
should be one to two characters long.

=item street

Name of the street, without directional or type qualifiers.

=item type

Abbreviated street type, e.g. Rd, St, Ave, etc. See the USPS official
type abbreviations at L<http://www.usps.com/ncsc/lookups/abbr_suffix.txt> 
for a list of abbreviations used.

=item suffix

Directional suffix for the street, as above.

=item unit

Unit information where the address specifies part of a building, e.g.,
Apt #1043 or Suite 9B

=item city

Name of the city, town, or other locale that the address is situated in.

=item state

The state which the address is situated in, given as its two-letter
postal abbreviation. See L<http://www.usps.com/ncsc/lookups/abbr_state.txt>
for a list of abbreviations used.

=item zip

Five digit ZIP postal code for the address, including leading zero, if needed.

=back

=head2 INTERSECTION SPECIFIER

=over 4

=item prefix1, prefix2

Directional prefixes for the streets in question.

=item street1, street2

Names of the streets in question.

=item type1, type2

Street types for the streets in question.

=item suffix1, suffix2

Directional suffixes for the streets in question.

=item city

City or locale containing the intersection, as above.

=item state

State abbreviation, as above.

=item zip

Five digit ZIP code, as above.

=back

=cut 

use 5.6.1;
use strict;
use warnings;

our $VERSION = '0.99';

=head1 GLOBAL VARIABLES

Geo::StreetAddress::US contains a number of global variables which it
uses to recognize different bits of US street addresses. Although you
will probably not need them, they are documented here for completeness's
sake.

=over 4

=item %Directional

Maps directional names (north, northeast, etc.) to abbreviations (N, NE, etc.).

=item %Direction_Code

Maps directional abbreviations to directional names.

=cut

our %Directional = (
    north	=> "N",
    northeast	=> "NE",
    east	=> "E",
    southeast	=> "SE",
    south	=> "S",
    southwest	=> "SW",
    west	=> "W",
    northwest	=> "NW",
);

our %Direction_Code = reverse %Directional;

=item %Street_Type

Maps lowercased USPS standard street types to their canonical postal
abbreviations as found in TIGER/Line.  See eg/get_street_abbrev.pl in
the distrbution for how this map was generated.

=cut

our %Street_Type = (
    allee	=> "aly",
    alley	=> "aly",
    ally	=> "aly",
    anex	=> "anx",
    annex	=> "anx",
    annx	=> "anx",
    arcade	=> "arc",
    av		=> "ave",
    aven	=> "ave",
    avenu	=> "ave",
    avenue	=> "ave",
    avn		=> "ave",
    avnue	=> "ave",
    bayoo	=> "byu",
    bayou	=> "byu",
    beach	=> "bch",
    bend	=> "bnd",
    bluf	=> "blf",
    bluff	=> "blf",
    bluffs	=> "blfs",
    bot		=> "btm",
    bottm	=> "btm",
    bottom	=> "btm",
    boul	=> "blvd",
    boulevard	=> "blvd",
    boulv	=> "blvd",
    branch	=> "br",
    brdge	=> "brg",
    bridge	=> "brg",
    brnch	=> "br",
    brook	=> "brk",
    brooks	=> "brks",
    burg	=> "bg",
    burgs	=> "bgs",
    bypa	=> "byp",
    bypas	=> "byp",
    bypass	=> "byp",
    byps	=> "byp",
    camp	=> "cp",
    canyn	=> "cyn",
    canyon	=> "cyn",
    cape	=> "cpe",
    causeway	=> "cswy",
    causway	=> "cswy",
    cen		=> "ctr",
    cent	=> "ctr",
    center	=> "ctr",
    centers	=> "ctrs",
    centr	=> "ctr",
    centre	=> "ctr",
    circ	=> "cir",
    circl	=> "cir",
    circle	=> "cir",
    circles	=> "cirs",
    ck		=> "crk",
    cliff	=> "clf",
    cliffs	=> "clfs",
    club	=> "clb",
    cmp		=> "cp",
    cnter	=> "ctr",
    cntr	=> "ctr",
    cnyn	=> "cyn",
    common	=> "cmn",
    corner	=> "cor",
    corners	=> "cors",
    course	=> "crse",
    court	=> "ct",
    courts	=> "cts",
    cove	=> "cv",
    coves	=> "cvs",
    cr		=> "crk",
    crcl	=> "cir",
    crcle	=> "cir",
    crecent	=> "cres",
    creek	=> "crk",
    crescent	=> "cres",
    cresent	=> "cres",
    crest	=> "crst",
    crossing	=> "xing",
    crossroad	=> "xrd",
    crscnt	=> "cres",
    crsent	=> "cres",
    crsnt	=> "cres",
    crssing	=> "xing",
    crssng	=> "xing",
    crt		=> "ct",
    curve	=> "curv",
    dale	=> "dl",
    dam		=> "dm",
    div		=> "dv",
    divide	=> "dv",
    driv	=> "dr",
    drive	=> "dr",
    drives	=> "drs",
    drv		=> "dr",
    dvd		=> "dv",
    estate	=> "est",
    estates	=> "ests",
    exp		=> "expy",
    expr	=> "expy",
    express	=> "expy",
    expressway	=> "expy",
    expw	=> "expy",
    extension	=> "ext",
    extensions	=> "exts",
    extn	=> "ext",
    extnsn	=> "ext",
    falls	=> "fls",
    ferry	=> "fry",
    field	=> "fld",
    fields	=> "flds",
    flat	=> "flt",
    flats	=> "flts",
    ford	=> "frd",
    fords	=> "frds",
    forest	=> "frst",
    forests	=> "frst",
    forg	=> "frg",
    forge	=> "frg",
    forges	=> "frgs",
    fork	=> "frk",
    forks	=> "frks",
    fort	=> "ft",
    freeway	=> "fwy",
    freewy	=> "fwy",
    frry	=> "fry",
    frt		=> "ft",
    frway	=> "fwy",
    frwy	=> "fwy",
    garden	=> "gdn",
    gardens	=> "gdns",
    gardn	=> "gdn",
    gateway	=> "gtwy",
    gatewy	=> "gtwy",
    gatway	=> "gtwy",
    glen	=> "gln",
    glens	=> "glns",
    grden	=> "gdn",
    grdn	=> "gdn",
    grdns	=> "gdns",
    green	=> "grn",
    greens	=> "grns",
    grov	=> "grv",
    grove	=> "grv",
    groves	=> "grvs",
    gtway	=> "gtwy",
    harb	=> "hbr",
    harbor	=> "hbr",
    harbors	=> "hbrs",
    harbr	=> "hbr",
    haven	=> "hvn",
    havn	=> "hvn",
    height	=> "hts",
    heights	=> "hts",
    hgts	=> "hts",
    highway	=> "hwy",
    highwy	=> "hwy",
    hill	=> "hl",
    hills	=> "hls",
    hiway	=> "hwy",
    hiwy	=> "hwy",
    hllw	=> "holw",
    hollow	=> "holw",
    hollows	=> "holw",
    holws	=> "holw",
    hrbor	=> "hbr",
    ht		=> "hts",
    hway	=> "hwy",
    inlet	=> "inlt",
    island	=> "is",
    islands	=> "iss",
    isles	=> "isle",
    islnd	=> "is",
    islnds	=> "iss",
    jction	=> "jct",
    jctn	=> "jct",
    jctns	=> "jcts",
    junction	=> "jct",
    junctions	=> "jcts",
    junctn	=> "jct",
    juncton	=> "jct",
    key		=> "ky",
    keys	=> "kys",
    knol	=> "knl",
    knoll	=> "knl",
    knolls	=> "knls",
    la		=> "ln",
    lake	=> "lk",
    lakes	=> "lks",
    landing	=> "lndg",
    lane	=> "ln",
    lanes	=> "ln",
    ldge	=> "ldg",
    light	=> "lgt",
    lights	=> "lgts",
    lndng	=> "lndg",
    loaf	=> "lf",
    lock	=> "lck",
    locks	=> "lcks",
    lodg	=> "ldg",
    lodge	=> "ldg",
    loops	=> "loop",
    manor	=> "mnr",
    manors	=> "mnrs",
    meadow	=> "mdw",
    meadows	=> "mdws",
    medows	=> "mdws",
    mill	=> "ml",
    mills	=> "mls",
    mission	=> "msn",
    missn	=> "msn",
    mnt		=> "mt",
    mntain	=> "mtn",
    mntn	=> "mtn",
    mntns	=> "mtns",
    motorway	=> "mtwy",
    mount	=> "mt",
    mountain	=> "mtn",
    mountains	=> "mtns",
    mountin	=> "mtn",
    mssn	=> "msn",
    mtin	=> "mtn",
    neck	=> "nck",
    orchard	=> "orch",
    orchrd	=> "orch",
    overpass	=> "opas",
    ovl		=> "oval",
    parks	=> "park",
    parkway	=> "pkwy",
    parkways	=> "pkwy",
    parkwy	=> "pkwy",
    passage	=> "psge",
    paths	=> "path",
    pikes	=> "pike",
    pine	=> "pne",
    pines	=> "pnes",
    pk		=> "park",
    pkway	=> "pkwy",
    pkwys	=> "pkwy",
    pky		=> "pkwy",
    place	=> "pl",
    plain	=> "pln",
    plaines	=> "plns",
    plains	=> "plns",
    plaza	=> "plz",
    plza	=> "plz",
    point	=> "pt",
    points	=> "pts",
    port	=> "prt",
    ports	=> "prts",
    prairie	=> "pr",
    prarie	=> "pr",
    prk		=> "park",
    prr		=> "pr",
    rad		=> "radl",
    radial	=> "radl",
    radiel	=> "radl",
    ranch	=> "rnch",
    ranches	=> "rnch",
    rapid	=> "rpd",
    rapids	=> "rpds",
    rdge	=> "rdg",
    rest	=> "rst",
    ridge	=> "rdg",
    ridges	=> "rdgs",
    river	=> "riv",
    rivr	=> "riv",
    rnchs	=> "rnch",
    road	=> "rd",
    roads	=> "rds",
    route	=> "rte",
    rvr		=> "riv",
    shoal	=> "shl",
    shoals	=> "shls",
    shoar	=> "shr",
    shoars	=> "shrs",
    shore	=> "shr",
    shores	=> "shrs",
    skyway	=> "skwy",
    spng	=> "spg",
    spngs	=> "spgs",
    spring	=> "spg",
    springs	=> "spgs",
    sprng	=> "spg",
    sprngs	=> "spgs",
    spurs	=> "spur",
    sqr		=> "sq",
    sqre	=> "sq",
    sqrs	=> "sqs",
    squ		=> "sq",
    square	=> "sq",
    squares	=> "sqs",
    station	=> "sta",
    statn	=> "sta",
    stn		=> "sta",
    str		=> "st",
    strav	=> "stra",
    strave	=> "stra",
    straven	=> "stra",
    stravenue	=> "stra",
    stravn	=> "stra",
    stream	=> "strm",
    street	=> "st",
    streets	=> "sts",
    streme	=> "strm",
    strt	=> "st",
    strvn	=> "stra",
    strvnue	=> "stra",
    sumit	=> "smt",
    sumitt	=> "smt",
    summit	=> "smt",
    terr	=> "ter",
    terrace	=> "ter",
    throughway	=> "trwy",
    tpk		=> "tpke",
    tr		=> "trl",
    trace	=> "trce",
    traces	=> "trce",
    track	=> "trak",
    tracks	=> "trak",
    trafficway	=> "trfy",
    trail	=> "trl",
    trails	=> "trl",
    trk		=> "trak",
    trks	=> "trak",
    trls	=> "trl",
    trnpk	=> "tpke",
    trpk	=> "tpke",
    tunel	=> "tunl",
    tunls	=> "tunl",
    tunnel	=> "tunl",
    tunnels	=> "tunl",
    tunnl	=> "tunl",
    turnpike	=> "tpke",
    turnpk	=> "tpke",
    underpass	=> "upas",
    union	=> "un",
    unions	=> "uns",
    valley	=> "vly",
    valleys	=> "vlys",
    vally	=> "vly",
    vdct	=> "via",
    viadct	=> "via",
    viaduct	=> "via",
    view	=> "vw",
    views	=> "vws",
    vill	=> "vlg",
    villag	=> "vlg",
    village	=> "vlg",
    villages	=> "vlgs",
    ville	=> "vl",
    villg	=> "vlg",
    villiage	=> "vlg",
    vist	=> "vis",
    vista	=> "vis",
    vlly	=> "vly",
    vst		=> "vis",
    vsta	=> "vis",
    walks	=> "walk",
    well	=> "wl",
    wells	=> "wls",
    wy		=> "way",
);

our %_Street_Type_List = map { $_ => 1 } %Street_Type; 

=item %State_Code

Maps lowercased US state and territory names to their canonical two-letter
postal abbreviations. See eg/get_state_abbrev.pl in the distrbution
for how this map was generated.

=cut

our %State_Code = (
    "alabama" => "AL",
    "alaska" => "AK",
    "american samoa" => "AS",
    "arizona" => "AZ",
    "arkansas" => "AR",
    "california" => "CA",
    "colorado" => "CO",
    "connecticut" => "CT",
    "delaware" => "DE",
    "district of columbia" => "DC",
    "federated states of micronesia" => "FM",
    "florida" => "FL",
    "georgia" => "GA",
    "guam" => "GU",
    "hawaii" => "HI",
    "idaho" => "ID",
    "illinois" => "IL",
    "indiana" => "IN",
    "iowa" => "IA",
    "kansas" => "KS",
    "kentucky" => "KY",
    "louisiana" => "LA",
    "maine" => "ME",
    "marshall islands" => "MH",
    "maryland" => "MD",
    "massachusetts" => "MA",
    "michigan" => "MI",
    "minnesota" => "MN",
    "mississippi" => "MS",
    "missouri" => "MO",
    "montana" => "MT",
    "nebraska" => "NE",
    "nevada" => "NV",
    "new hampshire" => "NH",
    "new jersey" => "NJ",
    "new mexico" => "NM",
    "new york" => "NY",
    "north carolina" => "NC",
    "north dakota" => "ND",
    "northern mariana islands" => "MP",
    "ohio" => "OH",
    "oklahoma" => "OK",
    "oregon" => "OR",
    "palau" => "PW",
    "pennsylvania" => "PA",
    "puerto rico" => "PR",
    "rhode island" => "RI",
    "south carolina" => "SC",
    "south dakota" => "SD",
    "tennessee" => "TN",
    "texas" => "TX",
    "utah" => "UT",
    "vermont" => "VT",
    "virgin islands" => "VI",
    "virginia" => "VA",
    "washington" => "WA",
    "west virginia" => "WV",
    "wisconsin" => "WI",
    "wyoming" => "WY",
);

=item %State_FIPS

Maps two-digit FIPS-55 US state and territory codes (including the
leading zero!) as found in TIGER/Line to the state's canonical two-letter
postal abbreviation. See eg/get_state_fips.pl in the distrbution for
how this map was generated. Yes, I know the FIPS data also has the state
names. Oops.

=cut

our %State_FIPS = (
    "01" => "AL",
    "02" => "AK",
    "04" => "AZ",
    "05" => "AR",
    "06" => "CA",
    "08" => "CO",
    "09" => "CT",
    "10" => "DE",
    "11" => "DC",
    "12" => "FL",
    "13" => "GA",
    "15" => "HI",
    "16" => "ID",
    "17" => "IL",
    "18" => "IN",
    "19" => "IA",
    "20" => "KS",
    "21" => "KY",
    "22" => "LA",
    "23" => "ME",
    "24" => "MD",
    "25" => "MA",
    "26" => "MI",
    "27" => "MN",
    "28" => "MS",
    "29" => "MO",
    "30" => "MT",
    "31" => "NE",
    "32" => "NV",
    "33" => "NH",
    "34" => "NJ",
    "35" => "NM",
    "36" => "NY",
    "37" => "NC",
    "38" => "ND",
    "39" => "OH",
    "40" => "OK",
    "41" => "OR",
    "42" => "PA",
    "44" => "RI",
    "45" => "SC",
    "46" => "SD",
    "47" => "TN",
    "48" => "TX",
    "49" => "UT",
    "50" => "VT",
    "51" => "VA",
    "53" => "WA",
    "54" => "WV",
    "55" => "WI",
    "56" => "WY",
    "72" => "PR",
    "78" => "VI",
);

our %FIPS_State = reverse %State_FIPS;

=item %Addr_Match

A hash of compiled regular expressions corresponding to different
types of address or address portions. Defined regexen include
type, number, fraction, state, direct(ion), dircode, zip, corner,
street, place, address, and intersection.

=cut

our %Addr_Match = (
    type    => join("|", keys %_Street_Type_List),
    number  => qr/\d+-?\d*/,
    fraction => qr{\d+\/\d+},
    state   => join("|",
		    # escape spaces in state names (e.g., "new york" --> "new\\ york")
		    # so they still match in the x environment below
		    map { ( quotemeta $_) } keys %State_Code,
		    values %State_Code),
    direct  => join("|",
		    # map direction names to direction codes
                    keys %Directional,
		    # also map the dotted version of the code to the code itself
                    map { my $c = $_;
                          $c =~ s/(\w)/$1./g;
                          ( quotemeta $c, $_ ) }
                    sort { length $b <=> length $a }
                    values %Directional),
    dircode => join("|", keys %Direction_Code), 
    zip	    => qr/\d{5}(?:-\d{4})?/,
    corner  => qr/(?:\band\b|\bat\b|&|\@)/i,
    unit    => qr/(?:(?:su?i?te|p\W*[om]\W*b(?:ox)?|dept|apt|ro*m|fl|apt|unit|box)\W+|#\W*)[\w-]+/i,
);

{
    use re 'eval';
    $Addr_Match{street} = qr/
        (?:
          # special case for addresses like 100 South Street
          (?:($Addr_Match{direct})\W+           (?{ $_{street} = $^N })
             ($Addr_Match{type})\b              (?{ $_{type}   = $^N }))
          |
          (?:($Addr_Match{direct})\W+		(?{ $_{prefix} = $^N }))?
          (?:
            ([^,]+)				(?{ $_{street} = $^N })
            (?:[^\w,]+($Addr_Match{type})\b	(?{ $_{type}   = $^N }))
            (?:[^\w,]+($Addr_Match{direct})\b	(?{ $_{suffix} = $^N }))?
           |
            ([^,]*\d)				(?{ $_{street} = $^N })
            ($Addr_Match{direct})\b		(?{ $_{suffix} = $^N })
           |
            ([^,]+?)				(?{ $_{street} = $^N })
            (?:[^\w,]+($Addr_Match{type})\b	(?{ $_{type}   = $^N }))?
            (?:[^\w,]+($Addr_Match{direct})\b	(?{ $_{suffix} = $^N }))?
          )
        )
	/ix;

    $Addr_Match{place} = qr/
	(?:
	    ([^\d,]+?)\W+			(?{ $_{city}   = $^N })
	    ($Addr_Match{state})\W*		(?{ $_{state}  = $^N })
	)?
	(?:($Addr_Match{zip})			(?{ $_{zip}    = $^N }))?
	/ix;

    $Addr_Match{address} = qr/^\W*
	(  $Addr_Match{number})\W*		(?{ $_{number} = $^N })
        (?:($Addr_Match{fraction})\W*   (?{$_{fraction} = $^N }))?
	   $Addr_Match{street}\W+
	(?:($Addr_Match{unit})\W+   (?{$_{unit} = $^N }))?
	   $Addr_Match{place}
	\W*$/ix;

    $Addr_Match{intersection} = qr/^\W*
	   $Addr_Match{street}\W*?	
	    (?{ @_{qw{prefix1 street1 type1 suffix1}}
		= delete @_{qw{prefix street type suffix }} })

	\s+$Addr_Match{corner}\s+

	   $Addr_Match{street}\W+
	    (?{ @_{qw{prefix2 street2 type2 suffix2}}
		= delete @_{qw{prefix street type suffix }} })

	   $Addr_Match{place}
	\W*$/ix;
}

our %Normalize_Map = (
    prefix  => \%Directional,
    prefix1 => \%Directional,
    prefix2 => \%Directional,
    suffix  => \%Directional,
    suffix1 => \%Directional,
    suffix2 => \%Directional,
    type    => \%Street_Type,
    type1   => \%Street_Type,
    type2   => \%Street_Type,
    state   => \%State_Code,
);

=back

=head1 CLASS METHODS

=item Geo::StreetAddress::US->parse_location( $string )

Parses any address or intersection string and returns the appropriate
specifier, by calling parse_intersection() or parse_address() as needed.

=cut

sub parse_location {
    my ($class, $addr, $normalize) = @_;
    if (!defined($normalize)) { #$normalize defaults to true
      $normalize = 1;
    }
    if ($addr =~ /$Addr_Match{corner}/ios) {
	$class->parse_intersection($addr,$normalize);
    } else {
	$class->parse_address($addr,$normalize);
    }
}

=item Geo::StreetAddress::US->parse_address( $address_string )

Parses a street address into an address specifier, returning undef if
the address cannot be parsed. You probably want to use parse_location()
instead.

=cut

sub parse_address {
    my ($class, $addr, $normalize) = @_;
    if (!defined($normalize)) { #$normalize defaults to true
      $normalize = 1;
    }
    local %_;

    if ($addr =~ /$Addr_Match{address}/ios) {
	my %part = %_;
	### the next line is just to make fossil tests work
	$part{$_} ||= undef for qw{prefix type suffix city state zip};
	if ($normalize) {
	  return $class->normalize_address(\%part);
	} else {
	  return \%part;
	}
    }
}

=item Geo::StreetAddress::US->parse_intersection( $intersection_string )

Parses an intersection string into an intersection specifier, returning
undef if the address cannot be parsed. You probably want to use
parse_location() instead.

=cut

sub parse_intersection {
    my ($class, $addr, $normalize) = @_;
    if (!defined($normalize)) { #$normalize defaults to true
      $normalize = 1;
    }
    local %_;

    if ($addr =~ /$Addr_Match{intersection}/ios) {
	my %part = %_;
	### the next line is just to make fossil tests work
	$part{$_} ||= undef 
	    for qw{prefix1 type1 suffix1 prefix2 type2 suffix2 city state zip};

	if ( $part{type2} and $part{type2} =~ s/s\W*$//ios ) {
	    if ( $part{type2} =~ /^$Addr_Match{type}$/ios && ! $part{type1} ) {
		$part{type1} = $part{type2};
	    } else {
		$part{type2} .= "s";
	    }
	}

	if ($normalize) {
	  return $class->normalize_address(\%part);
	} else {
	  return \%part;
	}
    }
}

=item Geo::StreetAddress::US->normalize_address( $spec )

Takes an address or intersection specifier, and normalizes its components,
stripping out all leading and trailing whitespace and punctuation, and
substituting official abbreviations for prefix, suffix, type, and state values.
Also, city names that are prefixed with a directional abbreviation (e.g. N, NE,
etc.) have the abbreviation expanded.  The normalized specifier is returned.

Typically, you won't need to use this method, as the C<parse_*()> methods
call it for you.

N.B., C<normalize_address()> crops 9-digit ZIP codes to 5 digits. This is for
the benefit of Geo::Coder::US and may not be what you want. E-mail me if this
is a problem and I'll see what I can do to fix it.

=cut

sub normalize_address {
    my ($class, $part) = @_;

    # strip off punctuation
    defined($_) && s/^\s+|\s+$|[^\w\s\-]//gos for values %$part;

    while (my ($key, $map) = each %Normalize_Map) {
	$part->{$key} = $map->{lc $part->{$key}}
              if  exists $part->{$key}
	      and exists $map->{lc $part->{$key}};
    }

    $part->{$_} = ucfirst lc $part->{$_} 
	for grep(exists $part->{$_}, qw( type type1 type2 ));

    # attempt to expand directional prefixes on place names
    $part->{city} =~ s/^($Addr_Match{dircode})\s+(?=\S)
		      /\u$Direction_Code{$1} /iosx
		      if $part->{city};

    # strip ZIP+4
    $part->{zip} =~ s/-.*$//os if $part->{zip};

    return $part;
}

1;
__END__

=head1 BUGS, CAVEATS, MISCELLANY

Geo::StreetAddress::US might not correctly parse house numbers that contain
hyphens, such as those used in parts of Queens, New York. Also, some addresses
in rural Michigan and Illinois may contain letter prefixes to the building
number that may cause problems. Fixing these edge cases is on the to-do list,
to be sure. Patches welcome!

This software was originally part of Geo::Coder::US (q.v.) but was split apart
into an independent module for your convenience. Therefore it has some
behaviors which were designed for Geo::Coder::US, but which may not be right
for your purposes. If this turns out to be the case, please let me know.

Geo::StreetAddress::US does B<NOT> perform USPS-certified address normalization.

=head1 SEE ALSO

This software was originally part of Geo::Coder::US(3pm).

Lingua::EN::AddressParse(3pm) and Geo::PostalAddress(3pm) both do something
very similar to Geo::StreetAddress::US, but are either too strict/limited in
their address parsing, or not really specific enough in how they break down
addresses (for my purposes). If you want USPS-style address standardization,
try Scrape::USPS::ZipLookup(3pm). Be aware, however, that it scrapes a form on
the USPS website in a way that may not be officially permitted and might break
at any time. If this module does not do what you want, you might give the
othersa try. All three modules are available from the CPAN.

You can see Geo::StreetAddress::US in action at L<http://geocoder.us/>.

=head1 APPRECIATION

Many thanks to Dave Rolsky for submitting a very useful patch to fix fractional
house numbers, dotted directionals, and other kinds of edge cases, e.g. South
St. He even submitted additional tests!

=head1 AUTHOR

Schuyler D. Erle E<lt>schuyler@geocoder.usE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by Schuyler D. Erle.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.

=cut
