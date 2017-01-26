package PubManager::Utils;
use strict;
use warnings FATAL => 'all';

require 5.10.1;

use Exporter qw(import);
our @EXPORT_OK = qw(setOutputFolder setXMLOutputFolder processPMCDoc getErrorMsg getErrorCode setDebug);

use File::Path qw(make_path);
use File::Spec::Functions;
use File::Copy qw(move);
use Data::Dumper;
use PubManager::Log;
local $PubManager::Log::Caller_Info = 0;

# Disable smartmatch warnings
no if $] >= 5.017011, warnings => 'experimental::smartmatch';

our (
    $source,
    $outdir,
    $xmloutdir,
    $debug,
);

# defaults
$outdir = $outdir // "./ppp";
$xmloutdir = $xmloutdir // "./xml";
$debug = 0;

#print "DEFAULT Value of \$outdir = \'$outdir'\n";

## XPaths for items of interest
my (
    # error (if there is an error element in the XML)
    $requestError,
    # article (relative to root)
    $artiPath,
    # article title
    $titlPath,
    # abstract (relative to <article>)
    $abstPath,
    # body (relative to <article>)
    $bodyPath,
    # section (relative to <body>, only top-level ones)
    $sectPath,
    # section title (relative to <sec>)
    $sttlPath,
    # paragraphs (relative to <sec>, only top-level ones)
    $paraPath,
    # paragraphs in all subsections (relative to <sec>)
    $pRecPath,
    # figures (relative tp <sec>)
    $figPath,
    # figure captions (relative to <fig>)
    $figCPath,
);


## GLOBALS
our (
    $errorMsg,
    $errorCode,
    $artiNode,	# <article>
    $mUnitInd, # major unit indicator
    $titlNode,	# <article-title>
    $abstNode,	# <abstract>
    $bodyNode,	# <body>
    @sections,	# <sec>*
    $intrNode,	# sec::introduction
    $resuNode,	# sec::results (may include discussion)
    $discNode,	# sec::discussion
    $concNode,	# sec::conclusion
);

## STATIC MAPPINGS

my %curlErrorCodeMapping = (
    1, 'Unsupported protocol. This build of curl has no support for this protocol.',
    2, 'Failed to initialize.',
    3, 'URL malformat. The syntax was not correct.',
    4, 'URL user malformatted. The user-part of the URL syntax was not correct.',
    5, 'Could not resolve proxy. The given proxy host could not be resolved.',
    6, 'Could not resolve host. The given remote host was not resolved.',
    7, 'Failed to connect to host.',
    8, 'FTP weird server reply. The server sent data curl couldn’t parse.',
    9, 'FTP access denied. The server denied login.',
    10, 'FTP user/password incorrect. Either one or both were not accepted by the server.',
    11, 'FTP weird PASS reply. Curl couldn’t parse the reply sent to the PASS request.',
    12, 'FTP weird USER reply. Curl couldn’t parse the reply sent to the USER request.',
    13, 'FTP weird PASV reply. Curl couldn’t parse the reply sent to the PASV request.',
    14, 'FTP weird 227 format. Curl couldn’t parse the 227-line the server sent.',
    15, 'FTP can’t get host. Couldn’t resolve the host IP we got in the 227-line.',
    16, 'FTP can’t reconnect. Couldn’t connect to the host we got in the 227-line.',
    17, 'FTP couldn’t set binary. Couldn’t change transfer method to binary.',
    18, 'Partial file. Only a part of the file was transfered.',
    19, 'FTP couldn’t download/access the given file, the RETR (or similar) command failed.',
    20, 'FTP write error. The transfer was reported bad by the server.',
    21, 'FTP quote error. A quote command returned error from the server.',
    22, 'HTTP page not retrieved. The requested url was not found or returned another error with the HTTP error code being 400 or above. This return code only appears if -f/--fail is used.',
    23, 'Write error. Curl couldn’t write data to a local filesystem or similar.',
    24, 'Malformed user. User name badly specified.',
    25, 'FTP couldn’t STOR file. The server denied the STOR operation, used for FTP uploading.',
    26, 'Read error. Various reading problems.',
    27, 'Out of memory. A memory allocation request failed.',
    28, 'Operation timeout. The specified time-out period was reached according to the conditions.',
    29, 'FTP couldn’t set ASCII. The server returned an unknown reply.',
    30, 'FTP PORT failed. The PORT command failed. Not all FTP servers support the PORT command, try doing a transfer using PASV instead!',
    31, 'FTP couldn’t use REST. The REST command failed. This command is used for resumed FTP transfers.',
    32, 'FTP couldn’t use SIZE. The SIZE command failed. The command is an extension to the original FTP spec RFC 959.',
    33, 'HTTP range error. The range "command" didn’t work.',
    34, 'HTTP post error. Internal post-request generation error.',
    35, 'SSL connect error. The SSL handshaking failed.',
    36, 'FTP bad download resume. Couldn’t continue an earlier aborted download.',
    37, 'FILE couldn’t read file. Failed to open the file. Permissions?',
    38, 'LDAP cannot bind. LDAP bind operation failed.',
    39, 'LDAP search failed.',
    40, 'Library not found. The LDAP library was not found.',
    41, 'Function not found. A required LDAP function was not found.',
    42, 'Aborted by callback. An application told curl to abort the operation.',
    43, 'Internal error. A function was called with a bad parameter.',
    44, 'Internal error. A function was called in a bad order.',
    45, 'Interface error. A specified outgoing interface could not be used.',
    46, 'Bad password entered. An error was signaled when the password was entered.',
    47, 'Too many redirects. When following redirects, curl hit the maximum amount.',
    48, 'Unknown TELNET option specified.',
    49, 'Malformed telnet option.',
    51, 'The remote peer’s SSL certificate wasn’t ok.',
    52, 'The server didn’t reply anything, which here is considered an error.',
    53, 'SSL crypto engine not found.',
    54, 'Cannot set SSL crypto engine as default.',
    55, 'Failed sending network data.',
    56, 'Failure in receiving network data.',
    57, 'Share is in use (internal error).',
    58, 'Problem with the local certificate.',
    59, 'Couldn’t use specified SSL cipher.',
    60, 'Problem with the CA cert (path? permission?).',
    61, 'Unrecognized transfer encoding.',
    62, 'Invalid LDAP URL.',
    63, 'Maximum file size exceeded.'
);

## SUBROUTINES

sub setOutputFolder
{
    my($outputDir) = @_;
    $errorMsg = undef;
    $errorCode = undef;

    # verify that something was passed into the subroutine
    unless ($outputDir)
    {
        $errorMsg = "Invalid Argument.  No output folder was passed in.";
        $errorCode = 1010;
        return -1;
    }

    # check to see if something already exists here
    if (-e $outputDir)
    {
        # is it a directory?
        unless (-d $outputDir)
        {
            # this is pointing to a file, not a directory!!
            $errorMsg = "The passed in output folder points to a file.";
            $errorCode = 1011;
            return -2;
        }
    }

    # Okay it either already exists as a folder or does not exists yet.
    $outdir = $outputDir;
    return 0;
}

sub setXMLOutputFolder
{
    my($outputDir) = @_;
    $errorMsg = undef;
    $errorCode = undef;

    # verify that something was passed into the subroutine
    unless ($outputDir)
    {
        $errorMsg = "Invalid Argument.  No xml output folder was passed in.";
        $errorCode = 1010;
        return -1;
    }

    # check to see if something already exists here
    if (-e $outputDir)
    {
        # is it a directory?
        unless (-d $outputDir)
        {
            # this is pointing to a file, not a directory!!
            $errorMsg = "The passed in xml output folder points to a file.";
            $errorCode = 1011;
            return -2;
        }
    }

    unless (-e $outputDir)
    {
        eval { make_path($outputDir) };
        if ($@) {
            $errorCode = 1000;
            $errorMsg = "Unable to create xml output dir: $outputDir";
            return -3;
        }
    }

    # Okay it either already exists as a folder or does not exists yet.
    $xmloutdir = $outputDir;
    return 0;
}

sub setDebug
{
    my($debugEnabled) = @_;
    $debug = $debugEnabled;
}

sub getErrorCode
{
    return $errorCode;
}

sub getErrorMsg
{
    return $errorMsg;
}

sub processPMCDoc
{
    my ($PMCID) = @_;
    $errorMsg = undef;
    $errorCode = undef;

    DEBUG 1, "retrieve the document.";
    getPMCDoc($PMCID);
    if ($errorCode)
    {
        return $errorCode;
    }

    DEBUG 1, "parse the document.";
    parseNXML($PMCID);
    if ($errorCode)
    {
        return $errorCode;
    }

    DEBUG 1, "finished.";
    return 0;
}

sub getPMCDoc
{
    my ($PMCID) = @_;
    my $outfile = "$xmloutdir/$PMCID.xml";
    my $headfile = "$xmloutdir/head.txt";
    my $pmcurl = "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:$PMCID&metadataPrefix=pmc";
    #    my $pmcurl = "http://httpstat.us/501";
    my $rc = 0;
    my @httpCode;

    # Use curl to get the publication. (-s) disable curl output, (-o) save the results in $outfile and
    # (-D) the http response header in $headfile, and (-L) follow redirects.
    $rc = system ('curl', '-s', '-o', $outfile, '-D', $headfile, '-L', $pmcurl);
    $rc = $rc >> 8;

    if ($rc != 0)
    {
        unlink $headfile;
        unlink $outfile;
        $errorCode = $rc;
        $errorMsg = $curlErrorCodeMapping{$rc}
    }
    else
    {
        open (HEADER, "<$headfile");
        my @lines;
        while (<HEADER>)
        {
            push(@lines, $_);
        }
        close (HEADER);
        unlink $headfile;

        # Parse the http header for the response code and then determine if there was an http issue.
        @httpCode = split(' ', $lines[0]);

        $rc = int($httpCode[1]); ## convert

        if ($rc < 200 or $rc >= 300)
        {
            unlink $outfile;
            $errorCode = $rc;
            $errorMsg = join (' ', @httpCode[2 .. $#httpCode]);
        }
        else
        {
            $rc = 0;
        }
    }

    return $rc;
}

sub parseNXML
{
    use XML::LibXML;
    my ($PMCID) = @_;
    my $xmlfile = "$xmloutdir/$PMCID.xml";

    open my $fh, '<', $xmlfile or do {
        unlink $xmlfile;
        $errorCode = 1001;
        $errorMsg = "Error opening file: \'$xmlfile\'. ($!)";
        return 0;
    };
    binmode $fh;
    my $doc = XML::LibXML->new->load_xml(IO => $fh, line_numbers => 1);
    close $fh;
#    unlink $xmlfile;

    # Determine the format of the xml document.
    $source = 'UNKNOWN';
    my $root = $doc->documentElement();
    if ($root->nodeName eq "OAI-PMH") {
        $source = "PMCOA";
    } elsif ($root->nodeName eq "html") {
        $source = "XHTML";
    }
    if ($source eq "UNKNOWN")
    {
        $errorCode = 1002;
        $errorMsg = 'Unknown document format.';
        return 0;
    }

    _init();

    # Check for error message in the xml response.
    ### error
    ($artiNode) = $doc->findnodes($requestError);
    if ($artiNode) {
        $errorCode = $artiNode->getAttribute('code');
        $errorMsg = $artiNode->textContent;
        return 0;
    }

    # Make sure there are articles ($artiPath) in the document.
    ### article
    ($artiNode) = $doc->findnodes($artiPath);

    unless ($artiNode) {
        $errorCode = 1003;
        $errorMsg = 'No <article> element found in the document.';
        return 0;
    }

    unless (-e $outdir)
    {
        eval { make_path($outdir) };
        if ($@) {
            $errorCode = 1000;
            $errorMsg = "Unable to create output dir: $outdir";
            return 0;
        }
    }

    # Get the article title
    $mUnitInd = "0T";
    ($titlNode) = $artiNode->findnodes($titlPath);
    INFO "[%s] Found title", "t"
        if $titlNode;
    &saveToFile($titlNode, "");

    ### 2. ABSTRACT
    $mUnitInd = "1A";
    ## Notes:
    ## 1. some abstracts are organized into sections, with titles
    ## 2. there may be several abstract sections!
    ##    - <abstract abstract-type="toc"> has a high-level, easy-to-understand
    ##      very short summary of the paper
    ##    - <abstract abstract-type="editor"> is the editor's summary; it may
    ##      be quite detailed and may also contain sections, with titles
    ##    - <abstract abstract-type="summary"> is the author's summary
    ##    - there are several other abstract types, such as: "precis", "short",
    ##      "editorial-summary", "hidden-editorial-summary", "web-summary",
    ##      "graphical", "author-highlights", "teaser", etc. this list is
    ##      open-ended.
    ##      @@ http://jats.nlm.nih.gov/articleauthoring/tag-library/1.1d3/attribute/abstract-type.html
    ##    - i am taking the position that certain abstract types can be safely
    ##      ignored for lacking substance: "toc", "short", "teaser", "graphical"
    ##      (though these sometimes contain useful highlights). also ignored
    ##      should be "hidden-editorial-summary", which doesn't sound like
    ##      something that would be published

    my @ignored_abstract_types = ("toc", "short", "teaser", "graphical", "hidden-editorial-summary");
    my @abstracts = $artiNode->findnodes($abstPath);
    my $absNr = 0;
    foreach my $abstNode (@abstracts) {
        ++$absNr;
        my $abstType = $abstNode->getAttribute("abstract-type");
        if ($abstType) {
            if (grep {$abstType eq $_} @ignored_abstract_types) {
                INFO "[a] Skipped: abstract-type=%s", $abstType;
                next;
            }
            INFO "[a] Found: abstract-type=%s", $abstType;
        } else {
            INFO "[a] Found: abstract";
        }
        &pullParagraphs($abstNode, $absNr, "a", 1);
    }

    ### 3. BODY
    $mUnitInd = "2B";
    ($bodyNode) = $artiNode->findnodes($bodyPath);

    unless ($bodyNode) {
        INFO "No <body> found!"; # could be legit, so we exit cleanly
        return;
    }

    # body paragraphs outside sections (some journals allow it instead of an
    # introduction section)
    &pullParagraphs($bodyNode, 0, "b", 0);

    # sections (only top-level)
    @sections = $bodyNode->findnodes($sectPath);
    INFO "Found %d sections", scalar(@sections);

    my $secNr = 0;
    foreach my $sec (@sections) {
        my $secType = $sec->getAttribute("sec-type");
        unless ($secType)
        {
            $secType = "unknown";
        }
        my ($secTitleNode) = $sec->findnodes($sttlPath);
        my $secTitle = undef;
        if ($secTitleNode) {
            $secTitle = $secTitleNode->textContent;
        }
        ++$secNr;

        INFO "> Section %d (sec-type=%s): %s", $secNr, $secType, $secTitle;

        if ($secTitle =~ m/^([0-9]+\.?\s+)?(Introduction|Background)/i)
        {
            &pullFigureCaptions($sec, $secNr, "i");
            &pullParagraphs($sec, $secNr, "i", 1);
        } elsif (($secType eq "results") || ($secTitle =~ m/^([0-9]+\.?\s+)?Results/i))
        {
            &pullFigureCaptions($sec, $secNr, "r");
            &pullParagraphs($sec, $secNr, "r", 1);
        } elsif (($secType eq "discussion") || ($secTitle =~ m/^([0-9]+\.?\s+)?Discussion/i))
        {
            &pullFigureCaptions($sec, $secNr, "d");
            &pullParagraphs($sec, $secNr, "d", 1);
        } elsif (($secType eq "conclusion") || ($secTitle =~ m/^([0-9]+\.?\s+)?Conclusion/i))
        {
            &pullFigureCaptions($sec, $secNr, "c");
            &pullParagraphs($sec, $secNr, "c", 1);
        } else
        {
            INFO "  * ignored";
        }
    }

    return 1;
}


sub _init {
    _init_pmcoa() if $source eq "PMCOA";
    _init_xhtml() if $source eq "XHTML";
}

sub _init_pmcoa {
    # pmcoa request error (relative to root)
    $requestError = '//*[local-name()="error"]';
    # article (relative to root)
    $artiPath = '//*[local-name()="article"]';
    # article title
    $titlPath = '//*[local-name()="title-group"]/*[local-name()="article-title"]';
    # abstract (relative to <article>)
    $abstPath = './/*[local-name()="abstract"]';
    # body (relative to <article>)
    $bodyPath = './/*[local-name()="body"]';
    # section (relative to <body>, only top-level ones)
    $sectPath = '*[local-name()="sec"]';
    # section title (relative to <sec>)
    $sttlPath = './*[local-name()="title"]';
    # paragraphs (relative to <sec>, only top-level ones)
    $paraPath = '*[local-name()="p"]';
    # paragraphs in all subsections (relative to <sec>);
    # - includes subsection titles
    $pRecPath = './/*[local-name()="sec"]/*[local-name()="title" or local-name()="p"]';
    # figures (relative tp <sec>)
    $figPath = './/*[local-name()="fig"]';
    # figure captions (relative to <fig>)
    $figCPath = '*[local-name()="caption"]';
}

sub _init_xhtml {
    # pmcoa request error (relative to root)
    $requestError = '//*[local-name()="div" and '.&attrContains('@class','error').']';
    # article (relative to root)
    $artiPath = '//*[local-name()="div" and '.&attrContains('@class','article').']';
    # article title
    $titlPath = './/*[local-name()="h1" and @id="article-title-1"]';
    # abstract (relative to <article>)
    $abstPath = './/*[local-name()="div" and '.&attrContains('@class','abstract').']';
    # body (relative to <article>)
    $bodyPath = '.';
    # section (relative to <body>, only top-level ones)
    $sectPath = '*[local-name()="div" and starts-with(@id,"sec-")]';
    # section title (relative to <sec>)
    $sttlPath = './*[local-name()="h2"]';
    # paragraphs (relative to <sec>, only top-level ones)
    $paraPath = '*[local-name()="p"]';
    # paragraphs in all subsections (relative to <sec>)
    # - includes subsection titles (h3)
    # - in some papers subsection titles are inlined with the next paragraph
    $pRecPath = './/*[local-name()="div" and @class="subsection"]/*[local-name()="h3" or local-name()="p"]';
    # figures (relative tp <sec>)
    $figPath = './/*[local-name()="div" and '.&attrContains('@class','fig').']';
    # figure captions (relative to <fig>)
    $figCPath = '*[local-name()="div" and @class="fig-caption"]';

}


# get paragraphs from section
# basename convention: <m-unit>_<secno:##><sectype:a>_<parno:##>
# <m-unit> is obtained by saveToFile from the global $mUnitInd
sub pullParagraphs {
    my ($sNode, $sNo, $sType, $rec) = @_;

    # remove big undesirable subsections
    {
        # remove boxed text
        removeUndesirables($sNode, "boxed-text");
        # remove tables
        removeUndesirables($sNode, "table-wrap");
    }

    my @plist = $sNode->findnodes($paraPath);
    if ($rec) {
        push @plist, $sNode->findnodes($pRecPath);
    }
    INFO "[%s] Found %d paragraphs", $sType, scalar(@plist);

    my $index = 1;
    foreach my $p (@plist) {
        my $outfile = sprintf("%02d%s_%02d", $sNo, $sType, $index);
        # remove references
        if ($source eq "PMCOA") {
            simplifyTags($p, "xref");
        } elsif ($source eq "XHTML") {
            simplifyTags($p, "a", { 'class' => qr{^xref-} } );
        }

        # remove external links
        if ($source eq "PMCOA") {
            removeUndesirables($p, "ext-link", { 'ext-link-type' => q{doi} } );
            removeUndesirables($p, "ext-link", { 'ext-link-type' => q{ftp} } );
            removeUndesirables($p, "ext-link", { 'ext-link-type' => q{uri},
                    'xlink:href' => qr{^(http|www)}i } );
        } elsif ($source eq "XHTML") {
            removeUndesirables($p, "a", { 'href' => qr{.} } );
        }

        saveToFile($p, $outfile);
        $index++;
    }
}

# get figure captions from section (figures are not tied directly to paragraphs)
# basename convention: <m-unit>_<secno:##><sectype:a>-<figId>_<parno:##>
# <m-unit> is obtained by saveToFile from the global $mUnitInd
sub pullFigureCaptions {
    my ($sNode, $sNo, $sType) = @_;

    # find figures
    my @fList = $sNode->findnodes($figPath);
    if (scalar(@fList) > 0) {
        INFO "Found %d embedded fig elements", scalar(@fList);
    }
    foreach my $f (@fList) {
        my $fId = $f->getAttribute('id');
        my ($fc) = $f->findnodes($figCPath);
        my @plist = $fc->findnodes($paraPath);
        INFO "Fig. %s caption: Found %d paragraphs", $fId, scalar(@plist);
        my $pNo = 1;
        foreach my $p (@plist) {
            my $outfile = sprintf("%02d%s-%s_%02d",
                $sNo, $sType, $fId, $pNo);
            &saveToFile($p, $outfile);
            $pNo++;
        }
        # now we can safely remove the figure itself
        my $parent = $f->parentNode;
        $parent->removeChild($f);
    }
}

# get figure id
sub getFigureId {
    my $f_node = shift;
    if ($source eq "PMCOA") {
        return $f_node->getAttribute("id");
    } elsif ($source eq "XHTML") {
        my ($label_node) = $f_node->findnodes('./*[local-name()="span" and @class="fig-label"]');
        DEBUG 2, "label_node: %s", $label_node;
        unless ($label_node)
        {
            FATAL "no label?" unless $label_node;
            return 0;
        }
        my $label = $label_node->textContent;
        $label =~ s/\s+|[[:punct:]]//g;
        return $label;
    }
}

# smartly removes a set of undesirable nodes
# it always leaves in place a (possibly empty) node at the end of a group,
# to prevent the possibility of leaving punctuation preceded by spaces,
# as this can have a detrimental effect on sentencization.
sub removeUndesirables {
    my ($p, $elem, $attrs) = @_;

    # build XPath
    my $uPath = './/*[local-name()="'.$elem.'"]';
    # select nodes
    my @uList =
        grep { checkAttrs($_, $attrs) } $p->findnodes($uPath);
    # find fillers inbetween undesirable nodes
    my @groups = groupNodes(@uList);
    foreach my $group (@groups) {
        # remove all but the last
        my $last = pop @$group;
        foreach my $node (@$group) {
            INFO "  Removed: %s", $node;
            $node->removeChildNodes();
            # remove $node as well, since it's empty
            my $parent = $node->parentNode;
            $parent->removeChild($node);
        }
        # if last one is the only child of its parent, remove it
        my @last_siblings = $last->parentNode->childNodes();
        if ((scalar(@last_siblings) == 1) &&
            ($last->parentNode->nodeValue eq $last->nodeValue))
        {
            #            INFO "  Removed: %s", $node;
            $last->parentNode->removeChild($last);
        }
        # we leave the last one, but strip it of attributes
        $last->removeChildNodes();
        removeAttributes($last);
    }
}

sub simplifyTags {
    my ($p, $elem, $attrs) = @_;

    # build XPath
    my $uPath = './/*[local-name()="'.$elem.'"]';
    # select nodes
    my @uList =
        grep { checkAttrs($_, $attrs) } $p->findnodes($uPath);

    foreach my $node (@uList) {
        removeAttributes($node);
    }
}

sub removeAttributes {
    my $node= shift;
    foreach my $attr ($node->findnodes('./@*')) {
        $node->removeAttribute($attr->nodeName);
    }
}

sub checkAttrs {
    my ($node, $attrs) = @_;
    return 1 unless $attrs;
    unless (ref $attrs eq 'HASH') {
        WARN "Improper attribute restriction: $attrs. Ignored.";
        return 1;
    }
    foreach my $attr (keys %$attrs) {
        my $attrRestriction = $attrs->{$attr};
        my $attrValue = $node->getAttribute($attr);
        if (! defined $attrValue) {
            return 0;
        }
        return 0 unless ($attrValue ~~ $attrRestriction);
    }
    return 1;
}

# takes a set of nodes and groups them in subsets
# in addition, includes in each subset filler text nodes
sub groupNodes {
    my @nodes = @_;
    my @result;
    my $i = 0;
    while ($i < scalar(@nodes)) {
        my @set = ($nodes[$i]);
        my $j = $i + 1;
        while ($j < scalar(@nodes)) {
            my ($prev, $next) = @nodes[$j-1..$j];
            my $parent = $prev->parentNode;
            DEBUG 2, "prev: %s", $prev;
            DEBUG 2, "next: %s", $next;
            last unless $parent->isSameNode($next->parentNode);
            my $a_node = $prev->nextSibling;
            last unless $a_node->nodeType() == XML_TEXT_NODE;
            last unless $a_node->isSameNode($next->previousSibling);
            DEBUG 1, "a_node: [%s]", $a_node;
            last unless is_filler($a_node);
            push @set, $a_node, $next;
            $j++;
        }
        push @result, [@set];
        $i = $j;
    }
    DEBUG 2, "groups: %s", Dumper(\@result);
    return @result;
}

sub is_filler {
    my $node = shift;
    my $text = $node->nodeValue;
    if ($text =~ m{\A(?:[,;]?\s*)\Z} # sequence
        or
        $text =~ m{\A[-\x{2010}\x{2011}\x{2013}\x{2014}]\Z}) # dashes
    {
        DEBUG 1, "==> FILLER";
        return 1;
    }
    return 0;
}

# saves content of element to file
sub saveToFile {
    my ($elem, $basename) = @_;

    # check if textContent has any actual text in it
    my $text = $elem->textContent;
    # must contain some text
    return unless (defined $text) && $text =~ m/[a-z]/i;

    my $filename = $mUnitInd;
    if ($basename) {
        $filename .= "_" . $basename;
    }
    #print (" saveToFile: " . $outdir."/".$filename . ".xml" . "\n");
    open my $xfh, '>', catfile($outdir, $filename . ".xml");
    binmode $xfh, ":utf8";
    my $xml = $elem->toString();
    # clean up a bit
    $xml =~ s/^\s+//mg;
    print $xfh $xml;
    close $xfh;

    open my $tfh, '>', catfile($outdir, $filename . ".txt");
    binmode $tfh, ":utf8";
    # clean up a bit
    $text =~ s/^\s+//mg;
    print $tfh $text;
    close $tfh;

}

# helpers
sub attrContains {
    my ($attr, $str) = @_;
    return sprintf("contains(concat(\' \',normalize-space(%s),\' \'),\' %s \')", @_);
}

#use Mozilla::CA;
#use LWP::UserAgent;
#
#sub processPMCDoc1
#{
#    my ($PMCID) = @_;
#    $errorMsg = undef;
#    $errorCode = undef;
#
#    my $resp = getPMCDoc1($PMCID);
#
#    # check the outcome
#    if ($resp->is_error) {
#        # TODO:  Need to determine the correct response based on the status code and text.
#        $errorMsg = $resp->status_line;
#        $errorCode = 1;
#        print "Error: " . $resp->status_line . "\n";
#        return 0;
#    }
#
#    binmode STDOUT, ":utf8";
#    print $resp->decoded_content . "\n";
#
#    #    return parseNXML ($resp->decoded_content);
#}
#
#sub getPMCDoc1
#{
#    my ($PMCID) = @_;
#    my $ua = LWP::UserAgent->new;
#    $ua->agent("$0/0.1 " . $ua->agent);
#    my $pmcurl = "https://www.ncbi.nlm.nih.gov/pmc/oai/oai.cgi?verb=GetRecord&identifier=oai:pubmedcentral.nih.gov:$PMCID&metadataPrefix=pmc";
#
#    my $req = HTTP::Request->new(GET => $pmcurl);
#    $req->header('Accept' => 'text/html');
#
#    # send request
#    return $ua->request($req);
#}
#

1;
