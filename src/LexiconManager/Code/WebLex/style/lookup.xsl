<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="html" 
    doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"
    doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN" />

<xsl:template match="/WORD">
 <html>
 <head>
 <title>TRIPS Word Lookup: <xsl:value-of select="@name" /></title>
 <meta name="date" scheme="RFC822" content="{@modified}" />

<!-- jQuery autocomplete -->
  <script src="../jquery/jquery-latest.js"></script>
  <link rel="stylesheet" href="../jquery/jquery.autocomplete.css" type="text/css" />
  <script type="text/javascript" src="../jquery/jquery.bgiframe.min.js"></script>
  <script type="text/javascript" src="../jquery/jquery.dimensions.js"></script>
  <script type="text/javascript" src="../jquery/jquery.autocomplete.js"></script>
  <script>
<![CDATA[
  $(document).ready(function(){
    $("#search-input").autocomplete("../cgi/autocomplete?package=W");
  });
]]>
  </script>
<!-- end jQuery stuff -->

 <script type="text/javascript">
<![CDATA[

var inFrames = false

// go to the XML file corresponding to the word in the search form's text field
function lookup()
{
  var newHref = encodeURIComponent("W::" + document.getElementsByName('word')[0].value.replace(/\s+/, '_') + ".xml");
  $.ajax({
    type: "HEAD", // check if the file exists; if it doesn't, go to "unknown"
    url: newHref,
    statusCode: {
      200: function() {
	window.location.href = newHref;
      },
      404: function() {
	window.location.href = encodeURIComponent("W::unknown.xml");
      }
    }
  });
}

// for hide/show components
function toggleVisible(id, linktext)
{
  var span = document.getElementById(id)
  var link = document.getElementById(id + '-link')
  if (span.style.display == "none")
  {
    link.innerHTML = link.innerHTML.replace("show", "hide")
    span.style.display = null
    if (!span.innerHTML.match("<"))
    {
      if (id.match("-synset"))
      {
	span.innerHTML = linkifyWords(span.innerHTML)
      } else
      {
	span.innerHTML = linkifyAncestors(span.innerHTML)
      }
    }
  } else
  {
    link.innerHTML = link.innerHTML.replace("hide", "show")
    span.style.display = "none"
  }
}

// turn a comma-separated list of words into a list of links to their data files
function linkifyWords(wordstring)
{
  var words = wordstring.split(',')
  var linkstring = ""
  for (i in words)
  {
    linkstring += "<a href=\"" + encodeURIComponent("W::" + words[i] + ".xml") + "\">" + words[i] + "</a>, "
  }
  return linkstring + "<br />"
}

// same as above for ancestor ONTs
function linkifyAncestors(ancestorstring)
{
  var ancestors = ancestorstring.split(',')
  var linkstring = "<br />"
  var endstring = ""
  for (i in ancestors)
  {
    linkstring += "<ul><li><a href=\"../cgi/browseontology-ajax?search=" + ancestors[i] + "#highlight\"" + (inFrames? 'target="ontology"' : '') + ">" + ancestors[i] + "</a>, "
    endstring += "</li></ul>"
  }
  return linkstring + endstring
}

function setTargets() {
  if (top.location.href != window.location.href) { // we're in frames
    inFrames = true
    var links = document.getElementsByTagName("a")
    for (var i = 0; i < links.length; i++) {
      if (/browseontology/.test(links[i].href)) {
        links[i].target = "ontology"
      }
    }
  }
}

]]>
 </script>
 </head>
 <body onload="setTargets()">
 <h1><img src="/images/monalogo_thin.jpg" alt="URCS (logo)" /> TRIPS Word Lookup</h1>
 <form action="javascript:lookup()">
  <input type="text" size="40" name="word" id="search-input" />
  <input type="submit" value="Look Up" />
 </form>
 <h2><xsl:value-of select="@name" /></h2>
 <ul>
  <xsl:for-each select="POS">
   <xsl:variable name="pos" select="@name" />
   <li>
   <xsl:choose> <!-- map abbreviated parts-of-speech to their spelled-out versions -->
    <xsl:when test="$pos = 'adj'">Adjective</xsl:when>
    <xsl:when test="$pos = 'adv'">Adverb</xsl:when>
    <xsl:when test="$pos = 'art'">Article</xsl:when>
    <xsl:when test="$pos = 'conj'">Conjunction</xsl:when>
    <xsl:when test="$pos = 'fp'">Filled-Pause</xsl:when>
    <xsl:when test="$pos = 'n'">Noun</xsl:when>
    <xsl:when test="$pos = 'prep'">Preposition</xsl:when>
    <xsl:when test="$pos = 'pro'">Pronoun</xsl:when>
    <xsl:when test="$pos = 'punc'">Punctuation</xsl:when>
    <xsl:when test="$pos = 'quan'">Quantifier</xsl:when>
    <xsl:when test="$pos = 'v'">Verb</xsl:when>
    <xsl:otherwise><xsl:value-of select="$pos" /></xsl:otherwise>
   </xsl:choose> Classes:<br />
    <dl style="padding-left: 2.0em">
     <xsl:for-each select="MORPH[@cat='nom' and @from]">
      <dt>Nominalization of
       <a href="./W::{@from}.xml"><xsl:value-of select="@from" /></a>
      </dt>
     </xsl:for-each>
     <xsl:for-each select="CLASS">
      <dt>
       <a href="../cgi/browseontology-ajax?search={@onttype}#highlight" style="color: #7f0000">ONT::<xsl:value-of select="@onttype" /></a>
       (<a href="javascript:toggleVisible('{$pos}-{@onttype}-synset')" id="{$pos}-{@onttype}-synset-link">show synset</a>)
       <span style="display: none" id="{$pos}-{@onttype}-synset"><xsl:value-of select="@words" /></span>
       (<a href="javascript:toggleVisible('{$pos}-{@onttype}-ancestors')" id="{$pos}-{@onttype}-ancestors-link">show ancestors</a>)
       <span style="display: none" id="{$pos}-{@onttype}-ancestors"><xsl:value-of select="@ancestors" /></span>
      </dt>
      <dd>
       Frames:
       <dl style="padding-left: 2.0em">
        <xsl:for-each select="FRAME">
	 <dt style="color: #007f00"><xsl:value-of select="@desc" /></dt>
	 <!-- nilled is not supported by IE or Firefox, only Safari -->
	 <!-- xsl:if test="not(nilled(@example))" -->
	 <xsl:if test="@example">
	  <dd>Example: <i><xsl:value-of select="@example" /></i></dd>
	 </xsl:if>
	</xsl:for-each>
       </dl>
      </dd>
     </xsl:for-each>
    </dl>
   </li>
  </xsl:for-each>
 </ul>
 <p>Data file last modified: <xsl:value-of select="@modified" /></p>
 </body>
 </html>
</xsl:template>

</xsl:stylesheet>
