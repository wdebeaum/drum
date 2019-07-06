<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns="http://www.w3.org/1999/xhtml"
		>

 <xsl:template match="sense">
  <xsl:if test="parent::synset and position() > 1">, </xsl:if>
  <xsl:variable name="link-text">
   <xsl:choose>
    <xsl:when test=".//text()">
     <xsl:value-of select="." />
    </xsl:when>
    <xsl:otherwise>
     <xsl:value-of select="@lemma" />
    </xsl:otherwise>
   </xsl:choose>
  </xsl:variable>
  <xsl:choose>
   <xsl:when test="translate(@lemma,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz') = /word/@lemma">
    <span class="sense selected" id="{@sense_key}">
     <span class="lemma"><xsl:value-of select="$link-text" /></span>
     <span class="sense_key"><xsl:value-of select="@sense_key" /></span>
    </span>
   </xsl:when>
   <xsl:otherwise>
    <a class="sense" id="{@sense_key}" href="get-word-xml.pl?search={@sense_key}&amp;show_sense_keys={/*/@show_sense_keys}&amp;lang={/*/@lang}">
     <span class="lemma"><xsl:value-of select="$link-text" /></span>
     <span class="sense_key"><xsl:value-of select="@sense_key" /></span>
    </a>
   </xsl:otherwise>
  </xsl:choose>
 </xsl:template>

 <xsl:template match="definition">
  <xsl:if test="position() > 1">; </xsl:if>
  <span class="definition">
   <xsl:apply-templates />
  </span>
 </xsl:template>
 
 <xsl:template match="example">
  <xsl:if test="position() > 1">; </xsl:if>
  <span class="example">
   <xsl:text>"</xsl:text><xsl:apply-templates /><xsl:text>"</xsl:text>
  </span>
 </xsl:template>
  
 <xsl:template match="pointers">
  <li class="pointer_type">
   <a class="pointer_type" onclick="toggleSynsets(this)">
    <xsl:value-of select="@type" />
   </a>
   <ul class="synsets">
    <xsl:apply-templates />
   </ul>
  </li>
 </xsl:template>

 <xsl:template match="synset">
  <li id="{@ss_type}{@synset_offset}{@lang}">
   <xsl:attribute name="class">
    <xsl:text>synset</xsl:text>
    <xsl:if test="sense/@sense_key = /word/@selected_sense_key">
     <xsl:text> selected</xsl:text>
    </xsl:if>
   </xsl:attribute>
   <xsl:if test="*">
    <a onclick="togglePointers(this)">S:</a>
    <xsl:text> </xsl:text>
    <span class="ss_type">(<xsl:value-of select="@ss_type" />)</span>
    <xsl:text> </xsl:text>
    <span class="senses"><xsl:apply-templates select="sense" /></span>
    <xsl:text> </xsl:text>
    <span class="definitions"><xsl:text>(</xsl:text>
     <xsl:apply-templates select="definition" />
    <xsl:text>)</xsl:text></span>
    <xsl:text> </xsl:text>
    <span class="examples">
     <xsl:apply-templates select="example" />
    </span>
    <span class="english_senses" />
    <ul class="pointers">
     <xsl:apply-templates select="pointers" />
     <xsl:if test="frame">
      <li class="frames">
       <a class="frames" onclick="togglePointers(this)">Sentence frame</a>
       <ul class="frames">
        <xsl:for-each select="frame">
	 <li class="frame"><xsl:apply-templates /></li>
	</xsl:for-each>
       </ul>
      </li>
     </xsl:if>
    </ul>
   </xsl:if>
  </li>
 </xsl:template>

 <xsl:template match="noun|verb|adjective|adverb">
  <h2 class="pos"><xsl:value-of select="local-name()" /></h2>
  <ol class="synsets">
   <xsl:apply-templates />
  </ol>
 </xsl:template>

 <xsl:template match="/word">
  <html>
   <head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    <link href="wn.css" rel="stylesheet" type="text/css" />
    <title><xsl:value-of select="@lemma" /> - WordNet</title>
    <style type="text/css">
     .sense_key { display: none }
     .lemma { }
    </style>
    <script type="text/javascript" src="wn.js"></script>
   </head>
   <body onload="bodyOnLoad()">
    <h1>WordNet 3.0 Search</h1>
    <form action="get-word-xml.pl" method="GET">
     <label>Language: <select name="lang">
      <option value="en">
       <xsl:if test="@lang='en' or not(@lang)">
        <xsl:attribute name="selected">selected</xsl:attribute>
       </xsl:if>
       <xsl:text>English</xsl:text>
      </option>
      <option value="es">
       <xsl:if test="@lang='es'">
        <xsl:attribute name="selected">selected</xsl:attribute>
       </xsl:if>
       <xsl:text>Spanish</xsl:text>
      </option>
     </select></label>
     <xsl:text> </xsl:text>
     <label>Word to search for: <input type="text" name="search" /></label>
     <input type="submit" value="Search WordNet" />
     <label>
      <input type="checkbox" name="show_sense_keys" id="show_sense_keys" value="1" onclick="setSenseKeyVisibility()">
       <xsl:if test="@show_sense_keys = '1'">
        <xsl:attribute name="checked">checked</xsl:attribute>
       </xsl:if>
      </input>
      <xsl:text> Show sense keys</xsl:text>
     </label>
    </form>
    <xsl:apply-templates />
   <hr />
   <p>
    <a href="http://wordnet.princeton.edu/">WordNet 3.0</a>
    <xsl:text> </xsl:text>
    <a href="http://wordnet.princeton.edu/wordnet/license/">Copyright 2006</a>
    by <a href="http://www.princeton.edu/">Princeton University</a>.
    See also the official (slow)
    <a href="http://wordnetweb.princeton.edu/perl/webwn">WordNet 3.0 Search</a>.
   </p>
   </body>
  </html>
 </xsl:template>

</xsl:stylesheet>
