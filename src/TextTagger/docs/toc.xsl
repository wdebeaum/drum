<?xml version="1.0" encoding="iso-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xhtml="http://www.w3.org/1999/xhtml" xmlns="http://www.w3.org/1999/xhtml" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" exclude-result-prefixes="xhtml xsl">
<xsl:output method="xml" version="1.0" encoding="iso-8859-1" doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN" doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd" media-type="text/xml" indent="yes"/>
  <!-- Adds the title from the header and the table of contents at the
      start of the document. -->
  <xsl:template match="xhtml:body">
    <body>
    <h1><xsl:value-of select="/xhtml:html/xhtml:head/xhtml:title"/></h1>
    <hr/>
    <xsl:if test="//xhtml:div[@class='section']">
      <h2>Table of contents</h2>
      <ul style="list-style-type: none">
        <xsl:apply-templates mode="toc"/>
      </ul>
      <hr/>
    </xsl:if>
    <xsl:apply-templates/>
    </body>
  </xsl:template>
  <!-- Evaluates to the number of a particular section, relative
      to its siblings. -->
  <xsl:template match="xhtml:div[@class='section']" mode="number">
    <xsl:value-of select="count(preceding-sibling::xhtml:div[@class='section']) + 1"/>
    <xsl:text>.</xsl:text>
  </xsl:template>
  <!-- Adds a section entry to the table of contents. -->
  <xsl:template match="xhtml:div[@class='section']" mode="toc">
    <xsl:variable name="number">
      <xsl:apply-templates select="ancestor-or-self::xhtml:div[@class='section']" mode="number"/>
    </xsl:variable>
    <li>
      <a href="#sec-{$number}">
        <xsl:value-of select="$number"/>
        <xsl:text> </xsl:text>
        <xsl:value-of select="xhtml:h1|xhtml:h2|xhtml:h3|xhtml:h4|xhtml:h5|xhtml:h6"/>
      </a>
      <xsl:if test="xhtml:div[@class='section']">
        <ul style="list-style-type: none">
          <xsl:apply-templates mode="toc"/>
        </ul>
      </xsl:if>
    </li>
  </xsl:template>
  <!-- Adds an "id" attribute to a section based on its number. -->
  <xsl:template match="xhtml:div[@class='section']">
    <xsl:variable name="number">
      <xsl:apply-templates select="ancestor-or-self::xhtml:div[@class='section']" mode="number"/>
    </xsl:variable>
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:attribute name="id">
        <xsl:text>sec-</xsl:text><xsl:value-of select="$number"/>
      </xsl:attribute>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>
  <!-- Adds its number to a section title. -->
  <xsl:template match="xhtml:h1|xhtml:h2|xhtml:h3|xhtml:h4|xhtml:h5|xhtml:h6">
    <xsl:variable name="number">
      <xsl:apply-templates select="ancestor-or-self::xhtml:div[@class='section']" mode="number"/>
    </xsl:variable>
    <xsl:copy>
      <xsl:value-of select="$number"/>
      <xsl:text> </xsl:text>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>
  <!-- Identity rule for the default mode. -->
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>
  <!-- Identity rule for the "toc" mode. -->
  <xsl:template match="@*|node()" mode="toc">
    <xsl:apply-templates select="@*|node()" mode="toc"/>
  </xsl:template>
</xsl:stylesheet>
