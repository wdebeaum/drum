<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:str="http://exslt.org/strings"
  extension-element-prefixes="str">
<!-- ontonotes-inventory-to-html.xsl - convert OntoNodes 3.0 sense-inventories/*.xml files to HTML files like those linked from the Unified Verb Index (but do it for nouns too) -->

<xsl:output method="html" />

<xsl:template match="@*|text()" />

<xsl:template match="/inventory">
 <html>
 <head><title><xsl:value-of select="@lemma" /></title></head>
 <body>
 <h1><xsl:value-of select="@lemma" />; <xsl:value-of select="count(sense)" /> Senses</h1>
 <xsl:if test="substring(@lemma, string-length(@lemma), 1) = 'v'">
  <a href="http://verbs.colorado.edu/html_groupings/{@lemma}.html">See also the UVI version of this page.</a><br />
 </xsl:if>
 <xsl:apply-templates />
 </body></html>
</xsl:template>

<xsl:template match="commentary">
 <xsl:if test="string-length(normalize-space(.)) &gt; 0">
  <p>Commentary:<font color="33066">
   <xsl:for-each select="str:tokenize(., '&#10;')">
    <xsl:if test="string-length(.) &gt; 0">
     <xsl:value-of select="." /><br />
    </xsl:if>
   </xsl:for-each>
  </font></p>
 </xsl:if>
</xsl:template>

<xsl:template match="sense">
 <font color="8B0000">
 <h3>Sense Number <xsl:value-of select="@n" />: <xsl:value-of select="@name" /></h3>
 </font>
 <xsl:apply-templates />
</xsl:template>

<xsl:template match="examples">
 <p><u>Examples:</u><br />
  <xsl:for-each select="str:tokenize(., '&#10;')">
   <xsl:if test="string-length(.) &gt; 0">
    <xsl:value-of select="." /><br />
   </xsl:if>
  </xsl:for-each>
 </p>
</xsl:template>

<xsl:template match="mappings">
 <p><u>Mappings:</u><br />
  <font color="008000">
   <xsl:apply-templates select="vn" />
   <xsl:apply-templates select="fn" />
   <xsl:apply-templates select="pb" />
  </font>
  <xsl:apply-templates select="wn[not(@lemma)]" />
  <xsl:if test="wn[@lemma and not(@lemma='')]">
   <font color="0066CC">WordNet Verb Particle Constructions, Multiword Expressions:<br />
    <xsl:apply-templates select="wn[@lemma and not(@lemma='')]" />
   </font>
  </xsl:if>
 </p>
</xsl:template>

<!--template match="gr_sense">
 This is sometimes present, but always empty, so I don't know what it means, and it doesn't show up on UVI either.
</template -->

<xsl:template match="wn">
 <xsl:if test="string-length(.) &gt; 0">
  <font color="000066">"WordNet" Sense Numbers (version = <xsl:value-of select="@version" />): <xsl:value-of select="." /></font><br />
 </xsl:if>
</xsl:template>

<xsl:template match="wn[@lemma]">
 ("WordNet" version = <xsl:value-of select="@version" />): <xsl:value-of select="@lemma" /><xsl:text> </xsl:text><xsl:value-of select="." /><br />
</xsl:template>

<!--template match="omega">
 This is always present, but always empty, so it's meaningless.
</template -->

<xsl:template match="vn | fn | pb">
 <xsl:if test="string-length(.) &gt; 0">
  <xsl:choose>
   <xsl:when test="local-name() = 'vn'">VerbNet: </xsl:when>
   <xsl:when test="local-name() = 'fn'">FrameNet: </xsl:when>
   <xsl:when test="local-name() = 'pb'">PropBank: </xsl:when>
  </xsl:choose>
  <xsl:value-of select="." /><br />
 </xsl:if>
</xsl:template>

<!-- template match="relations">
 There are only two relations in the entire corpus, not worth it
</template -->

<!-- Not included: SENSE_META, WORD_META, ita -->

</xsl:stylesheet>
