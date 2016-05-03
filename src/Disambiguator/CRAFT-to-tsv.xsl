<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:str="http://exslt.org/strings"
  xmlns:mention="http:///edu/ucdenver/ccp/nlp/core/uima/mention.ecore"
  xmlns:cas="http:///uima/cas.ecore"
  xmlns:xmi="http://www.omg.org/XMI"
  xmlns:annotation="http:///edu/ucdenver/ccp/nlp/core/uima/annotation.ecore"
  xmlns:tcas="http:///uima/tcas.ecore"
  xmlns:type2="http:///org/cleartk/token/type.ecore"
  xmlns:type="http:///org/cleartk/syntax/constituent/type.ecore"
  xmlns:metadata="http:///edu/ucdenver/ccp/nlp/core/uima/annotation/metadata.ecore"
  >
<!--
CRAFT-to-tsv.xsl - convert UIMA XMI CCP files from CRAFT-1.0 corpus to a TSV file with info on the text, POS tags, sense tags, and sentences
William de Beaumont
2015-01-30
-->
<xsl:output method="text" />

<xsl:template match="@*|text()" />

<xsl:variable name="text" select="//cas:Sofa[@sofaID='_InitialView']/@sofaString" />

<xsl:template name="frame">
 <xsl:text>	</xsl:text>
 <xsl:value-of select="@begin" />
 <xsl:text>	</xsl:text>
 <xsl:value-of select="@end" />
</xsl:template>

<xsl:template match="cas:Sofa[@sofaID='_InitialView']">
 <xsl:text>_BEGIN_SOFA_
</xsl:text>
 <xsl:value-of select="@sofaString" />
 <xsl:text>
_END_SOFA_
</xsl:text>
</xsl:template>

<xsl:template match="type:TerminalTreebankNode">
 <xsl:if test="@begin != @end">
  <xsl:text>pos</xsl:text>
  <xsl:call-template name="frame" />
  <xsl:text>	</xsl:text>
  <xsl:value-of select="@nodeType" />
  <xsl:text>
</xsl:text>
 </xsl:if>
</xsl:template>

<xsl:template match="type2:Sentence">
 <xsl:text>sentence</xsl:text>
 <xsl:call-template name="frame" />
 <xsl:text>
</xsl:text>
</xsl:template>

<xsl:template match="annotation:CCPTextAnnotation[@classMention and @numberOfSpans = 1]">
 <xsl:variable name="cm" select="@classMention" />
 <!-- FIXME should be able to use xsl:key or id here, but nothing I've tried works -->
 <xsl:variable name="m" select="//mention:CCPClassMention[@xmi:id=$cm]" />
 <xsl:variable name="mn" select="$m/@mentionName" />
 <xsl:choose>
  <xsl:when test="$mn = 'Entrez Gene sequence'">
   <xsl:variable name="sm" select="$m/@slotMentions" />
   <xsl:variable name="sv" select="//mention:CCPIntegerSlotMention[@xmi:id=$sm]/@slotValues" />
   <xsl:text>sense</xsl:text>
   <xsl:call-template name="frame" />
   <xsl:text>	EntrezGene::</xsl:text>
   <xsl:value-of select="$sv" />
   <xsl:text>
</xsl:text>
  </xsl:when>
  <xsl:when test="not(contains($mn, ':'))">
   <!-- do nothing -->
  </xsl:when>
  <xsl:otherwise>
   <xsl:text>sense</xsl:text>
   <xsl:call-template name="frame" />
   <xsl:text>	</xsl:text>
   <xsl:choose>
    <xsl:when test="contains($mn, ':')">
     <xsl:value-of select="substring-before($mn, ':')" />
     <xsl:text>::</xsl:text>
     <xsl:value-of select="substring-after($mn, ':')" />
    </xsl:when>
    <xsl:otherwise>
     <xsl:value-of select="$mn" />
    </xsl:otherwise>
   </xsl:choose>
   <xsl:text>
</xsl:text>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

</xsl:stylesheet>
