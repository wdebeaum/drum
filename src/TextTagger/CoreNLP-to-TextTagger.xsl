<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:set="http://exslt.org/sets"
		xmlns:str="http://exslt.org/strings">
<!--
CoreNLP-to-TextTagger.xsl - convert XML output of Stanford CoreNLP to native-format TextTagger tags
William de Beaumont
2015-05-11
-->
<xsl:output method="text" />

<xsl:template match="@*|text()" />

<xsl:template name="escape-for-quotes">
 <xsl:param name="str" />
 <xsl:for-each select="str:tokenize($str, '&quot;')">
  <xsl:for-each select="str:tokenize(., '\')">
   <xsl:value-of select="." />
   <xsl:if test="position() != last()">
    <xsl:text>\\</xsl:text>
   </xsl:if>
  </xsl:for-each>
  <xsl:if test="position() != last()">
   <xsl:text>\"</xsl:text>
  </xsl:if>
 </xsl:for-each>
</xsl:template>

<xsl:template name="tokens-to-string">
 <xsl:param name="tokens" />
 <xsl:text>"</xsl:text>
 <xsl:for-each select="$tokens">
  <xsl:call-template name="escape-for-quotes">
   <xsl:with-param name="str" select="word" />
  </xsl:call-template>
  <xsl:if test="position() != last()">
   <xsl:value-of select="str:padding(following-sibling::token/CharacterOffsetBegin - CharacterOffsetEnd)" />
  </xsl:if>
 </xsl:for-each>
 <xsl:text>"</xsl:text>
</xsl:template>

<xsl:template match="sentences/sentence">
 <xsl:text>(sentence :text </xsl:text>
 <xsl:call-template name="tokens-to-string">
  <xsl:with-param name="tokens" select="tokens/token" />
 </xsl:call-template>
 <xsl:text> :start </xsl:text>
 <xsl:value-of select="tokens/token[1]/CharacterOffsetBegin" />
 <xsl:text> :end </xsl:text>
 <xsl:value-of select="tokens/token[last()]/CharacterOffsetEnd" />
 <xsl:text>)
</xsl:text>
 <xsl:apply-templates />
</xsl:template>

<xsl:template match="token">
 <xsl:variable name="common">
  <xsl:text> :lex "</xsl:text>
  <xsl:call-template name="escape-for-quotes">
   <xsl:with-param name="str" select="word" />
  </xsl:call-template>
  <xsl:text>" :lemma "</xsl:text>
  <xsl:call-template name="escape-for-quotes">
   <xsl:with-param name="str" select="lemma" />
  </xsl:call-template>
  <xsl:text>" :start </xsl:text>
  <xsl:value-of select="CharacterOffsetBegin" />
  <xsl:text> :end </xsl:text>
  <xsl:value-of select="CharacterOffsetEnd" />
 </xsl:variable>

 <xsl:choose>
  <xsl:when test="POS='.' or POS=',' or POS=':' or POS='``' or POS=&quot;''&quot;">

   <xsl:text>(punctuation</xsl:text>
   <xsl:value-of select="$common" />
   <xsl:text>)
</xsl:text>

  </xsl:when>
  <xsl:otherwise>

   <xsl:text>(word</xsl:text>
   <xsl:value-of select="$common" />
   <xsl:text>)
</xsl:text>

   <xsl:text>(pos</xsl:text>
   <xsl:value-of select="$common" />
   <xsl:text> :penn-pos (</xsl:text>
   <xsl:value-of select="POS" />
   <xsl:text>))
</xsl:text>

  </xsl:otherwise>
 </xsl:choose>

 <xsl:if test="NER!='O' and preceding-sibling::token[1]/NER!=NER">
  <xsl:variable name="ne-class" select="NER" />
  <xsl:variable name="following-token" select="following-sibling::token[NER!=$ne-class][1]" />
  <xsl:variable name="ne-tokens" select=". | set:intersection(following-sibling::token, $following-token/preceding-sibling::token)" />
  <xsl:text>(named-entity :lex </xsl:text>
  <xsl:call-template name="tokens-to-string">
   <xsl:with-param name="tokens" select="$ne-tokens" />
  </xsl:call-template>
  <xsl:text> :start </xsl:text>
  <xsl:value-of select="CharacterOffsetBegin" />
  <xsl:text> :end </xsl:text>
  <xsl:value-of select="$ne-tokens[last()]/CharacterOffsetEnd" />
  <xsl:text> :stanford-ner-class </xsl:text>
  <xsl:value-of select="$ne-class" />
  <xsl:text>)
</xsl:text>
 </xsl:if>
</xsl:template>

<xsl:template match="parse">
 <xsl:variable name="tokens" select="preceding-sibling::tokens[1]/token" />
 <xsl:text>(parse :text </xsl:text>
 <xsl:call-template name="tokens-to-string">
  <xsl:with-param name="tokens" select="$tokens" />
 </xsl:call-template>
 <xsl:text> :start </xsl:text>
 <xsl:value-of select="$tokens[1]/CharacterOffsetBegin" />
 <xsl:text> :end </xsl:text>
 <xsl:value-of select="$tokens[last()]/CharacterOffsetEnd" />
 <!-- putting this in a string because I'm not entirely sure it's valid KQML -->
 <xsl:text> :penn-parse-tree "</xsl:text>
 <xsl:call-template name="escape-for-quotes">
  <xsl:with-param name="str" select="." />
 </xsl:call-template>
 <xsl:text>")
</xsl:text>
</xsl:template>

</xsl:stylesheet>
