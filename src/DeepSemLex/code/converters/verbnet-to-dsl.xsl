<?xml version="1.0"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- verbnet-to-dsl.xsl - convert VerbNet 3.2b XML files to DeepSemLex Lisp data files -->

<xsl:output method="text" />

<xsl:template match="@*|text()" />

<!-- emit a newline and an appropriate level of indentation -->
<xsl:template name="nl-indent">
 <xsl:text>
</xsl:text>
 <xsl:for-each select="ancestor::VNCLASS | ancestor::VNSUBCLASS | ancestor::THEMROLES | ancestor::FRAMES[count(FRAME) > 1] | ancestor::FRAME | ancestor::SYNTAX | ancestor::SEMANTICS">
  <xsl:text>  </xsl:text>
 </xsl:for-each>
</xsl:template>

<xsl:template name="concept">
 <xsl:call-template name="nl-indent" />
 <xsl:text>(concept VN::</xsl:text>
 <xsl:value-of select="@ID" />
 <xsl:call-template name="nl-indent" />
 <xsl:text>  (aliases VN::_</xsl:text>
 <xsl:value-of select="substring-after(@ID, '-')" />
 <xsl:text>)</xsl:text>
 <xsl:apply-templates />
 <xsl:call-template name="nl-indent" />
 <xsl:text>  )</xsl:text>
</xsl:template>

<xsl:template match="/VNCLASS">
 <xsl:text>;;; AUTOMATICALLY GENERATED
(provenance VerbNet (version "3.2b") (filename "</xsl:text>
 <xsl:value-of select="@ID" />
 <xsl:text>.xml"))
(pos V)</xsl:text>
 <xsl:call-template name="concept" />
 <xsl:text>

</xsl:text>
</xsl:template>

<xsl:template match="VNSUBCLASS">
 <xsl:call-template name="concept" />
</xsl:template>

<xsl:template match="MEMBER">
 <xsl:call-template name="nl-indent" />
 <xsl:text>(sense (word </xsl:text>
 <xsl:choose>
  <xsl:when test="contains(@name, '_') or contains(@name, ' ')">
   <xsl:text>(</xsl:text>
   <xsl:value-of select="replace(@name,'_',' ')"/>
   <!-- FIXME detect prepositions and make them particles? -->
   <xsl:text>)</xsl:text>
  </xsl:when>
  <xsl:otherwise>
   <xsl:value-of select="@name"/>
  </xsl:otherwise>
 </xsl:choose>
 <xsl:text>)</xsl:text>
 <xsl:choose>
  <xsl:when test="@wn != '' or @grouping != ''">
   <xsl:call-template name="nl-indent" />
   <xsl:text>  (overlap</xsl:text>
   <xsl:if test="@wn != ''">
    <!-- TODO do I need to tokenize here too, or is it just for ON? -->
    <xsl:text> WN::|</xsl:text>
    <xsl:value-of select="replace(replace(@wn, '\?', ''), '\s+', '::| WN::|')" />
    <xsl:text>::|</xsl:text>
   </xsl:if>
   <xsl:if test="@grouping != ''">
    <xsl:variable name="name" select="@name" />
    <xsl:for-each select="tokenize(@grouping, ' ')">
     <xsl:text> ON::</xsl:text>
     <xsl:value-of select="$name" />
     <xsl:text>.v.</xsl:text>
     <xsl:value-of select="replace(substring-after(., '.'), '^0', '')" />
    </xsl:for-each>
   </xsl:if>
   <xsl:text>)</xsl:text>
   <xsl:call-template name="nl-indent" />
   <xsl:text>  )</xsl:text>
  </xsl:when>
  <xsl:otherwise>
   <xsl:text>)</xsl:text>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>
 
<xsl:template match="THEMROLES[THEMROLE]">
 <xsl:call-template name="nl-indent" />
 <xsl:text>(sem-frame</xsl:text>
 <xsl:apply-templates />
 <xsl:call-template name="nl-indent" />
 <xsl:text>  )</xsl:text>
</xsl:template>

<xsl:template match="THEMROLE">
 <xsl:variable name="type" select="@type" />
 <xsl:call-template name="nl-indent" />
 <xsl:text>(VN::</xsl:text>
 <xsl:value-of select="$type" />
 <xsl:apply-templates />
 <xsl:choose>
  <xsl:when test="../../FRAMES/FRAME/SYNTAX[not(child::node()[@value=$type])]">
   <xsl:text> optional</xsl:text>
  </xsl:when>
 </xsl:choose>
 <xsl:text>)</xsl:text>
 <xsl:if test="../../FRAMES/FRAME and not(../../FRAMES/FRAME/SYNTAX/*[@value=$type] | ../../FRAMES/FRAME/SEMANTICS[@value=$type or @value=concat('?',$type)])">
  <xsl:text> ; unused in this class; may be used in subclasses</xsl:text>
 </xsl:if>
</xsl:template>

<xsl:template match="SELRESTRS">
 <xsl:choose>
  <xsl:when test="@logic='or'">
   <xsl:text> (or</xsl:text>
   <xsl:for-each select="SELRESTR | SELRESTRS">
    <xsl:if test="local-name()='SELRESTR'">
     <xsl:text> (sem-feats</xsl:text>
    </xsl:if>
    <xsl:apply-templates select="." />
    <xsl:if test="local-name()='SELRESTR'">
     <xsl:text>)</xsl:text>
    </xsl:if>
   </xsl:for-each>
   <xsl:text>)</xsl:text>
  </xsl:when>
  <xsl:when test="SELRESTR">
   <xsl:text> (sem-feats</xsl:text>
   <xsl:apply-templates />
   <xsl:text>)</xsl:text>
  </xsl:when>
  <xsl:otherwise>
   <xsl:text> t</xsl:text>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<!-- TODO translate these to be more TRIPS-like? -->
<xsl:template match="SELRESTR">
 <xsl:text> (VN::</xsl:text>
 <xsl:value-of select="@type" />
 <xsl:text> </xsl:text>
 <xsl:value-of select="@Value" />
 <xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="FRAMES">
 <xsl:if test="count(FRAME) > 1">
  <xsl:call-template name="nl-indent" />
  <xsl:text>(or ; VN frames</xsl:text>
 </xsl:if>
 <xsl:apply-templates />
 <xsl:if test="count(FRAME) > 1">
  <xsl:call-template name="nl-indent" />
  <xsl:text>  )</xsl:text>
 </xsl:if>
</xsl:template>

<xsl:template match="FRAME">
 <xsl:call-template name="nl-indent" />
 <xsl:text>(concept</xsl:text>
 <!-- xsl:text> VN::</xsl:text>
 <xsl:value-of select="../../@ID" />
 <xsl:text>-f</xsl:text>
 <xsl:value-of select="position()" / -->
 <xsl:apply-templates />
 <xsl:call-template name="nl-indent" />
 <xsl:text>  )</xsl:text>
</xsl:template>

<xsl:template match="EXAMPLE">
 <xsl:call-template name="nl-indent" />
 <xsl:text>(example (text "</xsl:text>
 <xsl:value-of select="." />
 <xsl:text>"))</xsl:text>
</xsl:template>

<xsl:template match="SYNTAX">
 <xsl:call-template name="nl-indent" />
 <xsl:text>(syn-sem</xsl:text>
 <xsl:apply-templates select="NP" />
 <xsl:call-template name="nl-indent" />
 <xsl:text>  )</xsl:text>
</xsl:template>

<xsl:template match="SEMANTICS">
 <xsl:call-template name="nl-indent" />
 <xsl:text>(entailments</xsl:text>
 <xsl:apply-templates />
 <xsl:call-template name="nl-indent" />
 <xsl:text>  )</xsl:text>
</xsl:template>

<xsl:template match="PRED">
 <xsl:call-template name="nl-indent" />
 <xsl:text>"</xsl:text>
 <xsl:value-of select="../../../../@ID" />
 <!-- TODO add args to lhs of rule like class_name(Role : ThemRole, Role : ThemRole,...) -->
 <xsl:text> => </xsl:text>
 <xsl:if test="@bool='!'"><xsl:text>not(</xsl:text></xsl:if>
 <xsl:value-of select="@value" />
 <xsl:text>(</xsl:text>
 <xsl:apply-templates />
 <xsl:text>)</xsl:text>
 <xsl:if test="@bool='!'"><xsl:text>)</xsl:text></xsl:if>
 <xsl:text>"</xsl:text>
</xsl:template>

<xsl:template match="ARG">
 <xsl:if test="position() > 1"><xsl:text>, </xsl:text></xsl:if>
 <xsl:value-of select="@value" />
 <xsl:text> : </xsl:text>
 <xsl:value-of select="@type" />
</xsl:template>

<!-- TODO SYNRESTRs, esp. those that identify an "NP" as really a to-infinitive complement or something -->

<xsl:template match="NP">
 <xsl:call-template name="nl-indent" />
 <xsl:choose>
  <xsl:when test="not(preceding-sibling::node())">
   <xsl:text>(lsubj NP </xsl:text>
  </xsl:when>
  <xsl:when test="local-name(preceding-sibling::node()[1])='PREP'">
   <xsl:text>(lcomp </xsl:text>
   <xsl:choose>
    <xsl:when test="not(preceding-sibling::PREP[1]/@value)">
     <xsl:text>PP</xsl:text>
     <!-- TODO SELRESTRS? -->
    </xsl:when>
    <xsl:when test="contains(preceding-sibling::PREP[1]/@value, ' ')">
     <xsl:text>(PP (or </xsl:text>
     <xsl:value-of select="preceding-sibling::PREP[1]/@value" />
     <xsl:text>))</xsl:text>
    </xsl:when>
    <xsl:otherwise>
     <xsl:text>(PP </xsl:text>
     <xsl:value-of select="preceding-sibling::PREP[1]/@value" />
     <xsl:text>)</xsl:text>
    </xsl:otherwise>
   </xsl:choose>
   <xsl:text> </xsl:text>
  </xsl:when>
  <xsl:when test="local-name(preceding-sibling::node()[1])='VERB' and
                  local-name(following-sibling::node()[1])='NP'">
   <xsl:text>(liobj NP </xsl:text>
  </xsl:when>
  <xsl:otherwise>
   <xsl:text>(lobj NP </xsl:text>
  </xsl:otherwise>
 </xsl:choose>
 <xsl:text>VN::</xsl:text>
 <xsl:value-of select="@value" />
 <xsl:text>)</xsl:text>
</xsl:template>

<!-- TODO ADJ ADV LEX -->

<!-- TODO convert SEMANTICS/PRED/ARGS/ARG to LF terms? -->

</xsl:stylesheet>
