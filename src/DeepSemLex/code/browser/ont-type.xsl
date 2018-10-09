<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:exsl="http://exslt.org/common"
  xmlns:str="http://exslt.org/strings"
  extension-element-prefixes="str exsl">

<xsl:import href="str.tokenize.template.xsl" />

<xsl:output method="html" />

<xsl:key name="concept-name" match="/dsl/concept" use="@name" />

<xsl:template name="inherited">
 <xsl:param name="part" />
 <xsl:variable name="tokens">
  <xsl:call-template name="str:tokenize">
   <xsl:with-param name="string" select="normalize-space(relation[@label='inherit'])" />
   <xsl:with-param name="delimiters" select="' '" />
  </xsl:call-template>
 </xsl:variable>
 <xsl:for-each select="key('concept-name',exsl:node-set($tokens)/token)">
  <xsl:variable name="part-nodes" select="*[local-name()=$part]" />
  <xsl:if test="$part-nodes">
   <h3>From <xsl:apply-templates select="." mode="ref" /></h3>
   <xsl:apply-templates select="$part-nodes" />
  </xsl:if>
  <xsl:call-template name="inherited">
   <xsl:with-param name="part" select="$part" />
  </xsl:call-template>
 </xsl:for-each>
</xsl:template>

<xsl:template match="/dsl">
 <html>
  <head>
   <title><xsl:value-of select="concept[1]/@name" /></title>
   <script type="text/javascript">
    function go() {
      window.location.href =
        encodeURIComponent(
	  'ONT::' + document.getElementById('type_name').value + '.xml')
    }
   </script>
  </head>
  <body>
   <form action="javascript:go()">
    <label>ONT::<input type="text" id="type_name" /></label>
    <input type="submit" value="Go" />
   </form>
   <xsl:apply-templates select="concept[1]" mode="def" />
  </body>
 </html>
</xsl:template>

<xsl:template match="concept" mode="def">
 <xsl:variable name="name" select="@name" />
 <h1><xsl:value-of select="@name" /></h1>
 <xsl:if test="comment">
  <p>Comment: <xsl:value-of select="comment" /></p>
 </xsl:if>
 <h2>Relations</h2>
 <xsl:apply-templates select="relation" />
 <h2>Semantic Features</h2>
 <xsl:apply-templates select="sem-feats" />
 <xsl:call-template name="inherited">
  <xsl:with-param name="part" select="'sem-feats'" />
 </xsl:call-template>
 <h2>Semantic Frame</h2>
 <xsl:apply-templates select="sem-frame" />
 <xsl:call-template name="inherited">
  <xsl:with-param name="part" select="'sem-frame'" />
 </xsl:call-template>
 <h2>Children</h2>
 <ul>
  <xsl:for-each select="/dsl/concept[contains(relation[@label='inherit'], $name)]">
   <li><xsl:apply-templates select="." mode="ref" /></li>
  </xsl:for-each>
 </ul>
 <h2>Senses</h2>
 <xsl:apply-templates select="/dsl/sense" />
</xsl:template>

<xsl:template match="concept" mode="ref">
 <xsl:choose>
  <xsl:when test="starts-with(@name,'ont::')">
   <a href="ONT%3A%3A{substring(@name,6)}.xml">
    <xsl:value-of select="@name" />
   </a>
  </xsl:when>
  <xsl:when test="starts-with(@name,'wn::')">
   <xsl:variable name="sk" select="substring(@name,6,string-length(@name) - 6)" />
   <xsl:variable name="sk2">
    <xsl:variable name="tokens">
     <xsl:call-template name="str:tokenize">
      <xsl:with-param name="string" select="$sk" />
      <xsl:with-param name="delimiters" select="':'" />
     </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="token-count" select="count(exsl:node-set($tokens)/token)" />
    <xsl:for-each select="exsl:node-set($tokens)/token">
     <xsl:if test="position() != 1">%3A</xsl:if>
     <xsl:value-of select="." />
    </xsl:for-each>
    <xsl:if test="4 > $token-count"><xsl:text>%3A</xsl:text></xsl:if>
    <xsl:if test="5 > $token-count"><xsl:text>%3A</xsl:text></xsl:if>
   </xsl:variable>
   <a href="http://trips.ihmc.us/WordNetWeb/get-word-xml.pl?lang=en&amp;search={$sk2}">
    <xsl:value-of select="@name" />
   </a>
  </xsl:when>
  <xsl:otherwise>
   <b><xsl:value-of select="@name" /></b>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

<xsl:template match="sense">
 <xsl:for-each select="morph/word">
  <h3><xsl:apply-templates select="." /></h3>
 </xsl:for-each>
 <xsl:if test="not(morph/word)">
  <h3>MISSING WORD SPEC</h3> <!-- for now -->
 </xsl:if>
 POS: <b><xsl:value-of select="morph/pos/@pos" /></b><br/>
 <xsl:if test="morph/forms">
  Forms:<br/>
  <ul>
   <xsl:for-each select="morph/forms/* | morph/forms/text()[normalize-space() != '']">
    <li>
     <xsl:choose>
      <xsl:when test="self::text()"><b><xsl:value-of select="." /></b></xsl:when>
      <xsl:otherwise>
       <xsl:value-of select="local-name()" />
       <xsl:text>: </xsl:text>
       <b><xsl:apply-templates /></b>
      </xsl:otherwise>
     </xsl:choose>
    </li>
   </xsl:for-each>
  </ul>
 </xsl:if>
 Template call: 
 <b>
  <xsl:variable name="tokens">
   <xsl:call-template name="str:tokenize">
    <xsl:with-param name="string" select="normalize-space(relation[@label='inherit'])" />
    <xsl:with-param name="delimiters" select="' '" />
   </xsl:call-template>
  </xsl:variable>
  <xsl:for-each select="exsl:node-set($tokens)/token">
   <xsl:if test="substring(., string-length(.) - 5) = '-templ'">
    (<xsl:value-of select="." />)
   </xsl:if>
  </xsl:for-each>
  <xsl:for-each select="syntax/template-call">
   <xsl:text>(</xsl:text>
   <xsl:value-of select="@template" />
   <xsl:for-each select="@*[local-name() != 'template']">
    <xsl:text> :</xsl:text>
    <xsl:value-of select="local-name()" />
    <xsl:text> </xsl:text>
    <xsl:value-of select="." />
   </xsl:for-each>
   <xsl:text>)</xsl:text>
  </xsl:for-each>
 </b>
 <br/>
 <xsl:if test="example">
  Examples:
  <ul>
   <xsl:for-each select="example">
    <li><xsl:value-of select="@text" /></li>
   </xsl:for-each>
  </ul>
 </xsl:if>
</xsl:template>

<xsl:template match="word">
 <xsl:value-of select="@first-word" />
 <xsl:if test="@remaining-words">
  <xsl:text> </xsl:text>
  <xsl:value-of select="@remaining-words" />
 </xsl:if>
 <xsl:if test="@particle">
  <xsl:text> (</xsl:text>
  <xsl:value-of select="@particle" />
  <xsl:text>)</xsl:text>
 </xsl:if>
</xsl:template>

<xsl:template match="relation">
 <h3><xsl:value-of select="@label" /></h3>
 <ul>
  <xsl:variable name="tokens">
   <xsl:call-template name="str:tokenize">
    <xsl:with-param name="string" select="normalize-space()" />
    <xsl:with-param name="delimiters" select="' '" />
   </xsl:call-template>
  </xsl:variable>
  <xsl:for-each select="exsl:node-set($tokens)/token">
   <xsl:choose>
    <xsl:when test="substring(., string-length(.) - 5) = '-templ'">
     <!-- ignore -->
    </xsl:when>
    <xsl:when test="starts-with(., 'ont::') or starts-with(., 'wn::')">
     <li>
      <xsl:variable name="concept-node">
       <concept name="{.}" />
      </xsl:variable>
      <xsl:apply-templates select="exsl:node-set($concept-node)" mode="ref" />
     </li>
    </xsl:when>
    <xsl:otherwise>
     <li><xsl:value-of select="." /></li>
    </xsl:otherwise>
   </xsl:choose>
  </xsl:for-each>
 </ul>
</xsl:template>

<xsl:template match="or">
 <xsl:for-each select="*">
  <xsl:if test="position()!=1">
   <i> or </i>
  </xsl:if> 
  <xsl:choose>
   <xsl:when test="self::concept"><xsl:apply-templates select="." mode="ref" /></xsl:when>
   <xsl:otherwise><xsl:apply-templates /></xsl:otherwise>
  </xsl:choose>
 </xsl:for-each>
</xsl:template>

<xsl:template match="sem-feats">
 <ul>
  <li>
   <xsl:text>Feature list type: </xsl:text>
   <xsl:choose>
    <xsl:when test="relation[@label='inherit']">
     <b><xsl:value-of select="relation[@label='inherit']" /></b>
    </xsl:when>
    <xsl:when test="or">
     <xsl:apply-templates select="or" />
    </xsl:when>
    <xsl:otherwise>
     <xsl:apply-templates select="concept" mode="ref" />
    </xsl:otherwise>
   </xsl:choose>
  </li>
  <xsl:apply-templates select="feat" />
 </ul>
</xsl:template>

<xsl:template match="feat">
 <li>
  <xsl:value-of select="@name" />
  <xsl:text>: </xsl:text>
  <xsl:choose>
   <xsl:when test="or">
    <xsl:variable name="tokens">
     <xsl:call-template name="str:tokenize">
      <xsl:with-param name="string" select="normalize-space(or)" />
      <xsl:with-param name="delimiters" select="' '" />
     </xsl:call-template>
    </xsl:variable>
    <xsl:for-each select="exsl:node-set($tokens)/token">
     <xsl:if test="position()!=1">
      <i> or </i>
     </xsl:if>
     <b><xsl:value-of select="." /></b>
    </xsl:for-each>
   </xsl:when>
   <xsl:otherwise>
    <b><xsl:value-of select="." /></b>
   </xsl:otherwise>
  </xsl:choose>
 </li>
</xsl:template>

<xsl:template match="sem-frame">
 <ul>
  <xsl:apply-templates select="role-restr-map" />
 </ul>
</xsl:template>

<xsl:template match="role-restr-map">
 <li>
  <b><xsl:value-of select="translate(@roles,' ','=')" /></b>
  <xsl:if test="@optional='optional'">
   <xsl:text> (</xsl:text><i>optional</i><xsl:text>)</xsl:text>
  </xsl:if>
  <xsl:if test="not(concept/@name='t')">
   <xsl:text> restricted to </xsl:text>
   <xsl:choose>
    <xsl:when test="or">
     <xsl:apply-templates select="or" />
    </xsl:when>
    <xsl:when test="sem-feats">
     <xsl:apply-templates select="sem-feats" />
    </xsl:when>
    <xsl:otherwise>
     <xsl:apply-templates select="concept" mode="ref" />
    </xsl:otherwise>
   </xsl:choose>
  </xsl:if>
 </li>
</xsl:template>

</xsl:stylesheet>
