<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- this converts ONT::*.xml to a list item that can dynamically load its children -->

<xsl:param name="mode" select="'details'" />

<xsl:template name="sem">
 <xsl:param name="id" />
 (<a id="{$id}-sem-link" href="javascript:toggleFeatures('{$id}')">hide sem</a>)
 <span id="{$id}-sem">
  <br /><xsl:value-of select="@fltype" />
  <xsl:if test="FEATURES">
   <ul>
    <xsl:for-each select="FEATURES/@*">
     <li><xsl:value-of select="name()" /> = <xsl:value-of select="." /></li>
    </xsl:for-each>
   </ul>
  </xsl:if>
 </span>
</xsl:template>

<xsl:template match="/ONTTYPE" mode="tree">
 <li id="{@name}">
  <xsl:choose>
   <xsl:when test="CHILD">
    <a id="{@name}-children-link" href="javascript:toggleChildren('{@name}')" style="text-decoration: none">&gt; </a>
   </xsl:when>
   <xsl:otherwise>- </xsl:otherwise>
  </xsl:choose>
  <a id="{@name}-link" href="../data/ONT%3A%3A{@name}.xml" target="ont-type-details"><xsl:value-of select="@name" /></a>
  <xsl:if test="CHILD">
   <ul id="{@name}-children" style="display: none; list-style: none">
    <xsl:for-each select="CHILD">
     <li id="{@name}"></li>
    </xsl:for-each>
   </ul>
  </xsl:if>
  <!-- this is not shown by default so that only the top-level date is shown when we change its display style -->
  <div style="display: none" id="{@name}-modified">Last modified: <xsl:value-of select="@modified" /></div>
 </li>
</xsl:template>

<xsl:template match="/ONTTYPE" mode="details">
 <html>
  <head>
   <title>ONT::<xsl:value-of select="@name" /></title>
   <script type="text/javascript" src="../style/onttype.js"></script>
  </head><body onload="setTargets()">
  <h2><xsl:value-of select="@name" /></h2>
  <xsl:if test="MAPPING[@to='wordnet']">
   <div>(<xsl:for-each select="MAPPING[@to='wordnet']">
     <a href="{@url}"><xsl:value-of select="@name" /></a><xsl:if test="position() != last()">, </xsl:if>
    </xsl:for-each>)</div>
  </xsl:if>
  <xsl:if test="WORD">
   <div>(<a id="{@name}-words-link" href="javascript:toggleWords('{@name}')">hide words</a>)
   <span id="{@name}-words">
    <xsl:for-each select="WORD">
     <a href="../data/W%3A%3A{@name}.xml"><xsl:value-of select="@name" /></a><xsl:if test="position() != last()">, </xsl:if>
    </xsl:for-each>
   </span>
   </div>
  </xsl:if>
  <xsl:for-each select="SEM">
   <div>
   <xsl:call-template name="sem">
    <xsl:with-param name="id" select="../@name" />
   </xsl:call-template>
   </div>
  </xsl:for-each>
  <xsl:if test="ARGUMENT">
   <div>(<a id="{@name}-roles-link" href="javascript:toggleRoles('{@name}')">hide roles</a>)
   <ul id="{@name}-roles">
    <xsl:for-each select="ARGUMENT">
     <li id="{../@name}-role-{@role}"><xsl:value-of select="@role" />
      <xsl:call-template name="sem">
       <xsl:with-param name="id"><xsl:value-of select="../@name" />-role-<xsl:value-of select="@role" /></xsl:with-param>
      </xsl:call-template>
     </li>
    </xsl:for-each>
   </ul>
   </div>
  </xsl:if>
  <xsl:if test="@definitions">
   <div>(<a id="{@name}-definitions-link" href="javascript:toggleDefinitions('{@name}')">hide definitions</a>)
   <span id="{@name}-definitions">
    <xsl:value-of select="@definitions" />
   </span>
   </div>
  </xsl:if>
  <xsl:if test="@comment">
   <div>(<a id="{@name}-comment-link" href="javascript:toggleComment('{@name}')">hide comment</a>)
   <span id="{@name}-comment">
    <xsl:value-of select="@comment" />
   </span>
   </div>
  </xsl:if>
  <div>Last modified: <xsl:value-of select="@modified" /></div>
 </body></html>
</xsl:template>

<xsl:template match="/ONTTYPE">
 <xsl:choose>
  <xsl:when test="$mode='tree'">
   <xsl:apply-templates select="." mode="tree" />
  </xsl:when>
  <xsl:otherwise>
   <xsl:apply-templates select="." mode="details" />
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>

</xsl:stylesheet>

