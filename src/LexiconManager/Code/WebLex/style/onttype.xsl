<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- this converts ONT::*.xml to a list item that can dynamically load its children -->

<xsl:template match="/ONTTYPE">
 <li id="{@name}">
  <xsl:choose>
   <xsl:when test="CHILD">
    <a id="{@name}-children-link" href="javascript:toggleChildren('{@name}')" style="text-decoration: none">&gt; </a>
   </xsl:when>
   <xsl:otherwise>- </xsl:otherwise>
  </xsl:choose>
  <xsl:value-of select="@name" />
  <xsl:if test="MAPPING[@to='wordnet']">
   (<xsl:for-each select="MAPPING[@to='wordnet']">
     <a href="{@url}"><xsl:value-of select="@name" /></a><xsl:if test="position() != last()">, </xsl:if>
    </xsl:for-each>)
  </xsl:if>
  <xsl:if test="WORD">
   (<a id="{@name}-words-link" href="javascript:toggleWords('{@name}')">show words</a>)
   <span id="{@name}-words" style="display: none">
    <xsl:for-each select="WORD">
     <a href="../data/W%3A%3A{@name}.xml"><xsl:value-of select="@name" /></a><xsl:if test="position() != last()">, </xsl:if>
    </xsl:for-each>
    <br />
   </span>
  </xsl:if>
  <xsl:if test="ARGUMENT">
   (<a id="{@name}-roles-link" href="javascript:toggleRoles('{@name}')">show roles</a>)
   <ul id="{@name}-roles" style="display: none">
    <xsl:for-each select="ARGUMENT">
     <li id="{../@name}-role-{@role}"><xsl:value-of select="@role" />
      <xsl:if test="FEATURES">
       (<a id="{../@name}-role-{@role}-sem-link" href="javascript:toggleFeatures('{../@name}-role-{@role}')">show sem</a>)
       <span id="{../@name}-role-{@role}-sem" style="display: none">
        <br /><xsl:value-of select="@fltype" />
	<ul>
	 <xsl:for-each select="FEATURES/@*">
	  <li><xsl:value-of select="name()" /> = <xsl:value-of select="." /></li>
	 </xsl:for-each>
	</ul>
       </span>
      </xsl:if>
     </li>
    </xsl:for-each>
   </ul>
  </xsl:if>
  <xsl:if test="@comment">
   (<a id="{@name}-comment-link" href="javascript:toggleComment('{@name}')">show comment</a>)
   <span id="{@name}-comment" style="display: none">
    <xsl:value-of select="@comment" />
   </span>
  </xsl:if>
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

</xsl:stylesheet>

