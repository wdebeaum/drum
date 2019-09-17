<?xml version="1.0"?>
<stylesheet version="1.0"
 xmlns="http://www.w3.org/1999/XSL/Transform"
 xmlns:str="http://exslt.org/strings"
 xmlns:xces="http://www.xces.org/schema/2003"
 xmlns:xs="http://www.w3.org/2001/XMLSchema">
<!--
glosstag-standoff-to-sql.xsl - convert the "standoff" XML format of the WordNet tagged gloss corpus to an SQL script that fills structs and feats tables
William de Beaumont
2012-02-27
-->
<output method="text" />

<key name="id" match="xces:node | xces:struct" use="@id" />

<!-- don't copy text or attributes by default -->
<template match="text()|@*" priority="-1"></template>

<template match="xces:struct">
 <if test="contains('def ex wf cf punc ignore auto man', @type) and @type != 'un'">
  <text>INSERT INTO structs VALUES ('</text>
  <value-of select="substring(@id,2,8)" /><text>','</text>
  <value-of select="substring(@id,1,1)" /><text>','</text>
  <value-of select="@id" /><text>','</text>
  <value-of select="@type" /><text>','</text>
  <!-- NOTE: $first-word originally had key('id',@from) inline in place of $fromnode, but there is a bug in xsltproc's implementation of the key() function that prevents it from working properly in that situation. Factoring out $fromnode fixes it somehow. -->
  <variable name="fromnode" select="key('id',@from)" />
  <variable name="first-word" select="$fromnode[local-name()='struct'] | key('id',$fromnode/@ref)" />
  <choose>
   <when test="$first-word">
    <value-of select="$first-word/@from" /><text>','</text>
    <value-of select="$first-word/@to" />
   </when>
   <otherwise>
    <value-of select="@from" /><text>','</text>
    <value-of select="@to" />
   </otherwise>
  </choose>
  <text>');
</text>
 </if>
</template>

<template match="xces:feat">
 <if test="contains('pos wnsk lemma', @name)">
  <text>INSERT INTO feats VALUES ('</text>
  <variable name="pid" select="parent::xces:struct/@id" />
  <value-of select="$pid" /><text>','</text>
  <value-of select="@name" /><text>','</text>
  <value-of select="str:replace(@value, &quot;'&quot;, &quot;''&quot;)" /><text>');
</text>
 </if>
</template>

<template match="/">
 <apply-templates select="//xces:struct" />
 <apply-templates select="//xces:feat" />
</template>

</stylesheet>
