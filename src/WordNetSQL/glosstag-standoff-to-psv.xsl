<?xml version="1.0"?>
<stylesheet version="2.0"
 xmlns="http://www.w3.org/1999/XSL/Transform"
 xmlns:xces="http://www.xces.org/schema/2003"
 xmlns:xs="http://www.w3.org/2001/XMLSchema"
 xmlns:wdb="http://ihmc.us/users/user.php?UserID=wBeaumont">
<!--
glosstag-standoff-to-psv.xsl - convert the "standoff" XML format of the WordNet tagged gloss corpus to a pipe-separated value table
William de Beaumont
2011-03-09
-->
<output method="text" />
<!-- don't copy text or attributes by default -->
<template match="text()|@*" priority="-1"></template>

<!-- fix non-standard sense keys in gloss corpus that use 3 instead of 5 for the ss-type of satellite adjectives -->
<function name="wdb:fix-sense-key" as="xs:string">
 <param name="sense-key" as="xs:string" />
 <sequence
   select="
     if (matches($sense-key, '^[^%]+%3:\d\d:\d\d:[^:]+:\d\d$'))
       then replace($sense-key, '%3', '%5')
       else $sense-key
   " />
</function>

<template match="xces:struct">
 <variable name="synset_offset" select="substring(@id,2,8)" />
 <variable name="ss_type" select="substring(@id,1,1)" />
 <variable name="synset_id" select="substring(@id,1,9)" />
 <variable name="gloss_start" select="../xces:struct[substring(@id,1,9) = $synset_id][1]/@from" />

 <variable name="prefix">
  <value-of select="$synset_offset" />
  <text>|</text>
  <value-of select="$ss_type" />
  <text>|</text>
  <value-of select="@from - $gloss_start" />
  <text>|</text>
  <value-of select="@to - $gloss_start" />
  <text>|</text>
 </variable>

 <choose>
  <when test="@type = 'def'">
   <value-of select="$prefix" />
   <text>def||
</text>
  </when>
  <when test="@type = 'ex'">
   <value-of select="$prefix" />
   <text>ex||
</text>
  </when>
  <when test="xces:feat[@name='pos']">
   <for-each select="xces:feat[@name='pos']">
    <value-of select="$prefix" />
    <text>pos|</text>
    <value-of select="@value" />
    <text>|
</text>
   </for-each>
  </when>
  <when test="xces:feat[@name='wnsk']">
   <for-each select="xces:feat[@name='wnsk']">
    <value-of select="$prefix" />
    <text>sns||</text>
    <value-of select="wdb:fix-sense-key(@value)" />
    <text>
</text>
   </for-each>
  </when>
 </choose>
</template>

<template match="/">
 <!-- text>synset_offset|ss_type|from|to|tag_type|pos|sense_key
</text -->
 <apply-templates select="//xces:struct" />
</template>

</stylesheet>
