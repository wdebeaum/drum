<?xml version="1.0"?>
<stylesheet version="2.0" xmlns="http://www.w3.org/1999/XSL/Transform">

<!-- ontonotes-inventory-to-dsl.xsl - convert OntoNodes 3.0 sense-inventories/*.xml files to DeepSemLex Lisp data files -->

<output method="text" />

<template match="@*|text()" />

<!-- emit a newline and an appropriate level of indentation -->
<template name="nl-indent">
 <text>
</text>
 <for-each select="ancestor::inventory | ancestor::word | ancestor::sense | ancestor::mappings">
  <text>  </text>
 </for-each>
</template>

<variable name="lemma" select="replace(/inventory/@lemma, '-[nv]$', '')" />
<variable name="pos" select="replace(/inventory/@lemma, '^.*-', '')" />

<template match="/inventory">
 <text>;;; AUTOMATICALLY GENERATED
(provenance OntoNotes (version "3.0") (filename "data/english/metadata/sense-inventories/</text>
 <value-of select="@lemma" />
 <text>.xml"))
(word </text>
 <value-of select="$lemma" />
 <text> (morph (pos </text>
 <value-of select="upper-case($pos)" />
 <text>)</text>
 <apply-templates />
 <call-template name="nl-indent" />
 <text>  ))
</text>
</template>

<template match="commentary">
 <!-- turn commentary into comments, eliminating blank lines -->
 <variable name="nl-indent">
  <call-template name="nl-indent" />
 </variable>
 <for-each select="tokenize(string(.), '\n')">
  <if test="string-length(.) gt 0">
   <value-of select="$nl-indent" />
   <text>;; </text>
   <value-of select="." />
  </if>
 </for-each>
</template>

<template match="sense">
 <call-template name="nl-indent" />
 <text>(sense ON::</text>
 <value-of select="$lemma" />
 <text>.</text>
 <value-of select="$pos" />
 <text>.</text>
 <value-of select="@n" />
 <call-template name="nl-indent" />
 <text>  (definition (text "</text>
 <value-of select="replace(@name, '\s+', ' ')" />
 <text>"))</text>
 <apply-templates />
 <call-template name="nl-indent" />
 <text>  )</text>
</template>

<template match="examples">
 <variable name="nl-indent">
  <call-template name="nl-indent" />
 </variable>
 <for-each select="tokenize(string(.), '\n')">
  <if test="string-length(.) gt 0">
   <value-of select="$nl-indent" />
   <text>(example (text "</text>
   <value-of select="." />
   <text>"))</text>
  </if>
 </for-each>
</template>

<template match="mappings">
 <variable name="mappings">
  <apply-templates />
 </variable>
 <if test="string-length($mappings) gt 0">
  <call-template name="nl-indent" />
  <text>(overlap</text>
  <value-of select="$mappings" />
  <call-template name="nl-indent" />
  <text>  )</text>
 </if>
</template>

<!--template match="gr_sense">
 This is sometimes present, but always empty, so I don't know what it means.
</template -->

<template match="wn">
 <variable name="nl-indent">
  <call-template name="nl-indent" />
 </variable>
 <variable name="version" select="@version" />
 <variable name="wn-lemma" select="if (@lemma) then @lemma else $lemma" />
 <for-each select="tokenize(string(.), ',')">
  <value-of select="$nl-indent" />
  <choose>
   <when test="string-length($version) eq 0"><text>WN</text></when>
   <when test="matches($version, '^[MW](erriam[ _-])?W(ebster)?([ _-][Oo]ne?line)?$')">
    <text>MWO</text>
   </when>
   <when test="matches($version, '^M(AC|ac)$')">
    <text>MAC</text>
   </when>
   <when test="matches($version, '[Aa]nswers(\.com)?')">
    <text>answers.com</text>
   </when>
   <when test="matches($version, '^\d\.\d$')">
    <text>WN-</text>
    <value-of select="$version" />
   </when>
   <otherwise>
    <text>; version=</text>
    <value-of select="$version" />
    <text> </text>
   </otherwise>
  </choose>
  <text>::</text>
  <value-of select="$wn-lemma" />
  <text>.</text>
  <value-of select="$pos" />
  <text>.</text>
  <value-of select="." />
 </for-each>
</template>

<!--template match="omega">
 This is always present, but always empty, so it's meaningless.
</template -->

<template match="pb | vn | fn">
 <variable name="ns" select="upper-case(local-name())" />
 <variable name="nl-indent">
  <call-template name="nl-indent" />
 </variable>
 <for-each select="tokenize(string(.), ',')">
  <if test="string-length(.) gt 2"> <!-- sometimes it's empty, 'NP', or 'NM' -->
   <value-of select="$nl-indent" />
   <choose>
    <when test="$ns = 'PB' and matches(., '-\d')">
     <!-- VerbNet classes sometimes get appended to <pb> -->
     <text>VN</text>
    </when>
    <otherwise>
     <value-of select="$ns" />
    </otherwise>
   </choose>
   <text>::</text>
   <value-of select="." />
  </if>
 </for-each>
</template>

<!-- template match="relations">
 There are only two relations in the entire corpus, not worth it
</template -->

<!-- Not included: SENSE_META, WORD_META, ita -->

</stylesheet>
