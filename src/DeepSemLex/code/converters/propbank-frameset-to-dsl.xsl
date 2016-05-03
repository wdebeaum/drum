<?xml version="1.0"?>
<stylesheet version="2.0" xmlns="http://www.w3.org/1999/XSL/Transform">

<!-- propbank-frameset-to-dsl.xsl - convert PropBank frameset files (OntoNotes 3.0 frames/*.xml) to DeepSemLex Lisp data files -->

<output method="text" />

<template match="@*|text()" />

<!-- emit a newline and an appropriate level of indentation -->
<template name="nl-indent">
 <text>
</text>
 <for-each select="ancestor::predicate | ancestor::roleset | ancestor::roles">
  <text>  </text>
 </for-each>
</template>

<template match="/frameset">
 <text>;;; AUTOMATICALLY GENERATED
(provenance PropBank (filename "</text>
 <value-of select="@lemma" />
 <text>.xml"))</text>
 <apply-templates />
</template>

<template match="predicate">
 <call-template name="nl-indent" />
 <text>(word </text>
 <value-of select="@lemma" /><!-- TODO underscores? -->
 <apply-templates />
 <call-template name="nl-indent" />
 <text>  )
</text>
</template>

<template match="note">
 <!-- turn notes into comments, eliminating blank lines but preserving indentation -->
 <call-template name="nl-indent" />
 <value-of select="replace(replace(replace(string(.),'^[ \t]*$','','m'),'\n+','&#x0A;'), '^([ \t]*)', '$1;; ', 'm')" />
</template>

<template match="roleset">
 <call-template name="nl-indent" />
 <text>(concept PB::</text>
 <value-of select="replace(@id, '\.', '.v.')" /> <!-- FIXME do these actually correspond to senses in the sense inventory like this? I kind of doubt it because some are missing from there -->
 <if test="@vncls and @vncls != '-'">
  <call-template name="nl-indent" />
  <text>  (overlap VN::_</text>
  <!-- FIXME need to tokenize on space e.g. for absolve -->
  <value-of select="@vncls" /> <!-- this is only the numeric part -->
  <text>)</text>
 </if>
 <apply-templates />
 <call-template name="nl-indent" />
 <text>  )</text>
</template>

<template match="roles">
 <call-template name="nl-indent" />
 <text>(sem-frame</text>
 <apply-templates />
 <call-template name="nl-indent" />
 <text>  )</text>
</template>

<template match="role">
 <!-- TODO handle the case of count(vnrole) != 1 (can be 0 or >1) -->
 <apply-templates />
 <text> ;; </text>
 <value-of select="@descr" />
</template>

<template match="vnrole">
 <call-template name="nl-indent" />
 <text>(</text>
 <value-of select="@vntheta" />
 <text> t)</text>
</template>

<template match="example">
 <call-template name="nl-indent" />
 <text>(example</text>
 <!-- TODO @name, @src, inflection, arg, rel -->
 <apply-templates />
 <text>)</text>
</template>

<template match="text">
 <text> (text "</text>
 <!-- get rid of extra whitespace due to line wrapping and indentation -->
 <value-of select="replace(replace(string(.), '^\s+|\s+$', ''), '\s+', ' ')" />
 <text>")</text>
</template>

</stylesheet>
