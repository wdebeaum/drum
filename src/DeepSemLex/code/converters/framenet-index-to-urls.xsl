<?xml version="1.0"?>
<stylesheet version="2.0"
  xmlns="http://www.w3.org/1999/XSL/Transform"
  xmlns:fn="http://framenet.icsi.berkeley.edu"
  >

<!-- framenet-index-to-urls.xsl - convert FrameNet data/{frame,lu}Index.xml files to lists of URLs of the frame and lu XML files they refer to, one per line -->

<output method="text" />

<template match="@*|text()" />

<template match="/fn:frameIndex/fn:frame">
 <text>https://framenet2.icsi.berkeley.edu/fnReports/data/frame/</text>
 <value-of select="@name" />
 <text>.xml
</text>
</template>

<template match="/fn:luIndex/fn:lu">
 <text>https://framenet2.icsi.berkeley.edu/fnReports/data/lu/lu</text>
 <value-of select="@ID" />
 <text>.xml
</text>
</template>

</stylesheet>
