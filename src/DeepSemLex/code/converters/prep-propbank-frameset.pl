#!/usr/bin/perl -p

# fix some problems with OntoNotes/PropBank frameset data, and strip DOCTYPE declarations so they don't confuse xsltproc

$_='' if (/DOCTYPE|SYSTEM/);
s{<rel>brayed</arg>}{<rel>brayed</rel>};
s/"vntheta="/" vntheta="/;
s/<vnrole vnlcs=/<vnrole vncls=/;

