# OBOParser.pm
# 2014-08-27
# William de Beaumont

# A very simple parser for the OBO (Open Biological/Biomedical Ontology) format.
# Parses an OBO file into a list of hashrefs, the first one being the header
# and the rest being the stanzas.
# See http://www.geneontology.org/GO.format.obo-1.2.shtml for the full spec.

package TextTagger::OBOParser;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(read_obo read_header read_stanza read_stanza_name remove_trailing_modifier);

use IO::Handle; # for ungetc

use strict vars;

# Get a single character from a filehandle and immediately put it back so it's
# available for the next read. Return undef at end of file.
sub peek_char {
  my $fh = shift;
  my $c = $fh->getc;
  $fh->ungetc(ord($c)) if (defined($c));
  return $c;
}

# Read characters from a filehandle until the next character to be read
# satisfies the regexp, and return that character.
sub read_until {
  my ($fh, $re) = @_;
  my $c;
  do {
    $c = $fh->getc;
    $.++ if ($c eq "\n");
  } while (defined($c) and $c !~ $re);
  if (defined($c)) {
    $fh->ungetc(ord($c));
    $.-- if ($c eq "\n");
  }
  return $c;
}

# read whitespace and comments
sub read_whitespace {
  my $fh = shift;
  my $c;
  for(;;) {
    $c = read_until($fh, qr/\S/);
    if ($c eq '!') { # comment
      $c = read_until($fh, qr/[\r\n]/);
    } else {
      return $c;
    }
  }
}

# Map the character after the backslash to the resulting unescaped string.
my %escapes = (
  'n' => "\n",
  'W' => ' ',
  't' => "\t",
  "\n" => ''
  # all other characters just have the backslash removed
); 

# Like read_until, except that it interprets escapes (without checking the
# unescaped character against $re) and returns the read string instead of the
# next character.
sub read_escaped_until {
  my ($fh, $re) = @_;
  my $str = '';
  my $c;
  for(;;) {
    $c = $fh->getc;
    return $str unless (defined($c)); # eof
    $.++ if ($c eq "\n");
    if ($c eq "\\") {
      $c = $fh->getc;
      defined($c) or die "backslash at end of file";
      $.++ if ($c eq "\n");
      $c = $escapes{$c} if (exists($escapes{$c}));
    } elsif ($c =~ $re) {
      $fh->ungetc(ord($c));
      $.-- if ($c eq "\n");
      return $str;
    }
    $str .= $c;
  }
}

# Read a string enclosed by double quotes, possibly including escapes.
sub read_quoted_string {
  my $fh = shift;
  my $c = $fh->getc;
  defined($c) or die "expected quoted string at end of file";
  $c eq '"' or die "expected '\"' but got '$c'";
  my $str = read_escaped_until($fh, '"');
  # discard close "
  defined($fh->getc) or die "expected closing '\"' at end of file";
  return $str;
}

# like getline, except it plays nice with $. and getc
sub read_line {
  my $fh = shift;
  my $str = '';
  my $c;
  do {
    $c = $fh->getc;
    unless (defined($c)) {
      last unless ($str eq '');
      return undef;
    }
    $str .= $c;
  } until ($str =~ m{\Q$/\E$});
  $.++;
  return $str;
}

# Parse the value of a synonym tag into a hashref
sub read_and_parse_synonym {
  my ($fh, $name) = @_;
  read_whitespace($fh);
  my $synonym = read_quoted_string($fh);
  my $rest = read_escaped_until($fh, qr/[!\r\n]/);
  my @groups = ($rest =~ /
    ^ \s*
    (?: ( EXACT | BROAD | NARROW | RELATED ) \s+ )?
    (?: ( [^\s\[\]]+ ) \s+ )?
    \[ ( [^\]]* ) \] \s*
    ( { [^}]+ } )?
    $
  /x);
  @groups or die "malformed synonym tag rest: $rest";
  my ($scope, $type, $dbxrefs, $modifier) = @groups;
  $scope = uc($1) if ($name =~ /^(\w+)_synonym$/);
  return +{
    synonym => $synonym,
    scope => ($scope || 'RELATED'),
    type => $type,
    dbxrefs => [ split(/\s*,\s*/, $dbxrefs) ],
    ( $modifier ne '' ? (modifier => { split(/\s*[,=]\s*/, $modifier) }) : ())
  };
}

# Remove a trailing modifier from a tag value.
sub remove_trailing_modifier {
  my $str = shift;
  $str =~ s/\s*{[^\}]+?}$//;
  return $str;
}

# Read a tag-value pair and any whitespace/comments following it. As a special
# case, synonym tags are parsed into hashrefs.
sub read_tag_value_pair {
  my $fh = shift;
  my $name = read_escaped_until($fh, qr/:/);
  $name =~ s/^\s+|\s+$//g; # trim whitespace from name
  # discard :
  defined($fh->getc) or die "tag with no value at end of file: $name";
  my $value;
  if ($name =~ /^(exact_|narrow_|broad_|)synonym$/) {
    $value = read_and_parse_synonym($fh, $name);
    $name = 'synonym';
  } else {
    $value = read_escaped_until($fh, qr/[!\r\n]/);
    $value =~ s/^\s+|\s+$//g; # trim whitespace from value
  }
  read_whitespace($fh);
  return ($name, $value);
}

# Read a sequence of tag-value pairs up to EOF or the next stanza, and return a
# hash mapping tag names to listrefs of tag values.
sub read_header {
  my $fh = shift;
  my %pairs = ();
  for(;;) {
    my $c = peek_char($fh);
    return %pairs unless (defined($c) and $c ne '[');
    my ($name, $value) = read_tag_value_pair($fh);
    push @{$pairs{$name}}, $value;
  }
}

# Read the name line of a stanza.
sub read_stanza_name {
  my $fh = shift;
  my $stanza_name = read_line($fh);
  $stanza_name =~ /^\s*\[([^\]]+)\]\s*$/ or die "expected stanza name in [], but got $stanza_name";
  return $1;
}

# Read a complete stanza and return it as a hashable list. The stanza name has
# the key stanza_name.
sub read_stanza {
  my $fh = shift;
  return (stanza_name => read_stanza_name($fh), read_header($fh));
}

# Read a whole OBO file and return it as a list of hashrefs. The first hashref
# is the header, the rest are stanzas.
sub read_obo {
  my $fh = shift;
  my @chunks = (+{ read_header($fh) });
  push @chunks, +{ read_stanza($fh) } until ($fh->eof);
  return @chunks;
}

