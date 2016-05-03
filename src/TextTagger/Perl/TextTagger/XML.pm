package TextTagger::XML;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(read_xml process_xml);

use IPC::Open2;
use KQML::KQML;
use TextTagger::Tags2Trips qw(trips2tagNative);

use strict vars;

my $debug = 0;

sub read_xml {
  my $input_handle = shift;
  my $xml = '';
  my $document_element = undef;
  my $done = 0;
  while (<$input_handle>) {
    if (/<\?xml\s/) { # start of XML
      $xml = $& . $';
      $_ = '';
      print STDERR "Read start of XML.\n" if ($debug);
    }
    if ($xml ne '') { # somewhere in the middle
      $xml .= $_;
      if ((not defined($document_element)) and
	  $xml =~ /<([a-z][\w:-]*)/i) {
	$document_element = $1;
	print STDERR "Read start of XML document element.\n" if ($debug);
      }
      if (defined($document_element) and
	  $xml =~ /<\/$document_element>/) {
	$xml = $` . $&;
	print STDERR "Read end of XML.\n" if ($debug);
	$done = 1;
	last;
      }
    }
  }
  die "Failed to read full XML document!" unless ($done);
  return $xml;
}

sub process_xml {
  my ($xsl_filename, $xml) = @_;
  my ($xsltproc_in, $xsltproc_out);
  print STDERR "Starting xsltproc...\n" if ($debug);
  my $xsltproc_pid = open2($xsltproc_in, $xsltproc_out, 'xsltproc', '--novalid', $xsl_filename, '-');
  binmode $xsltproc_in, ':utf8';
  binmode $xsltproc_out, ':utf8';
  print STDERR "Sending XML to xsltproc...\n" if ($debug);
  print $xsltproc_out "$xml\n";
  print STDERR "Closing xsltproc's input filehandle...\n" if ($debug);
  close $xsltproc_out;
  # get the results from xsltproc, which are KQML-ish native-format tags, one
  # per line
  print STDERR "Receiving tags from xsltproc...\n" if ($debug);
  my @tags = ();
  while (<$xsltproc_in>) {
    push @tags, trips2tagNative(KQML::KQMLReadFromString($_));
  }
  print STDERR "Received " . scalar(@tags) . " tags from xsltproc.\n"
    if ($debug);
  print STDERR "Waiting for xsltproc to die...\n" if ($debug);
  waitpid $xsltproc_pid, 0;
  return @tags;
}

1;
