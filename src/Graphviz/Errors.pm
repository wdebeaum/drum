package Graphviz::Errors;
# CWMS-style failure reports

require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(unknown_action missing_argument invalid_argument unknown_object nested_error get_typed_value get_typed_argument);

use Graphviz::Escape qw(escape_for_quotes);

use strict vars;

sub program_error {
  my $msg = shift;
  return ['failure', ':type', 'cannot-perform', ':reason',
    ['program-error', ':message', '"' . escape_for_quotes($msg) . '"']];
}

sub nested_error {
  my ($prefix, $err) = @_;
  if (ref($err) eq 'ARRAY' and $err->[0] eq 'failure') { # already a failure
    return $err;
  } else { # just a regular string, wrap it as a program_error
    print STDERR "$err\n";
    return program_error($prefix . $err);
  }
}

sub missing_argument {
  my ($op, $arg) = @_;
  return ['failure', ':type', 'failed-to-interpret', ':reason',
    ['missing-argument',
      ':operator', $op,
      ':argument', $arg
    ]
  ];
}

sub invalid_argument {
  my ($operation, $argument, $expected) = @_;
  if (ref($operation) eq 'HASH') {
    my $op = $operation->{verb};
    my $got = $operation->{$argument};
    return ['failure', ':type', 'failed-to-interpret', ':reason',
      ['invalid-argument',
	':operator', $op,
	':argument', $argument,
	':expected', '"' . escape_for_quotes($expected) . '"',
	':got', $got
      ]
    ];
  } elsif (ref($operation) eq 'ARRAY') {
    my $op = $operation->[0];
    my $got = $operation->[$argument];
    return ['failure', ':type', 'failed-to-interpret', ':reason',
      ['invalid-argument',
	':operator', $op,
	':argument', $argument,
	':expected', '"' . escape_for_quotes($expected) . '"',
	':got', $got
      ]
    ];
  }
}

sub unknown_action {
  my $what = shift;
  return ['failure', ':type', 'failed-to-interpret', ':reason',
    ['unknown-action', ':what', $what]
  ];
}

sub unknown_object {
  my $what = shift;
  return ['failure', ':type', 'cannot-perform', ':reason',
    ['unknown-object', ':what', $what]
  ];
}

# given a string representing a type, and a KQML representation of a value,
# interpret the value to be of that type and return a Perl representation of
# the value. die on type mismatch. Possible type strings are:
# number	checks for numeric scalar
# integer	checks for integer scalar
# symbol	checks for non-numeric, non-string scalar
# string	checks for and removes double quotes and unescapes
# list		checks for ARRAY ref or NIL, converts NIL to []
# performative	checks for ARRAY ref, and keywordifies
sub get_typed_value {
  my ($type, $value) = @_;
  print STDERR "1\n";
  if ($type eq 'number') {
    print STDERR "2\n";
    return $value
      if ((not ref($value)) and $value =~ /^[+-]?(?:\d*\.\d+|\d+\.?)$/);
    print STDERR "3\n";
  } elsif ($type eq 'integer') {
    print STDERR "4\n";
    return $value if ((not ref($value)) and $value =~ /^[+-]?\d$/);
    print STDERR "5\n";
  } elsif ($type eq 'symbol') {
    print STDERR "6\n";
    return $value
      unless (ref($value) or $value =~ /^[+-]?\.?\d/ or
	      KQML::KQMLAtomIsString($value));
    print STDERR "7\n";
  } elsif ($type eq 'string') {
    print STDERR "8\n";
    return KQML::KQMLStringAtomAsPerlString($value)
      if ((not ref($value)) and KQML::KQMLAtomIsString($value));
    print STDERR "9\n";
  } elsif ($type eq 'list') {
    print STDERR "10\n";
    return $value if (ref($value) eq 'ARRAY');
    print STDERR "11\n";
    return [] if ((not ref($value)) and lc($value) eq 'nil');
    print STDERR "12\n";
  } elsif ($type eq 'performative') {
    print STDERR "13\n";
    return KQML::KQMLKeywordify($value) if (ref($value) eq 'ARRAY');
    print STDERR "14\n";
  }
  print STDERR "15\n";
  die;
}

# given a list or performative, an index/key into it, an expected type, and an
# optional default value, return a Perl representation of the indexed value of
# that type, or die with either missing_argument or invalid_argument.
# See also get_typed_value, and getTypedArgument in ../ChartDisplay/Args.java
sub get_typed_argument {
  my ($perf, $key, $type, $default) = @_;
  if (ref($perf) eq 'HASH') {
    if (exists($perf->{$key})) {
      my $value = undef;
      eval {
        $value = get_typed_value($type, $perf->{$key});
	1
      } || die invalid_argument($perf, $key, $type);
      return $value;
    } elsif (defined($default)) {
      return $default;
    } else {
      missing_argument($perf->{verb}, $key);
    }
  } elsif (ref($perf) eq 'ARRAY') {
    if (exists($perf->[$key])) {
      my $value = undef;
      eval {
        $value = get_typed_value($type, $perf->[$key]);
	1
      } || die invalid_argument($perf, $key, $type);
      return $value;
    } elsif (defined($default)) {
      return $default;
    } else {
      missing_argument($perf->{verb}, $key);
    }
  } else {
    die "tried to look up $key in something that's not a HASH or ARRAY ref: " . Data::Dumper->Dump([$perf],['*perf']);
  }
}

1;
