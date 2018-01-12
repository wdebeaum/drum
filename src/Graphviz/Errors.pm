package Graphviz::Errors;
# CWMS-style failure reports

require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(unknown_action missing_argument invalid_argument unknown_object nested_error);

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

1;
