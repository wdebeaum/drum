#!/usr/bin/perl -p
# usage: make-config-dot-pm.pl <Config.pm.in >Config.pm

# replace $ENV{foo} with the "compile-time" value of the foo environment variable
s/\$ENV{(.+?)}/"$ENV{$1}"/g;

$optional = 1 if (/^# all optional taggers/);

# handle "use"s that reference non-existent files (this checkout might not have them)
if (/^use TextTagger::(\w+);/ and not -e "$1.pm") {
  if ($optional) {
    $_ = "# $_";
  } else {
    push @required_but_absent, "$1.pm";
  }
}

END {
  if (@required_but_absent) {
    print "__END__\n"; # cvs prints to STDOUT, so prevent it from munging the pm
    print STDERR "\nThe following files are required for TextTagger to function, but missing from this CVS checkout:\n";
    for (@required_but_absent) { print STDERR "$_\n"; }
    print STDERR "Now using cvs update to get them. Enter your CVS password if necessary.\n";
    my $status = system('cvs', 'update', @required_but_absent);
    die "cvs update failed (status=$status)\n" if ($status != 0);
  }
}
