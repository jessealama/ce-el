#!/usr/bin/env perl

use strict;
use warnings;

require v5.10.0; # for the 'say' feature
use feature 'say';

use Readonly;
use File::Temp qw(tempfile);
use IPC::Run qw(harness);

Readonly my $VALIDATOR_URI => 'http://validator.w3.org/check';

# Check command line

if (scalar @ARGV != 1) {
    say {*STDERR} 'Usage: validate.pl XHTML-FILE-NAME';
    exit 1;
}

my $xhtml_path = $ARGV[0];

# Sanity check: the XHTML file exists and is readable

if (-d $xhtml_path) {
    say {*STDERR} 'Error: the given XHTML file ', $xhtml_path, ' is actually a directory.';
    exit 1;
}

if (! -r $xhtml_path) {
    say {*STDERR} 'Error: the given XHTML file ', $xhtml_path, ' does not exist, or is unreadable.';
    exit 1;
}

my $temp_fh = File::Temp->new (UNLINK => 0,
			       SUFFIX => '.html');
my $w3c_response_html_path = $temp_fh->filename ();

my @wget_call = ('wget',
		 "--post-data=${xhtml_path}",
		 '--timeout=30',
		 "--output-document=${w3c_response_html_path}",
		 $VALIDATOR_URI);
my $wget_err = '';
my $wget_harness = harness (\@wget_call,
			    '2>', \$wget_err);

$wget_harness->start ();
$wget_harness->finish ();

my $wget_exit_code = ($wget_harness->results)[0];

if ($wget_exit_code != 0) {
    say {*STDERR} 'Error: wget did not exit cleanly.  Its exit code was ', $wget_exit_code, '.';
    say {*STDERR} 'The error output was:';
    say {*STDERR} $wget_err;
    exit 1;
}

if (! -r $w3c_response_html_path) {
    say {*STDERR} 'Error: the output of wget does not exit, or is unreadable.';
    exit 1;
}

system ('open', $w3c_response_html_path);

exit $?;
