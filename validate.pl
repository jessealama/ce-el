#!/usr/bin/env perl

use strict;
use warnings;

require v5.10.0; # for the 'say' feature
use feature 'say';
use English qw(-no_match_vars);
use Readonly;
use File::Temp qw(tempfile);
use IPC::Run qw(harness);

Readonly my $EMPTY => q{};
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

my @curl_call = ('curl',
		 '--form', 'uploaded_file=@' . $xhtml_path,
		 '--connect-timeout', '30',
		 '--output', $w3c_response_html_path,
		 $VALIDATOR_URI);
my $curl_err = $EMPTY;
my $curl_harness = harness (\@curl_call,
			    '2>', \$curl_err);

$curl_harness->start ();
$curl_harness->finish ();

my $curl_exit_code = ($curl_harness->results)[0];

if ($curl_exit_code != 0) {
    say {*STDERR} 'Error: curl did not exit cleanly.  Its exit code was ', $curl_exit_code, '.';
    say {*STDERR} 'The error output was:';
    say {*STDERR} $curl_err;
    exit 1;
}

if (! -r $w3c_response_html_path) {
    say {*STDERR} 'Error: the output of curl does not exit, or is unreadable.';
    exit 1;
}

system 'open', $w3c_response_html_path;

exit $CHILD_ERROR;
