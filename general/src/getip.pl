#!/usr/local/bin/perl -w
use CGI qw/:standard/;
print "Content-type: text/html\n\n";
print "BEGINIP".$ENV{REMOTE_ADDR}."ENDIP";