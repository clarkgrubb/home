% DOM-AWK(1)
% Clark Grubb
% February 17, 2013


# NAME

dom-awk - read HTML or XML into a DOM object and process it with a Ruby script.

# SYNOPSIS

dom-awk [-x|-h] [-f FILE] RUBY_SCRIPT

# DESCRIPTION

Read an HTML or XML document from standard input, or from FILE if specified using the -f option.

The HTML/XML document is parsed using the Ruby Nokogiri library and made available to the Ruby script which is provided as a command line argument.  The Ruby DOM object is put in the global accumulator variable $_.

# EXAMPLES

    curl www.google.com | dom-awk  '$_.xpath("//a").each {|o| puts o["href"] }'

    echo '<xml><foo>bar</foo></xml>' | dom-awk '$_.xpath("//foo").each { |o| puts o.content }'

# OPTIONS

-f FILE, \--file=FILE
: Read from FILE instead of standard input.

-h, \--html
: Used to indicate input is HTML.

-x, \--xml
: Used to indicate input is XML.

# SEE ALSO

`curl` (1), `w3m` (1)

http://nokogiri.org/tutorials/searching\_a\_xml\_html\_document.html

