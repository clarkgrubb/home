% CSV-TO-TSV(1)
% Clark Grubb
% February 16, 2013


# NAME

csv-to-tsv - convert CSV to TSV

# SYNOPSIS

csv-to-tsv OPTIONS 

# DESCRIPTION

Read a CSV file from standard input and write the corresponding TSV file to standard output.

In the TSV format fields are delimited by tabs and records are terminated by an end-of-line marker.  `csv-to-tsv` uses newline as the end-of-line marker.

There is no mechanism for quoting tabs or newlines, and by default `csv-to-tsv` will fail if they occur in the fields of the CSV file.  

# OPTIONS

-d DELIMITER, \--delimiter=DELIMITER
: Used to read CSV files which use DELIMITER to separate fields instead of a comma.

-e, \--escape
: Use backslash escape sequences to escape tabs, carriage returns, newlines, and backslashes.

-q QUOTECHAR, \--quotechar=QUOTECHAR
: Used to read CSV files which use QUOTECHAR to quote fields instead of double quotes.

-x, \--strip
: Remove tabs, carriage returns, and newlines in fields.

-z, \--squeeze
: replace tabs, carriage returns, and newlines in fields with spaces and then replace adjacent spaces with a single space.


# SEE ALSO

`tsv-to-csv` (1)

http://www.ietf.org/rfc/rfc4180.txt

http://www.iana.org/assignments/media-types/text/tab-separated-values