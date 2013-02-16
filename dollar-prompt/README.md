
## OVERVIEW ##

The file dollar-prompt.txt is a list common commands found
at the "dollar prompt".  The script dollar-prompt.rb will search
the current PATH and list the commands that are missing.

Implementation for a few of the commands are provided.

## TODO ##

cmd-audit
csv-to-tab
tab-to-csv
json-awk
xpath-grep
ini-grep
  Takes section and name, returns property,  Flags to include section
  and name separated by tabs or specified char.  Section and name can be
  regex to dump the entiire ini in a 3 colum format.

cookie jar format
record jar format

## CSV

* line terminator variation
* trim fields (don't by default)
* escaping double quotes (double by default)
* skip header (-h, with NUM arg skip NUM lines)
* output rs and us (newline and tab by default)
