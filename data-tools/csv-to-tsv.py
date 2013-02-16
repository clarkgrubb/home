#!/usr/bin/env python

# Convert CSV and similar formats to TSV (tab and newline delimited).

import argparse
import csv
import re
import sys

STRIPPER_CHARS = "\n\r\t"
ESCAPER_REGEX = re.compile('([\n\r\t])')


def stripper(row):
    return [field.translate(None, STRIPPER_CHARS) for field in row]


def escaper(row):
    return [ESCAPER_REGEX.sub(field, '\\\1') for field in row]


def csv_to_tsv(input_stream,
               output_stream,
               delimiter=',',
               quotechar='"',
               sanitizer=stripper):

    rows = csv.reader(input_stream,
                      delimiter=delimiter,
                      quotechar=quotechar)

    for row in rows:
        output_stream.write('\t'.join(sanitizer(row)) + '\n')


if __name__ == '__main__':

    parser = argparse.ArgumentParser()

    parser.add_argument('--delimiter', '-d',
                        dest='delimiter',
                        default=',')
    parser.add_argument('--quotechar', '-q',
                        dest='quotechar',
                        default='"')
    parser.add_argument('--escapechar', '-e',
                        dest='escapechar')

    args = parser.parse_args()

    if not args.escapechar:
        sanitizer = stripper
    elif args.escapechar == '\\':
        sanitizer = escaper
    else:
        raise ValueError('unsupported escape char: {}'.format(args.escapechar))

    csv_to_tsv(sys.stdin,
               sys.stdout,
               delimiter=args.delimiter,
               quotechar=args.quotechar,
               sanitizer=sanitizer)
