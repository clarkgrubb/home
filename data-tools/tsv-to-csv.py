#!/usr/bin/env python

# Convert TSV (tab and newline delimited) to CSV.

import argparse
import csv
import sys


def tsv_to_csv(input_stream,
               output_stream,
               delimiter=',',
               quotechar='"'):

    csv_writer = csv.writer(output_stream,
                            delimiter=delimiter,
                            quotechar=quotechar)

    for line in input_stream:
        row = line.rstrip('\n\r').split('\t')
        csv_writer.writerow(row)



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

    tsv_to_csv(sys.stdin,
               sys.stdout,
               delimiter=args.delimiter,
               quotechar=args.quotechar)
