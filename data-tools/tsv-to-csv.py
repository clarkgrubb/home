#!/usr/bin/env python

# Convert TSV (tab and newline delimited) to CSV.

import csv
import sys


def tsv_to_csv(input_stream,
               output_stream):

    csv_writer = csv.writer(output_stream)

    for line in input_stream:
        row = line.rstrip('\n\r').split('\t')
        csv_writer.writerow(row)


if __name__ == '__main__':

    tsv_to_csv(sys.stdin,
               sys.stdout)
