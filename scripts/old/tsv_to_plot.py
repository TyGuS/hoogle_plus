#!/usr/bin/python3

import argparse
import os
import csv
import numpy as np

PLOT_CSV_FILE = "tmp/plot.csv"

def safe_list_get (l, idx, default):
  try:
    return l[idx]
  except IndexError:
    return default

def cleanFile(filepath):
    with open(filepath) as result_file:
        reader = csv.reader(result_file, delimiter="\t")
        lines = list(reader)
        columns = np.transpose(lines)
        data_columns = columns[2:6]
        newclms = []
        for idx in range(len(data_columns)):
            column = data_columns[idx]
            header = column[0]
            rest = column[1:]
            sorted_column = sorted(map(float, rest))
            newclm = []
            for item in sorted_column:
                if item >= 0.1:
                    newclm.append(item)
            newclms.append([header] + newclm)
        total = len(lines) - 1 # minus header
        dataFile = []
        dataHeader = ["index"]
        for clm in newclms:
            dataHeader.append(clm[0])
        for idx in range(total):
            row = [idx]
            for clm in newclms:
                row.append(safe_list_get(clm[1:], idx, ""))
            dataFile.append(row)
        plot_data = dataFile
        with open(PLOT_CSV_FILE, "w") as plot_file:
            writer = csv.writer(plot_file, delimiter=",")
            for line in plot_data:
                writer.writerow(line)
        print("wrote to %s", PLOT_CSV_FILE)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("result", help="result.tsv file")
    args = parser.parse_args()
    cleanFile(args.result)
    print(args)

if __name__ == '__main__':
    main()
