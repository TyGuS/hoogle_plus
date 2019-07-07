#!/usr/bin/python3

import os
import signal
import re
import yaml
import time
import argparse
import csv
from multiprocessing import Pool

HPLUS_CMD = "stack exec -- evaluation %s -q %s -f TSV -o %s"

BMS_PER_GROUP = 4
OUTPUT_FILE = "results.tsv"

def chunks(l, n):
    """Yield successive n-sized chunks from l."""
    for i in range(0, len(l), n):
        yield l[i:i + n]

def run_benchmarks(experiment, benchmarks):
    groups = list(chunks(benchmarks, BMS_PER_GROUP))
    run_pairs = []
    # chunk the benchmarks
    for i in range(len(groups)):
        group = groups[i]
        filename = "tmp/in%s.yml" % str(i)
        outname = "tmp/out%s.tsv" % str(i)
        run_pairs.append({
            "input_file": filename,
            "output_file": outname,
        })
        with open(filename, 'w') as out_file:
            out_file.write(yaml.dump(group))
    # run the actual evaluations
    cmds = []
    for p in run_pairs:
        cmd = HPLUS_CMD % (experiment, p["input_file"], p["output_file"])
        cmds.append(cmd)
    execute_bms(cmds)
    # combine the results
    allResults = []
    headers = []
    for p in run_pairs:
        with open(p["output_file"]) as tsv_file:
            reader = csv.reader(tsv_file, delimiter="\t")
            lines = list(reader)
            headers = lines[0]
            allResults += lines[1:]
    with open(OUTPUT_FILE, "w") as final_table:
        writer = csv.writer(final_table, delimiter="\t", quoting=csv.QUOTE_ALL)
        writer.writerow(headers)
        for line in allResults:
            writer.writerow(line)
    print("wrote to %s" % OUTPUT_FILE)

def execute_bms(cmds):
    with Pool(processes=4) as pool:
        pool.map(os.system, cmds)

def main ():
    parser = argparse.ArgumentParser()
    parser.add_argument("experiment", help="what experiment to run")
    parser.add_argument("benchmarkfile", help="which benchmark file to run")
    args = parser.parse_args()

    print(args)
    with open(args.benchmarkfile) as f:
        bms = yaml.load(f)
        print(bms)
        run_benchmarks(args.experiment, bms)

if __name__ == '__main__':
    main()