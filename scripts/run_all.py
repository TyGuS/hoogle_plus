#!/usr/bin/python3

import os
import re
import sys
import time
import pickle
import json
import subprocess
from colorama import init, Fore, Back, Style
from benchmark import load_queries, SynthesisResult

HPLUS_CMD = ['stack', 'exec', '--', 'hplus'] # Command to call hoogle+
TIMEOUT_CMD = 'timeout' # Timeout command
TIMEOUT = '60' # Timeout value (seconds)
CMD_OPTS = ['--stop-refine', '--stop-threshold=10']
LOGFILE = 'data/results.log'                                         # Log file
CSV_FILE = 'data/result.tsv'                                         # CSV-output file
DEFAULT_QUERY_FILE = "benchmark/suites/working.yml"
DUMPFILE = 'data/results'                                            # Result serialization file
FNULL = open(os.devnull, 'w')

def run_benchmark(name, query, examples, default_opts):
    '''Run benchmark name with command-line options opts (use default_opts with running the common context variant); record results in the results dictionary'''

    with open(LOGFILE, 'a+') as logfile:
        logfile.write(name + '\n')
        logfile.seek(0, os.SEEK_END)
        # run Hoogle+ on the benchmark:
        command = HPLUS_CMD + CMD_OPTS + ['--json={{"query":"{}", "inExamples":{}}}'.format(query, json.dumps(examples))]
        # start the timer
        start = time.time()
        try:
            output = subprocess.check_output(command, stderr=subprocess.STDOUT, timeout=TIMEOUT)
        except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
            output = b""
        end = time.time()
        # end the timer and print the consumed time
        print('{0:0.2f}'.format(end - start), end=' ')

        solution = None
        for line in output.split(b'\n'):
            # print(line)
            if line[:8] == b'RESULTS:':
                solution = json.loads(line[8:])

        if not solution or solution['outError']: # Synthesis failed
            print(Fore.RED + 'FAIL' + Style.RESET_ALL, end='\n')
            results[name] = SynthesisResult(name, end - start)
        else: # Synthesis succeeded: code metrics from the output and record synthesis time
            print(Fore.GREEN + 'OK' + Style.RESET_ALL, end='\n')
            results[name] = SynthesisResult(name, '{0:0.2f}'.format(end - start), solution['outCandidates'][0]['solution'])

def format_time(t):
    if t < 0:
        return '-'
    else:
        return '{0:0.2f}'.format(t)

def write_csv():
    '''Generate CSV file from the results dictionary'''
    with open(CSV_FILE, 'w') as outfile:
        outfile.write ('Name\tQuery\tTime\tSolution\n')
        for group in groups:
            for b in group.benchmarks:
                outfile.write (b.name + '\t')
                outfile.write (b.query + '\t')
                result = results [b.name]
                outfile.write (format_time(result.time) + '\t')
                outfile.write (result.solution)
                outfile.write ('\n')

def cmdline():
    import argparse
    a = argparse.ArgumentParser()
    a.add_argument('--medium', action='store_true')
    a.add_argument('--small', action='store_true')
    return a.parse_args()

if __name__ == '__main__':
    init()

    cl_opts = cmdline()

    # Check if there are serialized results
    if os.path.isfile(DUMPFILE) and os.path.getsize(DUMPFILE) > 0:
        results = pickle.load(open(DUMPFILE, 'rb'))
    else:
        results = dict()

    # Delete old log file
    if os.path.isfile(LOGFILE):
        os.remove(LOGFILE)

    # Run experiments
    groups = load_queries(DEFAULT_QUERY_FILE)

    for group in groups:
        for b in group.benchmarks:
            if b.name in results:
                print(b.str() + ' ' + Fore.YELLOW + Style.BRIGHT + 'SKIPPED' + Style.RESET_ALL)
            else:
                print(b.str(), end=' ')
                run_benchmark(b.name, b.query, b.options, group.default_options)
                with open(DUMPFILE, 'wb') as data_dump:
                    pickle.dump(results, data_dump)

    # Generate CSV
    write_csv()