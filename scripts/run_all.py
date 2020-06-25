#!/usr/bin/python3

import os
import re
import sys
import time
import yaml
import pickle
import subprocess
import json
from subprocess import call, check_output, STDOUT
from colorama import init, Fore, Back, Style

HPLUS_CMD = ['stack', 'exec', '--', 'hplus'] # Command to call hoogle+
TIMEOUT_CMD = 'timeout' # Timeout command
TIMEOUT = '60' # Timeout value (seconds)
CMD_OPTS = ['--stop-refine', '--stop-threshold=10', '--solver-name=z3smt']
LOGFILE = 'data/results.log'                                         # Log file
CSV_FILE = 'data/result.tsv'                                         # CSV-output file
DEFAULT_QUERY_FILE = "benchmark/suites/working.yml"
DUMPFILE = 'data/results'                                            # Result serialization file
FNULL = open(os.devnull, 'w')

class Benchmark:
    def __init__(self, name, query, example, options=[]):
        self.name = name                # Id
        self.query = query              # Query signature
        self.example = example          # Examples
        self.options = options          # Command-line options to use for this benchmark when running in individual context

    def str(self):
        return self.name + ': ' + self.query

class BenchmarkGroup:
    def __init__(self, name, default_options):
        self.name = name                        # Id
        self.default_options = default_options  # Command-line options to use for all benchmarks in this group when running in common context
        self.benchmarks = []

class SynthesisResult:
    def __init__(self, name, time, solution, encoding_time, solver_time, refine_time, checker_time, path_len):
        self.name = name                        # Benchmark name
        self.time = float(time)                        # Synthesis time (seconds)
        self.solution = solution
        self.encoding_time = encoding_time
        self.solver_time = solver_time
        self.refine_time = refine_time
        self.checker_time = checker_time
        self.length = path_len
        self.variant_times = {                  # Synthesis times for Synquid variants:
                                'def': -3.0         # default exploration bounds
                             }

    def str(self):
        return self.name + ', ' + '{0:0.2f}'.format(self.time)

def run_benchmark(name, query, examples, default_opts):
    '''Run benchmark name with command-line options opts (use default_opts with running the common context variant); record results in the results dictionary'''

    with open(LOGFILE, 'a+') as logfile:
        logfile.write(name + '\n')
        logfile.seek(0, os.SEEK_END)
        # Run Synquid on the benchmark:
        # return_code = call([TIMEOUT_CMD] + [TIMEOUT] + HPLUS_CMD + CMD_OPTS + opts + [query], stdout=logfile, stderr=logfile)
        command = HPLUS_CMD + CMD_OPTS + ['--json={{"query":"{}", "inExamples":{}}}'.format(query, json.dumps(examples))]
        # print(command)

        start = time.time()
        try:
            output = check_output(command, stderr=STDOUT, timeout=60)
        except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
            output = b""
        end = time.time()

        logfile.write(output.decode('utf-8') + '\n')
        solution = None
        for line in output.split(b'\n'):
            # print(line)
            if line[:8] == b'RESULTS:':
                solution = json.loads(line[8:])

        print('{0:0.2f}'.format(end - start), end=' ')

        if not solution or solution['outError']: # Synthesis failed
            print(Fore.RED + 'FAIL' + Style.RESET_ALL, end='\n')
            results[name] = SynthesisResult(name, (end - start), '-', '-', '-', '-', '-', '-')
        else: # Synthesis succeeded: code metrics from the output and record synthesis time
            results[name] = SynthesisResult(name, '{0:0.2f}'.format(end - start), solution['outCandidates'][0]['solution'], None, None, None, None, None)
            print(Fore.GREEN + 'OK' + Style.RESET_ALL, end='\n')


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
                if isinstance(result.solution, str):
                    outfile.write (result.solution)
                else:
                    outfile.write (result.solution['outCandidates'][0]['solution'])
                # outfile.write (format_time(result.variant_times['def']) + ',')
                outfile.write ('\n')

def cmdline():
    import argparse
    a = argparse.ArgumentParser()
    a.add_argument('--medium', action='store_true')
    a.add_argument('--small', action='store_true')
    return a.parse_args()

def load_queries():
    group_map = dict()
    with open(DEFAULT_QUERY_FILE) as f:
        queries = yaml.full_load(f)
        for q in queries:
            # print(q['name'])
            # print(json.dumps(q['example']))
            group_name = q['source']
            if group_name not in group_map:
                group_map[group_name] = BenchmarkGroup(group_name, [])
            group = group_map[group_name]
            group.benchmarks.append(Benchmark(q['name'], q['query'], q['example']))
    return group_map.values()

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
    # groups = ALL_BENCHMARKS[:1] if cl_opts.small else ALL_BENCHMARKS
    groups = load_queries()

    for group in groups:
        for b in group.benchmarks:
            if b.name in results:
                print(b.str() + ' ' + Fore.YELLOW + 'SKIPPED' + Style.RESET_ALL)
            else:
                print(b.str(), end=' ')
                run_benchmark(b.name, b.query, b.example, group.default_options)
                with open(DUMPFILE, 'wb') as data_dump:
                    pickle.dump(results, data_dump)

    # Generate CSV
    write_csv()
