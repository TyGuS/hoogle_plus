#!/usr/bin/python3

import os
import re
import sys
import time
import yaml
import pickle
from subprocess import call, check_output, STDOUT
from colorama import init, Fore, Back, Style

HPLUS_CMD = 'hplus' # Command to call hoogle+
TIMEOUT_CMD = 'timeout' # Timeout command
TIMEOUT = '60' # Timeout value (seconds)
CMD_OPTS = ['--stop-refine', '--stop-threshold=10', '--disable-filter']
LOGFILE = 'data/results.log'                                         # Log file
CSV_FILE = 'data/result.tsv'                                         # CSV-output file
DEFAULT_QUERY_FILE = "benchmark/suites/working.yml"
DUMPFILE = 'data/results'                                            # Result serialization file

class Benchmark:
    def __init__(self, name, query, description, options=[]):
        self.name = name                # Id
        self.query = query              # Query signature
        self.description = description  # Description
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

def run_benchmark(name, query, opts, default_opts):
    '''Run benchmark name with command-line options opts (use default_opts with running the common context variant); record results in the results dictionary'''

    with open(LOGFILE, 'a+') as logfile:
        start = time.time()
        logfile.write(name + '\n')
        logfile.seek(0, os.SEEK_END)
        # Run Synquid on the benchmark:
        return_code = call([TIMEOUT_CMD] + [TIMEOUT] + [HPLUS_CMD] + CMD_OPTS + opts + [query], stdout=logfile, stderr=logfile)
        end = time.time()

        print('{0:0.2f}'.format(end - start), end=' ')
        if return_code: # Synthesis failed
            print(Fore.RED + Style.BRIGHT + 'FAIL' + Style.RESET_ALL, end='\n')
            results[name] = SynthesisResult(name, (end - start), '-', '-', '-', '-', '-', '-')
        else: # Synthesis succeeded: code metrics from the output and record synthesis time
            lastLines = os.popen("tail -n 12 %s" % LOGFILE).read().split('\n')
            solution = re.match(r"^SOLUTION: (.+)$", lastLines[0]).group(1)
            encoding_time = re.match(r"Encoding time: (\d+.*)$", lastLines[2]).group(1)
            solver_time = re.match(r"Solver time: (\d+.*)$", lastLines[3]).group(1)
            refine_time = re.match(r"Refine time: (\d+.*)$", lastLines[4]).group(1)
            checker_time = re.match(r"Checker time: (\d+.*)$", lastLines[5]).group(1)
            total_time = re.match(r"Total time: (\d+.*)$", lastLines[6]).group(1)
            path_len = re.match(r"Path length: (\d+)$", lastLines[7]).group(1)
            results[name] = SynthesisResult(name, total_time, solution, encoding_time, solver_time, refine_time, checker_time, path_len)
            print(Fore.GREEN + Style.BRIGHT + 'OK' + Style.RESET_ALL, end='\n')

        # variant_options = [   # Command-line options to use for each variant of Synquid
        #     ('def', default_opts)
        #     ]

        # Run each variant:
        # for (variant_id, opts) in variant_options:
        #     run_version(name, query, variant_id, opts, logfile)

def run_version(name, query, variant_id, variant_opts, logfile):
    '''Run benchmark name using command-line options variant_opts and record it as a Synquid variant variant_id in the results dictionary'''

    start = time.time()
    logfile.seek(0, os.SEEK_END)
    # Run Synquid on the benchmark, mute output:
    return_code = call([TIMEOUT_CMD] + [TIMEOUT] + [HPLUS_CMD] + CMD_OPTS +
        variant_opts + [query], stdout=FNULL, stderr=STDOUT)
    end = time.time()

    print('{0:0.2f}'.format(end - start), end='')
    if return_code == 124:  # Timeout: record timeout
      print(Fore.RED + Style.BRIGHT + 'TIMEOUT' + Style.RESET_ALL, end='')
      results[name].variant_times[variant_id] = -1
    elif return_code: # Synthesis failed: record failure
      print(Fore.RED + Style.BRIGHT + 'FAIL' + Style.RESET_ALL, end='')
      results[name].variant_times[variant_id] = -2
    else: # Synthesis succeeded: record time for variant
      results[name].variant_times[variant_id] = (end - start)
      print(Fore.GREEN + Style.BRIGHT + 'OK' + Style.RESET_ALL, end='')

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
                # outfile.write (format_time(result.variant_times['def']) + ',')
                outfile.write ('\n')

def write_latex():
    '''Generate Latex table from the results dictionary'''
    pass
    # total_count = 0
    # to_def = 0
    # to_nrt = 0
    # to_ncc = 0
    # to_nmus = 0

    # with open(LATEX_FILE, 'w') as outfile:
    #     for group in groups:
    #         outfile.write ('\multirow{')
    #         outfile.write (str(group.benchmarks.__len__()))
    #         outfile.write ('}{*}{\\parbox{1cm}{\vspace{-0.85\baselineskip}\center{')
    #         outfile.write (group.name)
    #         outfile.write ('}}}')

    #         for b in group.benchmarks:
    #             result = results [b.name]
    #             row = \
    #                 ' & ' + b.description +\
    #                 ' & ' + result.goal_count +\
    #                 ' & ' + b.components + \
    #                 ' & ' + result.measure_count + \
    #                 ' & ' + result.spec_size + \
    #                 ' & ' + result.code_size + \
    #                 ' & ' + format_time(result.time) + \
    #                 ' & ' + format_time(result.variant_times['def']) + \
    #                 ' & ' + format_time(result.variant_times['nrt']) + \
    #                 ' & ' + format_time(result.variant_times['ncc']) + \
    #                 ' & ' + format_time(result.variant_times['nmus']) + ' \\\\'
    #             outfile.write (row)
    #             outfile.write ('\n')

    #             total_count = total_count + 1
    #             if result.variant_times['def'] < 0.0:
    #                to_def = to_def + 1

    #         outfile.write ('\\hline')

    # print 'Total:', total_count
    # print 'TO def:', to_def

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
            group_name = q['source']
            if group_name not in group_map:
                group_map[group_name] = BenchmarkGroup(group_name, [])
            group = group_map[group_name]
            group.benchmarks.append(Benchmark(q['name'], q['query'], ''))
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
                print(b.str() + ' ' + Fore.YELLOW + Style.BRIGHT + 'SKIPPED' + Style.RESET_ALL)
            else:
                print(b.str(), end=' ')
                run_benchmark(b.name, b.query, b.options, group.default_options)
                with open(DUMPFILE, 'wb') as data_dump:
                    pickle.dump(results, data_dump)

    # Generate CSV
    write_csv()
    # Generate Latex table
    # write_latex()

    # Compare with previous solutions and print the diff
    # if os.path.isfile(ORACLE_FILE) and (not cl_opts.small):
    #     fromlines = open(ORACLE_FILE).readlines()
    #     tolines = open(LOGFILE, 'U').readlines()
    #     diff = difflib.unified_diff(fromlines, tolines, n=0)
    #     print
    #     sys.stdout.writelines(diff)
