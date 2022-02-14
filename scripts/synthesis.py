import os
import re
import sys
import time
import pickle
import json
import subprocess
from multiprocessing import Pool
from colorama import init, Fore, Back, Style
from benchmark import *

HPLUS_CMD = ['stack', 'exec', '--', 'hplus']  # Command to call hoogle+
TIMEOUT_CMD = 'timeout'  # Timeout command
TIMEOUT = '300'  # Timeout value (seconds)
CMD_OPTS = ['--stop-refine', '--stop-threshold=10', '--disable-filter', '--cnt=10000']
LOGFILE = 'results.log'                                         # Log file
# Result serialization file
DUMPFILE = 'results.pkl'
# CSV-output file
CSV_FILE = 'result.tsv'
FNULL = open(os.devnull, 'w')


def format_time(t):
    if t < 0:
        return '-'
    else:
        return '{0:0.2f}'.format(t)


def run_benchmark(output_dir, results, name, query, examples, desired_solution, default_opts,
                  timeout=TIMEOUT, is_filtering=False):
    '''
        Run benchmark name with command-line options opts
        (use default_opts with running the common context variant);
        record results in the results dictionary
    '''
    # start the timer
    start = time.time()
    command = [TIMEOUT_CMD, TIMEOUT] + HPLUS_CMD + CMD_OPTS + [query] + default_opts
    result = None
    with subprocess.Popen(command, stdout=subprocess.PIPE, stderr=FNULL) as process:
        try:
            # output, unused_err = process.communicate()
            for line in iter(process.stdout.readline, b''):
                if line[:9] == b'SOLUTION:':
                    result = line[9:].decode().strip(' \n')
                    if result == desired_solution:
                        break
                    else:
                        print("they are different", result, desired_solution, len(result), len(desired_solution))

            process.kill()
        except subprocess.TimeoutExpired:
            process.kill()
            print(name, ':', query, Fore.YELLOW + 'TIMEOUT' + Style.RESET_ALL,
                  Fore.GREEN + 'OK' + Style.RESET_ALL, end='\n')

    end = time.time()
    # end the timer and print the consumed time
    print(format_time(end - start), end=' ')
    if not result:  # Synthesis failed
        print(Fore.RED + 'FAIL' + Style.RESET_ALL, end='\n')
        results[name] = SynthesisResult(name, end - start)
    else:  # Synthesis succeeded: code metrics from the output and record synthesis time
        print(Fore.GREEN + 'OK' + Style.RESET_ALL, end='\n')
        results[name] = SynthesisResult(name,
                                        format_time(end - start),
                                        result)


def write_csv(output_dir, groups, results):
    '''Generate CSV file from the results dictionary'''
    with open(os.path.join(output_dir, CSV_FILE), 'w+') as outfile:
        outfile.write('Name\tQuery\tTime\tSolution\n')
        for group in groups.values():
            for b in group.benchmarks:
                outfile.write(b.name + '\t')
                outfile.write(b.query + '\t')
                result = results[b.name]
                outfile.write(format_time(result.time) + '\t')
                outfile.write(result.solution)
                outfile.write('\n')


def run_synthesis(groups, output_dir, timeout=TIMEOUT, options=[], is_filtering=False):
    results = {}
    for group in groups.values():
        for b in group.benchmarks:
            if b.name in results:
                print(str(b) + ' ' + Fore.YELLOW +
                        Style.BRIGHT + 'SKIPPED' + Style.RESET_ALL)
            else:
                print('Running', str(b))
                run_benchmark(output_dir, results, b.name, b.query, [], b.desired_solution,
                                group.default_options + options, timeout, is_filtering)

    # Generate CSV if not testing the filtering algorithm
    if not is_filtering:
        write_csv(output_dir, groups, results)
