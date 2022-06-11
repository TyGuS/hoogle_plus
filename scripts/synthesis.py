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
CMD_OPTS = ['--stop-refine', '--stop-threshold=10', '--disable-filter', '--cnt=10000', '--log=0']
LOGFILE = 'results.log'                                         # Log file
# Result serialization file
DUMPFILE = 'results.pkl'
# CSV-output file
CSV_FILE = 'result_ecta.tsv'
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
    # FIXME: pass examples here if you want to test with examples
    command = [TIMEOUT_CMD, TIMEOUT] + HPLUS_CMD + CMD_OPTS + \
        ['--json={{"query":"{}", "inExamples":[], "inArgNames": []}}'.format(
            query)] + default_opts
    # start the timer
    start = time.time()
    solution = None
    end = None
    with subprocess.Popen(command, stdout=subprocess.PIPE, stderr=FNULL) as process:
        try:
            # output, unused_err = process.communicate(timeout=timeout)
            for line in iter(process.stdout.readline, b''):
                # print(line)
                if line[:8] == b'RESULTS:':
                    candidates = json.loads(line[8:])['outCandidates']
                    if candidates:
                        outSolutions = [x['qualSolution'] for x in candidates]
                        if desired_solution in outSolutions:
                            solution = desired_solution
                            print(name, ':', query, Fore.GREEN + 'OK' + Style.RESET_ALL, end='\n')
                            end = time.time()
                            break

            process.kill()
        except subprocess.TimeoutExpired:
            process.kill()
            print(name, ':', query, Fore.YELLOW + 'TIMEOUT' + Style.RESET_ALL,
                  Fore.GREEN + 'OK' + Style.RESET_ALL, end='\n')
            output, unused_err = process.communicate()
    
    if end is None:
        end = time.time()

    # store synthesis results into json files
    if is_filtering:
        json_file = os.path.join(output_dir, '{}.json'.format(name))
        with open(json_file, 'w') as f:
            json.dump({'name': name, 'query': query,
                       'result': solution}, f)
    else:
        # end the timer and print the consumed time
        print(format_time(end - start), end=' ')
        if not solution:  # Synthesis failed
            print(Fore.RED + 'FAIL' + Style.RESET_ALL, end='\n')
            results[name] = SynthesisResult(name, end - start)
        else:  # Synthesis succeeded: code metrics from the output and record synthesis time
            print(Fore.GREEN + 'OK' + Style.RESET_ALL, end='\n')
            results[name] = SynthesisResult(name,
                                            format_time(end - start),
                                            solution)


def write_csv(output_dir, tsv_prefix, groups, results):
    '''Generate CSV file from the results dictionary'''
    with open(os.path.join(output_dir, tsv_prefix + ".tsv"), 'w+') as outfile:
        outfile.write('Name\tQuery\tTime\tSolution\n')
        for group in groups.values():
            for b in group.benchmarks:
                outfile.write(b.name + '\t')
                outfile.write(b.query + '\t')
                result = results[b.name]
                outfile.write(format_time(result.time) + '\t')
                outfile.write(result.solution)
                outfile.write('\n')


def run_synthesis(groups, output_dir, tsv_prefix, timeout=TIMEOUT, options=[], is_filtering=False):
    for i in range(1):
        results = {}
        for group in groups.values():
            for b in group.benchmarks:
                if b.name in results:
                    print(str(b) + ' ' + Fore.YELLOW +
                            Style.BRIGHT + 'SKIPPED' + Style.RESET_ALL)
                else:
                    print('Running', str(b))
                    run_benchmark(output_dir, results, b.name, b.query, b.examples, b.desired_solution,
                                    group.default_options + options, timeout, is_filtering)

        # Generate CSV if not testing the filtering algorithm
        if not is_filtering:
            write_csv(output_dir, tsv_prefix + "_" + str(i), groups, results)
