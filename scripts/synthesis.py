import os
import re
import sys
import time
import pickle
import json
import subprocess
from colorama import init, Fore, Back, Style
from benchmark import *

HPLUS_CMD = ['stack', 'exec', '--', 'hplus'] # Command to call hoogle+
TIMEOUT_CMD = 'timeout' # Timeout command
TIMEOUT = '60' # Timeout value (seconds)
CMD_OPTS = ['--stop-refine', '--stop-threshold=10']
LOGFILE = 'results.log'                                         # Log file
DUMPFILE = 'results.pkl'                                        # Result serialization file
CSV_FILE = 'result.tsv'                                         # CSV-output file
FNULL = open(os.devnull, 'w')

def format_time(t):
    if t < 0:
        return '-'
    else:
        return '{0:0.2f}'.format(t)

def run_benchmark(output_dir, name, query, examples, default_opts,
                  timeout = TIMEOUT, is_filtering = False):
    '''
        Run benchmark name with command-line options opts
        (use default_opts with running the common context variant);
        record results in the results dictionary
    '''
    log_path = os.path.join(output_dir, LOGFILE)
    with open(log_path, 'a+') as logfile:
        logfile.write(name + '\n')
        logfile.seek(0, os.SEEK_END)
        # run Hoogle+ on the benchmark:
        command = HPLUS_CMD + CMD_OPTS + ['--json={{"query":"{}", "inExamples":{}}}'.format(query, json.dumps(examples))] + default_opts
        # start the timer
        start = time.time()
        try:
            output = subprocess.check_output(command, stderr=subprocess.STDOUT,
                                             timeout=timeout)
        except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
            output = b""
        end = time.time()
        # end the timer and print the consumed time
        print(format_time(end - start), end=' ')

        solution = []
        for line in output.split(b'\n'):
            # print(line)
            if line[:8] == b'RESULTS:':
                solution.append(json.loads(line[8:]))

        if not solution or solution['outError']: # Synthesis failed
            print(Fore.RED + 'FAIL' + Style.RESET_ALL, end='\n')
            results[name] = SynthesisResult(name, end - start)
        else: # Synthesis succeeded: code metrics from the output and record synthesis time
            print(Fore.GREEN + 'OK' + Style.RESET_ALL, end='\n')
            results[name] = SynthesisResult(name,
                                            format_time(end - start),
                                            solution[0]['outCandidates'][0]['solution'])

            # store synthesis results into json files
            if is_filtering:
                json_file = path.join(output_dir, '{}.json'.format(name))
                with open(json_file, 'w') as f:
                    json.dump({'name': name, 'query': query, 'results': results}, f)

def write_csv(output_dir):
    '''Generate CSV file from the results dictionary'''
    with open(os.path.join(output_dir, CSV_FILE), 'w+') as outfile:
        outfile.write ('Name\tQuery\tTime\tSolution\n')
        for group in groups:
            for b in group.benchmarks:
                outfile.write (b.name + '\t')
                outfile.write (b.query + '\t')
                result = results [b.name]
                outfile.write (format_time(result.time) + '\t')
                outfile.write (result.solution)
                outfile.write ('\n')

def run_synthesis(groups, output_dir, timeout, is_filtering):
    # Check if there are serialized results
    dumpfile = os.path.join(output_dir, DUMPFILE)
    if os.path.isfile(dumpfile) and os.path.getsize(dumpfile) > 0:
        results = pickle.load(open(dumpfile, 'rb'))
    else:
        results = {}

    for group in groups:
        for b in group.benchmarks:
            if b.name in results:
                print(str(b) + ' ' + Fore.YELLOW + Style.BRIGHT + 'SKIPPED' + Style.RESET_ALL)
            else:
                print(str(b), end=' ')
                run_benchmark(output_dir, b.name, b.query, b.options,
                              group.default_options, timeout, is_filtering)
                if not is_filtering:
                    with open(dumpfile, 'wb+') as data_dump:
                        pickle.dump(results, data_dump)

    # Generate CSV if not testing the filtering algorithm
    if not is_filtering:
        write_csv(output_dir)
