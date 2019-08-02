#!/usr/bin/python3

import os
import sys
import time
from subprocess import call, check_output, STDOUT

HPLUS_CMD = 'stack exec -- hplus' # Command to call hoogle+
TIMEOUT_CMD = 'timeout' # Timeout command
TIMEOUT = '60' # Timeout value (seconds)
CMD_OPTS = ['--stop-refine', '--stop-threshold=10']

class Benchmark:
    def __init__(self, name, query, description, options=[]):
        self.name = name                # Id
        self.query = query              # Query signature
        self.description = description  # Description
        self.options = options          # Command-line options to use for this benchmark when running in individual context

    def str(self):
        return self.name + ': ' + self.description + ' ' + str(self.options)

class BenchmarkGroup:
    def __init__(self, name, default_options, benchmarks):
        self.name = name                        # Id
        self.default_options = default_options  # Command-line options to use for all benchmarks in this group when running in common context
        self.benchmarks = benchmarks            # List of benchmarks in this group

class SynthesisResult:
    def __init__(self, name, time):
        self.name = name                        # Benchmark name
        self.time = time                        # Synthesis time (seconds)
        self.variant_times = {                  # Synthesis times for Synquid variants:
                                'def': -3.0,         # default exploration bounds
                                'nrt': -3.0,         # round-trip checking disabled
                                'ncc': -3.0,         # consistency checks disabled
                                'nmus': -3.0,        # MUSFix disabled
                             }

    def str(self):
        return self.name + ', ' + '{0:0.2f}'.format(self.time)

def run_benchmark(name, opts, default_opts):
    '''Run benchmark name with command-line options opts (use default_opts with running the common context variant); record results in the results dictionary'''

    with open(LOGFILE, 'a+') as logfile:
        start = time.time()
        logfile.write(name + '\n')
        logfile.seek(0, os.SEEK_END)
        # Run Synquid on the benchmark:
        return_code = call([HPLUS_CMD] + CMD_OPTS + opts + query, stdout=logfile, stderr=logfile)
        end = time.time()

        print '{0:0.2f}'.format(end - start),
        if return_code: # Synthesis failed
            print Back.RED + Fore.RED + Style.BRIGHT + 'FAIL' + Style.RESET_ALL,
            results [name] = SynthesisResult(name, (end - start), '-', '-', '-', '-')
        else: # Synthesis succeeded: code metrics from the output and record synthesis time
            lastLines = os.popen("tail -n 4 %s" % LOGFILE).read().split('\n')
            goal_count = re.match(r"\(Goals: (\d+)\).*$", lastLines[0]).group(1)
            measure_count = re.match(r"\(Measures: (\d+)\).*$", lastLines[1]).group(1)
            spec_size = re.match(r"\(Spec size: (\d+)\).*$", lastLines[2]).group(1)
            solution_size = re.match(r"\(Solution size: (\d+)\).*$", lastLines[3]).group(1)
            results [name] = SynthesisResult(name, (end - start), goal_count, solution_size, spec_size, measure_count)
            print Back.GREEN + Fore.GREEN + Style.BRIGHT + 'OK' + Style.RESET_ALL,

        variant_options = [   # Command-line options to use for each variant of Synquid
            ('def', default_opts),
            ('nrt', opts + INCREMENTAL_OFF_OPT),
            ('ncc', opts + CONSISTENCY_OFF_OPT),
            ('nmus', opts + BFS_ON_OPT)
            ]

        # Run each variant:
        for (variant_id, opts) in variant_options:
            run_version(name, variant_id, opts, logfile)

        print
