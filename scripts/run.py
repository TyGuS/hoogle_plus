import os
import re
import sys
import time
import json
import pickle
import subprocess
from benchmark import *
from colorama import init, Fore, Back, Style
from inference import run_type_inference
from filtering import run_filtering
from synthesis import run_synthesis, LOGFILE

assert sys.version_info >= (3, 5)
DEFAULT_QUERY_FILE = 'benchmark/suites/working.yml'
USER_QUERY_FILE = 'benchmark/suites/user-data.yml'
HKTV_QUERY_FILE = 'benchmark/suites/hktv.yml'

def cmdline():
    import argparse
    a = argparse.ArgumentParser()
    # options for choosing benchmark set
    a.add_argument('--small', action='store_true',
                   help='Run a small set of the default benchmarks')
    a.add_argument('--full', action='store_true', default=True,
                   help='Run a full set of the default benchmarks')
    # options for input and output paths
    a.add_argument('--benchmark-suite', action='store',
                   default=DEFAULT_QUERY_FILE,
                   help='Provide the file path of the benchmark suite')
    a.add_argument('--benchmarks', nargs='+', default=[],
                   help='Run selected benchmarks with provided names')
    a.add_argument('--output-dir', action='store', default='./output',
                   help='Provide an output directory to put log files and results')
    # options for experiments
    a.add_argument('--type-inference', action='store_true',
                   help='Run the type inference evaluation')
    a.add_argument('--use-study-data', action='store_true',
                   help='Run the type inference from user-provided examples')
    a.add_argument('--filtering', action='store_true',
                   help='Run the program filtering evaluation')
    a.add_argument('--synthesis', action='store_true',
                   help='Run the synthesis procedure for timing analysis')
    a.add_argument('--oopsla', action='store_true',
                   help='Run experiments that related to OOPSLA20 submission')
    a.add_argument('--all', action='store_true',
                   help='Run all experiments')
    return a.parse_args()

if __name__ == '__main__':
    init()

    cl_opts = cmdline()

    # Delete old log file
    logfile = os.path.join(cl_opts.output_dir, LOGFILE)
    if os.path.isfile(logfile):
        os.remove(logfile)

    # load benchmarks
    if cl_opts.use_study_data:
        cl_opts.benchmark_suite = USER_QUERY_FILE
    groups = load_queries(cl_opts.benchmark_suite)

    # sanity check
    if cl_opts.use_study_data and \
       (not cl_opts.type_inference or cl_opts.synthesis or cl_opts.filtering \
        or cl_opts.benchmarks):
        raise Exception('study data can only be used for type inference')

    # Run experiments
    if cl_opts.small:
        groups = {'study': groups['study']}

    if cl_opts.benchmarks:
        customized_group = BenchmarkGroup('customized', [])
        for b in cl_opts.benchmarks:
            benchmark = find_benchmark_in_groups(b, groups)
            customized_group.add_benchmark(benchmark)
        groups = {'customized': customized_group}

    if cl_opts.synthesis or cl_opts.all:
        run_synthesis(groups, cl_opts.output_dir)

    if cl_opts.type_inference or cl_opts.oopsla or cl_opts.all:
        run_type_inference(cl_opts.benchmark_suite, groups, cl_opts.output_dir, cl_opts.use_study_data)

    if cl_opts.filtering or cl_opts.oopsla or cl_opts.all:
        run_filtering(groups, cl_opts.output_dir)

    if cl_opts.oopsla or cl_opts.all:
        groups = load_queries(USER_QUERY_FILE)
        run_type_inference(USER_QUERY_FILE, groups, cl_opts.output_dir, True)
