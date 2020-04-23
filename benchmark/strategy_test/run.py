#!/usr/bin/python3

import yaml
import os
import time
import json
from util import *
from pathlib import Path

DEFAULT_QUERY_FILE="study.yml"
DEFAULT_OUTPUT_DIR="output"

cwd = os.getcwd()

def load_experiments(file_name):
    with open(file_name) as f:
        return yaml.load(f, Loader=yaml.FullLoader)

def run_experiments(experiments):
    for exp in experiments: 
        query = json.dumps({'query': exp['query'], 'inExamples': []})
        proc = run_hplus(["--json={}".format(query),
                          "--search-type={}".format(QueryType.search_programs.value),
                          "--cnt=10"])
        
        start_time = time.time()
        times = [start_time]
        results = []
        for line in get_results(proc, QueryType.search_programs, "qid"):
            times.append(time.time())
            results.append(line)

        write_experiment(str(exp['name']), exp['query'], times, results)
        print("Experiment Successful: {}".format(exp['name']))

def write_experiment(name, query, times, results):
    with open(os.path.join(cwd, DEFAULT_OUTPUT_DIR, "{}.json".format(name)), 'w') as f:
        json.dump({'name': name, 'query': query, 'times': times, 'results': results}, f)


def main():
    os.makedirs(DEFAULT_OUTPUT_DIR, exist_ok=True)
    experiments = load_experiments(DEFAULT_QUERY_FILE)
    run_experiments(experiments)


if __name__ == "__main__":
    main()