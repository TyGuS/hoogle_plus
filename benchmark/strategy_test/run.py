#!/usr/bin/python3

import yaml
import os
import time
import json
from util import QueryType, run_hplus, get_results 
from pathlib import Path

from multiprocessing import Pool
from os import path

DEFAULT_QUERY_FILE="../suites/working.yml"
DEFAULT_OUTPUT_DIR="output"

SCRIPT_DIR = os.path.dirname(os.path.realpath(__file__))

def compute_time_diff(times):
    return [round(t2 - t1, 2) for t1, t2 in zip(times[:-1], times[1:])]

def load_queries(file_name):
    with open(file_name) as f:
        return yaml.load(f, Loader=yaml.FullLoader)

class Experiment():
    def __init__(self, name, queries, options):
        self.name = name
        self.queries = queries
        self.options = options

        self.path = path.join(SCRIPT_DIR, DEFAULT_OUTPUT_DIR, name)

    def create_dir(self):
        os.makedirs(self.path, exist_ok=True)
    
    def run(self):
        with Pool(processes=8) as pool:
            for query in self.queries:
                pool.apply_async(self.run_query, args=(query,), error_callback=lambda x: print(x))
            pool.close()
            pool.join()
    
    def run_query(self, query):
        query_name = query['name']
        query = query['query']

        print("Running {} - {}: {}".format(self.name, query_name, query))
        query_input = json.dumps({'query': query, 'inExamples': []})
        process = run_hplus(["--json={}".format(query_input),
                            "--search-type={}".format(QueryType.search_programs.value),
                            "--cnt=20"] + self.options)

        times = [time.time()]
        results = []
        for line in get_results(process, QueryType.search_programs):
            times.append(time.time())
            results.append(line)
        times = compute_time_diff(times)

        with open(path.join(self.path, "{}.json".format(query_name)), 'w') as f:
            json.dump({'name': query_name, 'query': query, 'times': times, 'results': results}, f)


def main():
    os.makedirs(DEFAULT_OUTPUT_DIR, exist_ok=True)

    queries = load_queries(DEFAULT_QUERY_FILE)

    experiments = [
        Experiment("qc-all-filter", queries, ["--disable-filter=False"]), 
        Experiment("all-no-filter", queries, ["--disable-filter=True"])
    ]

    for expr in experiments:
        expr.create_dir()
        expr.run()


if __name__ == "__main__":
    main()