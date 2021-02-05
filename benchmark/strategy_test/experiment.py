#!/usr/bin/python3

import yaml
import json

from hplus import *

from os import makedirs
from os.path import join
from multiprocessing import Pool

RESULT_DIR = "output"
NUM_WORKER = 4

def load_query_set(file_name):
    with open(file_name, 'r') as f: return yaml.load(f, Loader=yaml.FullLoader)
def compute_time_diff(times): return [round(t2 - t1, 2) for t1, t2 in zip(times[:-1], times[1:])]

class OptionExperiment():
    def __init__(self, name, options, timeout):
        self.name       = name
        self.options    = options
        self.timeout    = timeout
        self.result_dir = join(SCRIPT_DIR, RESULT_DIR, name)

    def create_result_dir(self): makedirs(self.result_dir, exist_ok=True)

    def run_sync(self, query_set):
        for q in query_set: self.run_query(q)

    def run_pool(self, query_set):
        with Pool(processes=NUM_WORKER) as pool:
            # for q in query_set: pool.apply_async(self.run_query, args=(q,), error_callback=lambda x: print(x))
            pool.map(self.run_query, query_set)

    def run_query(self, query):
        query_name  = query['name']
        query_type  = query['query']
        query_json  = build_option_query_program(query_type)
        process     = run_hplus(self.options + query_json, self.timeout)
        
        times       = [time.time()]
        results     = []
        expr_data   = []

        for (result, data) in read_results(process):
            times.append(time.time())
            results.append(result)
            expr_data.append(data)
        times = compute_time_diff(times)

        with open(join(self.result_dir, f"{query_name}.json"), 'w') as file:
            json.dump({'name': query_name, 'query': query_type, 'time': times, 'result': results, 'expr_data': expr_data}, file)
