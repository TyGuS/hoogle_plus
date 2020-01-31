#!/usr/bin/python3

import csv
import yaml
import os
import shutil
import re
import argparse
from multiprocessing import Pool


BASE_CMD = 'echo "{count}" && timeout 120 stack exec -- hplus "{query}" --log=1 --cnt=5 {options} > {log_file}'
DEFAULT_QUERY_FILE = "benchmark/suites/working.yml"
LOG_DIR = "tmp/run_qual/logs/"
TSV_DIR = "tmp/run_qual/tsv/"
REPEATS = 1
NUM_POOLS = 2

def ensure_dir(directory):
    if not os.path.exists(directory):
        os.makedirs(directory)

class Variant():
    def __init__(self, name, options=""):
        self.name = name
        self.options = options

    @staticmethod
    def log_name(variant_name, query_name, directory):
        return os.path.join(directory, "%s+%s.log" % (variant_name, query_name))

    def run_cmd(self, progress_str, querydct, directory):
        qn = querydct["name"]
        query = querydct["query"]
        log_name = Variant.log_name(self.name, qn, directory)
        return BASE_CMD.format(count=progress_str, query=query, options=self.options, log_file=log_name)

    def log_to_dict(self, querydct, log_path):
        qn = querydct["name"]
        log_name = Variant.log_name(self.name, qn, log_path)

        with open(log_name) as f:
            lines = f.readlines()
            stats = lines[-2]
            solution = lines[-4]
            if "writeSolution" not in stats:
                # did not finish completely
                return
            obj = {}
            obj["Solution"] = solution
            if "****" not in lines[-3]:
                obj["Solution"] = "No Solution"
            reg_str = r'[a-zA-Z]+ = [^,]+|[a-zA-Z]+ = \[.*?\]|[a-zA-Z]+ = \".*?\"|[a-zA-Z]+ = \(.*?\)'
            res = re.findall(reg_str, stats)
            properties = map(lambda x: x.split('='), res)
            for p in properties:
                k, v = p[0].strip(), p[1].strip(' \"')
                if v[0] == '[':
                    v = v.strip('[]').split(',')
                obj[k] = v
            obj["name"] = qn
            obj["query"] = querydct["query"]
            transitions_regex = r"numOfTransitions = fromList \[(.*)\], num"
            res = re.findall(transitions_regex, stats)
            if res:
                last_transition = res[0].split(",")[-1][:-1]
                obj["numOfTransitions"] = last_transition
            return obj

    def log_solutions(self, querydct, log_path):
        qn = querydct["name"]
        log_name = Variant.log_name(self.name, qn, log_path)

        with open(log_name) as f:
            lines = f.readlines()
            obj = {}
            reg_str = r'^SOLUTION:\ ([a-zA-Z0-9\.\(\)\[\]\.\,\:\ ]+)\n'
            res = list(filter(lambda x: re.search(reg_str, x), lines))
            res = list(map(lambda x: re.search(reg_str, x).group(1), res))
            obj["name"] = qn
            obj["query"] = querydct["query"]
            obj["solutions"] = '\n'.join(res)
            return obj

    def write_tsv(self, idx, result_dict):
        tsv_name = "%s+%s.tsv" % (self.name, result_dict["name"])
        tsv_path = os.path.join(TSV_DIR, str(idx))
        ensure_dir(tsv_path)
        path = os.path.join(tsv_path, tsv_name)
        with open(path, "w") as f:
            writer = csv.DictWriter(f, fieldnames=result_dict.keys(), delimiter="\t", quoting=csv.QUOTE_ALL)
            writer.writeheader()
            writer.writerow(result_dict)

class Experiment():
    def __init__(self, variants):
        self.variants = variants

    def load_queries(self, benchmark_file):
        with open(benchmark_file) as f:
            self.queries = yaml.load(f)

    def setup(self):
        if not os.path.exists(LOG_DIR):
            os.makedirs(LOG_DIR)

    def run(self):
        cmds = []
        pairs = []
        for idx in range(REPEATS):
            out_dir = os.path.join(LOG_DIR, str(idx))
            ensure_dir(out_dir)
            for query in self.queries:
                for variant in self.variants:
                    pairs.append((query, out_dir, variant))
        pair_count = len(pairs)
        for idx in range(len(pairs)):
            (query, out_dir, variant) = pairs[idx]
            progress = "%s/%s" % (idx, pair_count)
            progress_str = "%s - %s - %s" % (progress, query["name"], variant.name)
            cmds.append(variant.run_cmd(progress_str, query, out_dir))

        with Pool(processes=NUM_POOLS) as pool:
            pool.map(os.system, cmds)

    def logs_to_csv(self):
        for idx in range(REPEATS):
            out_dir = os.path.join(LOG_DIR, str(idx))
            for query in self.queries:
                for variant in self.variants:
                    # result_dict = variant.log_to_dict(query, out_dir)
                    result_dict = variant.log_solutions(query, out_dir)
                    if result_dict is not None:
                        variant.write_tsv(idx, result_dict)

# Variant("tygarQ")
# Variant("nogar", "--use-refine=nogar")
# Variant("tygarQB5", "--stop-threshold=5")
# Variant("tygar0B5", "--stop-threshold=5 --use-refine=tygar0")

def main():
    parser = argparse.ArgumentParser(description='Collect top 5 solutions for queries for H+ within 120 seconds')
    parser.add_argument('--benchmarks', default=DEFAULT_QUERY_FILE, help ='path to benchmarks yml file')
    args = parser.parse_args()

    bmg = Experiment([
        # Variant("tygarQ"),
        # Variant("tygar0", "--use-refine=tygar0"),
        # Variant("tygarQB5", "--stop-refine=True --stop-threshold=5"),
        Variant("H", "--stop-refine=True --stop-threshold=10"),
        Variant("H-D", "--stop-refine=True --stop-threshold=10 --disable-demand"),
        Variant("H-R", "--stop-refine=True --stop-threshold=10 --disable-demand --disable-relevancy"),
        # Variant("tygarQB15", "--stop-refine=True --stop-threshold=15"),
        # Variant("tygarQB20", "--stop-refine=True --stop-threshold=20"),
        # Variant("nogar", "--use-refine=nogar"),
        ])
    bmg.load_queries(args.benchmarks)
    bmg.setup()
    bmg.run()
    bmg.logs_to_csv()

if __name__ == "__main__":
    main()