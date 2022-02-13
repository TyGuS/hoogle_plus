#!/usr/bin/python3

import csv
import yaml
import os
import shutil
import re
import argparse
from multiprocessing import Pool


BASE_CMD = 'echo "{count}" && timeout 60 stack exec -- hplus "{query}" --log=1 {options} > {log_file}'
DEFAULT_QUERY_FILE = "benchmark/suites/working.yml"
LOG_DIR = "tmp/run_each/logs/"
TSV_DIR = "tmp/run_each/tsv/"
REPEATS = 3
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
            try:
                lines = f.readlines()
                stats = lines[-2]
                solution = lines[-4]
                obj = {}
                obj["name"] = qn
                obj["query"] = querydct["query"]
                if "writeSolution" not in stats:
                    # did not finish completely
                    return obj
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
                transitions_regex = r"numOfTransitions = fromList \[(.*)\], num"
                res = re.findall(transitions_regex, stats)
                if res:
                    last_transition = res[0].split(",")[-1][:-1]
                    obj["numOfTransitions"] = last_transition
                return obj
            except (AttributeError, IndexError):
                return None

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

    def load_queries(self, benchmarks_file):
        with open(benchmarks_file) as f:
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
            ensure_dir(out_dir)
            for query in self.queries:
                for variant in self.variants:
                    result_dict = variant.log_to_dict(query, out_dir)
                    if result_dict is not None:
                        variant.write_tsv(idx, result_dict)

# Variant("tygarQ")
# Variant("nogar", "--use-refine=nogar")
# Variant("tygarQB5", "--stop-threshold=5")
# Variant("tygar0B5", "--stop-threshold=5 --use-refine=tygar0")

def main():
    parser = argparse.ArgumentParser(description='Run benchmarks for H+ 3 times.')
    parser.add_argument('--benchmarks', default=DEFAULT_QUERY_FILE, help ='path to benchmarks yml file')
    args = parser.parse_args()

    bmg = Experiment([
        Variant("tygarQ"),
        Variant("tygar0", "--use-refine=tygar0"),
        Variant("tygarQB5", "--stop-refine=True --stop-threshold=5"),
        Variant("tygarQB10", "--stop-refine=True --stop-threshold=10"),
        Variant("tygarQB15", "--stop-refine=True --stop-threshold=15"),
        Variant("tygarQB20", "--stop-refine=True --stop-threshold=20"),
        Variant("nogar", "--use-refine=nogar"),
        ])
    bmg.load_queries(args.benchmarks)
    bmg.setup()
    bmg.run()
    bmg.logs_to_csv()

if __name__ == "__main__":
    main()
