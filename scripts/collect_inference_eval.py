import os
import subprocess
import csv
import json
from benchmark import Benchmark

TEST_LOG_FILE = 'inference.tsv'
DEFAULT_INFERENCE_LOG = 'output/inference.tsv'

class InferenceResult:
    def __init__(self, benchmark, gen_examples, inf_types, correct_rank):
        self.benchmark = benchmark
        self.gen_examples = gen_examples
        self.inf_types = inf_types
        self.rank = correct_rank

    def add_example(self, example):
        self.gen_examples.append(example)

    def add_type(self, typ):
        self.inf_types.append(typ)

    def __str__(self):
        return '{benchmark: ' + str(self.benchmark) + \
               ', examples: ' + str(self.gen_examples) + \
               ', inferred types: ' + str(self.inf_types) + \
               ', rank: ' + str(self.rank) + '}'

    def __repr__(self):
        return self.__str__()

def parse_inference_results(benchmarks, file_path = DEFAULT_INFERENCE_LOG):
    results = []
    with open(file_path, 'r') as f:
        log_reader = csv.DictReader(f, delimiter='\t')
        curr_result = None
        for row in log_reader:
            name = row['bm_name']
            if name == '':
                curr_result.add_example(row['gen_exs'])
                curr_result.add_type(row['inf_typs'])
            else:
                # add the result into the results list
                if curr_result:
                    results.append(curr_result)

                
                benchmark = next(g.contains(name) for g in benchmarks.values() if g.contains(name))
                assert benchmark, \
                    f'Benchmark name {name} does not match that in the log file, please check'
                gen_examples = [row['gen_exs']]
                inf_typs = [row['inf_typs']]
                rank = int(row['rank']) if row['rank'] != 'NO ANSWER' else 11
                curr_result = InferenceResult(benchmark, gen_examples, inf_typs, rank)

    return results