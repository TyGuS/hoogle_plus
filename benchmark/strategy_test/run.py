#!/usr/bin/python3

from experiment import *

QUERY_FILE = "../suites/working.yml"
EXPERIMENT = [
    # OptionExperiment("notimeout-filter",   ["--disable-filter=False",  "--cnt=9999"], 180),
    # OptionExperiment("timeout-nofilter",   ["--disable-filter=True",   "--cnt=9999"], 60),
    OptionExperiment("timeout-filter",     ["--disable-filter=False",  "--cnt=9999"], 60),
]

def main():
    query_set = load_query_set(QUERY_FILE)
    for experiment in EXPERIMENT:
        experiment.create_result_dir()
        experiment.run_pool(query_set)


if __name__ == "__main__":
    main()