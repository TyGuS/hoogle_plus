#!/usr/bin/python3

from experiment import *

QUERY_FILE = "../suites/working.yml"
EXPERIMENT = [
    # OptionExperiment("360-no-filter",   ["--disable-filter=True",   "--cnt=30"], 360),
    OptionExperiment("360-filter",      ["--disable-filter=False",  "--cnt=30"], 360),
]

def main():
    query_set = load_query_set(QUERY_FILE)
    for experiment in EXPERIMENT:
        experiment.create_result_dir()
        experiment.run_pool(query_set)


if __name__ == "__main__":
    main()