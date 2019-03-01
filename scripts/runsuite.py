#!/usr/bin/python3

import os
import signal
from multiprocessing import Pool
import subprocess
from subprocess import Popen, PIPE, TimeoutExpired
import re
import json
import time
import argparse
import functools
import math

QUERY_FILE = "scripts/queries/queries.json"

gen_scripts = {
    "10": "./scripts/10.sh",
    "20": "./scripts/20.sh",
    "50": "./scripts/50.sh",
    "100": "./scripts/100.sh",
    "153": "./scripts/153.sh",
    "153-tier1": "./scripts/153-tier1.sh",
    "153-tier1-no-tuple": "./scripts/153-tier1-no-tuple.sh",
    "449": "./scripts/449.sh",
    "icfp": "./scripts/icfp.sh",
    "icfp-tier2": "./scripts/icfp-tier2.sh",
}

DECIMAL_REGEX = "(\d+.?\d*)"

COMPONENTS_REGEX = "^INSTRUMENTED: Components: ([0-9]+)"
TRANSITIONS_REGEX = "^Number of transitions: \[(.*)\]"
TYPES_REGEX = "^Number of places: \[(.*)\]"
CREATING_REGEX = f"^Petri net construction time: {DECIMAL_REGEX}"
ENCODING_REGEX = f"^Petri net encoding time: {DECIMAL_REGEX}"
Z3_REGEX = f"^Z3 solving time: {DECIMAL_REGEX}"
REFINEMENT_NUM_REGEX = "^Total iterations of refinements: ([0-9]+)"
TYPE_CHECKING_REGEX = f"^Hoogle plus type checking time: {DECIMAL_REGEX}"
SOLUTION_TIME_REGEX = f"^Search time for solution: {DECIMAL_REGEX}"
SOLUTION_REGEX = "^SOLUTION: (.*)"
SOLUTION_LENGTH = "^Solution Depth: (.*)"

SOLUTION_DELIMINTER = "********************END STATISTICS****************************"

REGEX_MAP = {
    'z3': Z3_REGEX,
    'encoding': ENCODING_REGEX,
    'creation': CREATING_REGEX,
    'types': TYPES_REGEX,
    'transitions': TRANSITIONS_REGEX,
    'components': COMPONENTS_REGEX,
    'solution': SOLUTION_REGEX,
    'time': SOLUTION_TIME_REGEX,
    'typeChecking': TYPE_CHECKING_REGEX,
    'refinements': REFINEMENT_NUM_REGEX,
    'length': SOLUTION_LENGTH,
}

INT_REGEXES = ['length', 'components', 'refinements']
FLOAT_REGEXES = ['z3', 'encoding', 'creation', 'time', 'typeChecking']

OUTPUT_DIR = "output/script/"

EXEC_BASE = './scripts/runquery.sh'

DEFAULT_MODES = ["queryrefinement", "norefine", "abstractrefinement", "combination"]
DEFAULT_COMPONENT_SETS = ["icfp"]
DEFAULT_TIMEOUT = 300
DEFAULT_SOLUTIONS_PER_QUERY = 1


def truncate(number, digits) -> float:
    stepper = pow(10.0, digits)
    return math.trunc(stepper * number) / stepper

def process_output(outlines):
    lines = outlines.decode("utf-8").split("\n")
    run_characteristics = []
    new_characteristics, rest = process_solution(lines)
    if not new_characteristics == {}:
        run_characteristics.append(new_characteristics)
    while rest is not None:
        new_characteristics, rest = process_solution(rest)
        if not new_characteristics == {}:
            run_characteristics.append(new_characteristics)
    return run_characteristics


def process_solution(outlines):
    '''
    process through the outlines until we read the end of a solution segment
    :param outlines:
    :return:
    '''
    run_characteristics = {}
    for i, line in enumerate(outlines):
        if line == SOLUTION_DELIMINTER:
            return run_characteristics, outlines[i+1:]
        for key, regexp in REGEX_MAP.items():
            res = re.findall(regexp, line)
            if len(res) < 1:
                continue
            if key in FLOAT_REGEXES:
                run_characteristics[key] = truncate(float(res[0]), 2)
            if key in INT_REGEXES:
                run_characteristics[key] = int(res[0])
            if key == 'solution':
                run_characteristics[key] = res[0]
            if key in ['transitions', 'types']:
                run_characteristics[key] = [int(x) for x in res[0].split(',')]
    return run_characteristics, None


def generate_env(component_set):
    gen_script = gen_scripts[component_set]
    subprocess.run(gen_script.split(" "), stdout=subprocess.PIPE)


def run_query(args, query, mode, component_set):
    solutions = args.solutions
    timeout = args.timeout
    query_cmd = f"{EXEC_BASE} {mode} {solutions} '{query}'"
    shell_cmd = f"timeout {timeout} {query_cmd}"
    info = f"query: {query}; component set: {component_set}; mode: {mode}"
    try:
        print(info)
        output = subprocess.check_output(shell_cmd, shell=True)
        data = process_output(output)
        return data
    except subprocess.CalledProcessError as e:
        print("Called process error")
        print(e.output)
        data = process_output(e.output)
        if data == {}:
            return {"error": "timeout"}
        return data
    except Exception as ex:
        try:
            if ex.returncode == 124:
                # print(proc.communicate())
                return {"error": "timeout"}
            # print(proc.communicate())
            return {"error": str(ex)}
        except AttributeError:
            return {"error": str(ex)}


def get_queries(args):
    queries = []
    qf = args.query_file
    if not qf == "":
        with open(qf, 'r') as file:
            queries = json.load(file)
    if args.add_query is not None:
        for i, user_query in enumerate(args.add_query):
            queries.append({
                "name": "user_query_" + str(i),
                "query": user_query
            })
    return queries


def run_queries(args, query_obj):
    results = {}
    name = query_obj["name"]
    query = query_obj["query"]
    results[name] = {"query": query}
    for component in args.component_set:
        if len(args.component_set) > 1:
            generate_env(component)
        results[name][component] = {}
        for mode in args.modes:
            results[name][component][mode] = run_query(args, query, mode, component)
    yield {"name": name, "result": results[name]}

def write_results(query, results):
    if not os.path.exists(OUTPUT_DIR):
        os.mkdir(OUTPUT_DIR)
    result_str = json.dumps(results)
    file_path = os.path.join(OUTPUT_DIR, query + ".json")
    with open(file_path, 'w') as file:
        file.write(result_str)

def run_suite_for_query(args, query_obj):
    for result_dict in run_queries(args, query_obj):
        name = result_dict["name"]
        result = result_dict["result"]
        print(result_dict)
        write_results(name, result)

def mk_run_suite_for_query(args):
    def newfunc(query_obj):
        return run_suite_for_query(args, query_obj)
    return newfunc

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--modes", nargs="+",
                        choices=DEFAULT_MODES,
                        default=DEFAULT_MODES,
                        help="choose which modes to run. All enabled by default")
    parser.add_argument("--component-set", nargs="+", type=str,
                        choices=DEFAULT_COMPONENT_SETS,
                        default=DEFAULT_COMPONENT_SETS,
                        help="choose the component sets to run on. All enabled by default")
    parser.add_argument("--query-file", type=str,
                        default=QUERY_FILE,
                        help="Choose the query file to load and run on")
    parser.add_argument("--add-query", type=str, action="append",
                        help="Include your own query")
    parser.add_argument("-v", "--verbosity", action="count",
                        help="increase output verbosity, max vvv")
    parser.add_argument("--timeout", type=int,
                        default=DEFAULT_TIMEOUT,
                        help="set each query timeout in seconds")
    parser.add_argument("--solutions", type=int,
                        default=DEFAULT_SOLUTIONS_PER_QUERY,
                        help="Number of solutions per query for which to search")
    args = parser.parse_args()
    # print(run_cmd(["./scripts/runquery.sh", "foo", "bar", "baz"], 1))
    print(args)

    if len(args.component_set) == 1:
        generate_env(args.component_set[0])
        queries = get_queries(args)
        run_suite = functools.partial(run_suite_for_query, args)
        with Pool() as p:
            p.map(run_suite, queries)
    else:
        queries = get_queries(args)
        for query_obj in queries:
            run_suite_for_query(args, query_obj)


if __name__ == '__main__':
    main()
