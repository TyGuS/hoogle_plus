import os
import subprocess
import re
import json
import time

queries = {
    "int_to_bytestring": "Int64 -> ByteString",
    "firstMaybe": "a -> List (Maybe a) -> a",
    "conjunctMaybe": "List (Maybe Bool) -> Bool",
}

TMP_BASE = "/tmp/base.txt"

CLEAR_SCRIPT = "rm " + TMP_BASE

gen_scripts = {
    10: "./generate_test.sh test10.txt",
    20: "./generate_test.sh test20.txt",
    50: "./generate_test.sh test50.txt",
    100: "./generate_test.sh test100.txt",
    189: CLEAR_SCRIPT + " && ./generate_189.sh",
}

COMPONENTS_REGEX = "^INSTRUMENTED: Components: ([0-9]+)"
TRANSITIONS_REGEX = "^INSTRUMENTED: transitions: ([0-9]+)"
TYPES_REGEX = "^INSTRUMENTED: Types in graph: ([0-9]+)"
CREATING_REGEX = "^INSTRUMENTED: time spent creating graph: (\d+\.*\d*)s"
ENCODING_REGEX = "^INSTRUMENTED: encoding time: (\d+\.*\d*)s"
Z3_REGEX = "^INSTRUMENTED: Z3 time: (\d+\.*\d*)s"

REGEX_MAP = {
    'z3': Z3_REGEX,
    'encoding': ENCODING_REGEX,
    'creation': CREATING_REGEX,
    'types': TYPES_REGEX,
    'transitions': TRANSITIONS_REGEX,
    'components': COMPONENTS_REGEX,
}

OUTPUT_DIR = "output/script/"

EXEC_BASE = 'stack exec -- synquid synthesis --path="PetriNet" '


def process_output(outlines):
    lines = outlines.decode("utf-8").split("\n")
    run_characteristics = {}
    for l in lines:
        line = l.strip()
        for key, regexp in REGEX_MAP.items():
            res = re.findall(regexp, line)
            if len(res) > 0:
                num_result = float(res[0])
                if key not in run_characteristics:
                    run_characteristics[key] = []
                run_characteristics[key] = run_characteristics[key] + [num_result]
    return run_characteristics



def run_query(query, gen, num):
    os.system(gen)  # Run the gen script
    print(query, gen, num)
    complete_query = EXEC_BASE + '"' + query + '"'
    print(complete_query)
    try:
        start_time = time.perf_counter()
        output = subprocess.check_output(complete_query, shell=True, timeout=300)
        end_time = time.perf_counter()
        data = process_output(output)
        data['time'] = end_time - start_time
        return data
    except subprocess.TimeoutExpired:
        return {"error": "timeout"}


def write_results(query, results):
    if not os.path.exists(OUTPUT_DIR):
        os.mkdir(OUTPUT_DIR)
    result_str = json.dumps(results)
    file_path = os.path.join(OUTPUT_DIR, query + ".json")
    with open(file_path, 'w') as file:
        file.write(result_str)


def main():
    results = {}
    for name, query in queries.items():
        results[name] = {}
        for key, script in gen_scripts.items():
            results[name][key] = run_query(query, script, key)
        write_results(name, results[name])

if __name__ == '__main__':
   main()
