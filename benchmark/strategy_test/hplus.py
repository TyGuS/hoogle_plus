#!/usr/bin/python3

from os import chdir
from os.path import join, dirname, realpath

import subprocess as sp

import io
import json
import time

HPLUS_CMD = ["stack", "exec", "--", "hplus"]
HPLUS_DIR = "../../"
SCRIPT_DIR = dirname(realpath(__file__))

DEFAULT_TIMEOUT     = 30
DEFAULT_TIMEOUT_KIL = 5
DEFAULT_TIMEOUT_CMD   = lambda timeout: ["gtimeout", "-k", str(DEFAULT_TIMEOUT_KIL), str(timeout)]

def run_hplus(options, timeout=DEFAULT_TIMEOUT):
    chdir(join(SCRIPT_DIR, HPLUS_DIR))

    commands = DEFAULT_TIMEOUT_CMD(timeout) + HPLUS_CMD + options
    print(" ".join(commands))

    return sp.Popen(commands, stdout=sp.PIPE, stderr=sp.PIPE)

def dump_stderrs(proc):
    for line in io.TextIOWrapper(proc.stderr, encoding="utf-8"):
        print(line)

def read_results(proc):
    latest_experiment_data = []
    for line in io.TextIOWrapper(proc.stdout, encoding="utf-8"):
        prefix = line[:8]
        suffix = line[8:]
        if prefix == 'RESULTS:':
            try:    yield (json.loads(suffix)['outCandidates'], latest_experiment_data)
            except: yield ({'error': '???'}, latest_experiment_data)
        elif prefix == 'EXPRMTS:':
            try:    latest_experiment_data = json.loads(suffix)
            except: continue

def build_option_query_program(signature):
    query_json = json.dumps({"query": signature, "inExamples": []})
    return ["--json", query_json]