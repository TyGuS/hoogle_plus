import os
import signal
import subprocess
import re
import enum
from flask import json

from os.path import join

HPLUS_CMD = 'stack exec -- hplus'.split()
HPLUS_DIR = '../../'
OPTIONS = []
TIMEOUT_CMD = 'timeout'
TIMEOUT = 120
TIMEOUT_KILL = 5
SCRIPT_DIR = os.path.dirname(os.path.realpath(__file__))

class QueryType(enum.Enum):
    search_programs = 'SearchPrograms'
    search_types = 'SearchTypes'
    search_results = 'SearchResults'
    search_examples = 'SearchExamples'

def run_hplus(options):
    os.chdir(join(SCRIPT_DIR, HPLUS_DIR))

    command = [TIMEOUT_CMD, "-k", str(TIMEOUT_KILL), str(TIMEOUT)] + HPLUS_CMD + OPTIONS + options
    print(" ".join(command))

    return subprocess.Popen(command, stdout=subprocess.PIPE)

def get_results(process, query_type):
    for line in iter(process.stdout.readline, b''):
        if line[:8] == b'RESULTS:':
            result = json.loads(line[8:])['outCandidates']
            yield result

