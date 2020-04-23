import os
import signal
import subprocess
import re
import enum
from flask import json

HPLUS_CMD = 'stack exec -- hplus'.split()
OPTIONS = ['--disable-filter=False']
TIMEOUT_CMD = 'timeout'
TIMEOUT = 60
TIMEOUT_KILL = 5
SCRIPT_DIR = os.path.dirname(os.path.realpath(__file__))

class QueryType(enum.Enum):
    search_programs = 'SearchPrograms'
    search_types = 'SearchTypes'
    search_results = 'SearchResults'
    search_examples = 'SearchExamples'


def run_hplus(options):
    # TODO: we may put these paths into configuration file
    os.chdir(os.path.join(SCRIPT_DIR, '../../'))
    command = [TIMEOUT_CMD, "-k", str(TIMEOUT_KILL), str(TIMEOUT)] + HPLUS_CMD + OPTIONS + options
    print(" ".join(command))
    process = subprocess.Popen(command, stdout=subprocess.PIPE)
    return process

def get_results(process, query_type, qid):
    for line in iter(process.stdout.readline, b''):
        print(line)
        if line[:8] == b'RESULTS:':
            result = json.loads(line[8:])['outCandidates']
            yield result

