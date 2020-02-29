import os
import subprocess
import enum
from flask import session

HPLUS_CMD = 'stack exec -- hplus'
OPTIONS = []
TIMEOUT_CMD = 'timeout'
TIMEOUT = '60'
DUMP_FILE = 'data/results'

class QueryType(enum.Enum):
    search_programs = 'SearchPrograms'
    search_types = 'SearchTypes'
    search_results = 'SearchResults'
    search_examples = 'SearchExamples'

def run_hplus(options, qid, use_stream = False):
    command = ''.join([HPLUS_CMD] + options)
    process = subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
    # store the pid to the current session
    session[qid] = process.pid
    if use_stream:
        for line in iter(process.stdout.readline, b''):
            sys.stdout.write(line)
            yield line
    else:
        out, err = process.communicate()
        return out
