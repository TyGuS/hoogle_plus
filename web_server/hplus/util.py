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
SCRIPT_DIR = os.path.dirname(os.path.realpath(__file__))
FNULL = open(os.devnull, 'w')

class QueryType(enum.Enum):
    search_programs = 'SearchPrograms'
    search_types = 'SearchTypes'
    search_results = 'SearchResults'
    search_examples = 'SearchExamples'

def to_fe_doc(doc):
    obj = {}
    obj['name'] = doc['functionName']
    obj['signature'] = doc['functionSig']
    obj['doc'] = doc['functionDesc']
    return obj

def to_fe_entry(e):
    obj = {}
    obj['code'] = e['solution']
    obj['examples'] = e['outExamples']
    return obj

def build_object(query_type, result, qid = None):
    if query_type is QueryType.search_programs:
        return {
            'id': qid,
            'candidates': list(map(to_fe_entry, result['outCandidates'])),
            'error': result['outError'],
            'docs': list(map(to_fe_doc, result['outDocs']))
        }
    elif query_type is QueryType.search_types:
        return {
            'typeCandidates': result['examplesOrTypes'],
            'error': result['tqError']
        }
    elif query_type is QueryType.search_examples:
        return {
            'examples': result['examplesOrTypes'],
            'error': result['tqError']
        }
    elif query_type is QueryType.search_results:
        return {
            'result': result['execResult'],
            'error': result['execError']
        }
    else:
        raise Exception

def run_hplus(options):
    # TODO: we may put these paths into configuration file
    os.chdir(os.path.join(SCRIPT_DIR, '../../'))
    command = [TIMEOUT_CMD, str(TIMEOUT)] + HPLUS_CMD + OPTIONS + options
    # print(" ".join(command))
    with open("server.log", "a+") as f:
        f.write(" ".join(command))
    process = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=FNULL)
    return process

def get_results(process, query_type, qid):
    for line in iter(process.stdout.readline, b''):
        # print(line)
        if line[:8] == b'RESULTS:':
            result = json.loads(line[8:])
            obj = build_object(query_type, result, qid)
            str_to_send = json.dumps(obj)
            # print(f'yielding results: {str_to_send}')
            yield (str_to_send + '\n')

def communicate_result(process):
    try:
        result, err = process.communicate(timeout=60)
        result = result.decode('utf-8')
        m = re.findall('RESULTS:(.*)', result)
        # print(m)
        return json.loads(m[0])
    except subprocess.TimeoutExpired:
        process.terminate()
        return ""

