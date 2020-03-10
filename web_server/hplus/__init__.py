import os
import signal
import uuid
from flask import Flask, Response, stream_with_context, request, json, session
import subprocess
import enum
import re
import time
import logging
import json
from flask_cors import CORS, cross_origin

HPLUS_CMD = 'stack exec -- hplus'.split()
OPTIONS = []
TIMEOUT_CMD = 'timeout'
TIMEOUT = '60'

script_dir = os.path.dirname(os.path.realpath(__file__))

TASK_MAP = {
    "Bool -> a -> Maybe a": script_dir + "/../examples/json/training.json",
    "[[[a]]] -> [a]": script_dir + "/../examples/json/task1.json",
    "[Maybe a] -> a -> a": script_dir + "/../examples/json/task2.json",
    "(a -> Maybe b) -> [a] -> Maybe b": script_dir + "/../examples/json/task3.json",
    "[Maybe Bool] -> Bool": script_dir + "/../examples/json/task4.json"
}

class QueryType(enum.Enum):
    search_programs = 'SearchPrograms'
    search_types = 'SearchTypes'
    search_results = 'SearchResults'
    search_examples = 'SearchExamples'

def build_object(query_type, result, qid = None):
    if query_type is QueryType.search_programs:
        return {
            'id': qid,
            'candidate': result['solution'],
            'examples': result['outExamples'],
            'error': result['outError']
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
    os.chdir('../')
    command = [TIMEOUT_CMD, TIMEOUT] + HPLUS_CMD + options
    print(command)
    process = subprocess.Popen(command, stdout=subprocess.PIPE)
    os.chdir('web_server')
    return process

def get_results(process, query_type, qid):
    for line in iter(process.stdout.readline, b''):
        if line[:8] == b'RESULTS:':
            result = json.loads(line[8:])
            obj = build_object(query_type, result, qid)
            str_to_send = json.dumps(obj)
            print(f'yielding results: {str_to_send}')
            yield (str_to_send + '\n')

def communicate_result(process):
    result, err = process.communicate()
    result = result.decode('utf-8')
    m = re.findall('RESULTS:(.*)', result)
    print(m)
    return json.loads(m[0])

def create_app(test_config=None):
    # create and configure the app
    app = Flask(__name__, instance_relative_config=True)
    app.config.from_mapping(
        SECRET_KEY='dev',
    )
    CORS(app)


    if test_config is None:
        # load the instance config, if it exists, when not testing
        app.config.from_pyfile('config.py', silent=True)
    else:
        # load the test config if passed in
        app.config.from_mapping(test_config)

    # ensure the instance folder exists
    try:
        os.makedirs(app.instance_path)
    except OSError:
        pass

    # a simple page that says hello
    @app.route('/')
    def index():
        return 'This is a demo page for hoogle+'

    @app.route('/search/type', methods=['GET', 'POST'])
    def search_type():
        obj = json.loads(request.data)
        print(obj)
        sig = obj['typeSignature']
        if sig in TASK_MAP:
            task_file_name = TASK_MAP[sig]
            print(task_file_name)
            with open(task_file_name) as task_file:
                json_response = json.load(task_file)
                def generate():
                    for single_resp in json_response:
                        yield json.dumps(single_resp) + "\n"
                return Response(generate(), mimetype="application/json")
        return 401

    @app.route('/search/example', methods=['GET', 'POST'])
    def search_example():
        obj = json.loads(request.data)
        # print(obj)
        query = {
                 'query': '??',
                 'inExamples': obj['facts']
                }
        proc = run_hplus([f'--json={json.dumps(query)}',
                          f'--search-type={QueryType.search_types.value}'])
        return json.jsonify(build_object(QueryType.search_types,
                                         communicate_result(proc)))

    @app.route('/stop', methods=['GET', 'POST'])
    def stop():
        oid = request.get_json()['id']
        print(session)
        pid = session[oid]
        os.killpg(os.getpgid(pid), signal.SIGTERM)
        return ('', 204)

    @app.route('/examples', methods=['GET', 'POST'])
    def get_examples():
        obj = json.loads(request.data)
        # print(obj)
        query = {
                 'exampleQuery': obj['typeSignature'],
                 'exampleProgram': obj['candidate'],
                 'exampleExisting': obj['examples']
                }
        proc = run_hplus([f'--json={json.dumps(query)}',
                          f'--search-type={QueryType.search_examples.value}'])
        return json.jsonify(build_object(QueryType.search_examples,
                                         communicate_result(proc)))

    @app.route('/example/code', methods=['GET', 'POST'])
    def result_for():
        obj = json.loads(request.data)
        # print(obj)
        query = {
                 'execQuery': obj['typeSignature'],
                 'execProg': obj['candidate'],
                 'execArgs': obj['args']
                }
        proc = run_hplus([f'--json={json.dumps(query)}',
                          f'--search-type={QueryType.search_results.value}'])
        return json.jsonify(build_object(QueryType.search_results,
                                         communicate_result(proc)))

    return app
