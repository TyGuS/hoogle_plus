import os
import uuid
from flask import Flask, Response, stream_with_context, request, json, session
import subprocess
import enum

HPLUS_CMD = 'stack exec -- hplus'.split()
OPTIONS = []
TIMEOUT_CMD = 'timeout'
TIMEOUT = '60'

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

def run_hplus(options, query_type, qid = None):
    # TODO: we may put these paths into configuration file
    os.chdir('../')
    command = [TIMEOUT_CMD, TIMEOUT] + HPLUS_CMD + options
    print(command)
    process = subprocess.Popen(command, stdout=subprocess.PIPE)
    os.chdir('web_server')
    # store the pid to the current session
    if qid is not None:
        session[qid] = process.pid
        print(process.pid)
    for line in iter(process.stdout.readline, b''):
        if line[:8] == b'RESULTS:':
            print('yielding results')
            result = json.loads(line[8:])
            obj = build_object(query_type, result, qid)
            yield json.dumps(obj) + '\n'

def create_app(test_config=None):
    # create and configure the app
    app = Flask(__name__, instance_relative_config=True)
    app.config.from_mapping(
        SECRET_KEY='dev',
    )

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
        obj = request.get_json()
        query = {
                 'query': obj['typeSignature'],
                 'inExamples': obj['facts']
                }
        qid = uuid.uuid1()
        def generate():
            return run_hplus([f'--json={json.dumps(query)}',
                              f'--search-type={QueryType.search_programs.value}',
                              '--cnt=10'],
                             QueryType.search_programs,
                             qid)
        return Response(stream_with_context(generate()), mimetype='application/json')

    @app.route('/search/example', methods=['GET', 'POST'])
    def search_example():
        obj = request.get_json()
        # print(obj)
        query = {
                 'query': '??',
                 'inExamples': obj['facts']
                }
        def generate():
            return run_hplus([f'--json={json.dumps(query)}',
                              f'--search-type={QueryType.search_types.value}'],
                             QueryType.search_types)
        return Response(stream_with_context(generate()), mimetype='application/json')

    @app.route('/stop', methods=['GET', 'POST'])
    def stop():
        oid = request.get_json()['id']
        pid = session[oid]
        os.killpg(os.getpgid(pid), signal.SIGTERM)
        return ('', 204)

    @app.route('/examples', methods=['GET', 'POST'])
    def get_examples():
        obj = request.get_json()
        # print(obj)
        query = {
                 'exampleQuery': obj['typeSignature'],
                 'exampleProgram': obj['candidate'],
                 'exampleExisting': obj['examples']
                }
        def generate():
            return run_hplus([f'--json={json.dumps(query)}',
                              f'--search-type={QueryType.search_examples.value}'],
                             QueryType.search_examples)
        return Response(stream_with_context(generate()), mimetype='application/json')

    @app.route('/example/code', methods=['GET', 'POST'])
    def result_for():
        obj = request.get_json()
        # print(obj)
        query = {
                 'execQuery': obj['typeSignature'],
                 'execProg': obj['candidate'],
                 'execArgs': obj['args']
                }
        def generate():
            return run_hplus([f'--json={json.dumps(query)}',
                              f'--search-type={QueryType.search_results.value}'],
                             QueryType.search_results)
        return Response(stream_with_context(generate()), mimetype='application/json')

    return app
