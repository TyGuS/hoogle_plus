from flask import Flask, Response, stream_with_context, request, json
import uuid
import subprocess
import time
import logging
from flask_cors import CORS, cross_origin
from expiringdict import ExpiringDict
from hplus.util import *
import pexpect
import re
import threading

cache = ExpiringDict(max_len=100, max_age_seconds=(2 * TIMEOUT))
# initialize the interpreter
# interpreter = subprocess.Popen(["stack", "ghci", "InternalTypeGen.hs"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
# interpreter.stdin.write(b"import Test.SmallCheck\n")
# interpreter.stdin.write(b"import Test.SmallCheck.Drivers\n")
# interpreter.stdin.write(b":set -XScopedTypeVariables\n")
# interpreter.stdin.write(b":set -XFlexibleContexts\n")
# interpreter.stdin.flush()
# interpreter.stdout.read()
interpreter = pexpect.spawn('stack ghci ../InternalTypeGen.hs')
interpreter.setecho(False)
# skip GHCi preamble
for _ in range(8):
    interpreter.readline()

supportedModules = [
    'Test.SmallCheck',
    'Test.SmallCheck.Drivers',
    'Test.LeanCheck.Function.ShowFunction',
    'System.IO.Silently',
    'System.Timeout',
    'GHC.List',
    'Data.List',
    'Data.Maybe',
    'Data.Either',
    'Data.Tuple',
    'Data.Bool',
    'Data.Int',
    'Data.Char',
    'Text.Show',
    'Data.Function',
    'Data.String',
    'Data.ByteString.Lazy',
    'Data.ByteString.Builder',
    ]

for pkg in supportedModules:
    interpreter.sendline("import " + pkg + "\n")

# add extensions
interpreter.sendline(":set -XScopedTypeVariables\n")
interpreter.sendline(":set -XFlexibleContexts\n")
interpreter.sendline(":set -XTupleSections\n")

# force the interpreter to flush its output
interpreter.sendline("1\n")
line = interpreter.readline()
print("last interpreter output:", line)

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
        query = {
                 'query': obj['typeSignature'],
                 'inExamples': obj['facts'],
                 'inArgNames': []
                }
        qid = uuid.uuid1()
        proc = run_hplus([f'--json={json.dumps(query)}',
                          f'--search-category={QueryType.search_programs.value}',
                          '--cnt=10'])
        cache[str(qid)] = proc.pid
        # print(f"id: {str(qid)} => proc: {proc.pid}")
        # print(cache)
        def generate():
            return get_results(proc, QueryType.search_programs, qid)
        return Response(generate(), mimetype='application/json')

    @app.route('/search/example', methods=['GET', 'POST'])
    def search_example():
        obj = json.loads(request.data)
        # print(obj)
        query = {
                 'query': '??',
                 'inExamples': obj['facts'],
                 'inArgNames': obj['argNames']
                }
        proc = run_hplus([f'--json={json.dumps(query)}',
                          f'--search-category={QueryType.search_types.value}'])
        return json.jsonify(build_object(QueryType.search_types,
                                         communicate_result(proc)))

    @app.route('/stop', methods=['GET', 'POST'])
    def stop():
        qid = request.get_json()['id']
        pid = cache.get(qid)
        print('killing pid', pid, 'with group id', os.getpgid(pid))
        os.killpg(os.getpgid(pid), signal.SIGKILL)
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
                          f'--search-category={QueryType.search_examples.value}'])
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
                          f'--search-category={QueryType.search_results.value}'])
        return json.jsonify(build_object(QueryType.search_results,
                                         communicate_result(proc)))

    @app.route('/interpreter', methods=['GET', 'POST'])
    def interpreter_route():
        obj = request.data
        print("interpreter get:", obj)
        # print(type(obj))
        interpreter.sendline(obj.decode('utf-8'))
        
        def countdown():
            time.sleep(3)

        try:
            # create a thread to count down the timeout
            t1 = threading.Thread(target=countdown)
            t1.start()
            line = interpreter.readline()
        except:
            line = "timeout"
        finally:
            t1.join()
            interpreter.sendintr()
            interpreter.sendcontrol('c')

        # skip several lines for an exception
        interpreter.expect(">")
        # find the first appearance of '> "'
        match = re.search(r"> \"", line.decode('utf-8'))
        if match is None:
            return line.decode('utf-8')
        else:
            return line.decode('utf-8')[match.end()-1:]
    
    return app
