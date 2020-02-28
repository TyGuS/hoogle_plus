import os

from flask import Flask, Response, request, json, session
from communicate import QueryType, run_hplus

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
        # print(obj)
        query = {
                 'query': obj['typeSignature'],
                 'inExamples': obj['facts']
                }
        def generate:
            run_hplus([f'--json=\"{query}\"',
                       f'--search_type={QueryType.search_programs}]',
                       obj['id'],
                       use_stream = True)
        return Response(generate(), mimetype='application/json')

    @app.route('/search/example', methods=['GET', 'POST'])
    def search_example():
        obj = request.get_json()
        # print(obj)
        query = {
                 'query': '??'
                 'inExamples': obj['facts']
                }
        types = run_hplus([f'--json=\"{query}\"',
                           f'--search_type={QueryType.search_types}'],
                          obj['id'])
        return types

    @app.route('/stop', methods=['GET', 'POST'])
    def stop():
        oid = request.get_json()['id']
        pid = session[oid]
        os.killpg(os.getpgid(pid), signal.SIGTERM)
        return ('', 204)

    @app.route('/example/code', methods=['GET', 'POST'])
    def result_for():
        obj = request.get_json()
        # print(obj)
        query = {
                 'query': obj['typeSignature'],
                 'program': obj['candidate'],
                 'arguments': obj['args']
                }
        out = run_hplus([f'--json=\"{query}\"',
                         f'--search_type={QueryType.search_results}'],
                        obj['id'])
        return out

    return app
