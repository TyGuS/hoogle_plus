#!/usr/bin/python3

import os
from flask import json
import random
import enum
import re
from subprocess import Popen, PIPE


URL = 'http://localhost:5000'
N_CASES = 4

class Route(enum.Enum):
    SearchByType = '/search/type'
    SearchByExample = '/search/example'
    SearchForExample = '/examples'
    SearchForOutput = '/example/code'
    Stop = '/stop'

class Example:
    def __init__(self, inputs, output):
        self.inputs = inputs
        self.output = output

    def __repr__(self):
        return f'{{ "inputs":{self.inputs}, "output":"{self.output}" }}'

    def __str__(self):
        return self.__repr__()

class WrongExamples:
    def __init__(self, input_error, output_error, syntax_error):
        self.input_error = input_error
        self.output_error = output_error
        self.syntax_error = syntax_error

class ErrorType(enum.Enum):
    ErrIn = 'input error'
    ErrOut = 'output error'
    ErrSyn = 'syntax error'

class TestSuite:
    def __init__(self, name, type_signature,
                 examples, wrong_examples, candidates):
        self.name = name
        self.type_signature = type_signature
        self.examples = examples
        self.wrong_examples = wrong_examples
        self.candidates = candidates

    def get_random_examples(self, error_type = None):
        if error_type is None:
            num_of_examples = random.randint(0, len(self.examples))
            selected_examples = random.sample(self.examples, num_of_examples)
        elif error_type is ErrorType.ErrIn:
            selected_examples = [self.wrong_examples.input_error]
        elif error_type is ErrorType.ErrOut:
            selected_examples = [self.wrong_examples.output_error]
        elif error_type is ErrorType.ErrSyn:
            selected_examples = [self.wrong_examples.syntax_error]
        else:
            raise ValueException('unknown error type when getting random examples')

        return selected_examples

    def get_random_candidate(self):
        return random.choice(self.candidates)

    def get_random_arguments(self, error_type = None):
        if error_type is None:
            return random.choice(self.examples).inputs
        elif error_type is ErrorType.ErrIn:
            return self.wrong_examples.input_error.inputs
        elif error_type is ErrorType.ErrOut:
            return self.wrong_examples.output_error.inputs
        elif error_type is ErrorType.ErrSyn:
            return self.wrong_examples.syntax_error.inputs
        else:
            raise ValueException('unknown error type when getting random arguments')

    def make_program_search_case(self, error_type = None):
        """
        make a random test case for program search from types plus zero or more examples
        return an object which will be converted into a json string in the future
        """
        selected_examples = self.get_random_examples(error_type)

        return {
            'typeSignature': self.type_signature,
            'facts': selected_examples
        }

    def make_type_search_case(self, error_type = None):
        """
        make a random test case for type search from a set of examples
        return an object to be converted into a json string later
        """
        selected_examples = self.get_random_examples(error_type)

        return {
            'examples': selected_examples
        }

    def make_example_search_case(self):
        """
        make a random test case for example search
        given a candidate program, a type signature
        and a list of existing examples
        """
        selected_examples = self.get_random_examples()
        selected_candidate = self.get_random_candidate()

        return {
            'typeSignature': self.type_signature,
            'examples': selected_examples,
            'candidate': selected_candidate
        }

    def make_execution_case(self, error_type = None):
        """
        make a random test case for execution
        """
        selected_args = self.get_random_arguments(error_type)
        selected_candidate = self.get_random_candidate()

        return {
            'typeSignature': self.type_signature,
            'candidate': selected_candidate,
            'arguments': selected_args
        }

# check the version of flask
def check_flask():
    p1 = Popen('flask --version'.split(), stdout=PIPE)
    p2 = Popen('grep Python'.split(), stdin=p1.stdout, stdout=PIPE)
    out = p2.stdout.read().decode('utf-8')
    match = re.search(r'.* ([0-9]).([0-9]).*', out)
    if int(match.group(1)) < 3:
        print('This script requires flask installation with python3')
    else:
        print('Flask check passed')

# run curl commands
def run_curl_cmd(request_route, json_str):
    p = Popen(['curl', '--header', 'Content-Type: application/json',
                       '--request', 'POST',
                       '--data', json_str,
                       URL + request_route.value],
             stdout=PIPE)
    print(f'Sending request to {request_route}')
    print('Sending data:')
    print(json.dumps(json.loads(json_str), indent=2))
    for res in iter(p.stdout.readline, b''):
        print(res)
        if res[:8] == b'RESULTS:':
            yield res.decode('utf-8')

class TestCase:
    def __init__(self, route, generator, description):
        self.route = route
        self.generator = generator
        self.description = description

    def run(self):
        print(f'Testing {self.description}')
        for _ in range(N_CASES):
            case_obj = self.generator()
            case_json = json.dumps(case_obj, default=lambda o: o.__dict__)
            for res in run_curl_cmd(self.route, case_json):
                print(json.dumps(json.loads(res), indent=2))


def make_first_just():
    # make examples
    examples = []
    examples.append(Example(['1', '[]'], '1'))
    examples.append(Example(['1', '[Nothing, Just 2]'], '2'))
    examples.append(Example(['1', '[Just 2, Nothing]'], '2'))

    # make wrong examples
    input_error = Example(['1', '[Just 2.2]'], '1')
    output_error = Example(['1', '[Just 2]'], '2.0')
    syntax_error = Example(['1', '[Just 2'], '2')
    wrong_examples = WrongExamples(input_error, output_error, syntax_error)

    # make candidate programs
    candidates = ['\\d xs -> fromMaybe d (head xs)',
                  '\\arg0 arg1 -> fromMaybe arg0 (last arg1)',
                  '\\arg0 arg1 -> fromMaybe arg0 (listToMaybe (catMaybes arg1))']

    return TestSuite('firstJust',
                     'a -> [Maybe a] -> a',
                     examples,
                     wrong_examples,
                     candidates)

def make_concat_ntimes():
    # make examples
    examples = []
    examples.append(Example(['1', '[]'], '[]'))
    examples.append(Example(['2', '[1]'], '[1,1]')),
    examples.append(Example(['2', '[1,2,3]'], '[1,2,3,1,2,3]'))

    # make wrong examples
    input_error = Example(['1.0', '[1,2,3]'], '[1,2,3,1,2,3]')
    output_error = Example(['2', '[1,2,3,1,2,3]'], '[1,2,3,1,2.3]')
    syntax_error = Example(['2', '[1,2,3=3]'], '[1,2,3,1,2,3]')
    wrong_examples = WrongExamples(input_error, output_error, syntax_error)

    # make candidate programs
    candidates = ['\\xs n -> take n xs',
                  '\\xs n -> drop n xs',
                  '\\arg0 arg1 -> concat (replicate arg0 arg1)']

    return TestSuite('concatNTimes',
                     '[a] -> Int -> [a]',
                     examples,
                     wrong_examples,
                     candidates)


# run stop command
def test_stop(test_suite):
    # get the first output json string from a random search
    random_input = test_suite.make_program_search_case()
    generator = run_curl_cmd(Route.SearchByType, random_input)
    out = next(generator)
    out_obj = json.loads(out.decode('utf-8'))
    pid = out_obj['id']
    run_curl_cmd(Route.Stop, json.dumps({'id': pid}))

def run_test():
    # check the environment settings
    check_flask()

    # initialize tests
    print('Initializing the test environment')
    first_just = make_first_just()
    test_cases = [
        TestCase(Route.SearchByType,
                 first_just.make_program_search_case,
                 'search programs by type query'),
        """
        TestCase(Route.SearchByType,
                 lambda: first_just.make_program_search_case(error_type =
                                                             ErrorType.ErrIn),
                 'search programs by type query error'),
        TestCase(Route.SearchByExample,
                 first_just.make_type_search_case,
                 'search types by examples'),
        TestCase(Route.SearchByExample,
                 lambda: first_just.make_type_search_case(error_type =
                                                          ErrorType.ErrOut),
                 'search types by examples error'),
        TestCase(Route.SearchForExample,
                 first_just.make_example_search_case,
                 'search more examples'),
        TestCase(Route.SearchForOutput,
                 first_just.make_execution_case,
                 'search execution outputs'),
        TestCase(Route.SearchForOutput,
                 lambda: first_just.make_execution_case(error_type =
                                                        ErrorType.ErrIn),
                 'search execution outputs error'),
        TestCase(Route.SearchForOutput,
                 lambda: first_just.make_execution_case(error_type =
                                                        ErrorType.ErrSyn),
                 'search execution outputs error')
        """
    ]

    # start test
    for case in test_cases:
        case.run()

    # test for stop a search
    print('Testing kill a search')
    test_stop(first_just)

    # end test
    print('Test end')

if __name__=='__main__':
    run_test()
