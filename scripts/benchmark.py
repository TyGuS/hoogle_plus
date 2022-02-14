import yaml

class Benchmark:
    def __init__(self, name, query, description, desired_solution, examples=[], options=[]):
        self.name = name                # Id
        self.query = query              # Query signature
        self.description = description  # Description
        self.desired_solution = desired_solution
        self.examples = examples
        self.options = options          # Command-line options to use for this benchmark when running in individual context

    def __str__(self):
        return self.name + ': ' + self.query

    def __repr__(self):
        return self.__str__()

class BenchmarkGroup:
    def __init__(self, name, default_options):
        self.name = name                        # Id
        self.default_options = default_options  # Command-line options to use for all benchmarks in this group when running in common context
        self.benchmarks = []

    def contains(self, name):
        for b in self.benchmarks:
            if b.name == name:
                return b

        return None

    def add_benchmark(self, b):
        self.benchmarks.append(b)

    def __str__(self):
        return self.name + ': {' + ','.join(str(b) for b in self.benchmarks) + '}'

    def __repr__(self):
        return self.__str__()

class SynthesisResult:
    def __init__(self, name, time, solution = '-', encoding_time = -1., solver_time = -1., refine_time = -1., checker_time = -1., path_len = -1):
        self.name = name                        # Benchmark name
        self.time = float(time)                 # Synthesis time (seconds)
        self.solution = solution
        self.encoding_time = encoding_time
        self.solver_time = solver_time
        self.refine_time = refine_time
        self.checker_time = checker_time
        self.length = path_len

    def str(self):
        return self.name + ', ' + '{0:0.2f}'.format(self.time)

def load_queries(query_file):
    group_map = {}
    with open(query_file) as f:
        queries = yaml.full_load(f)
        for q in queries:
            group_name = q['source']
            if group_name not in group_map:
                group_map[group_name] = BenchmarkGroup(group_name, [])
            group = group_map[group_name]
            examples = q['examples'] if 'examples' in q else []
            group.add_benchmark(Benchmark(q['name'], q['query'], '', q['solution'], examples))
    return group_map

def find_benchmark_in_groups(name, benchmarks):
    benchmark = next(g.contains(name) for g in benchmarks.values() if g.contains(name))
    return benchmark
