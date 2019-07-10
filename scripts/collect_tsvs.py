#!/usr/bin/python3

import re
import os
import csv
import json
import yaml
import numpy
from tabulate import tabulate

DATA_DIR = "tmp/multi-run"
OUTPUT_FILE = "collected.csv"
OUTPUT_TEX = "result.tex"
quality_map = "data/quality-map.csv"
name_regex = r'(.*)\+(.*)\.tsv'

TYGARQ = "TYGAR-Q".lower()
TYGARQB5 = "TYGARQB5".lower()
TYGAR0 = "TYGAR-0".lower()
TYGAR0B5 = "TYGAR0B5".lower()
NOGAR = "NoGar".lower()
SYPET = "Sypet-Clone".lower()

class Result():
    def __init__(self):
        pass

class CollectTSVs():

    def __init__(self):
        self.quality_map = {}
        self.per_query = {}
        self.load_quality()

    def load_quality(self):
        with open(quality_map, 'r') as stream:
            qual_map = csv.DictReader(stream)
            for qm in qual_map:
                name = qm.get("Name")
                self.quality_map[name] = {
                    "name": name,
                    "H": qm.get("H"),
                    "H-D": qm.get("H-D"),
                    "H-R": qm.get("H-R")
                }

    def gather_files(self):
        dirs = os.listdir(DATA_DIR)
        per_query = {}
        for directory in dirs:
            sub_path = os.path.join(DATA_DIR, directory)
            subdir = os.listdir(sub_path)
            for file_name in subdir:
                match = re.search(name_regex, file_name)
                if not match:
                    continue
                # assume we have a match
                query_name = match.group(2)
                exp_name = match.group(1).lower()
                if query_name not in per_query:
                    per_query[query_name] = {}
                current_query = per_query[query_name]
                if exp_name not in current_query:
                    current_query[exp_name] = []
                current_query_exp = current_query[exp_name]
                file_path = os.path.join(sub_path, file_name)
                with open(file_path, 'r') as f:
                    reader = csv.DictReader(f, delimiter="\t")
                    query_exp_results = self.check_results(list(reader))
                    if query_exp_results:
                        current_query_exp.append(query_exp_results)
        self.per_query = per_query
        self.average_results()
        return self.per_query

    def average_results(self):
        new_per_query = {}
        for qn in self.per_query:
            if qn not in new_per_query:
                new_per_query[qn] = {}
            old_qn = self.per_query[qn]
            new_qn = new_per_query[qn]
            for exp_name in old_qn:
                if exp_name not in new_qn:
                    new_qn[exp_name] = {}
                new_qn[exp_name] = self.average_query(qn, exp_name, old_qn[exp_name])
        self.per_query = new_per_query
        return new_per_query

    def average_query(self, query_name, exp_name, results):
        numerics = ["totalTime", "constructionTime", "solverTime", "typeCheckerTime"]
        average_result = {}
        for numeric in numerics:
            values = []
            for result in results:
                try:
                    values.append(float(result.get(numeric)))
                except TypeError:
                    pass
            if len(values) > 0:
                average_result[numeric] = round(numpy.median(values), 2)
            if len(values) > 1:
                print("%s-%s: %s - variance %0.2f : %s" % (query_name, exp_name, numeric, numpy.var(values), str(values)))

        last_transitions = []
        for result in results:
            trs = json.loads(result["numOfTransitions"])
            if len(trs) > 0:
                last_transitions.append(trs[-1])
        if len(last_transitions) > 0:
            average_result["numOfTransitions"] = int(numpy.median(last_transitions))

        average_result["name"] = query_name
        average_result["query"] = results[0]["query"]
        return average_result


    def check_results(self, solutions):
        try:
            first = solutions[0]
            soln = first["Solution"]
            if not soln or "No Solution" in soln or "Runtime" in soln or "Timeout" in soln:
                first["totalTime"] = None
            return first
        except IndexError:
            return None

    def mk_table(self, per_query):
        table = []
        keys_list = list(per_query.keys())
        for idx in range(len(keys_list)):
            query_name = keys_list[idx]
            query_group = per_query[query_name]
            table.append(self.mk_row(idx, query_group, query_name))
        return table

    def header(self):
        return [
            "N",
            "Name",
            "Query",
            "H",
            "H-D",
            "H-R",
            "t-Q",
            "t-QB5",
            "t-0",
            "t-0B5",
            "t-NO",

            "st-Q",
            "st-QB5",
            "st-0",
            "st-0B5",
            "st-NO",

            "tc-Q",
            "tc-0",
            "tc-NO",

            "tr-Q",
            "tr-QB5",
            "tr-0",
            "tr-0B5",
        ]

    def mk_row(self, idx, query_group, query_name):
        res = [
            idx,
            query_name,
            safeGet(query_group, TYGARQ, "query"),

            self.get_quality(query_name, "H"),
            self.get_quality(query_name, "H-D"),
            self.get_quality(query_name, "H-R"),

            safeGet(query_group, TYGARQ, "totalTime"),
            safeGet(query_group, TYGARQB5, "totalTime"),
            safeGet(query_group, TYGAR0, "totalTime"),
            safeGet(query_group, TYGAR0B5, "totalTime"),
            safeGet(query_group, NOGAR, "totalTime"),

            safeGet(query_group, TYGARQ, "solverTime"),
            safeGet(query_group, TYGARQB5, "solverTime"),
            safeGet(query_group, TYGAR0, "solverTime"),
            safeGet(query_group, TYGAR0B5, "solverTime"),
            safeGet(query_group, NOGAR, "solverTime"),

            safeGet(query_group, TYGARQ, "typeCheckerTime"),
            safeGet(query_group, TYGAR0, "typeCheckerTime"),
            safeGet(query_group, NOGAR, "typeCheckerTime"),

            safeGet(query_group, TYGARQ, "numOfTransitions"),
            safeGet(query_group, TYGARQB5, "numOfTransitions"),
            safeGet(query_group, TYGAR0, "numOfTransitions"),
            safeGet(query_group, TYGAR0B5, "numOfTransitions"),
        ]
        assert len(res) == len(self.header())
        return res

    def get_quality(self, query_name, title):
        try:
            r = self.quality_map[query_name].get(title)
            return r
        except KeyError as e:
            print("missing %s" % e.args[0])
            return None

    def write_csv(self, table):
        with open(OUTPUT_FILE, "w") as f:
            writer = csv.writer(f)
            writer.writerow(self.header())
            for row in table:
                writer.writerow(row)

    def write_latex(self, table):
        with open(OUTPUT_TEX, "w") as f:
            f.write(tabulate(table, self.header(), tablefmt="latex"))


def mk_plot(per_query):
    per_exp = {}
    for qm in per_query:
        for exp in per_query[qm]:
            if exp not in per_exp:
                per_exp[exp] = []
            time = per_query[qm][exp].get("totalTime")
            if time is not None:
                per_exp[exp].append(time)
    for exp in per_exp:
        per_exp[exp] = sorted(per_exp[exp])

    for exp in per_exp:
        values = per_exp[exp]
        ys = range(0,len(values))
        m = "+"
        if "q" in exp:
            m="."
        plt.plot(values, ys, marker=m, label=exp)
        plt.legend(loc=0, prop={'size': 4})
        plt.xlabel("seconds")
        plt.ylabel("benchmarks solved")
        plt.savefig("bounds.pdf")
        plt.show()


def safeGet(dct, exp, field):
    try:
        return dct.get(exp,{}).get(field)
    except (KeyError, TypeError):
        return None

def main():
    ctsv = CollectTSVs()
    per_query = ctsv.gather_files()
    mk_plot(per_query)
    table = ctsv.mk_table(per_query)
    ctsv.write_csv(table)
    ctsv.write_latex(table)

if __name__ == '__main__':
    main()