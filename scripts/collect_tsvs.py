#!/usr/bin/python3

import re
import os
import csv
import json
import yaml
from tabulate import tabulate

DATA_DIR = "tmp/before2130"
OUTPUT_FILE = "collected.csv"
OUTPUT_TEX = "result.tex"
quality_map = "data/quality-map.csv"
name_regex = r'(.*)\+(.*)\.tsv'

TYGARQ = "TYGAR-Q"
TYGARQB5 = "TYGAR-QB-5"
TYGAR0 = "TYGAR-0"
TYGAR0B5 = "TYGAR-0B-5"
NOGAR = "NoGar"
SYPET = "Sypet-Clone"

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
        files = os.listdir(DATA_DIR)
        per_query = {}
        for file_name in files:
            match = re.search(name_regex, file_name)
            if not match:
                continue
            # assume we have a match
            query_name = match.group(2)
            exp_name = match.group(1)
            if query_name not in per_query:
                per_query[query_name] = {}
            per_query[query_name][exp_name] = []
            file_path = os.path.join(DATA_DIR, file_name)
            with open(file_path, 'r') as f:
                reader = csv.DictReader(f, delimiter="\t")
                query_exp_results = self.check_results(list(reader))
                if query_exp_results:
                    per_query[query_name][exp_name] = query_exp_results
        self.per_query = per_query
        return per_query

    def check_results(self, solutions):
        try:
            first = solutions[0]
            soln = first["Solution"]
            if not soln or "No Solution" in soln or "Runtime" in soln or "Timeout" in soln:
                first["totalTime"] = None
            return solutions
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
            "t-Sypet",
            "ct-Q",
            "ct-0",
            "ct-NO",
            "ct-Sypet",

            "st-Q",
            "st-QB5",
            "st-0",
            "st-0B5",
            "st-NO",
            "st-Sypet",

            "tc-Q",
            "tc-0",
            "tc-NO",
            "tc-Sypet",

            "tr-Q",
            "tr-QB5",
            "tr-0",
            "tr-0B5",
            "tr-Sypet",
        ]

    def mk_row(self, idx, query_group, query_name):
        return [
            idx,
            safeGetFirst(query_group, TYGARQ, "name"),
            safeGetFirst(query_group, TYGARQ, "query"),

            self.get_quality(query_name, "H"),
            self.get_quality(query_name, "H-D"),
            self.get_quality(query_name, "H-R"),

            safeGetFirst(query_group, TYGARQ, "totalTime"),
            safeGetFirst(query_group, TYGARQB5, "totalTime"),
            safeGetFirst(query_group, TYGAR0, "totalTime"),
            safeGetFirst(query_group, TYGAR0B5, "totalTime"),
            safeGetFirst(query_group, NOGAR, "totalTime"),
            safeGetFirst(query_group, SYPET, "totalTime"),
            safeGetFirst(query_group, TYGARQ, "constructionTime"),
            safeGetFirst(query_group, TYGAR0, "constructionTime"),
            safeGetFirst(query_group, NOGAR, "constructionTime"),
            safeGetFirst(query_group, SYPET, "constructionTime"),
            safeGetFirst(query_group, TYGARQ, "solverTime"),
            safeGetFirst(query_group, TYGARQB5, "solverTime"),
            safeGetFirst(query_group, TYGAR0, "solverTime"),
            safeGetFirst(query_group, TYGAR0B5, "solverTime"),
            safeGetFirst(query_group, NOGAR, "solverTime"),
            safeGetFirst(query_group, SYPET, "solverTime"),
            safeGetFirst(query_group, TYGARQ, "typeCheckerTime"),
            safeGetFirst(query_group, TYGAR0, "typeCheckerTime"),
            safeGetFirst(query_group, NOGAR, "typeCheckerTime"),
            safeGetFirst(query_group, SYPET, "typeCheckerTime"),

            get_transitions(query_group, TYGARQ),
            get_transitions(query_group, TYGARQB5),
            get_transitions(query_group, TYGAR0),
            get_transitions(query_group, TYGAR0B5),
            get_transitions(query_group, SYPET)
        ]

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


def safeGetFirst(dct, exp, field):
    try:
        return dct.get(exp)[0].get(field)
    except (IndexError, KeyError, TypeError):
        return None

def get_transitions(dct, exp):
    try:
        transitionsStr = safeGetFirst(dct, exp, "numOfTransitions")
        trs = json.loads(transitionsStr)
        return trs[-1]
    except (IndexError, TypeError):
        return None

def main():
    ctsv = CollectTSVs()
    per_query = ctsv.gather_files()
    table = ctsv.mk_table(per_query)
    ctsv.write_csv(table)
    ctsv.write_latex(table)

if __name__ == '__main__':
    main()