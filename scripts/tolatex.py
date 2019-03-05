#!/usr/bin/env python3

import os
import json
import argparse


DATA_FIELD_ORDERING = ["time", "encoding", "total", "length", "refinements", "transitions", "types",
                       "norefine_time", "norefine_trans",
                       "ar_time", "ar_trans"]

COLUMN_NAMES = ["N", "Query", "$t_{1}$", "$t_{enc}$", "$t_{t}$", "$l_{p}$", "$r_{f}$", "$tr_{f}$", "$\\tau_{f}$",
                "$t_{nr1}$", "$tr_{nrf}$",
                "$t_{iar1}$", "$tr_{iarf}$"]

DEFAULT_DATA_DIR = "output/script/"
DEFAULT_OUTPUT_DIR = "output/latex/"
DEFAULT_COMPONENT_SET = "icfp"

curated_queries = [
    "firstJust",
    "intToByteString",
    "both",
    "conjunctionMaybes",
    "mbMap",
    "guaranteedRight",
]

def safeget(dct, default, *keys):
    for key in keys:
        try:
            dct = dct[key]
        except (KeyError, IndexError):
            return default
    return dct


class LatexFormer(object):

    args = None
    def __init__(self, args):
        self.args = args
        self.create_dirs()

    def create_dirs(self):
        if not os.path.exists(self.args.output_dir):
            os.mkdir(self.args.output_dir)
        if not os.path.exists(self.args.data_dir):
            exit("missing input directory: " + self.args.data_dir)


    def group_by_curated(self, data_from_files):
        curated = []
        rest = []
        for dkey in data_from_files:
            datum_from_file = {
                "name": dkey,
                "result": data_from_files[dkey]
            }
            if dkey in curated_queries:
                curated.append(datum_from_file)
            else:
                rest.append(datum_from_file)
        return (curated, rest)

    def data_to_latex(self):
        data = self.read_json_files()
        (curated, rest) = self.group_by_curated(data)
        str_lines = []
        idx = 1
        for d in curated:
            str_lines.append(self.to_tool_table_line(d["result"], idx))
            idx += 1
        str_lines.append("")
        for d in rest:
            str_lines.append(self.to_tool_table_line(d["result"], idx))
            idx += 1
        return self.lines_to_table(str_lines)

    def read_json_files(self):
        data = {}
        for filename in os.listdir(self.args.data_dir):
            if filename.endswith(".json") and ("#" not in filename):
                relative_filename = os.path.join(self.args.data_dir, filename)
                with open(relative_filename, "r") as f:
                    name = filename.partition(".")[0]
                    data[name] = json.loads("".join(f.readlines()))
        return data

    def lines_to_table(self, lines):
        table = [self.table_header()] + lines
        content = "\n\\hline\n".join(table) + "\n\\hline"
        other_mode_columns = "|r|r|r|r|"
        columns = "|c|c|r|r|r|r|r|r|r|" + other_mode_columns
        start = " \\resizebox{\\textwidth}{!}{ \\begin{tabular}{%s}" % (columns)
        end = " \\end{tabular}}"
        return start + content + end

    def to_tool_table_line(self, data, idx):
        query = data["query"]
        dataset = data[self.args.component_set]
        method = "queryrefinement"
        if type(dataset[method]) != list:
            return self.to_line(idx, query)
        try:
            solutions = dataset[method]
            norefine = dataset["norefine"]
            abstract_refinement = dataset["abstractrefinement"]
            total = sum([soln["time"] for soln in solutions])
            data = {
                "total": total,
                "time": solutions[0]["time"],
                "length": solutions[0]["length"],
                "refinements": solutions[0]["refinements"],
                "transitions": solutions[0]["transitions"][-1],
                "types": solutions[0]["types"][-1],
                "encoding": solutions[0]["encoding"],

                "norefine_time": safeget(norefine, "-", 0, "time"),
                "norefine_trans": safeget(norefine, "-", 0, "transitions", -1),
                "ar_time": safeget(abstract_refinement, "-", 0, "time"),
                "ar_trans": safeget(abstract_refinement, "-", 0, "transitions", -1),
            }
            return self.to_line(idx, query, **data)
        except Exception as ex:
            print(ex)
            return self.to_line(idx, query)

    def to_line(self, id, name, **data_fields):
        safe_name = name.replace("->", "$\\rightarrow$")
        fields = [str(data_fields.get(f, "-")) for f in DATA_FIELD_ORDERING]
        return f"{id} & {safe_name} & " + " & ".join(fields) + " \\\\"

    def table_header(self):
        return "\hline\n" + " & ".join(COLUMN_NAMES) + "\\\\"

    def write_latex(self, lx_rows):
        filename = "eval_table.tex"
        filepath = os.path.join(DEFAULT_OUTPUT_DIR, filename)
        with open(filepath, "w") as f:
            f.write(lx_rows)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--output-dir", type=str, default=DEFAULT_OUTPUT_DIR,
                        help="Choose where to output the .tex file")
    parser.add_argument("--data-dir", type=str, default=DEFAULT_DATA_DIR,
                        help="Choose the input directory full of .json data files")
    parser.add_argument("--component-set", type=str, default=DEFAULT_COMPONENT_SET,
                        help="Choose the component set to use for the table.")
    args = parser.parse_args()
    lf = LatexFormer(args)
    lx = lf.data_to_latex()
    lf.write_latex(lx)


if __name__ == '__main__':
    main()
