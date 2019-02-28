#/usr/bin/env python3

import os
import json
import argparse


DATA_FIELD_ORDERING = ["time", "encoding", "length", "refinements", "transitions", "types", ]

DEFAULT_DATA_DIR = "output/script/"
DEFAULT_OUTPUT_DIR = "output/latex/"
DEFAULT_COMPONENT_SET = "icfp"


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

    def data_to_latex(self):
        data = self.read_json_files()
        str_lines = []
        idx = 1
        for dkey in data:
            str_lines.append(self.to_tool_table_line(dkey, data[dkey], idx))
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
        content = "\n\\hline \n".join(table) + "\n\\hline"
        start = " \\begin{tabular}{|c|c|c|c|c|c|c|c|}"
        end = " \\end{tabular}"
        return start + content + end

    def to_tool_table_line(self, name, data, idx):
        query = data["query"]
        dataset = data[self.args.component_set]
        method = "queryrefinement"
        if type(dataset[method]) != list:
            return self.to_line(idx, name)
        try:
            solutions = dataset[method]
            data = {
                "time": solutions[0]["time"],
                "length": solutions[0]["length"],
                "refinements": solutions[-1]["refinements"],
                "transitions": solutions[-1]["transitions"][-1],
                "types": solutions[-1]["types"][-1],
                "encoding": solutions[0]["encoding"],
            }
            return self.to_line(idx, name, **data)
        except Exception as ex:
            import ipdb; ipdb.set_trace()
            return self.to_line(idx, name)

    def to_line(self, id, name, **data_fields):
        safe_name = name.replace("_", "-")
        fields = [str(data_fields.get(f, "-")) for f in DATA_FIELD_ORDERING]
        return f"{id} & {safe_name} & " + " & ".join(fields) + " \\\\"

    def table_header(self):
        return "\hline\nid & query name & $t_{solution}$ & $t_{encoding}$ & $d_{solution}$ & $r_{num}$ & $tr_{final}$ & $types_{final}$ \\\\"

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
