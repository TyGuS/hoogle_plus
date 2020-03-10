from os import listdir
from os.path import isfile, join, dirname, realpath, splitext, basename
import json
import csv

script_dir = dirname(realpath(__file__))
csv_dir = join(script_dir, "csv")
json_dir = join(script_dir, "json")

for csv_fname in listdir(csv_dir):
    name = splitext(csv_fname)[0]
    with open(join(csv_dir, csv_fname)) as csv_file:
        reader = csv.reader(csv_file, delimiter='\t')
        inputs = [[i] for i in reader.__next__()[1:]]
        if name == "task3":
            inputs = [['\_ -> Nothing', '[]'], ['Just', '[]'], ['Just', '[1,2]']]
        out = [
            {
                "candidate": row[0],
                "examples": [
                    {"inputs": inputs[i], "output": row[i + 1]}
                    for i in range(len(inputs))
                ]
            }
            for row in reader
        ]
        with open(join(json_dir, name) + ".json", 'w+') as json_file:
            json_file.write(json.dumps(out))

