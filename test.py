import json
from ast import literal_eval as make_tuple

with open('trigram_weights_logprob.json') as f:
    data = json.load(f)

newdata = {}

for key in data:
    print make_tuple('("DUMMY")')
    newkey = str(sorted(make_tuple(key)))
    print newkey
    if newkey in newdata:
        newdata[newkey] = newdata[newkey] + data[key]
    else:
        newdata[newkey] = data[key]
