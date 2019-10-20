#!/usr/local/bin/python3

import re
import argparse
import matplotlib.pyplot as plt

# read the last line in the log file
def read_log(filename):
    f = open(filename, 'r')
    for x in f:
        pass
    f.close()
    return x

def make_object(obj_str):
    obj = {}
    reg_str = r'[a-zA-Z]+ = [^,]+|[a-zA-Z]+ = \[.*?\]|[a-zA-Z]+ = \".*?\"|\
                [a-zA-Z]+ = \(.*?\)'
    res = re.findall(reg_str, obj_str)
    properties = map(lambda x: x.split('='), res)
    for p in properties:
        k, v = p[0].strip(), p[1].strip(' \"')
        if v[0] == '[':
            v = v.strip('[]').split(',')
        obj[k] = v
    return obj

def read_object(log_str):
    results = re.findall(r'ResultSummary \{(.*?\})\}', log_str)
    result_objs = []
    for result in results:
        res = re.search(r', result = Result \{(.*?)\}', result).group(1)
        obj = make_object(result[:-len(res)])
        res_obj = make_object(res)
        obj['result'] = res_obj
        result_objs.append(obj)
    return result_objs

def output_res(filename, json_obj):
    # group the results by their experiment type
    res_dict = {}
    for obj in json_obj:
        key = obj['paramName']
        if key in res_dict:
            res_dict[key].append(obj)
        else:
            res_dict[key] = [obj]

    # output the results to the output file
    # f = open(filename, 'a')
    # for exp_type, exp_list in res_dict.items():
    #    # first line is the experiment type
    #    f.write(exp_type)
    #    f.write('\n')
    #    # head line of the table
    #    f.write('Query Name\t Query Str\t Solution\t Total Time(s)\t \
    #             Encoding Time(s)\t Solver Time(s)\t Iterations\t \
    #             Places\t Transitions\n')
    #    # lines of data
    #    for exp in exp_list:
    #        exp_res = exp['result']
    #        f.write('{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\n'.format(
    #            exp['queryName'],
    #            exp['queryStr'],
    #            exp_res['resSolutionOrError'],
    #            exp_res['resTFirstSoln'],
    #            exp_res['resTEncFirstSoln'],
    #            exp_res['resTSolveFirstSoln'],
    #            exp_res['resRefinementSteps'],
    #            exp_res['resTransitions'][0],
    #            exp_res['resTypes'][0]))
    #
    #f.close()
    
    # output the data structure for plotting
    plot_data = []
    for exp_type, exp_list in res_dict.items():
        plot_line = {}
        plot_line['name'] = exp_type
        filtered_list = filter(lambda x: x['result']['resTFirstSoln'] != '0.0', exp_list)
        plot_line['data'] = map(lambda x: float(x['result']['resTFirstSoln']),filtered_list)
        plot_data.append(plot_line)

    return plot_data

def plot(exps):
    
    for exp in exps:
        if exp['name'].find('TYGAR-0') >= 0:
            continue
        xs = [0] + sorted(exp['data'])
        ys = range(0, len(xs))
        m = '.'
        if exp['name'].find('incremental') >= 0:
            m = 'x'
        plt.plot(xs, ys, marker=m, label=exp['name'])
        
    plt.yticks(range(0, 21))
    plt.gca().legend()
    plt.show()

def main():
    parser = argparse.ArgumentParser(description='Process log from hoogle plus evaluation')
    parser.add_argument('input', help='the log filename to be processed')
    parser.add_argument('input2', help ='the second filename to be processed')
    parser.add_argument('--output', default='output.log',
                        help='the output filename')

    args = parser.parse_args()
    log = read_log(args.input)
    log2 = read_log(args.input2)
    obj = read_object(log)
    obj2 = read_object(log2)
    out = output_res(args.output, obj)
    out2 = output_res(args.output, obj2)
    for exp in out2:
        exp['name'] = exp['name'] + " - incremental"
    plot(out + out2)

if __name__ == '__main__':
    main()
