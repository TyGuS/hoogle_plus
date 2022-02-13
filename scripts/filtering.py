import os
import re
import json
import yaml
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from glob import glob
from matplotlib import cm
from matplotlib.ticker import FuncFormatter
from synthesis import run_synthesis

FILE_TO_CLS = os.path.join(os.path.dirname(__file__), '../benchmark/suites/classification.yml')
MODULE_NAME_RE = re.compile(r"[\w+\.\w+]+\.([\S]+)(?<!\))")

def remove_module_name(n):
    for r in MODULE_NAME_RE.finditer(n):
        n = n.replace(r.group(0), r.group(1))
    return n

def extract_solution_from_raw_result(result):
    if not result['result']:
        return []

    return list(map(lambda obj: remove_module_name(obj[0]['solution']),
                    result['result']))

def extract_result(result):
    return {
        'name': result['name'],
        'query': result['query'],
        'candidate': extract_solution_from_raw_result(result),
        'examples': []
    }

def load_experiment_result(groups, dir_name):
    '''Load the experiment result from a given directory.'''
    files = []
    for g in groups.values():
        for b in g.benchmarks:
            files.append(os.path.join(dir_name, '{}.json'.format(b.name)))

    raw_results = []
    for file_name in files:
        with open(file_name) as file: raw_results.append(json.load(file))

    return list(map(extract_result, raw_results))

def count_num_class(class_data, invalid_data, xs):
    num_class = 0
    xs_ = set(xs) - set(invalid_data)
    for cls in class_data:
        diff = xs_ - set(cls)
        if len(diff) != len(xs_): # new class found
            num_class += 1
        xs_ = diff

    if len(xs_) != 0:
        pass # TODO: print(f"fill it later: {xs_}")
    return num_class

def count_num_invalid(invalid_data, xs):
    return len(set(invalid_data).intersection(xs))

def count_misclassifications(row, class_data, invalid_data):
    universal_candidates = row["360-no-filter"]
    filtered_candidates = row["180-filter"]

    if not filtered_candidates: 
        return 0 # all due to timeout?
    
    last_in_universal = ""
    for c in filtered_candidates:
        if c in universal_candidates:
            last_in_universal = c

    if not last_in_universal: 
        return 0 # impossible to classify since they don't share common candidate

    universal_candidates = universal_candidates[:universal_candidates.index(last_in_universal)+1]
    filtered_candidates = filtered_candidates[:filtered_candidates.index(last_in_universal)+1]
    universal_classes = count_num_class(class_data, invalid_data, universal_candidates)
    filtered_classes = count_num_class(class_data, invalid_data, filtered_candidates)

    return universal_classes - filtered_classes

def compute_filter_stats(classifications, row):
    # compute misclassification and timeout

    # timeout: not in 180-filter, in 180-no-filter, in 360-filter

    # misclss: not in 180-filter, not in 360-no-filter, in 180-filter
    # misclss: classify 180-no-filter first, and count number of classes.

    # compute other stats: all synthesized solution, invalids, duplicates, interesting solution
    # since sometimes 180-no-filter include garbage, we use 360-no-filter

    subset_candidates = set(row["180-no-filter"])

    universal_filtered_candidates = set(row["360-filter"])
    subset_filtered_candidates = set(row["180-filter"])

    classification = classifications.loc[row.name[0]]
    cls_invalid = classification["invalid"]
    cls_classes = classification["class"]

    num_all_candidates = len(set(subset_candidates))
    num_invalid_candidates = count_num_invalid(cls_invalid, subset_candidates)
    num_all_classes = count_num_class(cls_classes, cls_invalid, subset_candidates)
    num_all_duplicates = num_all_candidates - num_invalid_candidates - num_all_classes

    num_timeout = len(universal_filtered_candidates.intersection(subset_candidates) - subset_filtered_candidates)
    # num_timeout = len(subset_candidates - subset_filtered_candidates)
    num_misclss = count_misclassifications(row, cls_classes, cls_invalid)

    row['num_timeout'] = num_timeout
    row['num_misclss'] = num_misclss
    row['num_all_candidates'] = num_all_candidates
    row['num_invalid_candidates'] = num_invalid_candidates
    row['num_all_duplicates'] = num_all_duplicates
    row['num_interesting'] = num_all_candidates - num_invalid_candidates - num_all_duplicates

    return row

# the old script
def compute_filter_data(classifications, row):

    all_results = row['180-no-filter']
    all_filter_results = row['360-filter']
    filter_results = row['180-filter']
    classification = classifications.loc[row.name[0]]
    invalid_results = classification["invalid"]
    cls_classes = classification["class"]

    set_all_results = set(all_results)
    set_all_filter_results = set(all_filter_results)
    set_filter_results = set(filter_results)
    set_invalid_results = set(invalid_results)

    loss_timeout = len(set_all_filter_results.intersection(set_all_results) - set_filter_results)
    # loss_timeout = len(set_all_filter_results) - len(set_filter_results)
    if loss_timeout < 0:
        loss_timeout = 0
    if loss_timeout >= len(set_all_results):
        print(set_all_filter_results)
        print(set_filter_results)

    # select H+ w/o filtering
    all_results_ = all_results
    if all_filter_results:
        last_solution = all_filter_results[-1]
        if last_solution in all_results:
            all_results_ = all_results[0:row['180-no-filter'].index(last_solution) + 1]
        else:
            pass
            # print(f"not in row: {row.name}")

    def count_num_classes(classes, xs):
        num_classes = 0
        xs_ = set(xs)
        for cls in classes:
            diff = xs_ - set(cls)
            if len(diff) != len(xs_): # new class found
                num_classes += 1
            xs_ = diff
        num_classes += len(xs_)
        return num_classes

    valid_results = set(all_results_) - set_invalid_results
    num_classes_valid = count_num_classes(cls_classes, valid_results)

    num_loss_misclassification = num_classes_valid - len(all_filter_results)
    num_loss_misclassification = max(num_loss_misclassification, 0)

    row['f_loss_timeout'] = loss_timeout
    row['f_loss_misclas'] = num_loss_misclassification
    row['total_classes'] = len(cls_classes)
    row['num_invalid'] = len(all_results) - len(filter_results) - loss_timeout - num_loss_misclassification#len(set_all_results - set_all_filter_results)
    row['num_useful'] = len(filter_results)
    row['total_solutions'] = len(all_results)

    return row

def plot_graph(output_dir, value_df):
    # prepare the percentages
    # graph_df = value_df.loc[:,['num_timeout', 'num_misclss', 'num_all_candidates', 'num_invalid_candidates', 'num_all_duplicates']]
    # graph_df.loc[:,'loss_timeout'] = graph_df.loc[:,'num_timeout'] / graph_df.loc[:,'num_all_candidates']
    # graph_df.loc[:,'loss_misclss'] = graph_df.loc[:,'num_misclss'] / graph_df.loc[:,'num_all_candidates']
    # graph_df.loc[:,'invalids'] = graph_df.loc[:,'num_all_duplicates'] / graph_df.loc[:,'num_all_candidates']
    # graph_df.loc[:,'useful'] = 1 - graph_df.loc[:,'invalids'] - graph_df.loc[:,'loss_misclss'] - graph_df.loc[:,'loss_timeout']
    # graph_df = graph_df.sort_values(by=['useful', 'loss_misclss', 'loss_timeout'])
    # graph_df = graph_df.reset_index().set_index('name')
    # if 'containsEdge' in graph_df:
    #     graph_df = graph_df.drop(['containsEdge'], axis=0)

    bar_df = value_df.loc[:,['f_loss_timeout', 'f_loss_misclas', 'total_solutions', 'num_useful']]
    bar_df['loss_timeout'] = bar_df['f_loss_timeout'] / bar_df['total_solutions']
    bar_df['loss_misclas'] = bar_df['f_loss_misclas'] / bar_df['total_solutions']
    bar_df['useful'] = bar_df['num_useful'] / bar_df['total_solutions']
    bar_df['sum'] = (bar_df['f_loss_timeout'] + bar_df['f_loss_misclas'] + bar_df['num_useful']) / bar_df['total_solutions']
    bar_df['bad'] = 1 - bar_df['sum']
    graph_df = bar_df.sort_values(by=['sum', 'useful'])
    graph_df = graph_df.reset_index().set_index('name')
    if 'indexesOf' in graph_df:
        graph_df = graph_df.drop(['indexesOf'])

    # plot the histogram
    # bar_df_ = graph_df[['useful', 'loss_timeout', 'loss_misclss', 'invalids']]
    bar_df_ = graph_df[['useful', 'loss_misclas', 'loss_timeout', 'bad']]
    ax = bar_df_.plot.bar(stacked=True, figsize=(14, 7),
                          color=['#51127c', '#fc8961', '#b73779','gainsboro'])
    ax.yaxis.set_major_formatter(FuncFormatter(lambda x, _: '{:.0%}'.format(x)))
    # ax.xaxis.set_visible(False)
    ax.axhline(1, color='k', linestyle='--')
    ax.legend(labels=['All H+ Solutions',
                      'Interesting Solutions',
                      'Loss by Timeout',
                      'Loss by Misclassification',
                      'Duplicates and Crashings'], loc='upper left')
    ax.set_ylabel('Percents of all generated solutions')
    plt.savefig(os.path.join(output_dir, 'filtering.png'))

def run_filtering(groups, output_dir):
    experiments = [('180-filter', ['--disable-filter=False', '--cnt=10'], 180),
                   ('180-no-filter', ['--disable-filter=True', '--cnt=10'], 180),
                   ('360-filter', ['--disable-filter=False', '--cnt=10'], 360)]
                #    ('360-no-filter', ['--disable-filter=True', '--cnt=30'], 360)]

    # run the experiment and store the returns
    dfs = []
    for exp, options, t in experiments:
        # create the directories if needed
        exp_dir = os.path.join(output_dir, exp)
        if not os.path.isdir(exp_dir):
            os.makedirs(exp_dir)

        run_synthesis(groups, exp_dir, t, options, True)

        df = pd.DataFrame(load_experiment_result(groups, exp_dir)).set_index(["name", "query"])
        dfs.append(df[["candidate"]].rename({"candidate": exp},
                                            axis="columns"))
    exp_df = pd.concat(dfs, axis=1, sort=False)

    # load the manual classification from the file
    with open(FILE_TO_CLS, 'r') as f:
        classifications = yaml.load(f, Loader=yaml.FullLoader)
    classifications = pd.DataFrame(classifications).set_index('name')

    # value_df = exp_df.apply(lambda r: compute_filter_stats(classifications, r),
    #                         axis=1)
    value_df = exp_df.apply(lambda r: compute_filter_data(classifications, r), axis=1,
                            result_type='expand')
    plot_graph(output_dir, value_df)
