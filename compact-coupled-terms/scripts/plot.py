import argparse
import itertools
import matplotlib.pyplot as plt

class Result:
    def __init__(self, name, time):
        self.name = name
        self.time = time

def parse_csv(file_name):
    """
    Parses a CSV file and returns a list of lists.
    """
    with open(file_name, 'r') as file:
        lines = file.readlines()
        lines = lines[1:] # discard first line
        results = []
        for i in range(0, len(lines), 2):
            name = lines[i].strip().split('\t')[0]
            if 'None' in lines[i+1]:
                time = None
            else:
                time = float(lines[i+1])
            results.append(Result(name, time))
        
    return results

def parse_tsv(file_name):
    """
    Parses a TSV file and returns a list of lists.
    """
    with open(file_name, 'r') as file:
        lines = file.readlines()
        lines = lines[1:] # discard first line
        results = []
        for line in lines:
            columns = line.strip().split('\t')
            time = float(columns[2]) if 'None' not in columns[2] else None
            results.append(Result(columns[0], time))

    return results

def avg_if_not_timeout(xs, threshold):
    xs = [x for x in xs if x < threshold]
    if len(xs):
        return sum(xs) / len(xs)
    else:
        return threshold

def aggregate_results(results_by_file, threshold):
    results_by_exp = []
    for i in range(0, len(results_by_file), 3):
        new_results = []
        # average the time every three files
        for j in range(len(results_by_file[i])):
            if results_by_file[i][j].time is not None:
                avg_time = avg_if_not_timeout([results_by_file[i][j].time, results_by_file[i+1][j].time, results_by_file[i+2][j].time], threshold)
                new_results.append(Result(results_by_file[i][j].name, avg_time))
            else:
                new_results.append(Result(results_by_file[i][j].name, None))

        results_by_exp.append(new_results)

    return results_by_exp

def plot_cactus(files, output='ablation.pdf'):
    assert len(files) % 3 == 0, 'Number of files must be a multiple of 3.'

    results_by_file = []
    for file in files:
        results = parse_csv(file)
        results_by_file.append(results)

    results_by_exp = aggregate_results(results_by_file, 300)
    marker = itertools.cycle(('v', '^', '+', 'x', 'o'))
    for exp_result in results_by_exp:
        real_results = [result.time for result in exp_result if result.time is not None and result.time < 300]
        y = list(range(1, len(real_results)+1)) + [len(real_results)]
        x = sorted(real_results) + [305]
        plt.plot(x, y, marker='^')
        plt.plot([], [], label='_nolegend_')

    plt.hlines(45, 0, 300, linestyles='dashed', color='gray', label="Total # of benchmarks")
    plt.xlim(-3, 300)
    plt.ylim(0, 47)
    plt.yticks(range(0, 50, 5))
    plt.legend(["Hectare", "Hectare-StaticOnly", "Hectare-DynamicOnly", "Total # of benchmarks"], loc="lower right")
    plt.xlabel("Time (s)")
    plt.ylabel("# Benchmarks Solved")


    #If fonttype = 1 doesn't work with LaTeX, try fonttype 42.
    plt.rc('pdf',fonttype = 42)
    plt.rc('ps',fonttype = 42)
    
    plt.savefig(output)
    plt.close()

def plot_cactus_stackoverflow(files, output='ablation.pdf'):
    assert len(files) % 3 == 0, 'Number of files must be a multiple of 3.'

    results_by_file = []
    for file in files[:3]:
        results = parse_csv(file)
        results_by_file.append(results)

    for file in files[3:]:
        results = parse_tsv(file)
        results_by_file.append(results)

    results_by_exp = aggregate_results(results_by_file, 600)
    marker = itertools.cycle(('v', '^', '+', 'x', 'o')) 
    for exp_result in results_by_exp:
        real_results = [result.time for result in exp_result if result.time is not None and result.time < 600]
        y = list(range(1, len(real_results)+1)) + [len(real_results)]
        x = sorted(real_results) + [605]
        plt.plot(x, y, marker='^')

    plt.hlines(19, 0, 600, linestyles='dashed', color='gray', label="Total # of benchmarks")
    plt.xlim(-3, 600)
    plt.ylim(0, 20)
    plt.yticks([0, 5, 10, 15, 19])
    plt.legend(["Hectare", "HplusAll", "Total # of benchmarks"], loc="center right")
    plt.xlabel("Time (s)")
    plt.ylabel("# Benchmarks Solved")


    #If fonttype = 1 doesn't work with LaTeX, try fonttype 42.
    plt.rc('pdf',fonttype = 42)
    plt.rc('ps',fonttype = 42)
    
    plt.savefig(output)
    plt.close()

def plot_cactus_hplus(files, output='eval_hplus.pdf'):
    assert len(files) % 3 == 0, 'Number of files must be a multiple of 3.'

    results_by_file = []
    for file in files[:3]:
        results = parse_csv(file)
        results_by_file.append(results)

    for file in files[3:]:
        results = parse_tsv(file)
        results_by_file.append(results)

    results_by_exp = aggregate_results(results_by_file, 300)
    marker = itertools.cycle(('v', '^', '+', 'x', 'o')) 
    for exp_result in results_by_exp:
        real_results = [result.time for result in exp_result if result.time is not None and result.time < 300]
        y = list(range(1, len(real_results)+1)) + [len(real_results)]
        x = sorted(real_results) + [305]
        plt.plot(x, y, marker='^')

    plt.hlines(45, 0, 300, linestyles='dashed', color='gray', label="Total # of benchmarks")
    plt.xlim(-3, 300)
    plt.ylim(0, 46)
    plt.yticks(range(0, 46, 5))
    plt.legend(["Hectare", "Hoogle+", "Total # of benchmarks"], loc="lower right")
    plt.xlabel("Time (s)")
    plt.ylabel("# Benchmarks Solved")


    #If fonttype = 1 doesn't work with LaTeX, try fonttype 42.
    plt.rc('pdf',fonttype = 42)
    plt.rc('ps',fonttype = 42)
    
    plt.savefig(output)
    plt.close()

def process_scatter_data(files):
    assert len(files) == 6, 'Number of files must be 6.'

    results_by_file = []
    for file in files[:3]:
        results = parse_csv(file)
        results_by_file.append(results)

    for file in files[3:]:
        results = parse_tsv(file)
        results_by_file.append(results)

    results_by_exp = aggregate_results(results_by_file, 300)
    assert len(results_by_exp) == 2, 'Number of experiments must be 2.'

    def regularize(xs):
        return [min(x.time, 300) for x in xs if x.time is not None]

    x = regularize(results_by_exp[0])
    y = regularize(results_by_exp[1])
    return x, y

def plot_scatter(files, output='scatter.pdf'):
    x, y = process_scatter_data(files)

    # plt.figure(figsize=(4,5))
    plt.scatter(x, y)
    plt.plot([0, 300], [0, 300], 'k--')
    plt.xlim(-3, 303)
    plt.ylim(-3, 303)

    plt.xlabel("Hectare Time (s)")
    plt.ylabel("Hoogle+ Time (s)")

    #If fonttype = 1 doesn't work with LaTeX, try fonttype 42.
    plt.rc('pdf',fonttype = 42)
    plt.rc('ps',fonttype = 42)
    
    plt.savefig(output)
    plt.close()

def plot_scatter_broken(files, output='baseline_broken.pdf'):
    x, y = process_scatter_data(files)

    # plot core data
    fig, (ax1, ax2) = plt.subplots(1, 2, sharey=True)
    fig.subplots_adjust(wspace=0.05)  # adjust space between axes

    ax1.set_xlim(0, 7)
    ax2.set_xlim(7, 303)
    ax1.set_ylim(-3, 303)
    ax1.set_xlabel("Hectare Time (s)", loc="right")
    ax1.set_ylabel("Hoogle+ Time (s)")
    ax1.scatter(x, y)
    ax2.scatter(x, y)
    ax1.plot([0, 300], [0, 300], 'k--')
    ax2.plot([0, 300], [0, 300], 'k--')

    # set border lines
    ax1.spines.right.set_visible(False)
    ax2.spines.left.set_visible(False)
    ax1.yaxis.tick_left()
    ax2.yaxis.tick_right()
    ax2.tick_params(labelright=False)

    # plot break lines
    d = .5  # proportion of vertical to horizontal extent of the slanted line
    kwargs = dict(marker=[(-d, -1), (d, 1)], markersize=12,
                linestyle="none", color='k', mec='k', mew=1, clip_on=False)
    ax2.plot([0, 0], [0, 1], transform=ax2.transAxes, **kwargs)
    ax1.plot([1, 1], [0, 1], transform=ax1.transAxes, **kwargs)

    plt.rc('pdf',fonttype = 42)
    plt.rc('ps',fonttype = 42)
    
    plt.savefig(output)
    plt.close()

def plot_scatter_zoomin(files, output='baseline_zoomin.pdf'):
    x, y = process_scatter_data(files)

    # plot core data
    fig, (ax1, ax2) = plt.subplots(1, 2, sharey=True,
                                   gridspec_kw={
                                       'width_ratios': [3, 1]})
    # fig.subplots_adjust(wspace=0.05)  # adjust space between axes

    ax1.set_xlim(-3, 303)
    ax2.set_xlim(0, 7)
    ax1.set_ylim(-3, 303)
    ax1.set_xlabel("Hectare Time (s)", loc="right")
    ax1.set_ylabel("Hoogle+ Time (s)")
    ax1.scatter(x, y)
    ax2.scatter(x, y)
    ax1.plot([0, 300], [0, 300], 'k--')
    ax2.plot([0, 300], [0, 300], 'k--')
    ax2.xaxis.set_ticks([0,3,7])

    # set border lines
    # ax1.spines.right.set_visible(False)
    # ax2.spines.left.set_visible(False)
    # ax1.yaxis.tick_left()
    # ax2.yaxis.tick_right()
    # ax2.tick_params(labelright=False)

    # plot break lines
    # d = .5  # proportion of vertical to horizontal extent of the slanted line
    # kwargs = dict(marker=[(-d, -1), (d, 1)], markersize=12,
    #             linestyle="none", color='k', mec='k', mew=1, clip_on=False)
    # ax2.plot([0, 0], [0, 1], transform=ax2.transAxes, **kwargs)
    # ax1.plot([1, 1], [0, 1], transform=ax1.transAxes, **kwargs)

    plt.rc('pdf',fonttype = 42)
    plt.rc('ps',fonttype = 42)
    
    plt.savefig(output)
    plt.close()

def run():
    parser = argparse.ArgumentParser(description='Plot the results of a simulation.')
    parser.add_argument('--files', nargs='+', help='The CSV file to plot.')
    parser.add_argument('--type', help='The type of plot to produce.', 
        choices=['ablation', 'baseline', 'baseline-broken', 'baseline-zoomin', 'stackoverflow', 'hplus-cactus'])
    parser.add_argument('--output', help='The output file to write.')
    
    args = parser.parse_args()

    if args.type == 'ablation':
        plot_cactus(args.files, args.output)
    elif args.type == 'baseline':
        plot_scatter(args.files, args.output)
    elif args.type == 'baseline-broken':
        plot_scatter_broken(args.files, args.output)
    elif args.type == 'baseline-zoomin':
        plot_scatter_zoomin(args.files, args.output)
    elif args.type == 'stackoverflow':
        plot_cactus_stackoverflow(args.files, args.output)
    elif args.type == 'hplus-cactus':
        plot_cactus_hplus(args.files, args.output)

if __name__ == "__main__":
    run()