import pathlib
from string import Template


def generate_run_configs(base_config, variable_values, filename, output_path):
    """
    This function generates run configuration files. It uses template strings. For each template key an array of
    possible values can be specified and for every possible combination of values, one configuration out of the given
    template is created.
    One configuration file consists of of an array of template strings. Each element represents one line in the file.

    :param list[str] base_config: A list of template strings. Each element represents one line in the config file.
    :param variable_values: dictionary where each template key is also a key in the dictionary. The corresponding
                    value is an array of possible values that should be used to replace the key in the template.
    :param filename: filename as Template string
    :param output_path: directory where the created config files should be saved.
    :return:
    """
    indices = {}
    for key in variable_values:
        indices[key] = 0
    keys = list(indices.keys())

    current_values = {}
    found_all_combinations = False
    while not found_all_combinations:
        for key, value in variable_values.items():
            current_values[key] = value[indices[key]]

        # create config for current values
        with open(output_path / filename.substitute(**current_values), 'w', newline='\n') as config_file:
            for line in base_config:
                substituted_line = Template(line).substitute(**current_values)
                config_file.write(substituted_line)
                config_file.write('\n')

        # increase indices like a k-ary counter
        keys_iterator = iter(keys)

        current_key = next(keys_iterator)
        indices[current_key] += 1
        while indices[current_key] >= len(variable_values[current_key]):
            indices[current_key] = 0
            try:
                current_key = next(keys_iterator)
                indices[current_key] += 1
            except StopIteration:
                found_all_combinations = True
                break


if __name__ == '__main__':
    OUTPUT_PATH = pathlib.Path("C:\\Users\\Cedrico.DESKTOP-3BCMGI6\\KIT\\BA\\experiments\\configs\\generated")

    run_name = "pc_v2_${target_block_selector}_${max_nzs}k_${cluster_size}_bfs${bfs_distance}"

    promising_cluster_config = [
        f"RUN_NAME={run_name}",
        "RUN_DISPLAY_NAME=\"ILP Promising Cluster ${max_nzs}k nzs ${time_limit}s with ${cluster_size} cluster, "
        "${pc_num_seeds} seeds bfs distance ${bfs_distance} and ${max_improvements} max improvements\"",
        "",
        "REF_ALGO=ilp",

        "BASE_PC_PARAMS=\"--threads 1 --r-ilp-move-selector=promising-cluster "
        "--r-lp-num-iterations=50 --r-ilp-pre-refiner=lp\"",

        "OTHER_PARAMS=\"$$BASE_PC_PARAMS --r-ilp-time-limit=${time_limit}"
        " --r-ilp-max-iterations=${max_iterations}"
        " --r-ilp-min-cut-change=${min_cut_change}"
        " --r-ilp-max-nzs=${max_nzs}000"
        " --r-ilp-pc-max-cluster=${cluster_size}"
        " --r-ilp-pc-num-seeds=${pc_num_seeds}"
        " --r-ilp-pc-target-block-selector=${target_block_selector}"
        " --r-ilp-pc-bfs-distance=${bfs_distance}"
        " --r-ilp-max-improvements=${max_improvements}"
        " --r-ilp-add-balance-obj=${balance_objective}\""
    ]

    complete_tuning = {
        'max_nzs': [10, 50, 100, 150],
        'max_iterations': [5],
        'min_cut_change': [0.0001],
        'time_limit': [5, 10, 20, 60],
        'cluster_size': [10, 20, 30],
        'pc_num_seeds': [1, 10, 20],
        'max_improvements': [2],
        'balance_objective': [False],
        'target_block_selector': ["best-and-random", "noisy-top"],
        'bfs_distance': [2, 4, 8]
    }

    no_time_limit = complete_tuning
    no_time_limit['time_limit'] = [20]

    no_time_few_cluster_few_nzs = complete_tuning
    no_time_few_cluster_few_nzs['time_limit'] = [20]
    no_time_few_cluster_few_nzs['cluster_size'] = [10, 30]
    no_time_few_cluster_few_nzs['max_nzs'] = [50, 100]

    custom_tuning = {
        'max_nzs': [100],
        'max_iterations': [4],
        'min_cut_change': [0.0001],
        'time_limit': [20],
        'cluster_size': [10, 30],
        'pc_num_seeds': [1],
        'max_improvements': [200],
        'balance_objective': [False],
        'target_block_selector': ["best-and-random", "noisy-top"],
        'bfs_distance': [2, 4, 8]
    }

    generate_run_configs(base_config=promising_cluster_config,
                         variable_values=custom_tuning,
                         filename=Template(run_name + ".config"),
                         output_path=OUTPUT_PATH)
