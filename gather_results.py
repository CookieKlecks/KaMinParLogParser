import argparse
import csv
import json
import os
import pathlib

import parse_logs
import utility

JSON_SUFFIX = '.json'
KEY_PATH_SEPARATOR = '.'


def gather_run_results(run_path, result_columns_mapping):
    with os.scandir(run_path) as single_run_iterator:
        for single_run in single_run_iterator:
            if not single_run.is_dir():
                continue
            run_name = get_run_name(single_run.path)
            print(f"========= RUN: {run_name} ==============")
            gather_graph_results(single_run.path, result_columns_mapping, run_path, run_name)


def get_run_name(single_run_path):
    return pathlib.Path(single_run_path).name


def gather_graph_results(single_run_path, result_columns_mapping, gathered_results_path, gathered_results_name,
                         gathered_results_suffix=".csv"):
    """
    Gathers the results of all tested graphs for one run configuration and saves them at gathered_results_path.

    It creates a csv-file at the base path of the run directory (single_run_path) where each row represents one result
    of one graph (=> if several rounds were calculated on the same graph, one row per round is created).

    The columns are picked according to the result_columns_mapping dictionary (ordered according to the iterator of the
    dictionary), where each key is the name of one column in the resulting csv-file and its value is the dot-separated
    key path in the result JSON-file of one graph. Example:
        result_columns_mapping = {
            totalPartitionTime: "TIME.partitioning"
        }
    => one column named 'totalPartitionTime' where each row contains result_file["TIME"]["partitioning"]

    The folder structure of the directory at location single_run_path is expected as follows:

    single_run_path
        |___ graph_1
            |___ result_1.json
            |___ result_2.json
            |___ ...
        |___ graph_2
            |___ ...
        |___ ...

    where the result JSON-files can be arbitrarily name. They only have to have the .json ending. NOTE: this implies,
    that **every** json file in the graph directory is parsed!

    :param single_run_path: Path to the base directory of this run, where every graph folder is located.
    :param result_columns_mapping: dictionary, mapping column names in the result to paths in the JSON files.
    :param str or os.Path gathered_results_path: Base path where the results should be saved
    :param str or os.Path gathered_results_name: Base name of the created file.
    :param str gathered_results_suffix: suffix of the created file.
    """
    gathered_results = []

    with os.scandir(single_run_path) as run_dir:
        for graph_dir in run_dir:  # walk through base dir
            if not graph_dir.is_dir():
                continue
            with os.scandir(graph_dir.path) as graph_dir_iterator:
                for result_file in graph_dir_iterator:
                    if not result_file.is_file():
                        continue
                    if not pathlib.Path(result_file).suffix == JSON_SUFFIX:
                        continue
                    # result_file is actual a .json file => extract the contained results
                    gathered_results += [extract_single_results(result_file.path, result_columns_mapping)]

    result_path = pathlib.Path(gathered_results_path, gathered_results_name + gathered_results_suffix)
    print(f"SAVING gathered results in {result_path}")
    save_gathered_results(result_path, gathered_results, result_columns_mapping.keys(), quoting=csv.QUOTE_NONNUMERIC)


def extract_single_results(result_json_path, result_columns_mapping):
    extracted_results = []
    with open(result_json_path) as single_result_file:
        single_result = json.load(single_result_file)
        for key_path in result_columns_mapping.values():
            value = utility.dict_get(single_result, key_path, path_separator=KEY_PATH_SEPARATOR)
            if value is None:
                raise ValueError(f"Result file at {result_json_path} is missing value for key {key_path}.")
            extracted_results += [value]
    return extracted_results


def save_gathered_results(path, gathered_results, column_names, **csv_writer_fmt_params):
    """
    
    :param path: 
    :param gathered_results: 
    :param column_names:
    :param csv_writer_fmt_params: (see https://docs.python.org/3/library/csv.html#csv-fmt-params)
    :return: 
    """
    with open(path, "w", newline='') as file:
        writer = csv.writer(file, **csv_writer_fmt_params)
        writer.writerow(column_names)
        for single_results in gathered_results:
            writer.writerow(single_results)


if __name__ == '__main__':
    argument_parser = argparse.ArgumentParser(
        description="A tool to gather all JSON-results from each graph for each single run in the passed directory. "
                    "The important information is saved in one csv-file for each run.")

    argument_parser.add_argument("runs_dir", type=pathlib.Path,
                                 help="The directory in which the single runs to gather can be found.",
                                 metavar="<runs-directory>")
    argument_parser.add_argument("-c", "--config", default="./config.json", type=argparse.FileType('r'),
                                 required=False, help="The path to the configuration file. (default='./config.json')",
                                 metavar="<config>", dest="config_file_path")
    args = argument_parser.parse_args()

    #TODO: parse commandline config file

    gather_run_results(args.runs_dir,
                       {
                           "graph": "INPUT.graph",
                           "n": "INPUT.n",
                           "part_time": "TIME.partitioning._"
                       })
