"""
This file contains methods to parse KaMinPar output logs, extract important information and safe it in an organized
JSON-file.

If run on its own, you have to pass a config file. The parser then recursively walks through the stated directory and
parses every *.log file into a JSON-file.

However you can also just import this file and use the provided functions to parse logs.
"""
import json
import os
import argparse
import pathlib
import utility
import aggregate


def parse_all_logs(base_dir,
                   keywords,
                   aggregate_values_dict=None,
                   log_file_suffix=".log",
                   override=False,
                   result_name=None,
                   result_suffix=".json",
                   verbose=True):
    """
    Recursively walk through base_dir and parse every file ending in log_file_suffix. If override==False a log file is
    only parsed if no result name with the corresponding result name exists. Otherwise older created results are
    overridden.
    The parser creates JSON-files containing the relevant information about the context, input graph, result and times
    stated in a log file. The result file is saved in the same directory as the log file. The base name is hereby the
    same as the log file, if result_name==None, otherwise it equals the passed result_name. The suffix can also be
    controlled via the result_suffix parameter.

    :param base_dir: Path to the base directory from which the recursive parsing should be started.
    :type base_dir: str or Path
    :param list[str] keywords: list of keywords with which the important lines start.
    :param dict aggregate_values_dict: a dictionary stated which values should be aggregated after reading the logs.
                            The keys in the dict represent a .-separated key path to a value.
                            The corresponding values are strings, that state the aggregate method.
                            To see how values are aggregated @see aggregate::aggregate_parsed_logs
    :param str log_file_suffix: File suffix to identify a log file.
    :param bool override: Whether existing result files should be overridden, or logs with existing result files
                            should  be skipped. NOTE: existing result files are only identified if they are named
                            according to the current result_name and result_suffix.
    :param str result_name: The base name of the created result file. If None, the basename of the log file is taken.
    :param str result_suffix: The file suffix of the created result files.
    :param bool verbose: if true, additional output will be generated.
    """
    print(f'Walking through: {base_dir}')
    for (dir_path, _, filenames) in os.walk(base_dir):
        for log_file_name in filenames:
            log_file = pathlib.Path(os.path.join(dir_path, log_file_name))
            if log_file.suffix == log_file_suffix:
                result_path = generate_result_path(log_file, result_name, result_suffix)
                if not override and result_path.exists():
                    continue
                if verbose:
                    print(f'GENERATE results: {result_path}')
                results = parse_log(log_file, keywords)

                if aggregate_values_dict is not None:
                    results = aggregate.aggregate_parsed_logs(results, aggregate_values_dict)

                with open(result_path, "w") as result_file:
                    json.dump(results, result_file, sort_keys=True, indent=4)


def parse_log(log_file_path, keywords):
    with open(log_file_path) as log_file:
        line = log_file.readline().strip()

        results = {}
        while line:
            # remove \n at end of line and whitespace characters
            line = line[:-1].strip()
            for keyword in keywords:
                if line.startswith(keyword):
                    parsed_object = parse_key_value_pairs(line[len(keyword) + 1:])
                    if keyword in results.keys():
                        results[keyword] = utility.merge_dicts_by_keys(results[keyword], parsed_object)
                    else:
                        results[keyword] = parsed_object
                    break
            # parse time
            if line.startswith("io="):
                results["TIME"] = parse_key_value_pairs(line)

            line = log_file.readline()
    return results


def parse_key_value_pairs(key_value_pairs):
    pairs = key_value_pairs.split(' ')
    parsed_pairs_list = map(parse_key_value_pair, pairs)
    parsed_pairs_dict = {}
    for parsed_pairs in parsed_pairs_list:
        parsed_pairs_dict = utility.merge_dicts_by_keys(parsed_pairs_dict, {parsed_pairs[0]: parsed_pairs[1]})
    return parsed_pairs_dict


def parse_key_value_pair(key_value_pair):
    split = key_value_pair.split("=")
    key = split[0]
    value = split[1]

    # if key contains '.', interpret the dot-separated key names as self nested keys.
    # e.g. "parent.child=val" => {parent: {child: val}}
    split_keys = key.split(".")
    if len(split_keys) > 1:
        key = split_keys[0]
        # key contains dots => turn value into nested object
        for nested_key in reversed(split_keys[1:]):
            value = {nested_key: value}

    # if the value is a number, try to convert it.
    value = utility.try_parse_int_or_float(value)
    return key, value


def generate_result_path(log_file, result_name, result_suffix):
    """
    Generate the name of the result file according to the passed result_name and result_suffix for the passed log file.

    :param Path log_file: Path of the log file which should be parsed and for which a result name should be
                                generated.
    :param str result_name: name of the result file (excluding file extension). If None the name of the log file
                                    is used.
    :param str result_suffix: The file ending of the result file.
    :return Path for a result file for the log_file.
    :rtype Path
    """
    if result_name is None:
        result_name = log_file.stem

    return pathlib.Path(os.path.join(log_file.parent, result_name + result_suffix))


if __name__ == '__main__':
    argument_parser = argparse.ArgumentParser(description="A tool to parse the important information of a KaMinPar log "
                                                          "file into a JSON-files.")
    argument_parser.add_argument("-c", "--config", default="./config.json", type=argparse.FileType('r'),
                                 required=False, help="The path to the configuration file. (default='./config.json')",
                                 metavar="<config>", dest="config_file_path")
    args = argument_parser.parse_args()
    CONFIG_FILE = pathlib.Path(args.config_file_path.name)

    with open(CONFIG_FILE) as config_file:
        config = json.load(config_file)

    # INPUT configs
    EXPERIMENTS_DIR = pathlib.Path(config['input']['experiments-dir'])
    LOG_FILE_SUFFIX = config['input']['log-suffix']

    # PARSE configs
    RESULT_OVERRIDE = config['parse']['override']
    KEYWORDS = config['parse']['keywords']
    RESULT_NAME = config['parse']['result']['name']
    if RESULT_NAME == "":
        RESULT_NAME = None
    RESULT_SUFFIX = config['parse']['result']['suffix']

    AGGREGATE = config['aggregate']

    parse_all_logs(EXPERIMENTS_DIR, KEYWORDS, AGGREGATE, LOG_FILE_SUFFIX, RESULT_OVERRIDE, RESULT_NAME, RESULT_SUFFIX)
