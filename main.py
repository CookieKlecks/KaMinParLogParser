import json
import os
from pathlib import Path

class Result:
    def __init__(self):
        self.graph_name = ""
        self.n = -1
        self.m = -1
        self.k = -1
        self.cut = -1
        self.balance = -1
        self.total_time = -1

def generate_result_path(config, log_file):
    """
    Generate the name of the result file according to the passed config for the passed log file.

    :param {} config: dictionary with config information. It has to include following information:
                    - output.result.name: name of the result file (excluding file extension). If empty string the name
                                            of the log file is used.
                    - output.result.suffix: the file ending of the result file
    :param {os.Path} log_file: Path of the log file which should be parsed and for which a result name should be
                                generated.
    :return {os.Path}: Path for a result file for the log_file.
    """
    result_name = config['output']['result']['name']
    if result_name == "":
        result_name = log_file.stem
    result_suffix = config['output']['result']['suffix']

    return Path(os.path.join(log_file.parent, result_name + result_suffix))


def parse_key_value_pair(key_value_pair):
    split = key_value_pair.split("=")
    return split[0], split[1]


def parse_log(log_file_path):
    with open(log_file_path) as log_file:
        line = log_file.readline().strip()

        results = {}
        while line:
            # remove \n at end of line and whitespace characters
            line = line[:-1].strip()
            keywords = ["INPUT", "RESULT"]
            for keyword in keywords:
                if line.startswith(keyword):
                    values = line[len(keyword) + 1:].split(" ")
                    values = dict(map(parse_key_value_pair, values))
                    results[keyword] = values
                    break
            # parse time
            if line.startswith("io="):
                values = line.split(" ")
                values = dict(map(parse_key_value_pair, values))
                results["TIME"] = values

            line = log_file.readline()
    return results


# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    CONFIG_FILE = "./config.json"

    with open("./config.json") as config_file:
        config = json.load(config_file)

    # INPUT configs
    RUNS_DIR = Path(config['input']['runs-dir'])
    LOG_FILE_SUFFIX = config['input']['log-suffix']

    # OUTPUT configs
    RESULT_OVERRIDE = config['output']['override']

    print(f'Walking through: {RUNS_DIR}')
    for (dir_path, _, filenames) in os.walk(RUNS_DIR):
        for log_file_name in filenames:
            log_file = Path(os.path.join(dir_path, log_file_name))
            if log_file.suffix == LOG_FILE_SUFFIX:
                result_path = generate_result_path(config, log_file)
                if not RESULT_OVERRIDE and result_path.exists():
                    continue
                print(f'GENERATE results: {result_path}')
                results = parse_log(log_file)

                with open(result_path, "w") as result_file:
                    json.dump(results, result_file, indent=4)
