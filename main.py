import argparse
import json
import os
import subprocess
import sys

import parse_logs
import gather_results
import utility

if __name__ == "__main__":
    argument_parser = argparse.ArgumentParser(
        description="This tool parses, gathers and visualizes results of KaMinPar experiments.")

    argument_parser.add_argument("-c", "--config", default="./config.json", type=argparse.FileType('r'),
                                 required=False,
                                 help="The path to the configuration file. (default='./config.json')",
                                 metavar="<config>", dest="config_file_path")

    args = argument_parser.parse_args()

    with open(args.config_file_path.name) as config_file:
        config = json.load(config_file)

    EXPERIMENTS_BASE_DIR = config['input']['experiments-dir']
    LOG_SUFFIX = config['input']['log-suffix']

    OVERRIDE = config['parse']['override']

    RESULT_FORMAT = config['parse']['result']['format']
    RESULT_NAME = config['parse']['result']['name']
    if RESULT_NAME == "":
        # empty string is interpreted that the name of the graph should be used as result name
        RESULT_NAME = None
    RESULT_SUFFIX = config['parse']['result']['suffix']

    GATHER_COLUMN_KEY_MAPPINGS = config['gather']['column-key-mapping']

    with os.scandir(EXPERIMENTS_BASE_DIR) as experiments_dir_iterator:
        for experiment_dir in experiments_dir_iterator:
            utility.print_banner(experiment_dir.name, lines=5)
            print()
            utility.print_banner("PARSE logs:", lines=1)
            parse_logs.parse_all_logs(base_dir=experiment_dir.path,
                                      log_file_suffix=LOG_SUFFIX,
                                      override=OVERRIDE,
                                      result_name=RESULT_NAME,
                                      result_suffix=RESULT_SUFFIX,
                                      verbose=False)

            print()
            utility.print_banner("GATHER results:", lines=1)
            gather_results.gather_experiment_results(experiment_dir, GATHER_COLUMN_KEY_MAPPINGS)
            print("\n")

    utility.print_banner("CREATE plots:", lines=5)
    print()

    create_plots_output = subprocess.run([f"RScript", f"create_plots.R", "--args", f"{EXPERIMENTS_BASE_DIR}",
                                          f"{OVERRIDE}"],
                                         cwd="./R",
                                         shell=True,
                                         stdout=sys.stdout,
                                         stderr=sys.stderr)

    if create_plots_output.returncode != 0:
        exit(create_plots_output.returncode)
