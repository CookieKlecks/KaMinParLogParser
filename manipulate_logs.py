import os
import pathlib
import tempfile
from typing import Union

Path = Union[pathlib.Path, str]


def add_ilp_counter_to_gurobi_partial_solution(experiment_dir: Path, override: bool = True,
                                               result_dir: Path = None, log_file_suffix: str = ".log",
                                               verbose: bool = False) -> None:
    """
    This utility functions manipulates logs of experiments that evaluate the partial solutions of the ILP runs.
    It appends to every line starting with 'GUROBI_PARTIAL_SOLUTION' or 'ILP_SOLUTION' a value 'ilp_id={n}'.
    The value n can be interpreted as:
        This partial solution is from the n-th ILP optimization globally in this run.

    Different ILP runs are expected to be separated by lines starting with 'ILP_SOLUTION'.

    Other lines are not manipulated.-

    Example:
    These logs:
    GUROBI_PARTIAL_SOLUTION objective=1 cur_best=0 ...
    ILP_SOLUTION ...
    GUROBI_PARTIAL_SOLUTION objective=1 cur_best=0 ...
    GUROBI_PARTIAL_SOLUTION objective=1 cur_best=3 ...
    Number solved objectives: 1
    GUROBI_PARTIAL_SOLUTION objective=2 cur_best=-250 ...
    ILP_SOLUTION ...

    are transformed to this:
    GUROBI_PARTIAL_SOLUTION objective=1 cur_best=0 ... ilp_id=0
    ILP_SOLUTION ...
    GUROBI_PARTIAL_SOLUTION objective=1 cur_best=0 ... ilp_id=1
    GUROBI_PARTIAL_SOLUTION objective=1 cur_best=3 ... ilp_id=1
    Number solved objectives: 1
    GUROBI_PARTIAL_SOLUTION objective=2 cur_best=-250 ... ilp_id=1
    ILP_SOLUTION ...

    It is used to correctly create plots for the partial solutions.

    :param Path experiment_dir: The directory where log files are recursively searched
    :param override: Whether the original logs should be overwritten. If override==false you have to specify result_dir!
    :param result_dir: The directory where the manipulated logs should be saved. The log files maintain the same
                        relative structure as they had in experiment_dir.
                        WARNING: only used if override==false
    :param log_file_suffix: the file suffix to identify log files.
    :param verbose: whether verbose output should be activated
    """
    experiment_dir = pathlib.Path(experiment_dir)
    result_dir = pathlib.Path(result_dir)
    if override:
        result_dir = experiment_dir

    for (dir_path, _, filenames) in os.walk(experiment_dir):  # iterate over every file in experiment_dir
        dir_path = pathlib.Path(dir_path)
        for log_file_name in filenames:
            log_file = dir_path / log_file_name
            if log_file.suffix != log_file_suffix:
                continue  # file is no log file => skip

            # create a temporary file to write the manipulated solution. This avoids conflicts, if overwrite == true
            tmp_file = tempfile.TemporaryFile("r+")
            with open(log_file, "r") as old_file:  # read not manipulated file line by line
                ilp_id = 0
                line = old_file.readline()
                while line != "":
                    # write everything except the new line character into the temporary file
                    # the .strip() call is necessary to avoid double spaces between two values
                    # (e.g 'elapsed_time=0  ilp_id=0'). This helps to avoid errors during log parsing.
                    tmp_file.write(line[0:-1].strip())
                    if line.startswith("GUROBI_PARTIAL_SOLUTION "):
                        # append current ilp id
                        tmp_file.write(f" ilp_id={ilp_id}")
                    elif line.startswith("ILP_SOLUTION "):
                        # also append current ilp id and increase ilp id counter
                        tmp_file.write(f" ilp_id={ilp_id}")
                        ilp_id += 1
                    tmp_file.write("\n")  # now add the removed new line character
                    line = old_file.readline()  # read next line
            # copy the temporary file into the result directory
            tmp_file.seek(0)
            new_file_path = result_dir / (dir_path.relative_to(experiment_dir)) / log_file_name
            # create missing directories
            new_file_path.parent.mkdir(parents=True, exist_ok=True)
            if verbose:
                print(f"Create/Override file at: {new_file_path}")
            with open(new_file_path, "w") as new_file:
                # write content of temporary file in result file
                line = tmp_file.readline()
                while line != "":
                    new_file.write(line)
                    line = tmp_file.readline()


if __name__ == "__main__":
    input_dir = "C:\\Users\\Cedrico\\KIT\\BA\\experiments\\local_test\\Gurobi_Partial_Solution_2022-03-05_0"
    output_dir = "C:\\Users\\Cedrico\\KIT\\BA\\experiments\\local_test\\Gurobi_Partial_Solution_manipulated_test"
    add_ilp_counter_to_gurobi_partial_solution(input_dir, override=False, result_dir=output_dir, verbose=True)
