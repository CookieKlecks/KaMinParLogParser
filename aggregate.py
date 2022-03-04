import collections.abc
import utility


def aggregate_parsed_logs(current_result, aggregate_keys_dict):
    """
    This function aggregates a list of values at a specific path in a dictionary into a single value by a specific
    aggregation method.

    Following aggregation methods exist:

    - 'success_percentage': number of truthy (as evaluated by 'bool' function) values divided by total number of values.

    :param current_result: dictionary containing the current values.
    :param aggregate_keys_dict: A dictionary mapping key paths to aggregation methods. Each key in this dict represents
                the path to a value in current_result that should be aggregated. The corresponding value is a string
                stating the aggregation method.
    :return: a copy of current_value where the stated values are aggregated.
    """

    if not isinstance(aggregate_keys_dict, collections.abc.MutableMapping):
        return current_result

    aggregated_result = current_result
    for key_path, method in aggregate_keys_dict.items():
        value = utility.dict_get(current_result, key_path)
        if value is None:
            continue

        aggregated_value = value
        if method == "success_percentage":
            successful_count = len(list(filter(bool, value)))
            aggregated_value = successful_count / len(value)

        utility.dict_set(aggregated_result, key_path, aggregated_value)

    return aggregated_result
