"""
This file contains some utility functions used by this project.
"""
import collections.abc
import math


def merge_dicts_by_keys(original_dict, other_dict):
    """
    Recursively merges two dictionaries as follows (conflicts prefer other_dict):
        - all disjunctive keys from original_dict and other_dict are taken unmodified.
        - if original_dict and other_dict have the same key 'a', the values orig_a = original_dict[a] and
          other_a = other_dict[a] are merged as follows:
            - if orig_a and other_a are both dicts, they are merged via this method
            - if exactly one of them (let it be orig_a) is not a dict, following dict is constructed:
                {'_': orig_a, **other_a}
            - if both are no dict, the values are contained in a list [orig_a, other_a]. If orig_a is already a list
                other_a is appended, otherwise a new list is created.

    :param dict[str, Any] original_dict: the original dictionary which builds the source of the merge. Values in
                                         original_dict can be overridden by other_dict if both have non-dict values for
                                         the identical key.
    :param dict[str, Any] other_dict: the dictionary, which is merged into a copy of original_dict. It is guaranteed,
                                      that every non-dict value will be visible in the resulting dict via the same keys.
    :return: a dictionary containing the recursive merge of original_dict and other_dict.
    :rtype: dict[str, Any]
    """
    if not isinstance(original_dict, collections.abc.MutableMapping) and not isinstance(other_dict,
                                                                                        collections.abc.MutableMapping):
        # two values for the exact same key => other_dict overrides key in original_dict
        if isinstance(original_dict, list):
            return original_dict + [other_dict]
        return [original_dict, other_dict]
    if not isinstance(original_dict, collections.abc.MutableMapping):
        # original_dict has direct value and other_dict has further nested keys => original_dict becomes "source" value
        # by assigning it to the key '_' for the resulting merged dict.
        return {'_': original_dict, **other_dict}
    if not isinstance(other_dict, collections.abc.MutableMapping):
        return {'_': other_dict, **original_dict}
    merge = original_dict.copy()
    for key, value in other_dict.items():
        if key in merge:
            merge[key] = merge_dicts_by_keys(original_dict[key], value)
        else:
            merge[key] = value
    return merge


def dict_path_to_array(path, path_separator="."):
    """
    Transforms a with a specified separator separated key path into an array containing the individual keys of every
    nested level.
    If the path is already a list it is directly returned.

    :param str or list path: the path that should be parsed
    :param str path_separator: separator that separates keys of different levels.
    :return: a list containing the keys for each nested level.
    """
    if isinstance(path, str):
        return path.split(path_separator)
    if not isinstance(path, collections.abc.Sequence):
        raise TypeError(
            f"only paths with type 'list' or 'str' are allowed. You provided '{path}' with type '{type(path)}'")
    return path


def dict_get(dictionary, path, default=None, path_separator="."):
    """
    Gets the (possibly nested) value at the provided path in the provided dictionary.
    A path can be:
        - string (not containing path_separator) => path is interpreted as simple key of the dictionary.
        - string (containing path_separator) => path is split at path_separator and the method is called executed as if
                                                it was called with the split array (see path with list type).
        - list of string => path is interpreted as list of (nested) keys. The value is: dictionary[path[0]][path[1]]...

    If dictionary does not contain a value at the given path, default is returned (no error is raised).

    :param dict dictionary:
    :param path: Path specifying where the value is found in dict. It can contain path_separator or can be an array to
                 specify a value of a nested object. Examples: "key" => d["key"], "key.sub" => d["key"]["sub"]
    :type path: str or list[str]
    :param Any default: the default value if no value at this path exists.
    :param str path_separator: separator to separate nested keys in a path.
    :return: The value of the given dictionary at the given path, or default, if this value does not exist.
    """
    cur_path = dict_path_to_array(path, path_separator)

    key = cur_path[0]
    rest_path = cur_path[1:]

    if key not in dictionary:
        return default

    value = dictionary[key]
    if len(rest_path) > 0:
        # further searching
        return dict_get(value, rest_path, default=default, path_separator=path_separator)
    return value


def dict_set(dictionary, path, new_value, path_separator="."):
    """
    Sets the value of a directory at a given path. See dict_get to see what paths are allowed.

    :param dict dictionary: the dictionary whose value should be updated.
    :param path: Path specifying which value should be updated.
    :param new_value: the new value for the given path.
    :param path_separator: separator to separate nested keys in a path.
    """
    cur_path = dict_path_to_array(path, path_separator)

    key = cur_path[0]
    rest_path = cur_path[1:]

    if len(rest_path) == 0:
        dictionary[key] = new_value
    else:
        dict_set(dictionary[key], rest_path, new_value, path_separator)


def try_parse_int_or_float(str_repr):
    """
    This method safely tries to parse a given string into int or float with following ordering int > float > str.

    Therefore first int parsing is tested. If successful, int value is returned.
    Otherwise float parsing is tested. If successful, float is returned.
    If no parsing is successful, str_repr is as the original value returned.

    :param str str_repr: the string that should be tried to parse.
    :return: the str_repr in the strongest parsable type (int > float > str).
    :rtype: int or float or str
    """

    try:
        return int(str_repr)
    except (ValueError, TypeError):
        pass  # value is no integer => do nothing and check if it is a float
    try:
        return float(str_repr)
    except (ValueError, TypeError):
        pass  # value is neither int nor float => do nothing
    return str_repr  # if no parse was successful, return value as is


def print_banner(text, lines=3, width=60, background="="):
    num_eqs = ((width - len(text) - 2) / 2)

    # print upper banner
    for i in range(math.ceil((lines - 1) / 2)):
        print(background * width)

    # Print content
    print(background * math.floor(num_eqs), text, background * math.ceil(num_eqs))

    # print lower banner
    for i in range(math.floor((lines - 1) / 2)):
        print(background * width)
