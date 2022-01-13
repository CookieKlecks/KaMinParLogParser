import unittest

import utility


class TestDictGet(unittest.TestCase):
    test_dict = {
        "a": {
            "number": 1,
            "b": {
                "c": "nested_value"
            }
        },
        "x": [],
        "simple_value": 42
    }

    def test_simple_key(self):
        value = utility.dict_get(
            self.test_dict,
            "simple_value"
        )
        self.assertEqual(value, self.test_dict["simple_value"])

    def test_nested_string_key(self):
        value = utility.dict_get(
            self.test_dict,
            "a.b.c"
        )
        self.assertEqual(value, self.test_dict["a"]["b"]["c"])

    def test_nested_string_key_non_default_separator(self):
        value = utility.dict_get(
            self.test_dict,
            "a/b/c",
            path_separator="/"
        )
        self.assertEqual(value, self.test_dict["a"]["b"]["c"])

    def test_array_key(self):
        value = utility.dict_get(
            self.test_dict,
            ["a", "b", "c"],
        )
        self.assertEqual(value, self.test_dict["a"]["b"]["c"])


class TestTryParseIntFloat(unittest.TestCase):
    def test_str_parse(self):
        original = "test"
        result = utility.try_parse_int_or_float(original)
        self.assertEqual(result, original)

    def test_int_parse(self):
        str_number = "1"
        number = utility.try_parse_int_or_float(str_number)
        self.assertEqual(number, 1)

    def test_negative_int_parse(self):
        str_number = "-1"
        number = utility.try_parse_int_or_float(str_number)
        self.assertEqual(number, -1)

    def test_float_parse(self):
        str_number = "2.5"
        number = utility.try_parse_int_or_float(str_number)
        self.assertEqual(number, 2.5)

    def test_negative_float_parse(self):
        str_number = "-200.5"
        number = utility.try_parse_int_or_float(str_number)
        self.assertEqual(number, -200.5)


if __name__ == '__main__':
    unittest.main()
