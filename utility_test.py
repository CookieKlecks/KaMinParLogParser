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


class TestDictSet(unittest.TestCase):
    original_dict = {}
    copied_dict = {}

    def setUp(self):
        super().setUp()
        self.original_dict = test_dict = {
            "a": {
                "number": 1,
                "b": {
                    "c": "nested_value"
                }
            },
            "x": [],
            "simple_value": 42
        }
        self.copied_dict = self.original_dict

    def test_simple_key(self):
        utility.dict_set(self.copied_dict, "simple_value", 1337)

        self.assertEqual(self.copied_dict["simple_value"], 1337)
        self.assertEqual(self.copied_dict["a"], self.original_dict["a"])
        self.assertEqual(self.copied_dict["x"], self.original_dict["x"])

    def test_nested_string_key(self):
        utility.dict_set(self.original_dict, "a.b.c", "new_value")

        self.assertEqual(self.original_dict["a"]["b"]["c"], "new_value")
        self.assertEqual(self.original_dict["a"]["number"], self.original_dict["a"]["number"])
        self.assertEqual(self.original_dict["x"], self.original_dict["x"])
        self.assertEqual(self.original_dict["simple_value"], self.original_dict["simple_value"])

    def test_nested_string_key_non_default_separator(self):
        utility.dict_set(self.original_dict, "a/b/c", "new/value", path_separator="/")

        self.assertEqual(self.original_dict["a"]["b"]["c"], "new/value")
        self.assertEqual(self.original_dict["a"]["number"], self.original_dict["a"]["number"])
        self.assertEqual(self.original_dict["x"], self.original_dict["x"])
        self.assertEqual(self.original_dict["simple_value"], self.original_dict["simple_value"])

    def test_array_key(self):
        utility.dict_set(self.original_dict, ["a", "b", "c"], "[new][value]")

        self.assertEqual(self.original_dict["a"]["b"]["c"], "[new][value]")
        self.assertEqual(self.original_dict["a"]["number"], self.original_dict["a"]["number"])
        self.assertEqual(self.original_dict["x"], self.original_dict["x"])
        self.assertEqual(self.original_dict["simple_value"], self.original_dict["simple_value"])


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
