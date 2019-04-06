#!/usr/bin/env python3
import pytest
from printer import *


def test_print_number():
    pr = PrettyPrinter()
    assert Number(0).accept(pr) == "0;\n"
    assert Number(1000).accept(pr) == "1000;\n"



if __name__ == "__main__":
    pytest.main()
