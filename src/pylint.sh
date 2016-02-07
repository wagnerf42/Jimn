#!/bin/bash
pylint3 `find jimn -name "*.py" | grep -v config.py`
