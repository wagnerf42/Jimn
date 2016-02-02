#!/bin/bash
pylint `find jimn -name "*.py" | grep -v config.py`
