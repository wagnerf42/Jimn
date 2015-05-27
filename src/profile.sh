#!/bin/sh
python -m cProfile -s cumtime ./stl2gcode.py ../test_files/cordoba-large.stl 0.1
