#!/bin/sh
python3 -m cProfile -s cumtime ./stl2gcode.py ../test_files/cordoba-large.stl 0.1
