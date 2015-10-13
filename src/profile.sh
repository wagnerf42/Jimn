#!/bin/sh
python3 -m cProfile -s cumtime ./stl2gcode.py ../test_files/cordoba.stl 0.5 0.03
