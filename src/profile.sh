#!/bin/sh
python3 -m cProfile -o profile.pyprof ./stl2gcode.py ../test_files/cordoba.stl 0.1 0.01
pyprof2calltree -i profile.pyprof -k
