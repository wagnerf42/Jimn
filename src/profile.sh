#!/bin/sh
# pypy3 -m cProfile -o profile.pyprof ./stl2gcode.py ../test_files/cordoba-large.stl --thickness 0.1 --radius 0.05
python3 -m cProfile -o profile.pyprof ./stl2gcode.py ../test_files/cordoba-large.stl --thickness 0.1 --radius 0.05
python3 /usr/bin/pyprof2calltree -i profile.pyprof -k
