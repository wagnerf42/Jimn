#!/usr/bin/env sh

export JIMN_PATH_ANIMATION=1
./stl2gcode.py ../test_files/cordoba.stl 0.4 0.1 a
./stl2gcode.py ../test_files/cordoba.stl 0.3 0.05 a
./stl2gcode.py ../test_files/cordoba.stl 0.05 0.05 a
./stl2gcode.py ../test_files/cordoba-large.stl 0.1 0.1 a
