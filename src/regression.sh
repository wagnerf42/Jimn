#!/usr/bin/env sh

export JIMN_PATH_ANIMATION=1
./stl2gcode.py --display --thickness 0.4 --radius 0.1 ../test_files/cordoba.stl
./stl2gcode.py --display --thickness 0.3 --radius 0.05 ../test_files/cordoba.stl
./stl2gcode.py --display --thickness 0.05 --radius 0.05 ../test_files/cordoba.stl
./stl2gcode.py --display --thickness 0.1 --radius 0.1 ../test_files/cordoba-large.stl
