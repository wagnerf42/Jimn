# pypy3 -m cProfile -o profile.pyprof pocket.py
python3 -m cProfile -o profile.pyprof pocket.py
python3 /usr/bin/pyprof2calltree -i profile.pyprof -k
