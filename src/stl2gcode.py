#!/usr/bin/env python3
"""
generate gcode out of stl file.
"""

import argparse
from jimn import compute_milling_path


def main():
    """
    main function.
    """
    parser = argparse.ArgumentParser(
        description="converts stl file to gcode",
        epilog="cut stl object into horizontal slices of given size\
        and compute path for cutter of given radius going through all\
        reachable areas."
    )

    parser.add_argument('--display', action='store_true',
                        help="display path animation in terminology")
    parser.add_argument('--thickness', metavar="THICKNESS", type=float,
                        default=0.3, help="thickness of stl slices")
    parser.add_argument('--radius', metavar="RADIUS", type=float,
                        default=0.1, help="radius of cutter")
    parser.add_argument('stl_file', metavar="STL_FILE", type=str,
                        help="filename of stl object")

    args = parser.parse_args()

    path = compute_milling_path(args.stl_file, args.thickness, args.radius)

    if args.display:
        path.animate(args.radius)

    print("done")


if __name__ == "__main__":
    main()
