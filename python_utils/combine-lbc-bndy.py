#!/usr/bin/env python3
"""
Combine LBC files from GFS into one
"""
import argparse

import netCDF4 as nc

parser = argparse.ArgumentParser(description="Combine LBCs.")

parser.add_argument(
    "--hours",
    nargs="+",
    help=(
        "Forecast hour strings (array, don't surround it with outer quotes). "
        "Not including forecast hour 0. "
        "Example: `003 006`."
    ),
    required=True,
)

args = parser.parse_args()
print(args)

fh_strings = args.hours
fh_strings.insert(0, "0" * len(fh_strings[0]))
print(fh_strings)

