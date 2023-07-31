#!/usr/bin/env python3
"""
Combine LBC files from GFS into one
"""
import argparse

import netCDF4 as nc4

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

# TODO: probably easier to build the array of input fps and pass it to this script
files = []
ofp = "t.nc"

# time
# assuming each file has only one time
times = []
srcs = []
for f in files:
    print(f)
    ds = nc4.Dataset(f)
    times_num_f = ds["time"][:]
    assert times_num_f.size == 1
    times.append(times_num_f[0])

    srcs.append(ds)

src0 = srcs[0]
dst = nc4.Dataset(ofp, "w", format="NETCDF4")

# dims
for name, dimension in src0.dimensions.items():
    dst.createDimension(name, len(dimension) if not dimension.isunlimited() else None)
assert dst.dimensions["time"].isunlimited()

# coords
time_units = src0["time"].units
coord_names = ["time", "latitude", "longitude"]
for name in coord_names:
    dst.createVariable(name, src0[name].dtype, src0[name].dimensions)
    dst[name].setncatts({key: getattr(src0[name], key) for key in src0[name].ncattrs()})
    if name == "time":
        dst[name][:] = times
    else:
        dst[name][:] = src0[name][:]
assert dst.variables["time"].size == len(srcs)

# variables
for name, variable in src0.variables.items():
    if name in coord_names:
        continue
    dst.createVariable(name, variable.dtype, variable.dimensions)
    dst[name].setncatts({key: getattr(variable, key) for key in variable.ncattrs()})

# data
for i, src in enumerate(srcs):
    for name in src.variables:
        if name in coord_names:
            continue
        dst[name][i] = src[name][:]

