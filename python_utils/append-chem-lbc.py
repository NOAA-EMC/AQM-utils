#!/usr/bin/env python3
"""
Add variables from chem LBC file into GFS LBC files in place.
Like `ncks -A a.nc b.nc`, where `a.nc` is the chem LBC file
and `b.nc` is one of the GFS LBC files.
"""
import argparse
import itertools
from glob import glob
from pathlib import Path

import netCDF4 as nc4

parser = argparse.ArgumentParser(description="Add chem to LBCs.")

parser.add_argument(
    "--chem",
    help="Chem LBC file.",
    required=True,
)

parser.add_argument(
    "--met",
    nargs="+",
    help=(
        "Meteo (GFS) LBC files, to which the chem variables will be added."
        "File name may have the pattern `gfs_bndy.tile?.???.nc`."
    ),
    required=True,
)

args = parser.parse_args()
print(args)

rm = True  # Create new files with chem vars removed instead

# TODO: probably easier to build the array of input fps and pass it to this script
files = []
files = sorted(glob("./tmp_AQM_LBCS/gfs_bndy.tile7.???.nc"))
chem_fp = "./tmp_AQM_LBCS/gfs_bndy_chem_08.tile7.000.nc"

# Open files
chem = nc4.Dataset(chem_fp, "r")
mets = {
    p: nc4.Dataset(p, "a")
    for p in files
}
rms = {}

met0 = mets[files[0]]
allowed_dims = set(met0.dimensions)

for name, variable in chem.variables.items():
    print(name)
    assert set(variable.dimensions) < allowed_dims
    for met_fp, met in mets.items():
        if name not in met.variables:
            # add
            met.createVariable(name, variable.dtype, variable.dimensions)
            met[name].setncatts({key: getattr(variable, key) for key in variable.ncattrs()})
            met[name][:] = chem[name][:]
            print(f"-> {met_fp}")

if rm:
    met_names = [vn for vn in met0.variables if vn not in chem.variables]
    dims_needed = set(itertools.chain.from_iterable(met0[vn].dimensions for vn in met_names))
    for met_fp, met in mets.items():
        p = Path(met_fp)
        p_new = p.with_stem(f"{p.stem}_clean")
        ds = nc4.Dataset(p_new, "w", format="NETCDF4")
        print(p_new.as_posix())

        # dims
        for name, dimension in met.dimensions.items():
            ds.createDimension(name, len(dimension) if not dimension.isunlimited() else None)

        # variables
        for name in met_names:
            v = met[name]
            v_new = ds.createVariable(name, v.dtype, v.dimensions)
            v_new.setncatts({key: getattr(variable, key) for key in v.ncattrs()})
            v_new[:] = v[:]
            print(f"- {name}")
            
        ds.close()


# Close
chem.close()
for met in mets.values():
    met.close()

raise SystemExit(0)

