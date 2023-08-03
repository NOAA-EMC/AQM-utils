#!/usr/bin/env python3

# Python script to add air quality tracer variables from previous 
# cycle's restart output to atmosphere's initial condition file 

import os
import sys
import argparse
import xarray as xr
import numpy as np

def add_aqm_tracers_ics(fv_tracer_file, wrk_ic_file):

    # Previous day aqm tracer file
    try: fv_tracer = xr.open_dataset(fv_tracer_file)
    except: raise Exception('Could NOT find the tracer file:',fv_tracer_file)

    print('========== AQM tracer file:', fv_tracer)

    # Previous day atmosphere file
    try: gfs_atm = xr.open_dataset(wrk_ic_file)
    except: raise Exception('Could NOT find the IC file:',wrk_ic_file)

    print('========== Original IC file W/O aqm tracers:', gfs_atm)

    # Extract attributes of atmosphere file to ensure it in the final file
    gfs_atm_attr_source = gfs_atm.attrs['source']

    # Remove time dimension and microphysics tracers from previous cycle's restart file
    ds_out = fv_tracer.drop({'Time','graupel','ice_wat','liq_wat',
                        'o3mr','rainwat','snowwat','sphum'})

    ds_out = ds_out.squeeze('Time')

    # Rename dimensions and coordinates from restart file to match names in atmosphere's IC file
    ds_out = ds_out.rename({'xaxis_1':'lon','yaxis_1':'lat','zaxis_1':'lev'})

    # GFS ICs are defined on one additional (top) layer than tracers in
    # the restart file. We extract the top layer from the tracer file 
    # and extend the number of vertical levels by 1 (i.e. from 64 to 65) 
    ds_out2 = ds_out.isel(lev=slice(1))

    ds_new = xr.concat([ds_out2,ds_out],dim='lev')

    # Rebuild the lev values from 1 to 65
    ds_new =ds_new.assign_coords(lev=np.arange(1,66,1.0))

    # Add the vertically-extended tracers to the GFS IC file
    ds_new2 = xr.merge([gfs_atm,ds_new],join='left')

    ds_new2 = ds_new2.drop({'lev','lat','lon'})

    # Add source attribute to the final file
    ds_news2 = ds_new2.attrs['source'] = gfs_atm_attr_source

    print('========== New IC file with AQM tracers (tmp1.nc):', ds_new2)

    # Output to a temporary file for the later steps
    try: ds_new2.to_netcdf('tmp1.nc')
    except: raise Exception('Error in creating output file tmp1.nc')

    print('===========================================================')
    print('The intermediate file tmp1.nc has been created successfully')
    print('===========================================================')

    return True


def parse_args(argv):
    """Parse command line arguments"""
    parser = argparse.ArgumentParser(
        description="Add air quality tracer variables to ICs."
    )

    parser.add_argument("-t", 
        "--fv_tracer_file", 
        dest="fv_tracer_file", 
        required=True, 
        help="Tracer file.")

    parser.add_argument("-c",
        "--wrk_ic_file",
        dest="wrk_ic_file",
        required=True,
        help="FV3 IC file.")

    return parser.parse_args(argv)


if __name__ == "__main__":
    args = parse_args(sys.argv[1:])
    add_aqm_tracers_ics(
        fv_tracer_file=args.fv_tracer_file,
        wrk_ic_file=args.wrk_ic_file
    )

