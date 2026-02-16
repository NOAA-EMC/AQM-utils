#!/usr/bin/env python3
"""
Handle fire emission (RAVE) to 9-km NA domain using xarray and xregrid.
"""

from __future__ import annotations

import argparse
import logging
import sys
from datetime import datetime
from typing import List, Optional

import xarray as xr
from xregrid import Regridder

# Setup logging
logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s")
logger = logging.getLogger(__name__)


def parse_args(argv: Optional[List[str]] = None) -> argparse.Namespace:
    """
    Parse command line arguments for RAVE remake.

    Parameters
    ----------
    argv : List[str], optional
        Command line arguments.

    Returns
    -------
    argparse.Namespace
        Parsed arguments.
    """
    parser = argparse.ArgumentParser(description="Handle fire emission data using xregrid.")

    parser.add_argument("-d", "--date", required=True, help="Date for regridding (YYYYMMDD).")
    parser.add_argument("-c", "--cyc", required=True, help="Cycle hour (HH).")
    parser.add_argument("-s", "--src_map", required=True, help="Source map file.")
    parser.add_argument("-t", "--tgt_map", required=True, help="Target map file.")
    parser.add_argument("-w", "--weight_file", required=True, help="Regridding weight file.")
    parser.add_argument("-i", "--input_fire", required=True, help="Path to input RAVE fire data.")
    parser.add_argument("-o", "--output_fire", required=True, help="Path to output data file.")

    return parser.parse_args(argv)


def RAVE_remake_allspecies(
    date: str,
    cyc: str,
    src_map: str,
    tgt_map: str,
    weight_file: str,
    input_fire: str,
    output_fire: str,
) -> None:
    """
    Process RAVE fire emissions and regrid to target domain.

    Parameters
    ----------
    date : str
        Date in YYYYMMDD format.
    cyc : str
        Cycle hour in HH format.
    src_map : str
        Path to source grid map.
    tgt_map : str
        Path to target grid map.
    weight_file : str
        Path to ESMF weights file.
    input_fire : str
        Path to input RAVE fire data.
    output_fire : str
        Path to save processed data.
    """
    logger.info(f"Processing RAVE remake for {date} cycle {cyc}")

    year, mm, dd = date[0:4], date[4:6], date[6:8]

    # Load datasets
    ds_in = xr.open_dataset(src_map)
    ds_out = xr.open_dataset(tgt_map)
    ds_togid = xr.open_dataset(input_fire)

    # Wrap longitudes if necessary
    src_lont = xr.where(ds_in["grid_lont"] > 0.0, ds_in["grid_lont"], ds_in["grid_lont"] + 360.0)
    ds_in = ds_in.assign_coords(lat=ds_in["grid_latt"], lon=src_lont)
    ds_out = ds_out.assign_coords(lat=ds_out["grid_latt"], lon=ds_out["grid_lont"])

    # Create regridder
    regridder = Regridder(ds_in, ds_out, weights=weight_file)

    area = ds_togid["area"]
    qa = ds_togid["QA"]
    tgt_area = ds_out["area"]
    land_cover = ds_out["land_cover"]

    vars_emis = [
        "PM25_scaled",
        "CO",
        "VOCs",
        "NOx",
        "BC_scaled",
        "OC_scaled",
        "SO2",
        "NH3",
        "FRP_MEAN",
    ]

    output_vars = {}

    for svar in vars_emis:
        logger.info(f"Regridding {svar}")

        # Prepare source data
        da_in = ds_togid[svar].fillna(0) / area
        da_in = xr.where(qa > 1, da_in, 0.0)

        # Apply regridding
        da_out = regridder(da_in)

        # Scale and handle specific variables
        if svar == "FRP_MEAN":
            da_out = da_out * (tgt_area * 1.0e-6)
            var_name = "MeanFRP"
            long_name = "Mean Fire Radiative Power"
            units = "MW"
        elif svar == "PM25_scaled":
            da_out = da_out * 1.0e-6 / 3600
            var_name = "PM2.5"
            long_name = "PM2.5 Biomass Emissions"
            units = "kg m-2 s-1"
        elif svar == "BC_scaled":
            da_out = da_out * 1.0e-6 / 3600
            var_name = "BC"
            long_name = "BC Biomass Emissions"
            units = "kg m-2 s-1"
        elif svar == "OC_scaled":
            da_out = da_out * 1.0e-6 / 3600
            var_name = "OC"
            long_name = "OC Biomass Emissions"
            units = "kg m-2 s-1"
        else:
            da_out = da_out * 1.0e-6 / 3600
            var_name = svar
            long_name = f"{svar} Biomass Emissions"
            units = "kg m-2 s-1"

        da_out.attrs.update(
            {
                "long_name": long_name,
                "units": units,
                "standard_name": var_name,
                "coordinates": "Time Latitude Longitude",
            }
        )
        output_vars[var_name] = da_out

    # Create output dataset
    ds_final = xr.Dataset(output_vars)

    # Add coordinate variables
    ds_final["Latitude"] = ds_out["grid_latt"]
    ds_final["Latitude"].attrs.update(
        {
            "units": "degrees_north",
            "long_name": "cell center latitude",
            "standard_name": "Latitude",
        }
    )

    ds_final["Longitude"] = ds_out["grid_lont"]
    ds_final["Longitude"].attrs.update(
        {
            "units": "degrees_east",
            "long_name": "cell center longitude",
            "standard_name": "Longitude",
        }
    )

    ds_final["land_cover"] = land_cover
    ds_final["land_cover"].attrs.update(
        {
            "units": "unitless",
            "long_name": "land cover type",
            "standard_name": "land_cover",
        }
    )

    # Global attributes
    ds_final.attrs.update(
        {
            "PRODUCT_ALGORITHM_VERSION": "Beta",
            "TIME_RANGE": "72 hours",
            "RangeBeginningDate(YYYY-MM-DD)": f"{year}-{mm}-{dd}",
            "RangeBeginningTime(UTC-hour)": cyc,
            "WestBoundingCoordinate(degree)": "152.859f",
            "EastBoundingCoordinate(degree)": "331.141f",
            "NorthBoundingCoordinate(degree)": "81.0247f",
            "SouthBoundingCoordinate(degree)": "7.81713f",
            "history": f"Created on {datetime.now().isoformat()} using xregrid and xarray",
        }
    )

    # Write output
    logger.info(f"Writing output to {output_fire}")
    ds_final.to_netcdf(output_fire)


if __name__ == "__main__":
    args = parse_args(sys.argv[1:])
    RAVE_remake_allspecies(
        date=args.date,
        cyc=args.cyc,
        src_map=args.src_map,
        tgt_map=args.tgt_map,
        weight_file=args.weight_file,
        input_fire=args.input_fire,
        output_fire=args.output_fire,
    )
