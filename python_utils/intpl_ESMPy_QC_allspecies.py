#!/usr/bin/env python3
"""
Regrid fire emission data using xarray and xregrid.
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
    Parse command line arguments for fire emission regridding.

    Parameters
    ----------
    argv : List[str], optional
        Command line arguments.

    Returns
    -------
    argparse.Namespace
        Parsed arguments.
    """
    parser = argparse.ArgumentParser(description="Regrid fire emission data using xregrid.")

    parser.add_argument("-d", "--date", help="Date for regridding (YYYYMMDD).", required=True)
    parser.add_argument("-c", "--cycle", help="Cycle hour (HH).", required=True)
    parser.add_argument("-s", "--source", help="Path to the source data file.", required=True)
    parser.add_argument("-o", "--output", help="Path to the output data file.", required=True)
    parser.add_argument("-w", "--weight", help="Path to the regridding weight file.", required=True)
    parser.add_argument("-sg", "--source_grid", help="Path to the source grid file.", required=True)
    parser.add_argument("-tg", "--target_grid", help="Path to the target grid file.", required=True)

    return parser.parse_args(argv)


def process_regrid(
    date: str,
    cycle: str,
    source_file: str,
    output_file: str,
    weight_file: str,
    source_grid_file: str,
    target_grid_file: str,
) -> None:
    """
    Regrid fire emission data using xarray and xregrid.

    Parameters
    ----------
    date : str
        Date in YYYYMMDD format.
    cycle : str
        Cycle hour in HH format.
    source_file : str
        Path to input RAVE fire data.
    output_file : str
        Path to save regridded data.
    weight_file : str
        Path to ESMF weights file.
    source_grid_file : str
        Path to source grid definition.
    target_grid_file : str
        Path to target grid definition.
    """
    logger.info(f"Processing regridding for {date} cycle {cycle}")

    year, mm, dd = date[0:4], date[4:6], date[6:8]

    # Load datasets
    ds_grid_in = xr.open_dataset(source_grid_file)
    ds_grid_out = xr.open_dataset(target_grid_file)
    ds_togid = xr.open_dataset(source_file)

    # Prepare coordinates for xregrid
    # xregrid looks for lat/lon coordinates. We use the grid center coordinates.
    ds_grid_in = ds_grid_in.assign_coords(lat=ds_grid_in["grid_latt"], lon=ds_grid_in["grid_lont"])
    ds_grid_out = ds_grid_out.assign_coords(lat=ds_grid_out["grid_latt"], lon=ds_grid_out["grid_lont"])

    # Create regridder
    # Note: weights can be passed to Regridder to reuse existing ESMF weights
    regridder = Regridder(ds_grid_in, ds_grid_out, weights=weight_file)

    vars_emis = ["PM2.5", "CO", "VOCs", "NOx", "BC", "OC", "SO2", "NH3", "FRP_MEAN"]

    output_vars = {}

    area = ds_togid["area"]
    qa = ds_togid["QA"]
    tgt_area = ds_grid_out["area"]

    for svar in vars_emis:
        logger.info(f"Regridding {svar}")

        # Prepare source data
        da_in = ds_togid[svar].fillna(0) / area
        da_in = xr.where(qa > 1, da_in, 0.0)
        da_in.attrs = ds_togid[svar].attrs

        # Apply regridding
        da_out = regridder(da_in)

        # Scale and handle specific variables
        if svar == "FRP_MEAN":
            da_out = da_out * (tgt_area * 1.0e-6)
            var_name = "MeanFRP"
            da_out.attrs.update(
                {
                    "long_name": "Mean Fire Radiative Power",
                    "units": "MW",
                    "standard_name": "MeanFRP",
                }
            )
        else:
            da_out = da_out * 1.0e-6 / 3600
            var_name = svar
            da_out.attrs.update(
                {
                    "long_name": f"{svar} Biomass Emissions",
                    "units": "kg m-2 s-1",
                    "standard_name": svar,
                }
            )

        da_out.attrs["coordinates"] = "Time Latitude Longitude"
        output_vars[var_name] = da_out

    # Create output dataset
    ds_final = xr.Dataset(output_vars)

    # Add Latitude and Longitude variables to match original format
    ds_final["Latitude"] = ds_grid_out["grid_latt"]
    ds_final["Latitude"].attrs.update(
        {
            "units": "degrees_north",
            "long_name": "cell center latitude",
            "standard_name": "Latitude",
            "coordinates": "Latitude Longitude",
        }
    )

    ds_final["Longitude"] = ds_grid_out["grid_lont"]
    ds_final["Longitude"].attrs.update(
        {
            "units": "degrees_east",
            "long_name": "cell center longitude",
            "standard_name": "Longitude",
            "coordinates": "Latitude Longitude",
        }
    )

    # Global attributes
    ds_final.attrs.update(
        {
            "PRODUCT_ALGORITHM_VERSION": "Beta",
            "TIME_RANGE": "72 hours",
            "RangeBeginningDate(YYYY-MM-DD)": f"{year}-{mm}-{dd}",
            "RangeBeginningTime(UTC-hour)": cycle,
            "WestBoundingCoordinate(degree)": "227.506f",
            "EastBoundingCoordinate(degree)": "297.434f",
            "NorthBoundingCoordinate(degree)": "52.058f",
            "SouthBoundingCoordinate(degree)": "22.136f",
            "history": f"Created on {datetime.now().isoformat()} using xregrid and xarray",
        }
    )

    # Write output
    logger.info(f"Writing output to {output_file}")
    ds_final.to_netcdf(output_file)


if __name__ == "__main__":
    args = parse_args(sys.argv[1:])
    process_regrid(
        date=args.date,
        cycle=args.cycle,
        source_file=args.source,
        output_file=args.output,
        weight_file=args.weight,
        source_grid_file=args.source_grid,
        target_grid_file=args.target_grid,
    )
