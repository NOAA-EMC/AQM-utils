#!/usr/bin/env python3
"""
Handle fire emission (RAVE) to 13-km NA domain using xarray.
"""

from __future__ import annotations

import argparse
import logging
import sys
from datetime import datetime
from typing import List, Optional

import xarray as xr

# Setup logging
logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s")
logger = logging.getLogger(__name__)


def parse_args(argv: Optional[List[str]] = None) -> argparse.Namespace:
    """
    Parse command line arguments for RAVE remake (13km).

    Parameters
    ----------
    argv : List[str], optional
        Command line arguments.

    Returns
    -------
    argparse.Namespace
        Parsed arguments.
    """
    parser = argparse.ArgumentParser(description="Handle fire emission data using xarray.")

    parser.add_argument("-d", "--date", required=True, help="Date for processing (YYYYMMDD).")
    parser.add_argument("-c", "--cyc", required=True, help="Cycle hour (HH).")
    parser.add_argument("-i", "--input_fire", required=True, help="Path to input RAVE fire data.")
    parser.add_argument("-o", "--output_fire", required=True, help="Path to output data file.")

    return parser.parse_args(argv)


def RAVE_remake_allspecies(
    date: str,
    cyc: str,
    input_fire: str,
    output_fire: str,
) -> None:
    """
    Process RAVE fire emissions for 13-km domain.

    Parameters
    ----------
    date : str
        Date in YYYYMMDD format.
    cyc : str
        Cycle hour in HH format.
    input_fire : str
        Path to input RAVE fire data.
    output_fire : str
        Path to save processed data.
    """
    logger.info(f"Processing RAVE remake (13km) for {date} cycle {cyc}")

    year, mm, dd = date[0:4], date[4:6], date[6:8]

    # Load input dataset
    ds_togid = xr.open_dataset(input_fire)

    area = ds_togid["area"]
    tgt_latt = ds_togid["grid_latt"]
    tgt_lont = ds_togid["grid_lont"]
    land_cover = ds_togid["land_cover"]

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
        logger.info(f"Processing {svar}")

        da_in = ds_togid[svar].fillna(0)

        if svar == "FRP_MEAN":
            da_out = da_in
            var_name = "MeanFRP"
            long_name = "Mean Fire Radiative Power"
            units = "MW"
        elif svar == "PM25_scaled":
            da_out = da_in / area / 3600
            var_name = "PM2.5"
            long_name = "PM2.5 Biomass Emissions"
            units = "kg m-2 s-1"
        elif svar == "BC_scaled":
            da_out = da_in / area / 3600
            var_name = "BC"
            long_name = "BC Biomass Emissions"
            units = "kg m-2 s-1"
        elif svar == "OC_scaled":
            da_out = da_in / area / 3600
            var_name = "OC"
            long_name = "OC Biomass Emissions"
            units = "kg m-2 s-1"
        else:
            da_out = da_in / area / 3600
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
    ds_final["Latitude"] = tgt_latt
    ds_final["Latitude"].attrs.update(
        {
            "units": "degrees_north",
            "long_name": "cell center latitude",
            "standard_name": "Latitude",
        }
    )

    ds_final["Longitude"] = tgt_lont
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
            "WestBoundingCoordinate(degree)": "151.981f",
            "EastBoundingCoordinate(degree)": "332.019f",
            "NorthBoundingCoordinate(degree)": "81.7184f",
            "SouthBoundingCoordinate(degree)": "7.22291f",
            "history": f"Created on {datetime.now().isoformat()} using xarray",
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
        input_fire=args.input_fire,
        output_fire=args.output_fire,
    )
