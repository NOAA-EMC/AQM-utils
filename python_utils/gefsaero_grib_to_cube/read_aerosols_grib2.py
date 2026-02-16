#!/usr/bin/env python3
# read_aerosols_grib2.py
# this script was created by AI

import logging
from typing import Dict, Optional, Tuple

import grib2io
import numpy as np

# Set up logging
logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s")
logger = logging.getLogger(__name__)


def read_aerosol_species_from_grib2(
    grib_file_path: str, level_range: Optional[Tuple[int, int]] = None
) -> Dict[str, np.ndarray]:
    """
    Read all aerosol species from a GRIB2 file using grib2io.

    This function reads particulate matter fine (PMTF) and coarse (PMTC) aerosol species
    from GRIB2 files, typically from chemical weather models like GEFS-Aerosols.

    Args:
        grib_file_path (str): Path to the GRIB2 file
        level_range (tuple, optional): Tuple of (start_level, end_level) to read specific
                                     hybrid levels. If None, reads all levels.

    Returns:
        Dict[str, np.ndarray]: Dictionary with aerosol species names as keys and
                              4D arrays (time, level, lat, lon) as values

    Raises:
        FileNotFoundError: If the GRIB2 file doesn't exist
        Exception: For other GRIB2 reading errors
    """

    try:
        logger.info(f"Opening GRIB2 file: {grib_file_path}")

        # Open the GRIB2 file
        grb = grib2io.open(grib_file_path)

        # Dictionary to store aerosol data
        aerosol_data = {}

        # Define aerosol species patterns based on the actual file structure
        # From the parameter analysis, we have these aerosol types:
        aerosol_species_info = {
            "dust_bin1": {
                "parameter": "du_pm2",
                "fullName": "Dust Dry Particulate matter (fine)",
            },
            "dust_bin2": {
                "parameter": "du_pm20",
                "fullName": "Dust Dry Particulate matter (fine)",
            },
            "dust_bin3": {
                "parameter": "du_pm36",
                "fullName": "Dust Dry Particulate matter (coarse)",
            },
            "dust_bin4": {
                "parameter": "du_pm60",
                "fullName": "Dust Dry Particulate matter (coarse)",
            },
            "dust_bin5": {
                "parameter": "du_pm120",
                "fullName": "Dust Dry Particulate matter (coarse)",
            },
            "seasalt_bin1": {
                "parameter": "ss_pm6",
                "fullName": "Sea Salt Dry Particulate matter (fine)",
            },
            "seasalt_bin2": {
                "parameter": "ss_pm2",
                "fullName": "Sea Salt Dry Particulate matter (fine)",
            },
            "seasalt_bin3": {
                "parameter": "ss_pm10",
                "fullName": "Sea Salt Dry Particulate matter (coarse)",
            },
            "seasalt_bin4": {
                "parameter": "ss_pm30",
                "fullName": "Sea Salt Dry Particulate matter (coarse)",
            },
            "seasalt_bin5": {
                "parameter": "ss_pm100",
                "fullName": "Sea Salt Dry Particulate matter (coarse)",
            },
            "sulfate": {
                "parameter": "so4_pm139",
                "fullName": "Sulphate Dry Particulate matter (fine)",
            },
            "organic_carbon_hydrophobic": {
                "parameter": "omho_pm424",
                "fullName": "Particulate organic matter hydrophobic dry Particulate matter (fine)",
            },
            "organic_carbon_hydrophilic": {
                "parameter": "omhi_pm424",
                "fullName": "Particulate organic matter hydrophilic dry Particulate matter (fine)",
            },
            "black_carbon_hydrophobic": {
                "parameter": "bcho_pm236",
                "fullName": "Black carbon hydrophobic dry Particulate matter (fine)",
            },
            "black_carbon_hydrophilic": {
                "parameter": "bchi_pm236",
                "fullName": "Black carbon hydrophilic dry Particulate matter (fine)",
            },
        }

        logger.info(f"Found {len(grb)} total messages in GRIB2 file")

        # Get grid information from the first message
        first_msg = grb[0]
        lats, lons = first_msg.grid()

        # Debug: print available attributes
        logger.info("Examining first message attributes...")
        msg_attrs = [attr for attr in dir(first_msg) if not attr.startswith("_")]
        logger.info(f"Available attributes: {msg_attrs[:20]}...")  # First 20 attributes

        # Print some key attributes that might help
        for attr in [
            "nx",
            "ny",
            "la1",
            "la2",
            "lo1",
            "lo2",
            "dx",
            "dy",
            "level",
            "shortName",
            "parameterNumber",
        ]:
            if hasattr(first_msg, attr):
                logger.info(f"{attr}: {getattr(first_msg, attr)}")
            else:
                logger.info(f"{attr}: NOT FOUND")

        # Try to get grid information with fallback options
        try:
            grid_info = {
                "ni": first_msg.nx,
                "nj": first_msg.ny,
                "lat_min": first_msg.la1 / 1e6,  # latitude of first grid point
                "lat_max": first_msg.la2 / 1e6,  # latitude of last grid point
                "lon_min": first_msg.lo1 / 1e6,  # longitude of first grid point
                "lon_max": first_msg.lo2 / 1e6,  # longitude of last grid point
                "dlat": first_msg.dy / 1e6,  # j direction increment
                "dlon": first_msg.dx / 1e6,  # i direction increment
                "lats": lats,  # latitude values
                "lons": lons,  # longitude values
            }
        except AttributeError as e:
            logger.warning(f"Grid attribute error: {e}")
            # Fallback to basic grid info
            grid_info = {
                "ni": getattr(first_msg, "nx", 720),
                "nj": getattr(first_msg, "ny", 361),
                "lat_min": -90.0,
                "lat_max": 90.0,
                "lon_min": 0.0,
                "lon_max": 360.0,
                "dlat": 0.5,
                "dlon": 0.5,
                "lats": lats,
                "lons": lons,
            }
            logger.warning("Using fallback grid information")

        logger.info(f"Grid info: {grid_info['ni']}x{grid_info['nj']} points")
        logger.info(f"Lat range: {grid_info['lat_min']} to {grid_info['lat_max']}")
        logger.info(f"Lon range: {grid_info['lon_min']} to {grid_info['lon_max']}")

        # Determine levels to read
        available_levels = set()
        for msg in grb:
            # Try different possible level attribute names
            level = None
            if hasattr(msg, "level"):
                level = msg.level
            elif hasattr(msg, "typeOfFirstFixedSurface"):
                level = getattr(msg, "scaledValueOfFirstFixedSurface", 1)
            elif hasattr(msg, "lev"):
                level = msg.lev

            if level is not None:
                # Ensure level is an integer
                try:
                    # Handle cases where level might be a string like "1 hybrid level"
                    if isinstance(level, str):
                        # Extract numeric part from strings like "1 hybrid level"
                        level_num = level.split()[0]
                        level = int(level_num)
                    else:
                        level = int(level)
                    available_levels.add(level)
                except (ValueError, TypeError, IndexError):
                    logger.warning(f"Could not extract level number from: {level}")
                    continue

        available_levels = sorted(list(available_levels))

        if len(available_levels) == 0:
            logger.warning("No valid levels found, using default level 1")
            available_levels = [1]

        logger.info(
            f"Available levels: {len(available_levels)} levels from {min(available_levels)} to {max(available_levels)}"
        )

        if level_range:
            levels_to_read = [
                level for level in available_levels if level_range[0] <= level <= level_range[1]
            ]
            logger.info(
                f"Reading levels {level_range[0]} to {level_range[1]}: {len(levels_to_read)} levels"
            )
        else:
            levels_to_read = available_levels
            logger.info(f"Reading all {len(levels_to_read)} levels")

        # Initialize data arrays for each aerosol species
        for species_name in aerosol_species_info.keys():
            # Shape: (level, lat, lon) - assuming single time for now
            aerosol_data[species_name] = np.zeros(
                (len(levels_to_read), grid_info["nj"], grid_info["ni"])
            )

        # Read data for each aerosol species and level
        species_found = {species: False for species in aerosol_species_info.keys()}

        for msg in grb:
            # Get message properties - try different level attribute names
            level = None
            if hasattr(msg, "level"):
                level = msg.level
            elif hasattr(msg, "typeOfFirstFixedSurface"):
                level = getattr(msg, "scaledValueOfFirstFixedSurface", 1)
            elif hasattr(msg, "lev"):
                level = msg.lev
            else:
                level = 1  # Default to level 1 if no level info found

            # Ensure level is an integer
            try:
                # Handle cases where level might be a string like "1 hybrid level"
                if isinstance(level, str):
                    # Extract numeric part from strings like "1 hybrid level"
                    level_num = level.split()[0]
                    level = int(level_num)
                else:
                    level = int(level)
            except (ValueError, TypeError, IndexError):
                logger.warning(f"Could not extract level number from: {level}, skipping message")
                continue

            if level not in levels_to_read:
                continue

            level_idx = levels_to_read.index(level)

            # Get parameter name and aerosol information
            param_name = getattr(msg, "shortName", getattr(msg, "parameterNumber", "unknown"))

            # Debug: print message info for first few messages
            if len(grb) <= 20:  # Only for small files or first few messages
                logger.info(
                    f"Message at level {level}: param={param_name}, fullName={getattr(msg, 'fullName', 'N/A')}"
                )

            # Try to match this message to one of our aerosol species
            for species_name, species_info in aerosol_species_info.items():
                # Match based on the exact parameter name from the species info
                if param_name == species_info["parameter"]:
                    # Read the data values
                    try:
                        # Try different ways to access the data
                        if hasattr(msg, "data"):
                            if callable(msg.data):
                                data_values = msg.data()
                            else:
                                data_values = msg.data
                        else:
                            logger.warning("No data attribute found in message")
                            continue

                        if data_values is not None:
                            # Reshape to match grid dimensions
                            if len(data_values.shape) == 1:
                                data_2d = data_values.reshape(grid_info["nj"], grid_info["ni"])
                            else:
                                data_2d = data_values

                            # Store in the aerosol_data array
                            aerosol_data[species_name][level_idx, :, :] = data_2d
                            if not species_found[species_name]:
                                species_found[species_name] = True
                                min_val = np.min(data_2d)
                                max_val = np.max(data_2d)
                                logger.info(
                                    f"Read {species_name} ({param_name}) for level {level}: min={min_val:.2e}, max={max_val:.2e}"
                                )
                                break
                    except Exception as e:
                        logger.warning(
                            f"Error reading data for {species_name} at level {level}: {e}"
                        )
                        continue

        # Log which species were found
        found_species = [s for s, found in species_found.items() if found]
        missing_species = [s for s, found in species_found.items() if not found]

        logger.info(f"Successfully read {len(found_species)} aerosol species: {found_species}")
        if missing_species:
            logger.warning(
                f"Could not find data for {len(missing_species)} species: {missing_species}"
            )

        # Add grid information to the returned data
        aerosol_data["_grid_info"] = grid_info
        aerosol_data["_levels"] = levels_to_read

        grb.close()

        return aerosol_data

    except FileNotFoundError:
        logger.error(f"GRIB2 file not found: {grib_file_path}")
        raise
    except Exception as e:
        logger.error(f"Error reading GRIB2 file {grib_file_path}: {e}")
        raise


def get_aerosol_species_by_parameter(
    grib_file_path: str, parameter_name: str = "PMTF", level: int = 1
) -> Dict[str, np.ndarray]:
    """
    Read specific aerosol parameter (PMTF or PMTC) from GRIB2 file.

    Args:
        grib_file_path (str): Path to the GRIB2 file
        parameter_name (str): Parameter to read ('PMTF' for fine, 'PMTC' for coarse)
        level (int): Hybrid level to read

    Returns:
        Dict[str, np.ndarray]: Dictionary with aerosol data and metadata
    """

    try:
        logger.info(f"Reading {parameter_name} data from level {level}")

        grb = grib2io.open(grib_file_path)

        # Find messages matching the criteria
        matching_data = {}

        for i, msg in enumerate(grb):
            if (
                hasattr(msg, "shortName")
                and msg.shortName == parameter_name
                and hasattr(msg, "level")
                and msg.level == level
            ):
                # Read the data
                data_values = msg.data()
                if data_values is not None:
                    # Create a unique key for this aerosol type
                    # You might want to extract more specific aerosol type info here
                    key = f"{parameter_name}_message_{i}_level_{level}"
                    matching_data[key] = {
                        "data": data_values.reshape(msg.ny, msg.nx),
                        "units": getattr(msg, "units", "unknown"),
                        "level": level,
                        "parameter": parameter_name,
                        "grid_shape": (msg.ny, msg.nx),
                    }

                    logger.info(
                        f"Read {key}: shape {data_values.shape}, min={np.min(data_values):.2e}, max={np.max(data_values):.2e}"
                    )

        grb.close()

        return matching_data

    except Exception as e:
        logger.error(f"Error reading {parameter_name} data: {e}")
        raise
