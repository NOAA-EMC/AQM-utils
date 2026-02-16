#!/usr/bin/env python3
"""
Script to generate GCAFS initial conditions from GRIB2 files and FV3 cold start files.

This script reads GRIB2 chemical species data and FV3 cubed-sphere cold start files
to generate initial conditions for the GCAFS (Global Chemistry Aerosol Forecasting System).
"""

import argparse
import os
import sys

import numpy as np
from fv3_cold_starts import (
    add_3d_fields_to_fv3_tile,
    create_aerosol_field_metadata,
    get_pressure_levels_from_vcoord,
    read_cubed_sphere_coordinates,
    read_vcoord_from_ctrl_file,
)
from interp_fields import interpolate_aerosols_to_cubed_sphere
from read_aerosols_grib2 import read_aerosol_species_from_grib2


def generate_gcafs_ics(grib_file, fv3_prefix, output_dir=None):
    """
    Generate GCAFS initial conditions from GRIB2 file and FV3 cold start files.

    Parameters:
    -----------
    grib_file : str
        Path to input GRIB2 file containing chemical species data
    fv3_prefix : str
        Path prefix for FV3 cold start files (should allow reading all 6 tiles)
        Example: "/path/to/fv3_data/gfs_data.tile" (will read gfs_data.tile1.nc through gfs_data.tile6.nc)
    output_dir : str, optional
        Directory to write output files. If None, uses current directory.

    Returns:
    --------
    bool
        True if successful, False otherwise
    """
    print(f"Starting GCAFS initial conditions generation for GRIB file: {grib_file} and FV3 prefix: {fv3_prefix}")
    # ...existing code...

    # Validate input files exist
    if not os.path.exists(grib_file):
        print(f"ERROR: GRIB file not found: {grib_file}")
        return False

    # Check for all 6 FV3 tile files
    fv3_files = []
    for tile in range(1, 7):
        tile_file = f"{fv3_prefix}tile{tile}.nc"
        if not os.path.exists(tile_file):
            print(f"ERROR: FV3 tile file not found: {tile_file}")
            return False
        fv3_files.append(tile_file)

    print(f"Found all 6 FV3 tile files: {fv3_files}")

    # Check for the FV3 ctrl file
    ctrl_file = f"{fv3_prefix.replace('data.', '')}ctrl.nc"
    if not os.path.exists(ctrl_file):
        print(f"ERROR: FV3 ctrl file not found: {ctrl_file}")
        return False
    print(f"Found FV3 ctrl file: {ctrl_file}")

    # Set output directory
    if output_dir is None:
        output_dir = os.getcwd()

    print(f"Extracting aerosol species data from GRIB2 file: {grib_file}")
    aerosol_data = read_aerosol_species_from_grib2(grib_file)
    if aerosol_data is None:
        print("ERROR: Failed to read aerosol species data from GRIB2 file.")
        return False
    print(f"Successfully extracted aerosol species data: {', '.join([k for k in aerosol_data.keys() if not k.startswith('_')])}")

    grid_info_grib = aerosol_data["_grid_info"]

    # Extract source grid coordinates from GRIB data
    source_lon = grid_info_grib["lons"]  # Assuming these are in the grid_info
    source_lat = grid_info_grib["lats"]
    # Flatten source_lon and source_lat to 1D arrays if they are 2D
    if source_lon.ndim == 2:
        source_lon = source_lon[0, :]
    if source_lat.ndim == 2:
        source_lat = source_lat[:, 0]

    # Define GRIB pressure levels (assuming L64 vertical structure)
    akbk_grib = np.array(
        [
            [0.000, 1.00000000],
            [0.000, 0.99467119],
            [0.575, 0.98862660],
            [5.741, 0.98174229],
            [21.516, 0.97386760],
            [55.712, 0.96482757],
            [116.899, 0.95443411],
            [214.015, 0.94249106],
            [356.223, 0.92879731],
            [552.720, 0.91315100],
            [812.489, 0.89535499],
            [1143.988, 0.87522360],
            [1554.789, 0.85259067],
            [2051.150, 0.82731884],
            [2637.553, 0.79930974],
            [3316.217, 0.76851468],
            [4086.614, 0.73494523],
            [4945.029, 0.69868292],
            [5884.206, 0.65988704],
            [6893.117, 0.61879962],
            [7956.908, 0.57574665],
            [9057.051, 0.53113482],
            [10171.712, 0.48544331],
            [11276.348, 0.43921080],
            [12344.490, 0.39301826],
            [13348.671, 0.34746849],
            [14261.435, 0.30316412],
            [15056.342, 0.26068545],
            [15708.893, 0.22057019],
            [16197.315, 0.18329624],
            [16503.144, 0.14926877],
            [16611.603, 0.11881219],
            [16511.736, 0.09216691],
            [16197.967, 0.06947458],
            [15683.489, 0.05064684],
            [14993.074, 0.03544162],
            [14154.316, 0.02355588],
            [13197.065, 0.01463712],
            [12152.937, 0.00829402],
            [11054.853, 0.00410671],
            [9936.614, 0.00163591],
            [8832.537, 0.00043106],
            [7777.150, 0.00003697],
            [6804.874, 0.00000000],
            [5937.050, 0.00000000],
            [5167.146, 0.00000000],
            [4485.493, 0.00000000],
            [3883.052, 0.00000000],
            [3351.460, 0.00000000],
            [2883.038, 0.00000000],
            [2470.788, 0.00000000],
            [2108.366, 0.00000000],
            [1790.051, 0.00000000],
            [1510.711, 0.00000000],
            [1265.752, 0.00000000],
            [1051.080, 0.00000000],
            [863.058, 0.00000000],
            [698.457, 0.00000000],
            [554.424, 0.00000000],
            [428.434, 0.00000000],
            [318.266, 0.00000000],
            [221.958, 0.00000000],
            [137.790, 0.00000000],
            [64.247, 0.00000000],
            [0.000, 0.00000000],
        ]
    )
    akbk_grib = akbk_grib.T

    pressure_levels_grib = get_pressure_levels_from_vcoord(akbk_grib)
    # Convert levels to layers by averaging adjacent pressure levels
    pressure_grib = np.zeros(len(pressure_levels_grib) - 1)
    for i in range(len(pressure_grib)):
        pressure_grib[i] = 0.5 * (pressure_levels_grib[i] + pressure_levels_grib[i + 1])

    print(f"Reading FV3 cubed-sphere grid information from tile files for prefix: {fv3_prefix}")
    # Read the vertical coordinate from the control file
    levels_fv3 = read_vcoord_from_ctrl_file(ctrl_file)
    pressure_levels_fv3 = get_pressure_levels_from_vcoord(levels_fv3)
    pressure_fv3 = np.zeros(len(pressure_levels_fv3) - 1)
    for i in range(len(pressure_fv3)):
        pressure_fv3[i] = 0.5 * (pressure_levels_fv3[i] + pressure_levels_fv3[i + 1])

    # Read the horizontal cubed-sphere coordinates
    coord_data = read_cubed_sphere_coordinates(fv3_prefix)
    target_geolon = coord_data["geolon"]  # Shape: (6, ny, nx)
    target_geolat = coord_data["geolat"]  # Shape: (6, ny, nx)

    # Perform complete interpolation: vertical and horizontal in one step
    print(f"Interpolating aerosol species data to FV3 cubed-sphere grid for GRIB file: {grib_file}")
    print(f"  Source grid: {len(source_lat)} x {len(source_lon)} x {len(pressure_grib)} (lat x lon x lev)")
    print(
        f"  Target grid: 6 tiles x {target_geolon.shape[1]} x {target_geolon.shape[2]} x {len(pressure_fv3)} (tile x ny x nx x lev)"
    )

    try:
        interpolated_aerosols = interpolate_aerosols_to_cubed_sphere(
            aerosol_data,
            source_lon,
            source_lat,
            pressure_grib,
            pressure_fv3,
            target_geolon,
            target_geolat,
            vertical_method="linear",
            horizontal_method="linear",
        )

        print(f"Complete interpolation finished successfully for prefix: {fv3_prefix}")
        print(f"Interpolated data contains {len([k for k in interpolated_aerosols.keys() if not k.startswith('_')])} species")

        # Print summary of interpolated data
        for species in [k for k in interpolated_aerosols.keys() if not k.startswith("_")]:
            data_shape = interpolated_aerosols[species].shape
            max_val = np.max(interpolated_aerosols[species])
            mean_val = (
                np.mean(interpolated_aerosols[species][interpolated_aerosols[species] > 0])
                if np.any(interpolated_aerosols[species] > 0)
                else 0.0
            )
            print(f"  {species}: shape {data_shape}, max {max_val:.2e}, mean {mean_val:.2e}")

        # Write interpolated data to FV3 tile files
        print("Writing interpolated aerosol fields to FV3 tile files...")

        # Create metadata for aerosol fields
        metadata = create_aerosol_field_metadata()

        # Get list of species
        species_list = [k for k in interpolated_aerosols.keys() if not k.startswith("_")]

        # Map species names to FV3 field names
        species_map = {
            "dust_bin1": "dust1",
            "dust_bin2": "dust2",
            "dust_bin3": "dust3",
            "dust_bin4": "dust4",
            "dust_bin5": "dust5",
            "seasalt_bin1": "seas1",
            "seasalt_bin2": "seas2",
            "seasalt_bin3": "seas3",
            "seasalt_bin4": "seas4",
            "seasalt_bin5": "seas5",
            "sulfate": "so4",
            "organic_carbon_hydrophobic": "oc1",
            "organic_carbon_hydrophilic": "oc2",
            "black_carbon_hydrophobic": "bc1",
            "black_carbon_hydrophilic": "bc2",
        }

        # Write to each tile file individually since each tile has different data
        output_files = []
        for tile in range(1, 7):
            tile_file = f"{fv3_prefix}tile{tile}.nc"

            # Extract data for this specific tile from cubed-sphere array
            tile_specific_data = {}
            for species in species_list:
                cubed_data = interpolated_aerosols[species]  # Shape: (6, nlev, ny, nx)
                output_species = species_map[species] if species in species_map else species
                tile_specific_data[output_species] = cubed_data[tile - 1, :, :, :]  # Shape: (nlev, ny, nx)

            try:
                print(f"  Writing aerosol fields to tile {tile}: {os.path.basename(tile_file)}")

                # Write to this tile
                output_file = add_3d_fields_to_fv3_tile(
                    tile_file,
                    tile_specific_data,
                    metadata,
                    backup_original=True,
                    output_path=(os.path.join(output_dir, os.path.basename(tile_file)) if output_dir != os.getcwd() else None),
                )

                output_files.append(output_file)
                print(f"    Successfully wrote {len(species_list)} species to: {os.path.basename(output_file)}")

            except Exception as e:
                print(f"    ERROR: Failed to write aerosol fields to tile {tile}: {e}")
                import traceback

                traceback.print_exc()
                return False

        print(f"Aerosol fields successfully written to {len(output_files)} FV3 tile files.")
        print(f"Output files created in: {output_dir if output_dir != os.getcwd() else 'current directory'}")

        # Print summary of what was written
        print(f"Summary for prefix {fv3_prefix}:")
        print(f"  - {len(species_list)} aerosol species written to each tile")
        print(f"  - {len(pressure_fv3)} vertical levels per species")
        print(f"  - Grid dimensions: {target_geolon.shape[1]} x {target_geolon.shape[2]} per tile")
        print("  - Original tile files backed up with .orig extension")

    except Exception as e:
        print(f"ERROR: Interpolation failed: {e}")
        import traceback

        traceback.print_exc()
        return False

    return True


def main():
    """Main function to parse arguments and run the GCAFS IC generation."""
    parser = argparse.ArgumentParser(description="Generate GCAFS initial conditions from GRIB2 and FV3 files")

    parser.add_argument(
        "--grib-file",
        type=str,
        required=True,
        help="Path to input GRIB2 file containing chemical species data",
    )

    parser.add_argument(
        "--fv3-prefix",
        type=str,
        required=True,
        help="Path prefix for FV3 cold start files (e.g., /path/to/gfs_data.tile)",
    )

    parser.add_argument(
        "--output-dir",
        type=str,
        default=None,
        help="Output directory for GCAFS initial condition files (default: current directory)",
    )

    args = parser.parse_args()

    # Run the IC generation
    success = generate_gcafs_ics(grib_file=args.grib_file, fv3_prefix=args.fv3_prefix, output_dir=args.output_dir)

    if success:
        print("GCAFS initial conditions generation completed successfully!")
        sys.exit(0)
    else:
        print("GCAFS initial conditions generation failed!")
        sys.exit(1)


if __name__ == "__main__":
    main()
