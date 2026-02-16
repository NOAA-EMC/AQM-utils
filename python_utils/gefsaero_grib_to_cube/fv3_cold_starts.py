#!/usr/bin/env python3
# fv3_cold_starts.py
# Functions to add 3D aerosol fields to FV3 tile files for cold starts

import logging
import shutil
from pathlib import Path
from typing import Dict, Optional, Union

import numpy as np
import xarray as xr

# Set up logging
logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s")
logger = logging.getLogger(__name__)


def read_vcoord_from_ctrl_file(ctrl_file_path: str) -> np.ndarray:
    """
    Read vertical coordinate information from FV3 control file.

    The vcoord array contains the hybrid sigma-pressure coordinate parameters
    used to define the vertical levels in the FV3 model.

    Args:
        ctrl_file_path (str): Path to the FV3 control file (e.g., gfs_ctrl.nc)

    Returns:
        np.ndarray: vcoord array with shape (2, nlev+1) containing:
                   - vcoord[0, :]: pressure values (Pa)
                   - vcoord[1, :]: sigma values

    Raises:
        FileNotFoundError: If the control file doesn't exist
        KeyError: If vcoord variable is not found in the file
        ValueError: If vcoord has unexpected dimensions
    """

    ctrl_path = Path(ctrl_file_path)
    if not ctrl_path.exists():
        raise FileNotFoundError(f"Control file not found: {ctrl_file_path}")

    logger.info(f"Reading vcoord from: {ctrl_file_path}")

    try:
        with xr.open_dataset(ctrl_file_path) as ds:
            if "vcoord" not in ds.data_vars:
                raise KeyError("'vcoord' variable not found in control file")

            vcoord = ds["vcoord"].values

            # Validate vcoord dimensions
            if len(vcoord.shape) != 2:
                raise ValueError(f"Expected vcoord to be 2D, got shape {vcoord.shape}")

            if vcoord.shape[0] != 2:
                raise ValueError(f"Expected vcoord first dimension to be 2, got {vcoord.shape[0]}")

            nlev_plus_1 = vcoord.shape[1]
            nlev = nlev_plus_1 - 1

            logger.info(f"Read vcoord: {nlev} levels, shape {vcoord.shape}")
            logger.info(f"Pressure range: {vcoord[0, :].min():.2f} to {vcoord[0, :].max():.2f} Pa")
            logger.info(f"Sigma range: {vcoord[1, :].min():.6f} to {vcoord[1, :].max():.6f}")

            # Extract attributes if available
            if hasattr(ds["vcoord"], "attrs"):
                attrs = ds["vcoord"].attrs
                if "long_name" in attrs:
                    logger.info(f"vcoord description: {attrs['long_name']}")
                if "units" in attrs:
                    logger.info(f"vcoord units: {attrs['units']}")

            return vcoord

    except Exception as e:
        logger.error(f"Failed to read vcoord from {ctrl_file_path}: {e}")
        raise


def get_pressure_levels_from_vcoord(vcoord: np.ndarray, surface_pressure: Union[float, np.ndarray] = 101325.0) -> np.ndarray:
    """
    Calculate pressure levels from vcoord and surface pressure.

    Uses the hybrid sigma-pressure coordinate formula:
    p(k) = vcoord[0, k] + vcoord[1, k] * surface_pressure

    Args:
        vcoord (np.ndarray): Vertical coordinate array from read_vcoord_from_ctrl_file()
        surface_pressure (Union[float, np.ndarray]): Surface pressure in Pa.
                                                   Can be scalar or 2D array for spatial variation

    Returns:
        np.ndarray: Pressure levels in Pa. Shape depends on surface_pressure:
                   - If surface_pressure is scalar: (nlev+1,)
                   - If surface_pressure is 2D: (nlev+1, ny, nx)
    """

    if len(vcoord.shape) != 2 or vcoord.shape[0] != 2:
        raise ValueError(f"vcoord must have shape (2, nlev+1), got {vcoord.shape}")

    ak = vcoord[0, :]  # pressure coefficients (Pa)
    bk = vcoord[1, :]  # sigma coefficients (dimensionless)

    if isinstance(surface_pressure, (int, float)):
        # Scalar surface pressure
        pressure_levels = ak + bk * surface_pressure
        logger.info(f"Calculated pressure levels from surface pressure {surface_pressure:.2f} Pa")
        logger.info(f"Pressure range: {pressure_levels.min():.2f} to {pressure_levels.max():.2f} Pa")

    else:
        # Spatially varying surface pressure
        surface_pressure = np.asarray(surface_pressure)
        if len(surface_pressure.shape) != 2:
            raise ValueError(f"surface_pressure array must be 2D, got shape {surface_pressure.shape}")

        # Broadcast for calculation: ak and bk are (nlev+1,), surface_pressure is (ny, nx)
        # Result will be (nlev+1, ny, nx)
        ak_expanded = ak[:, np.newaxis, np.newaxis]
        bk_expanded = bk[:, np.newaxis, np.newaxis]
        pressure_levels = ak_expanded + bk_expanded * surface_pressure[np.newaxis, :, :]

        logger.info("Calculated pressure levels from 2D surface pressure field")
        logger.info(f"Surface pressure range: {surface_pressure.min():.2f} to {surface_pressure.max():.2f} Pa")
        logger.info(f"Pressure levels range: {pressure_levels.min():.2f} to {pressure_levels.max():.2f} Pa")

    return pressure_levels


def read_cubed_sphere_coordinates(tile_file_prefix: str) -> dict:
    """
    Read geographic coordinates from all six FV3 cubed-sphere tile files.

    Args:
        tile_file_prefix (str): Path prefix for tile files (e.g., "/path/to/gfs_ctrl.tile")
                               Function will read gfs_ctrl.tile1.nc through gfs_ctrl.tile6.nc

    Returns:
        Dict[str, np.ndarray]: Dictionary containing:
            - 'geolon': Geographic longitude array with shape (6, ny, nx)
            - 'geolat': Geographic latitude array with shape (6, ny, nx)
            - 'tile_shapes': List of (ny, nx) shapes for each tile
            - 'grid_info': Dictionary with grid metadata

    Raises:
        FileNotFoundError: If any tile file doesn't exist
        KeyError: If coordinate variables are not found
        ValueError: If tiles have inconsistent dimensions
    """

    logger.info(f"Reading cubed-sphere coordinates from tile files: {tile_file_prefix}*.nc")

    geolon_tiles = []
    geolat_tiles = []
    tile_shapes = []
    grid_info = {}

    for tile_num in range(1, 7):
        tile_file = f"{tile_file_prefix}tile{tile_num}.nc"
        tile_path = Path(tile_file)

        if not tile_path.exists():
            raise FileNotFoundError(f"Tile file not found: {tile_file}")

        logger.info(f"Reading coordinates from tile {tile_num}: {tile_file}")

        try:
            with xr.open_dataset(tile_file) as ds:
                # Look for longitude coordinate variable
                lon_var = None
                lat_var = None

                # Check common coordinate variable names
                for lon_name in ["geolon", "lon", "longitude", "grid_lont", "grid_x"]:
                    if lon_name in ds.data_vars or lon_name in ds.coords:
                        lon_var = lon_name
                        break

                for lat_name in ["geolat", "lat", "latitude", "grid_latt", "grid_y"]:
                    if lat_name in ds.data_vars or lat_name in ds.coords:
                        lat_var = lat_name
                        break

                if lon_var is None:
                    # List available variables for debugging
                    available_vars = list(ds.data_vars.keys()) + list(ds.coords.keys())
                    raise KeyError(f"Longitude variable not found in tile {tile_num}. Available variables: {available_vars}")

                if lat_var is None:
                    available_vars = list(ds.data_vars.keys()) + list(ds.coords.keys())
                    raise KeyError(f"Latitude variable not found in tile {tile_num}. Available variables: {available_vars}")

                # Extract coordinate arrays
                geolon = ds[lon_var].values
                geolat = ds[lat_var].values

                # Ensure 2D arrays
                if len(geolon.shape) != 2:
                    raise ValueError(f"Expected 2D longitude array for tile {tile_num}, got shape {geolon.shape}")

                if len(geolat.shape) != 2:
                    raise ValueError(f"Expected 2D latitude array for tile {tile_num}, got shape {geolat.shape}")

                if geolon.shape != geolat.shape:
                    raise ValueError(
                        f"Longitude and latitude arrays have different shapes for tile {tile_num}: {geolon.shape} vs {geolat.shape}"
                    )

                ny, nx = geolon.shape
                tile_shapes.append((ny, nx))

                logger.info(f"Tile {tile_num}: shape {geolon.shape}")
                logger.info(f"  Longitude range: {geolon.min():.3f} to {geolon.max():.3f}°")
                logger.info(f"  Latitude range: {geolat.min():.3f} to {geolat.max():.3f}°")

                geolon_tiles.append(geolon)
                geolat_tiles.append(geolat)

                # Store grid metadata from first tile
                if tile_num == 1:
                    grid_info["nx"] = nx
                    grid_info["ny"] = ny
                    grid_info["lon_var_name"] = lon_var
                    grid_info["lat_var_name"] = lat_var

                    # Extract attributes if available
                    if hasattr(ds[lon_var], "attrs"):
                        grid_info["lon_attrs"] = dict(ds[lon_var].attrs)
                    if hasattr(ds[lat_var], "attrs"):
                        grid_info["lat_attrs"] = dict(ds[lat_var].attrs)

        except Exception as e:
            logger.error(f"Failed to read coordinates from tile {tile_num}: {e}")
            raise

    # Check that all tiles have the same dimensions
    if not all(shape == tile_shapes[0] for shape in tile_shapes):
        raise ValueError(f"Tiles have inconsistent shapes: {tile_shapes}")

    # Stack into 3D arrays: (6, ny, nx)
    geolon_all = np.stack(geolon_tiles, axis=0)
    geolat_all = np.stack(geolat_tiles, axis=0)

    logger.info("Successfully read coordinates from all 6 tiles")
    logger.info(f"Combined array shapes: geolon {geolon_all.shape}, geolat {geolat_all.shape}")
    logger.info(f"Global longitude range: {geolon_all.min():.3f} to {geolon_all.max():.3f}°")
    logger.info(f"Global latitude range: {geolat_all.min():.3f} to {geolat_all.max():.3f}°")

    return {
        "geolon": geolon_all,
        "geolat": geolat_all,
        "tile_shapes": tile_shapes,
        "grid_info": grid_info,
    }


def get_cubed_sphere_grid_info(tile_file_prefix: str) -> dict:
    """
    Get comprehensive grid information from FV3 cubed-sphere tile files.

    This is a convenience function that combines coordinate reading with
    additional grid metadata extraction.

    Args:
        tile_file_prefix (str): Path prefix for tile files

    Returns:
        Dict: Complete grid information including coordinates and metadata
    """

    # Read coordinates
    coord_data = read_cubed_sphere_coordinates(tile_file_prefix)

    # Add some derived information
    geolon = coord_data["geolon"]
    geolat = coord_data["geolat"]

    grid_info = coord_data["grid_info"].copy()
    grid_info.update(
        {
            "ntiles": 6,
            "total_points": geolon.size,
            "points_per_tile": geolon.shape[1] * geolon.shape[2],
            "global_lon_range": (geolon.min(), geolon.max()),
            "global_lat_range": (geolat.min(), geolat.max()),
            "coordinate_arrays": {"geolon": geolon, "geolat": geolat},
        }
    )

    return grid_info


def add_3d_fields_to_fv3_tile(
    tile_file_path: str,
    field_data: Dict[str, np.ndarray],
    field_metadata: Optional[Dict[str, Dict]] = None,
    output_path: Optional[str] = None,
    backup_original: bool = True,
) -> str:
    """
    Add 3D fields to an FV3 tile file (e.g., gfs_data.tile1.nc).

    Args:
        tile_file_path (str): Path to the FV3 tile file
        field_data (Dict[str, np.ndarray]): Dictionary of field names and their 3D data arrays
                                          Expected shape: (nlev, ny, nx) for each field
        field_metadata (Dict[str, Dict], optional): Metadata for each field including:
                                                  - units: str
                                                  - long_name: str
                                                  - standard_name: str (optional)
        output_path (str, optional): Path for output file. If None, overwrites original
        backup_original (bool): Whether to create a backup of the original file

    Returns:
        str: Path to the output file

    Raises:
        FileNotFoundError: If the tile file doesn't exist
        ValueError: If field dimensions don't match tile dimensions
    """

    tile_path = Path(tile_file_path)
    if not tile_path.exists():
        raise FileNotFoundError(f"Tile file not found: {tile_file_path}")

    logger.info(f"Opening FV3 tile file: {tile_file_path}")

    # Open the tile file
    with xr.open_dataset(tile_file_path) as ds:
        # Get tile dimensions
        if "pfull" in ds.dims:
            nlev = ds.dims["pfull"]
        elif "lev" in ds.dims:
            nlev = ds.dims["lev"]
        else:
            # Try to infer from existing 3D variables
            for var in ds.data_vars:
                if len(ds[var].dims) == 3:
                    nlev = ds[var].shape[0]
                    break
            else:
                raise ValueError("Cannot determine number of levels from tile file")

        # Get horizontal dimensions
        if "grid_yt" in ds.dims and "grid_xt" in ds.dims:
            ny, nx = ds.dims["grid_yt"], ds.dims["grid_xt"]
        elif "lat" in ds.dims and "lon" in ds.dims:
            ny, nx = ds.dims["lat"], ds.dims["lon"]
        else:
            # Try to infer from existing 2D/3D variables
            for var in ds.data_vars:
                if len(ds[var].dims) >= 2:
                    var_dims = ds[var].dims
                    if len(var_dims) == 3:
                        ny, nx = ds[var].shape[1], ds[var].shape[2]
                    else:
                        ny, nx = ds[var].shape[0], ds[var].shape[1]
                    break
            else:
                raise ValueError("Cannot determine horizontal dimensions from tile file")

        logger.info(f"Tile dimensions: {nlev} levels, {ny}x{nx} horizontal grid")

        # Create a copy of the dataset to modify
        ds_out = ds.copy(deep=True)

        # Add each field to the dataset
        for field_name, field_array in field_data.items():
            logger.info(f"Adding field: {field_name}")

            # Validate field dimensions
            if field_array.shape != (nlev, ny, nx):
                raise ValueError(f"Field {field_name} has shape {field_array.shape}, expected ({nlev}, {ny}, {nx})")

            # Determine coordinate names based on what's available in the dataset
            if "pfull" in ds.coords:
                lev_coord = "pfull"
            elif "lev" in ds.coords:
                lev_coord = "lev"
            else:
                # Create a level coordinate if it doesn't exist
                lev_coord = "pfull"
                ds_out[lev_coord] = (lev_coord, np.arange(1, nlev + 1))
                ds_out[lev_coord].attrs = {
                    "units": "level",
                    "long_name": "pressure level",
                }

            if "grid_yt" in ds.coords and "grid_xt" in ds.coords:
                lat_coord, lon_coord = "grid_yt", "grid_xt"
            elif "lat" in ds.coords and "lon" in ds.coords:
                lat_coord, lon_coord = "lat", "lon"
            else:
                # Use existing coordinate names from a 3D variable
                for var in ds.data_vars:
                    if len(ds[var].dims) == 3:
                        lat_coord, lon_coord = ds[var].dims[1], ds[var].dims[2]
                        break
                else:
                    raise ValueError("Cannot determine coordinate names")

            # Create the DataArray for the field
            field_da = xr.DataArray(
                field_array,
                dims=[lev_coord, lat_coord, lon_coord],
                coords={
                    lev_coord: ds_out[lev_coord],
                    lat_coord: ds_out[lat_coord],
                    lon_coord: ds_out[lon_coord],
                },
                name=field_name,
            )

            # Add metadata if provided
            if field_metadata and field_name in field_metadata:
                field_da.attrs.update(field_metadata[field_name])
            else:
                # Default metadata
                field_da.attrs = {
                    "units": "kg kg-1",
                    "long_name": f"{field_name} mass mixing ratio",
                }

            # Add the field to the dataset
            ds_out[field_name] = field_da

            logger.info(
                f"Added {field_name}: shape {field_array.shape}, min={np.min(field_array):.2e}, max={np.max(field_array):.2e}"
            )

    # Determine output path
    if output_path is None:
        output_path = f"{tile_file_path}.out.nc"
        # Create backup if requested
        if backup_original:
            backup_path = f"{tile_file_path}.backup"
            if not Path(backup_path).exists():
                logger.info(f"Creating backup: {backup_path}")
                shutil.copy2(tile_file_path, backup_path)

    # Write the modified dataset
    logger.info(f"Writing modified dataset to: {output_path}")
    ds_out.to_netcdf(output_path)

    logger.info(f"Successfully added {len(field_data)} fields to {output_path}")

    # Move the temporary file to the final output path
    if output_path != tile_file_path:
        shutil.move(output_path, tile_file_path)
        logger.info(f"Moved temporary output file to: {tile_file_path}")

    return output_path


def create_aerosol_field_metadata() -> Dict[str, Dict]:
    """
    Create metadata dictionary for aerosol fields matching FV3 tracer names.

    Returns:
        Dict[str, Dict]: Metadata for each aerosol field using FV3 naming convention
    """

    metadata = {
        # Dust species (dust1-dust5)
        "dust1": {
            "units": "kg kg-1",
            "long_name": "dust mixing ratio bin 1 (0.2-2 μm)",
            "standard_name": "mass_fraction_of_dust_dry_aerosol_particles_in_air",
        },
        "dust2": {
            "units": "kg kg-1",
            "long_name": "dust mixing ratio bin 2 (2-3.6 μm)",
            "standard_name": "mass_fraction_of_dust_dry_aerosol_particles_in_air",
        },
        "dust3": {
            "units": "kg kg-1",
            "long_name": "dust mixing ratio bin 3 (3.6-6 μm)",
            "standard_name": "mass_fraction_of_dust_dry_aerosol_particles_in_air",
        },
        "dust4": {
            "units": "kg kg-1",
            "long_name": "dust mixing ratio bin 4 (6-12 μm)",
            "standard_name": "mass_fraction_of_dust_dry_aerosol_particles_in_air",
        },
        "dust5": {
            "units": "kg kg-1",
            "long_name": "dust mixing ratio bin 5 (12-20 μm)",
            "standard_name": "mass_fraction_of_dust_dry_aerosol_particles_in_air",
        },
        # Sea salt species (seas1-seas5)
        "seas1": {
            "units": "kg kg-1",
            "long_name": "sea salt mixing ratio bin 1 (0.06-0.2 μm)",
            "standard_name": "mass_fraction_of_sea_salt_dry_aerosol_particles_in_air",
        },
        "seas2": {
            "units": "kg kg-1",
            "long_name": "sea salt mixing ratio bin 2 (0.2-1 μm)",
            "standard_name": "mass_fraction_of_sea_salt_dry_aerosol_particles_in_air",
        },
        "seas3": {
            "units": "kg kg-1",
            "long_name": "sea salt mixing ratio bin 3 (1-3 μm)",
            "standard_name": "mass_fraction_of_sea_salt_dry_aerosol_particles_in_air",
        },
        "seas4": {
            "units": "kg kg-1",
            "long_name": "sea salt mixing ratio bin 4 (3-10 μm)",
            "standard_name": "mass_fraction_of_sea_salt_dry_aerosol_particles_in_air",
        },
        "seas5": {
            "units": "kg kg-1",
            "long_name": "sea salt mixing ratio bin 5 (10-20 μm)",
            "standard_name": "mass_fraction_of_sea_salt_dry_aerosol_particles_in_air",
        },
        # Sulfate and other species
        "so4": {
            "units": "kg kg-1",
            "long_name": "sulfate aerosol mixing ratio",
            "standard_name": "mass_fraction_of_sulfate_dry_aerosol_particles_in_air",
        },
        "so2": {
            "units": "kg kg-1",
            "long_name": "sulfur dioxide mixing ratio",
            "standard_name": "mass_fraction_of_sulfur_dioxide_in_air",
        },
        "dms": {
            "units": "kg kg-1",
            "long_name": "dimethyl sulfide mixing ratio",
            "standard_name": "mass_fraction_of_dimethyl_sulfide_in_air",
        },
        "msa": {
            "units": "kg kg-1",
            "long_name": "methane sulfonic acid mixing ratio",
            "standard_name": "mass_fraction_of_methane_sulfonic_acid_in_air",
        },
        # Black carbon species
        "bc1": {
            "units": "kg kg-1",
            "long_name": "hydrophobic black carbon aerosol mixing ratio",
            "standard_name": "mass_fraction_of_black_carbon_dry_aerosol_particles_in_air",
        },
        "bc2": {
            "units": "kg kg-1",
            "long_name": "hydrophilic black carbon aerosol mixing ratio",
            "standard_name": "mass_fraction_of_black_carbon_dry_aerosol_particles_in_air",
        },
        # Organic carbon species
        "oc1": {
            "units": "kg kg-1",
            "long_name": "hydrophobic organic carbon aerosol mixing ratio",
            "standard_name": "mass_fraction_of_particulate_organic_matter_dry_aerosol_particles_in_air",
        },
        "oc2": {
            "units": "kg kg-1",
            "long_name": "hydrophilic organic carbon aerosol mixing ratio",
            "standard_name": "mass_fraction_of_particulate_organic_matter_dry_aerosol_particles_in_air",
        },
        # Particulate matter (diagnostic fields)
        "pm25": {
            "units": "kg kg-1",
            "long_name": "particulate matter with diameter <= 2.5 μm",
            "standard_name": "mass_fraction_of_pm2p5_ambient_aerosol_particles_in_air",
        },
        "pm10": {
            "units": "kg kg-1",
            "long_name": "particulate matter with diameter <= 10 μm",
            "standard_name": "mass_fraction_of_pm10_ambient_aerosol_particles_in_air",
        },
    }

    return metadata
