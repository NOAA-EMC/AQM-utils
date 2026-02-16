import numpy as np
from scipy.interpolate import RegularGridInterpolator, interp1d


def interpolate_aerosols_vertically(aerosol_data, pressure_source, pressure_target, extrapolate_method="constant"):
    """
    Interpolate aerosol species data vertically from source pressure levels to target pressure levels
    using logarithmic pressure coordinates.

    Parameters:
    -----------
    aerosol_data : dict
        Dictionary containing aerosol species arrays with shape (nlev_source, nlat, nlon)
        Special keys '_grid_info' and '_levels' are preserved and not interpolated
    pressure_source : np.ndarray
        Source pressure levels in Pa, shape (nlev_source,)
    pressure_target : np.ndarray
        Target pressure levels in Pa, shape (nlev_target,)
    extrapolate_method : str, optional
        Method for handling extrapolation outside source pressure range:
        - 'constant': Use nearest value (default)
        - 'zero': Set to zero outside range
        - 'linear': Linear extrapolation in log-pressure space

    Returns:
    --------
    dict
        Dictionary with interpolated aerosol species arrays with shape (nlev_target, nlat, nlon)
        Preserves metadata keys '_grid_info' and '_levels'
    """

    print(
        "Starting vertical interpolation using logarithmic pressure coordinates... "
        f"source shape: {pressure_source.shape}, target shape: {pressure_target.shape}"
    )
    print(f"Source pressure levels: {len(pressure_source)} ({pressure_source.min():.1f} to {pressure_source.max():.1f} Pa)")
    print(f"Target pressure levels: {len(pressure_target)} ({pressure_target.min():.1f} to {pressure_target.max():.1f} Pa)")

    # Validate inputs
    if len(pressure_source.shape) != 1:
        raise ValueError(f"pressure_source must be 1D, got shape {pressure_source.shape}")
    if len(pressure_target.shape) != 1:
        raise ValueError(f"pressure_target must be 1D, got shape {pressure_target.shape}")

    # Check for non-positive pressures
    if np.any(pressure_source <= 0):
        raise ValueError("pressure_source contains non-positive values - cannot take logarithm")
    if np.any(pressure_target <= 0):
        raise ValueError("pressure_target contains non-positive values - cannot take logarithm")

    # Sort both source and target pressures by decreasing pressure (surface to top)
    # This ensures monotonic coordinates for interpolation
    source_sort_idx = np.argsort(pressure_source)[::-1]  # Sort descending
    target_sort_idx = np.argsort(pressure_target)[::-1]  # Sort descending
    target_unsort_idx = np.argsort(target_sort_idx)  # To restore original order

    pressure_source_sorted = pressure_source[source_sort_idx]
    pressure_target_sorted = pressure_target[target_sort_idx]

    # Convert to logarithmic pressure coordinates
    log_pressure_source_sorted = np.log(pressure_source_sorted)
    log_pressure_target_sorted = np.log(pressure_target_sorted)

    print(
        f"Log-pressure ranges: source {log_pressure_source_sorted.min():.3f} to {log_pressure_source_sorted.max():.3f}, "
        f"target {log_pressure_target_sorted.min():.3f} to {log_pressure_target_sorted.max():.3f}"
    )

    # Initialize output dictionary
    interpolated_data = {}

    # Preserve metadata
    for key in ["_grid_info", "_levels"]:
        if key in aerosol_data:
            interpolated_data[key] = aerosol_data[key]

    # Get list of species to interpolate (exclude metadata keys)
    species_keys = [key for key in aerosol_data.keys() if not key.startswith("_")]

    print(f"Interpolating {len(species_keys)} aerosol species: {species_keys}")

    # Process each aerosol species
    for species in species_keys:
        data = aerosol_data[species]

        if len(data.shape) != 3:
            print(f"WARNING: Skipping {species} - expected 3D array, got shape {data.shape}")
            continue

        nlev_source, nlat, nlon = data.shape

        if nlev_source != len(pressure_source):
            print(
                f"WARNING: Skipping {species} - level mismatch: data has {nlev_source} levels, "
                f"pressure_source has {len(pressure_source)} levels"
            )
            continue

        print(f"  Interpolating {species}: {data.shape} -> ({len(pressure_target)}, {nlat}, {nlon})")

        # Sort the data according to the sorted source pressures
        data_sorted = data[source_sort_idx, :, :]

        # Check that log-pressure is monotonic after sorting
        if not np.all(log_pressure_source_sorted[:-1] >= log_pressure_source_sorted[1:]):
            print(f"  WARNING: Log-pressure not monotonic for {species} - interpolation may be unreliable")

        # Initialize output array (in sorted target pressure order)
        nlev_target = len(pressure_target)
        interpolated_species_sorted = np.zeros((nlev_target, nlat, nlon))

        # Interpolate for each horizontal grid point
        for i in range(nlat):
            for j in range(nlon):
                # Extract vertical profile at this grid point
                profile = data_sorted[:, i, j]

                # Skip interpolation if all values are zero or NaN
                if np.all(profile == 0) or np.all(np.isnan(profile)):
                    interpolated_species_sorted[:, i, j] = 0.0
                    continue

                # Create interpolator in log-pressure space
                try:
                    if extrapolate_method == "constant":
                        # Use constant extrapolation with boundary values
                        f = interp1d(
                            log_pressure_source_sorted,
                            profile,
                            kind="linear",
                            bounds_error=False,
                            fill_value=float(profile[0]),
                        )
                    elif extrapolate_method == "zero":
                        # Set to zero outside bounds
                        f = interp1d(
                            log_pressure_source_sorted,
                            profile,
                            kind="linear",
                            bounds_error=False,
                            fill_value=0.0,
                        )
                    elif extrapolate_method == "linear":
                        # Linear extrapolation in log-pressure space
                        f = interp1d(
                            log_pressure_source_sorted,
                            profile,
                            kind="linear",
                            bounds_error=False,
                            fill_value="extrapolate",  # type: ignore
                        )
                    else:
                        raise ValueError(f"Unknown extrapolate_method: {extrapolate_method}")

                    # Interpolate to target log-pressure levels (sorted)
                    interpolated_species_sorted[:, i, j] = f(log_pressure_target_sorted)

                    # Ensure non-negative values (aerosols should be >= 0)
                    interpolated_species_sorted[:, i, j] = np.maximum(interpolated_species_sorted[:, i, j], 0.0)

                except Exception as e:
                    print(f"    WARNING: Interpolation failed at grid point ({i}, {j}) for {species}: {e}")
                    interpolated_species_sorted[:, i, j] = 0.0

        # Restore original target pressure order
        interpolated_species = interpolated_species_sorted[target_unsort_idx, :, :]

        # Store interpolated data
        interpolated_data[species] = interpolated_species

        # Print some statistics
        orig_max = np.max(data)
        orig_mean = np.mean(data[data > 0]) if np.any(data > 0) else 0.0
        interp_max = np.max(interpolated_species)
        interp_mean = np.mean(interpolated_species[interpolated_species > 0]) if np.any(interpolated_species > 0) else 0.0

        print(f"    {species}: max {orig_max:.2e} -> {interp_max:.2e}, mean {orig_mean:.2e} -> {interp_mean:.2e}")

        # Check for extrapolation
        log_p_min = log_pressure_source_sorted.min()
        log_p_max = log_pressure_source_sorted.max()
        n_extrap_low = np.sum(log_pressure_target_sorted < log_p_min)
        n_extrap_high = np.sum(log_pressure_target_sorted > log_p_max)

        if n_extrap_low > 0 or n_extrap_high > 0:
            print(f"    {species}: extrapolating {n_extrap_low} levels below and {n_extrap_high} levels above source range")

    print(f"Vertical interpolation completed for {len(species_keys)} species")
    return interpolated_data


def interpolate_aerosols_horizontally(
    aerosol_data,
    source_lon,
    source_lat,
    target_geolon,
    target_geolat,
    method="linear",
    fill_value=0.0,
):
    """
    Interpolate aerosol species data horizontally from regular lat-lon grid to FV3 cubed-sphere tiles.

    Parameters:
    -----------
    aerosol_data : dict
        Dictionary containing aerosol species arrays with shape (nlev, nlat_source, nlon_source)
        Special keys '_grid_info' and '_levels' are preserved and not interpolated
    source_lon : np.ndarray
        Source longitude coordinates in degrees, shape (nlon_source,)
    source_lat : np.ndarray
        Source latitude coordinates in degrees, shape (nlat_source,)
    target_geolon : np.ndarray
        Target longitude coordinates in degrees, shape (6, ny_target, nx_target) for 6 tiles
    target_geolat : np.ndarray
        Target latitude coordinates in degrees, shape (6, ny_target, nx_target) for 6 tiles
    method : str, optional
        Interpolation method: 'linear', 'nearest', or 'cubic' (default: 'linear')
    fill_value : float, optional
        Value to use for points outside the source grid domain (default: 0.0)

    Returns:
    --------
    dict
        Dictionary with interpolated aerosol species arrays with shape (6, nlev, ny_target, nx_target)
        One array per tile, preserves metadata keys '_grid_info' and '_levels'
    """

    print(
        "Starting horizontal interpolation from regular lat-lon grid to cubed-sphere tiles... "
        f"source_lat shape: {source_lat.shape}, source_lon shape: {source_lon.shape}"
    )
    print(f"Source grid: {len(source_lat)} x {len(source_lon)} (lat x lon)")
    print(f"Target grid: 6 tiles of {target_geolon.shape[1]} x {target_geolon.shape[2]} each")
    print(f"Interpolation method: {method}")

    # Validate inputs
    if len(source_lon.shape) != 1 or len(source_lat.shape) != 1:
        raise ValueError("source_lon and source_lat must be 1D arrays")

    if target_geolon.shape != target_geolat.shape:
        raise ValueError("target_geolon and target_geolat must have the same shape")

    if len(target_geolon.shape) != 3 or target_geolon.shape[0] != 6:
        raise ValueError("target coordinates must have shape (6, ny, nx) for 6 tiles")

    # Handle longitude periodicity (ensure source longitude covers 0-360 or -180 to 180)

    if source_lon.min() < 0 and source_lon.max() > 180:
        # Data spans -180 to 180, convert target to same range
        target_geolon_adj = np.where(target_geolon > 180, target_geolon - 360, target_geolon)
    elif source_lon.min() >= 0 and source_lon.max() > 180:
        # Data spans 0 to 360, convert target to same range
        target_geolon_adj = np.where(target_geolon < 0, target_geolon + 360, target_geolon)
    else:
        target_geolon_adj = target_geolon.copy()

    print(f"Source longitude range: {source_lon.min():.1f} to {source_lon.max():.1f}°")
    print(f"Source latitude range: {source_lat.min():.1f} to {source_lat.max():.1f}°")
    print(f"Target longitude range: {target_geolon_adj.min():.1f} to {target_geolon_adj.max():.1f}°")
    print(f"Target latitude range: {target_geolat.min():.1f} to {target_geolat.max():.1f}°")

    # Initialize output dictionary
    interpolated_data = {}

    # Preserve metadata
    for key in ["_grid_info", "_levels"]:
        if key in aerosol_data:
            interpolated_data[key] = aerosol_data[key]

    # Get list of species to interpolate (exclude metadata keys)
    species_keys = [key for key in aerosol_data.keys() if not key.startswith("_")]

    print(f"Interpolating {len(species_keys)} aerosol species: {species_keys}")

    # Get dimensions
    ntiles, ny_target, nx_target = target_geolon.shape

    # Process each aerosol species
    for species in species_keys:
        data = aerosol_data[species]

        if len(data.shape) != 3:
            print(f"WARNING: Skipping {species} - expected 3D array, got shape {data.shape}")
            continue

        nlev, nlat_source, nlon_source = data.shape

        if nlat_source != len(source_lat) or nlon_source != len(source_lon):
            print(f"WARNING: Skipping {species} - grid size mismatch")
            print(f"  Data shape: {data.shape}, expected: ({nlev}, {len(source_lat)}, {len(source_lon)})")
            continue

        print(f"  Interpolating {species}: {data.shape} -> (6, {nlev}, {ny_target}, {nx_target})")

        # Initialize output array for this species (6 tiles)
        interpolated_species = np.zeros((ntiles, nlev, ny_target, nx_target))

        # Interpolate each vertical level
        for lev in range(nlev):
            level_data = data[lev, :, :]

            # Skip if all values are zero or NaN
            if np.all(level_data == 0) or np.all(np.isnan(level_data)):
                # All tiles for this level remain zero
                continue

            try:
                # Create interpolator for this level
                # Note: RegularGridInterpolator expects (lat, lon) order
                interpolator = RegularGridInterpolator(
                    (source_lat, source_lon),
                    level_data,
                    method=method,
                    bounds_error=False,
                    fill_value=fill_value,
                )

                # Interpolate to each tile
                for tile in range(ntiles):
                    # Get target coordinates for this tile
                    tile_lon = target_geolon_adj[tile, :, :].ravel()
                    tile_lat = target_geolat[tile, :, :].ravel()

                    # Create coordinate pairs for interpolation
                    target_points = np.column_stack((tile_lat, tile_lon))

                    # Interpolate
                    interpolated_values = interpolator(target_points)

                    # Reshape back to tile grid
                    interpolated_species[tile, lev, :, :] = interpolated_values.reshape(ny_target, nx_target)

                    # Ensure non-negative values
                    interpolated_species[tile, lev, :, :] = np.maximum(interpolated_species[tile, lev, :, :], 0.0)

            except Exception as e:
                print(f"    WARNING: Horizontal interpolation failed for {species} level {lev}: {e}")
                # Leave this level as zeros
                continue

        # Store interpolated data
        interpolated_data[species] = interpolated_species

        # Print some statistics
        orig_max = np.max(data)
        orig_mean = np.mean(data[data > 0]) if np.any(data > 0) else 0.0
        interp_max = np.max(interpolated_species)
        interp_mean = np.mean(interpolated_species[interpolated_species > 0]) if np.any(interpolated_species > 0) else 0.0

        print(f"    {species}: max {orig_max:.2e} -> {interp_max:.2e}, mean {orig_mean:.2e} -> {interp_mean:.2e}")

        # Check coverage for each tile
        for tile in range(ntiles):
            tile_data = interpolated_species[tile, :, :, :]
            n_nonzero = np.sum(tile_data > 0)
            total_points = tile_data.size
            coverage = n_nonzero / total_points * 100
            print(f"      Tile {tile + 1}: {coverage:.1f}% non-zero points")

    print(f"Horizontal interpolation completed for {len(species_keys)} species")
    return interpolated_data


def interpolate_aerosols_to_cubed_sphere(
    aerosol_data,
    source_lon,
    source_lat,
    pressure_source,
    pressure_target,
    target_geolon,
    target_geolat,
    vertical_method="constant",
    horizontal_method="linear",
):
    """
    Complete interpolation pipeline: vertical then horizontal interpolation of aerosol data
    from regular lat-lon grid to FV3 cubed-sphere tiles.

    Parameters:
    -----------
    aerosol_data : dict
        Dictionary containing aerosol species arrays with shape (nlev_source, nlat_source, nlon_source)
    source_lon : np.ndarray
        Source longitude coordinates in degrees, shape (nlon_source,)
    source_lat : np.ndarray
        Source latitude coordinates in degrees, shape (nlat_source,)
    pressure_source : np.ndarray
        Source pressure levels in Pa, shape (nlev_source,)
    pressure_target : np.ndarray
        Target pressure levels in Pa, shape (nlev_target,)
    target_geolon : np.ndarray
        Target longitude coordinates in degrees, shape (6, ny_target, nx_target)
    target_geolat : np.ndarray
        Target latitude coordinates in degrees, shape (6, ny_target, nx_target)
    vertical_method : str, optional
        Vertical extrapolation method: 'constant', 'zero', 'linear' (default: 'constant')
    horizontal_method : str, optional
        Horizontal interpolation method: 'linear', 'nearest', 'cubic' (default: 'linear')

    Returns:
    --------
    dict
        Dictionary with fully interpolated aerosol species arrays
        with shape (6, nlev_target, ny_target, nx_target)
    """

    print(f"Starting complete aerosol interpolation pipeline... [{aerosol_data.keys()}]")
    print(f"Step 1: Vertical interpolation (log-pressure coordinates) using method: {vertical_method}")

    # Step 1: Vertical interpolation
    vertically_interpolated = interpolate_aerosols_vertically(
        aerosol_data,
        pressure_source,
        pressure_target,
        extrapolate_method=vertical_method,
    )

    print(f"Step 2: Horizontal interpolation to cubed-sphere tiles [{target_geolon.shape}, {target_geolat.shape}]")

    # Step 2: Horizontal interpolation
    fully_interpolated = interpolate_aerosols_horizontally(
        vertically_interpolated,
        source_lon,
        source_lat,
        target_geolon,
        target_geolat,
        method=horizontal_method,
        fill_value=0.0,  # Use default fill value from RegularGridInterpolator
    )

    print(f"Complete interpolation pipeline finished successfully! [{fully_interpolated.keys()}]")

    return fully_interpolated
