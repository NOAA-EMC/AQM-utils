import numpy as np
import pytest
import xarray as xr


def mock_regrid_transformation(da: xr.DataArray) -> xr.DataArray:
    """
    Mock transformation that behaves like a regridding scaling.
    Ensures backend-agnostic behavior.

    Parameters
    ----------
    da : xr.DataArray
        Input DataArray (Eager or Lazy).

    Returns
    -------
    xr.DataArray
        Transformed DataArray.
    """
    # Use xarray operations which are backend-agnostic
    result = da * 1.0e-6 / 3600
    if "history" in da.attrs:
        result.attrs["history"] = da.attrs["history"] + "; applied scaling"
    else:
        result.attrs["history"] = "applied scaling"
    return result


@pytest.mark.parametrize("use_dask", [False, True])
def test_aero_scaling_logic(use_dask):
    """
    Verify the scaling logic works identically for NumPy and Dask backends.
    Following Aero Protocol Step 2.
    """
    # Create random data
    rng = np.random.default_rng()
    data = rng.random((24, 100, 100))
    da = xr.DataArray(
        data,
        dims=["time", "lat", "lon"],
        name="test_var",
        attrs={"units": "kg", "history": "initial data"},
    )

    if use_dask:
        # Convert to Dask-backed DataArray
        da = da.chunk({"time": 1, "lat": 50, "lon": 50})

    # Apply transformation
    result = mock_regrid_transformation(da)

    # Assertions
    if use_dask:
        # Verify it's still a dask array (lazy)
        assert result.chunks is not None
        # Compute for verification
        result_val = result.compute().values
    else:
        # Verify it's a numpy array (eager)
        assert result.chunks is None
        result_val = result.values

    # Verify values are identical
    expected = data * 1.0e-6 / 3600
    np.testing.assert_allclose(result_val, expected)

    # Verify metadata preservation
    assert result.attrs["history"].endswith("applied scaling")
