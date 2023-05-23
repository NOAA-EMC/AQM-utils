#!/usr/bin/env python3
"""
Merge NEI stack and inline point sources for each sector
"""

import logging
import sys
from datetime import datetime, timedelta
from functools import total_ordering
from pathlib import Path

import netCDF4 as nc
import numpy as np

#
# Constants
#

logging.basicConfig(stream=sys.stdout)
log = logging.getLogger("stack-pt-merge")
log.setLevel(logging.WARNING)

REF_YEAR = 2016

POINT_DIM_NAME = "nlocs"

NSTEP_DEFAULT = 73  # We want a total of 73 time steps: hour 0 to hour 72

INPUT_DIR_DEFAULT = Path("/scratch/zmoon/stack-pt/combined-input-test")  # Hopper

SECTORS_CONUS = ["ptegu", "ptnonipm", "pt_oilgas", "cmv_c1c2_12", "cmv_c3_12", "othpt"]
# Example file names:
#   daily    -- inln_mole_cmv_c1c2_12_20161207_12us1_cmaq_cb6_2016fh_16j.ncf
#   only one -- stack_groups_cmv_c1c2_12_12us1_2016fh_16j.ncf
#
# File counts:
#   cmv_c1c2_12 - 377
#   cmv_c3_12 - 377
#   othpt - 49
#   ptagfire - 730
#   ptegu - 377
#   ptfire - 732
#   ptfire_othna - 731
#   ptnonipm - 66
#   pt_oilgas - 66
#
# Upshot of file counts:
# - daily: ptegu, cmv_c1c2_12, cmv_c3_12
# - four days per month: othpt
# - four days per month + holidays: ptnonipm, pt_oilgas
# (They include 1227 as holiday but we do not, so 17 intead of 16 holidays)

SECTORS_HI = ["ptegu", "ptnonipm", "cmv_c1c2_3hi1", "cmv_c3_3hi1"]
# Example file names:
#   daily    -- inln_mole_ptegu_20161213_3HI1_cmaq_cb6_2016fh_16j.ncf
#   only one -- stack_groups_ptegu_3HI1_2016fh_16j.ncf

SECTORS_AK = ["ptegu", "ptnonipm", "pt_oilgas", "cmv_c1c2_9ak1", "cmv_c3_9ak1", "othpt"]
# Example file names:
#            -- inln_mole_ptegu_20161209_9AK1_cmaq_cb6_2016fh_16j.ncf
#            -- stack_groups_ptegu_9AK1_2016fh_16j.ncf

HOLIDAY_MD = {
    #   : "0101 0102 XXXX XXXX XXXX XXXX 0704 0705 XXXX XXXX XXXX XXXX XXXX 1224 1225 1226".split(),
    2015: "0101 0102 0403 0404 0525 0526 0704 0705 0907 0908 1125 1126 1127 1224 1225 1226".split(),
    2016: "0101 0102 0325 0326 0530 0531 0704 0705 0905 0906 1123 1124 1125 1224 1225 1226".split(),
    2017: "0101 0102 0414 0415 0529 0530 0704 0705 0904 0905 1122 1123 1124 1224 1225 1226".split(),
    2018: "0101 0102 0330 0331 0528 0529 0704 0705 0903 0904 1121 1122 1123 1224 1225 1226".split(),
    2019: "0101 0102 0419 0420 0527 0528 0704 0705 0902 0903 1127 1128 1129 1224 1225 1226".split(),
    2020: "0101 0102 0410 0411 0525 0526 0704 0705 0907 0908 1125 1126 1127 1224 1225 1226".split(),
    2021: "0101 0102 0402 0403 0531 0601 0704 0705 0906 0907 1124 1125 1126 1224 1225 1226".split(),
    2022: "0101 0102 0415 0416 0530 0531 0704 0705 0905 0906 1123 1124 1125 1224 1225 1226".split(),
    2023: "0101 0102 0407 0408 0529 0530 0704 0705 0904 0905 1122 1123 1124 1224 1225 1226".split(),
    2024: "0101 0102 0329 0330 0527 0528 0704 0705 0902 0903 1127 1128 1129 1224 1225 1226".split(),
    2025: "0101 0102 0418 0419 0526 0527 0704 0705 0901 0902 1126 1127 1128 1224 1225 1226".split(),
    2026: "0101 0102 0403 0404 0525 0526 0704 0705 0907 0908 1125 1126 1127 1224 1225 1226".split(),
}
# Holidays + day after:
# - New Years
# - Good Friday *
# - Memorial Day *
# - Independence day
# - Labor Day *
# Holidays + day before and after:
# - Thanksgiving *
# - Xmas
#
# 2018--2022 extracted from Youhua's ksh merge script, except
# - Labor Day corrected from 0902 to 0903 for 2018
# - Thanksgiving corrected from 1129 to 1128 for 2019
#   and from 1129 to 1122 for 2018
# TODO: generate and check the values?


@total_ordering
class Date:
    """Date/time with weekend and holiday info. Wraps a datetime.datetime instance."""

    def __init__(self, dt):
        """
        Parameters
        ----------
        dt : datetime.datetime or str
            If str, should be YYYYMMDD or YYYYMMDDHH format (any ``-`` or ``_`` will ignored).
        """
        if isinstance(dt, str):
            dt_ = dt.replace("-", "").replace("_", "").strip()
            if len(dt_) == 8:
                fmt = f"%Y%m%d"
            elif len(dt_) == 10:
                fmt = f"%Y%m%d%H"
            else:
                raise ValueError(f"{dt!r} has unsupported datetime format. Use YYYYMMDD[HH].")
            try:
                dt_ = datetime.strptime(dt_, fmt)
            except ValueError as e:
                raise ValueError(
                    f"{dt_!r} (from input {dt!r}) failed to parse as a {fmt} datetime"
                ) from e
        else:
            dt_ = dt

        if dt_.year not in HOLIDAY_MD:
            raise ValueError(f"Year {dt_.year} not supported.")

        if dt_.second != 0 or dt_.minute != 0 or dt_.microsecond != 0:
            raise ValueError(f"Second, minute, microsecond should all be 0, but parsed dt is {dt_}")

        self.dt = dt_
        """Associated datetime.datetime instance."""

    @property
    def dow(self):
        """Integer day-of-week (ISO weekday, 1 is Monday and 7 is Sunday)."""
        return self.dt.isoweekday()

    @property
    def woy(self):
        """Integer week-of-year (ISO)."""
        return self.dt.isocalendar()[1]

    @property
    def is_weekday(self):
        return self.dow in range(1, 6)

    @property
    def is_weekend(self):
        return self.dow in {6, 7}

    @property
    def is_holiday(self):
        md = self.dt.strftime(r"%m%d")
        return md in HOLIDAY_MD[self.dt.year]

    @property
    def _iholiday(self):
        """Index of holiday in the holiday array."""
        md = self.dt.strftime(r"%m%d")
        if self.is_holiday:
            return HOLIDAY_MD[self.dt.year].index(md)
        else:
            return None

    def replace(self, *args, **kwargs):
        """Return a *new instance* created using datetime.datetime.replace()."""
        return type(self)(self.dt.replace(*args, **kwargs))

    def add(self, *args, **kwargs):
        """Return a *new instance* by adding timedelta(*args, **kwargs)
        to the current datetime.datetime instance.
        """
        return type(self)(self.dt + timedelta(*args, **kwargs))

    def __repr__(self):
        return (
            f"{type(self).__name__}(\n"
            f"  dt={self.dt},\n"
            f"  dow={self.dow},\n"
            f"  woy={self.woy},\n"
            f"  is_weekday={self.is_weekday},\n"
            f"  is_weekend={self.is_weekend},\n"
            f"  is_holiday={self.is_holiday},\n"
            ")"
        )

    def __str__(self):
        return self.dt.strftime(r"%Y-%m-%d_%H")

    def __hash__(self):
        return hash(self.dt)

    def __eq__(self, other):
        if type(other) is type(self):
            return self.dt == other.dt
        else:
            return NotImplemented

    def __lt__(self, other):
        if type(other) is type(self):
            return self.dt < other.dt
        else:
            return NotImplemented


class SectorFiles:
    """Mapping of Date to Paths for a given sector directory."""

    _ref_year = REF_YEAR
    """Year in the emissions data."""

    def __init__(self, directory, sector):
        self.dir_ = Path(directory)
        self.sector = sector
        self.id_ = self.sector

        ps = sorted(self.dir_.glob(f"{self.sector}_????????.nc"))
        assert len(ps) >= 1
        self.fps = {Date(p.stem.split("_")[1]): p for p in ps}
        self.n_fps = len(self.fps)

    def __repr__(self):
        return (
            f"{type(self).__name__}(\n"
            f"  sector={self.sector!r},\n"
            f"  dir_={self.dir_},\n"
            f"  fps={{...}},\n"
            f"  n_fps={self.n_fps},\n"
            ")"
        )

    def find_closest_fp(self, target):
        """Return (matched Date, associated file path).

        Parameters
        ----------
        target : Date
        """
        log.debug(f"target={target}")

        # Extract data for the target's month
        dates_m = []
        fps_m = []
        for d, fp in self.fps.items():
            if d.dt.year == self._ref_year and d.dt.month == target.dt.month:
                # Note skipping 2015 data
                dates_m.append(d)
                fps_m.append(fp)

        # Data with holidays filtered out
        dates_m_nh = []
        fps_m_nh = []
        for d, fp in zip(dates_m, fps_m):
            if not d.is_holiday:
                dates_m_nh.append(d)
                fps_m_nh.append(fp)

        if len(dates_m_nh) == 5:
            # Some such ptnonipm have 2 days after Thanksgiving (2016-11-26) and Christmas (2016-12-27).
            # We aren't treating these as holidays, so just drop for now.
            # TODO: check that the one we are dropping here is indeed one of those two?

            s_dates = "\n".join(f"- {d}" for d in dates_m_nh)
            log.warning(
                f"dropping the last of these non-holiday dates in order to have 4 only:\n{s_dates}"
            )
            dates_m_nh = dates_m_nh[:-1]
            fps_m_nh = fps_m_nh[:-1]

        # Target date info
        iwd_t = target.dow
        iw_t = target.woy
        iw_rel_t = (iw_t - target.replace(day=1).woy) % 53
        log.debug(f"target iw={iw_t}, iw_rel={iw_rel_t}")

        d_r = fp_r = None  # `r` for reference
        if target.is_holiday:
            log.debug(f"target is a holiday")
            desired_md = HOLIDAY_MD[self._ref_year][target._iholiday]
            d = Date(f"{self._ref_year}{desired_md}")
            if d in self.fps:
                # Four days per month + holidays OR daily files
                d_r = d
                fp_r = self.fps[d]
            else:
                if len(dates_m) > 4:
                    # e.g. day before Thanksgiving, which we consider holiday,
                    # but ptnonipm (e.g.) has Thanksgiving and the two days after
                    s_dates = "\n".join(f"- {d}" for d in dates_m)
                    log.warning(
                        "for holiday without specific file, "
                        f"dropping the last of these dates in order to have 4 only:\n{s_dates}"
                    )
                    dates_m = dates_m[:4]
                    fps_m = fps_m[:4]

                # Only four days per month (no holidays)
                # (seems to be Mon, Tue, Sat, Sun)
                assert len(dates_m) == 4
                iwds_r = [d.dow for d in dates_m]
                assert len(set(iwds_r)) == len(iwds_r)
                assert set(iwds_r) == {1, 2, 6, 7}

                if target.is_weekend:
                    # Use weekend day
                    i = iwds_r.index(iwd_t)
                else:
                    # Use Saturday to approximate holiday
                    i = iwds_r.index(6)
                d_r = dates_m[i]
                fp_r = fps_m[i]

        else:
            log.debug(f"target is *not* a holiday")
            # If target is not a holiday, we don't want to match to a holiday
            if len(dates_m_nh) > 25:
                # Assume daily
                # Look for closest relative week-of-year with matching day-of-week

                iws_r = [d.woy for d in dates_m_nh]
                iws_rel_r = [(d.woy - dates_m_nh[0].replace(day=1).woy) % 53 for d in dates_m_nh]
                log.debug(f"ref iw={iws_r}")
                log.debug(f"ref iw_rel={iws_rel_r}")

                # Try all relative weeks in this month,
                # searching from target relative week outward.
                # The way the sorting works, the deltas are 0, -1, +1, -2, +2, ...
                best = None
                for iw_rel in sorted(set(iws_rel_r), key=lambda x: abs(x - iw_rel_t)):
                    log.debug(f"trying iw_rel={iw_rel}")
                    # TODO: simplify from here a bit
                    inds = [i for i, iw_rel_r_i in enumerate(iws_rel_r) if iw_rel_r_i == iw_rel]
                    iwds_r = [dates_m_nh[i].dow for i in inds]
                    if iwd_t in iwds_r:
                        best = iws_rel_r.index(iw_rel) + iwds_r.index(iwd_t)
                        log.debug(
                            f"match: ind={best}, iw_r={iws_r[best]}, iw_rel_r={iws_rel_r[best]}"
                        )
                        break
                else:
                    raise Exception(f"Failed to find good match for {target}.")

                assert best is not None
                d_r = dates_m_nh[best]
                fp_r = fps_m_nh[best]

            elif len(dates_m_nh) == 4:
                # Four days per month
                iwds_r = [d.dow for d in dates_m_nh]
                assert set(iwds_r) == {1, 2, 6, 7}
                if target.is_weekend:
                    # Use weekend day
                    i = iwds_r.index(iwd_t)
                else:
                    # Use Mon for Mon or Fri
                    # and Tue for Tue--Thu
                    if iwd_t in {1, 5}:
                        i = iwds_r.index(1)
                    else:
                        i = iwds_r.index(2)
                d_r = dates_m_nh[i]
                fp_r = fps_m_nh[i]

            else:
                s_fps = "\n".join(
                    f"- {date} {fp.as_posix()}" for date, fp in zip(dates_m_nh, fps_m_nh)
                )
                raise Exception(
                    f"Unexpected len-{len(fps_m_nh)} file set for target {target}:\n{s_fps}"
                )

        assert d_r is not None and fp_r is not None

        return (d_r, fp_r)


def print_heading(s, *, ol_char="=", ul_char="-"):
    """Print string under- and over-lined."""
    s = s.strip()
    assert "\n" not in s
    n = len(s)
    ol = ol_char * n
    ul = ul_char * n
    print(f"{ol}\n{s}\n{ul}")


#
# Main function and CLI
#


def main(
    date_str,
    *,
    nstep=NSTEP_DEFAULT,
    logger_info=False,
    logger_debug=False,
    stack_groups_only=False,
    input_dir=INPUT_DIR_DEFAULT,
):
    # Adjust logger settings
    if logger_info:
        log.setLevel(logging.INFO)
    if logger_debug:
        log.setLevel(logging.DEBUG)

    date_start = Date(date_str)
    date_final = date_start.add(hours=nstep - 1)
    ndays = date_final.dt.toordinal() - date_start.dt.toordinal() + 1
    pre = "pt" if not stack_groups_only else "sg"
    ofn = f"{pre}-{str(date_start).replace('-', '').replace('_', '')}.nc"

    print_heading("Info")
    print(f"Start time: {date_start}")
    print(f"Number of hourly time steps desired: {nstep} -> {ndays} unique day(s)")
    print(f"Final time: {date_final}")
    print(f"Using {REF_YEAR} point emissions data")
    print(f"Output filename: {ofn}")
    print(f"Input directory: {input_dir.resolve(strict=True).as_posix()}")

    def iter_days():
        """Yield floored Date objects and associated time slices for the daily files."""
        # Loop over days
        floored_date = date_start.replace(hour=0)
        iday = 0
        while iday < ndays:
            # Compute time slice
            a = 0
            b = 24
            if floored_date.dt.date() == date_start.dt.date():
                a = date_start.dt.hour
            if floored_date.dt.date() == date_final.dt.date():
                b = date_final.dt.hour + 1
            time_slice = slice(a, b)

            yield floored_date, time_slice

            # Increment day
            floored_date = floored_date.add(days=1)
            iday += 1

    # Create SectorFiles instances
    secs = []
    for sec_group in ["d", "4pm", "4pmh"]:
        log.debug(f"loading {sec_group} under {input_dir}")
        secs.append(SectorFiles(input_dir, sec_group))

    assert len(secs) == 3

    # Determine length of points dim and collect file paths
    print_heading("Determining total length of points dim ...")
    npoint_tot = 0
    data = []
    for sec in secs:
        log.info(sec.id_)

        paths_this_sec = []
        npoint_this_sec = []
        time_slices_this_sec = []
        for floored_date, time_slice_in in iter_days():
            log.info(floored_date)

            date_r, p = sec.find_closest_fp(floored_date)
            log.debug(f"{floored_date} -> {date_r} for sector {sec.id_}")

            paths_this_sec.append(p)
            time_slices_this_sec.append(time_slice_in)

            ds = nc.Dataset(p, "r")

            npoint = ds.dimensions["point"].size
            npoint_this_sec.append(npoint)
            log.info(f"{npoint} points")

            ds.close()

        assert len(set(npoint_this_sec)) == 1, f"should all be same but we have {npoint_this_sec}"
        data.append((paths_this_sec, time_slices_this_sec))
        npoint_tot += npoint

    print(f"{npoint_tot} total points")

    # Create output file
    print_heading("Creating output file ...")
    ds_out = nc.Dataset(ofn, "w")
    _ = ds_out.createDimension(POINT_DIM_NAME, npoint_tot)
    ntime = nstep
    tstep = 1  # hourly

    if not stack_groups_only:
        _ = ds_out.createDimension("time", None)  # unlimited for ntime
        time_coord = ds_out.createVariable("TIME", np.int32, ("time",))
        time_coord.units = "hours"
        time_coord.description = f"Time step (hours since {date_start})"
        time_coord[:] = np.arange(0, ntime * tstep, tstep)
    else:
        _ = ds_out.createDimension("nchar", 100)

    itime_start = 0
    for k in range(ndays):
        # Inner loop is over sectors, to fill the full point dim
        ipoint_start = 0
        for tup in data:
            p = tup[0][k]
            time_slice_in = tup[1][k]

            sg_id = p.stem
            print(f"loading {sg_id}")

            ds_in = nc.Dataset(p)

            # Number of points for this sector
            # Note: checked for consistency with b above
            npoint = ds_in.dimensions["point"].size

            # Check that time in the is what we presume
            dtstr = str(ds_in.SDATE * 100 + ds_in.STIME)  # these are ds attrs
            _ = datetime.strptime(dtstr, "%Y%j%H")
            tstep = int(ds_in.TSTEP / 10000)
            # ^ this is a ds attr; I guess units of hours * 10000 ...
            assert tstep == 1, f"these should be hourly files but tstep={tstep}"
            assert ds_in.STIME == 0, f"files should at start at hour 0 but STIME={ds_in.stime}"
            ntime = ds_in.dimensions["tstep"].size  # but also a dimension
            assert ntime == 25, f"expected 25 times but ntime={ntime}"

            # Define slices for the output file
            point_slice = slice(ipoint_start, ipoint_start + npoint)
            ntime_slice = time_slice_in.stop - time_slice_in.start
            time_slice = slice(itime_start, itime_start + ntime_slice)

            # Add variables from stack_groups file (all only vary in point dim)
            sg_vns = [
                "LATITUDE",
                "LONGITUDE",
                "STKDM",
                "STKHT",
                "STKTK",
                "STKVE",
                "STKFLW",
                "STKCNT",
            ]
            for vn in sg_vns:
                v_in = ds_in.variables[vn]
                if vn not in ds_out.variables:
                    v_out = ds_out.createVariable(vn, np.float32, (POINT_DIM_NAME,))
                    v_out.units = v_in.units.strip()
                    v_out.description = v_in.description.strip()
                    v_out[:] = 0
                else:
                    v_out = ds_out.variables[vn]
                v_out[point_slice] = v_in[:].data.squeeze()

            if stack_groups_only:
                # Add sector file identifying info
                vn = "SID"
                if vn not in ds_out.variables:
                    v_out = ds_out.createVariable(vn, "S1", (POINT_DIM_NAME, "nchar"))
                    v_out.long_name = "Group ID"
                    v_out.description = (
                        "Sector time group (daily, 4-per-month, or 4-per-month + holidays) "
                        "and reference year date"
                    )
                    v_out[:] = ""
                else:
                    v_out = ds_out.variables[vn]
                x = np.full((100,), "", "S1")
                x[: len(sg_id)] = [np.string_(c) for c in sg_id]
                v_out[point_slice, :] = x

            if not stack_groups_only:
                # Add variables from inln_mole file
                for vn in ds_in.variables:
                    if vn in sg_vns:
                        continue
                    v_in = ds_in.variables[vn]
                    if vn not in ds_out.variables:
                        v_out = ds_out.createVariable(
                            vn,
                            np.float32,
                            ("time", POINT_DIM_NAME),
                            zlib=True,
                            complevel=1,
                        )
                        # Note: `zlib=True` is deprecated in favor of `compression='zlib'`
                        # Note: complevel=4 is default, 0--9 with 9 most compression
                        v_out.units = v_in.units.strip()
                        v_out.description = v_in.description.strip()
                        v_out[:] = 0
                    else:
                        v_out = ds_out.variables[vn]
                    v_out[time_slice, point_slice] = v_in[time_slice_in, :].data.squeeze()

            # Increment (move along point dim in the output file)
            ipoint_start = point_slice.stop
            ds_in.close()

        # Increment (move along time dim " ")
        itime_start = time_slice.stop

    # Quick check
    assert np.isnan(ds_out.variables["LATITUDE"][:]).sum() == 0
    if not stack_groups_only:
        vn = "NO"
        x = np.isnan(ds_out.variables[vn][:])
        f = x.sum() / x.size
        if f > 0:
            log.warning(rf"{f * 100:.2f}% of {vn} values are NaN")

    print_heading("Writing ...")
    ds_out.close()
    # Note: 20G for full file with 73 times and all 16 sectors

    return 0


def parse_args(args=None):
    """Return dict of kwargs to pass to main function."""
    import argparse
    import inspect

    if args is None:
        args = sys.argv[1:]
    log.debug(f"args={args}")

    parser = argparse.ArgumentParser(
        description=(
            "For the stretch of time starting with START and including NSTEP "
            "hourly time steps, create a combined point emissions file based on "
            "data from 16 sectors."
        )
    )
    parser.add_argument(
        "-s",
        "--start",
        type=str,
        help="Start date/time, in YYYYMMDD[HH] format (`-`, `_` ignored).",
        required=True,
    )
    parser.add_argument(
        "-n",
        "--nstep",
        type=int,
        default=NSTEP_DEFAULT,
        help=(
            "Desired number of time steps for the output file (including start). "
            f"(default: {NSTEP_DEFAULT})"
        ),
    )
    parser.add_argument(
        "-i",
        "--input-dir",
        type=Path,
        default=INPUT_DIR_DEFAULT,
        help=(
            "Directory where the compiled sector group files are located. "
            f"(default: {INPUT_DIR_DEFAULT.as_posix()} (GMU Hopper))"
        ),
    )
    parser.add_argument(
        "--stack-groups-only",
        help="'stack_groups' data only.",
        action="store_true",
    )
    parser.add_argument(
        "--info",
        help="Print logger info messages.",
        action="store_true",
    )
    parser.add_argument(
        "--debug",
        help="Print logger debug (and info) messages.",
        action="store_true",
    )

    args = parser.parse_args(args)
    log.debug(f"argparse parsed args={args}")

    kwargs = {
        "date_str": args.start,
        "nstep": args.nstep,
        "input_dir": args.input_dir,
        "stack_groups_only": args.stack_groups_only,
        "logger_info": args.info,
        "logger_debug": args.debug,
    }

    # Check that we covered everything
    assert set(inspect.signature(main).parameters) == set(kwargs)

    return kwargs


if __name__ == "__main__":
    raise SystemExit(main(**parse_args()))
