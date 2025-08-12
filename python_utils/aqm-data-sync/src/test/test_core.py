from pathlib import Path

from aqm_data_sync.core import (
    TimeVaryingContext,
    TimeVaryingSyncRunner,
    UseCase,
    UseCaseKey,
    UseCaseAeromma,
    SRWFixedContext,
    SRWFixedSyncRunner,
)


class TestTimeVaryingSyncRunner:

    def test_happy_path(self, tmp_path: Path) -> None:
        """Test a dry run with a single forecast date."""
        first_cycle_date = "2023060112"
        ctx = TimeVaryingContext.model_validate(
            dict(first_cycle_date=first_cycle_date, dst_dir=tmp_path, dry_run=True)
        )
        runner = TimeVaryingSyncRunner(ctx)
        runner.run()

    def test_create_sync_command(self, tmp_path: Path) -> None:
        """Test an exact match for the AWS S3 sync command."""
        first_cycle_date = "2023060112"
        last_cycle_date = "2023060212"
        dst_dir = tmp_path / "output-for-this-test"
        ctx = TimeVaryingContext.model_validate(
            dict(
                first_cycle_date=first_cycle_date,
                last_cycle_date=last_cycle_date,
                dst_dir=dst_dir,
                dry_run=True,
            )
        )
        runner = TimeVaryingSyncRunner(ctx)
        actual = runner._create_sync_cmd_()
        expected = (
            "aws",
            "s3",
            "sync",
            "--no-sign-request",
            "--dryrun",
            "--exclude",
            "*",
            "--include",
            "GFS_SFC_DATA/gfs.20230601/12/atmos/gfs.t12z.sfcanl.nc",
            "--include",
            "FV3GFS/gfs.20230601/12/atmos/gfs.t12z.atmanl.nc",
            "--include",
            "RAVE_fire/20230601/*.nc",
            "--include",
            "FV3GFS/gfs.20230601/12/atmos/gfs.t12z.atmf000.nc",
            "--include",
            "FV3GFS/gfs.20230601/12/atmos/gfs.t12z.sfcf000.nc",
            "--include",
            "GFS_SFC_DATA/gfs.20230601/12/atmos/gfs.t12z.sfcf000.nc",
            "--include",
            "FV3GFS/gfs.20230601/12/atmos/gfs.t12z.atmf006.nc",
            "--include",
            "FV3GFS/gfs.20230601/12/atmos/gfs.t12z.sfcf006.nc",
            "--include",
            "GFS_SFC_DATA/gfs.20230601/12/atmos/gfs.t12z.sfcf006.nc",
            "--include",
            "FV3GFS/gfs.20230601/12/atmos/gfs.t12z.atmf012.nc",
            "--include",
            "FV3GFS/gfs.20230601/12/atmos/gfs.t12z.sfcf012.nc",
            "--include",
            "GFS_SFC_DATA/gfs.20230601/12/atmos/gfs.t12z.sfcf012.nc",
            "--include",
            "FV3GFS/gfs.20230601/12/atmos/gfs.t12z.atmf018.nc",
            "--include",
            "FV3GFS/gfs.20230601/12/atmos/gfs.t12z.sfcf018.nc",
            "--include",
            "GFS_SFC_DATA/gfs.20230601/12/atmos/gfs.t12z.sfcf018.nc",
            "--include",
            "FV3GFS/gfs.20230601/12/atmos/gfs.t12z.atmf024.nc",
            "--include",
            "FV3GFS/gfs.20230601/12/atmos/gfs.t12z.sfcf024.nc",
            "--include",
            "GFS_SFC_DATA/gfs.20230601/12/atmos/gfs.t12z.sfcf024.nc",
            "--include",
            "GFS_SFC_DATA/gfs.20230601/12/atmos/gfs.t12z.sfcf003.nc",
            "--include",
            "GFS_SFC_DATA/gfs.20230601/12/atmos/gfs.t12z.sfcf009.nc",
            "--include",
            "GFS_SFC_DATA/gfs.20230601/12/atmos/gfs.t12z.sfcf015.nc",
            "--include",
            "GFS_SFC_DATA/gfs.20230601/12/atmos/gfs.t12z.sfcf021.nc",
            "--include",
            "GEFS_Aerosol/20230601/00/gfs.t00z.atmf000.nemsio",
            "--include",
            "GEFS_Aerosol/20230601/00/gfs.t00z.atmf006.nemsio",
            "--include",
            "GEFS_Aerosol/20230601/00/gfs.t00z.atmf012.nemsio",
            "--include",
            "GEFS_Aerosol/20230601/00/gfs.t00z.atmf018.nemsio",
            "--include",
            "GEFS_Aerosol/20230601/00/gfs.t00z.atmf024.nemsio",
            "--include",
            "GEFS_Aerosol/20230601/00/gfs.t00z.atmf030.nemsio",
            "--include",
            "GEFS_Aerosol/20230601/00/gfs.t00z.atmf036.nemsio",
            "--include",
            "RESTART/*20230531*",
            "--include",
            "GFS_SFC_DATA/gfs.20230602/12/atmos/gfs.t12z.sfcanl.nc",
            "--include",
            "FV3GFS/gfs.20230602/12/atmos/gfs.t12z.atmanl.nc",
            "--include",
            "RAVE_fire/20230602/*.nc",
            "--include",
            "FV3GFS/gfs.20230602/12/atmos/gfs.t12z.atmf000.nc",
            "--include",
            "FV3GFS/gfs.20230602/12/atmos/gfs.t12z.sfcf000.nc",
            "--include",
            "GFS_SFC_DATA/gfs.20230602/12/atmos/gfs.t12z.sfcf000.nc",
            "--include",
            "FV3GFS/gfs.20230602/12/atmos/gfs.t12z.atmf006.nc",
            "--include",
            "FV3GFS/gfs.20230602/12/atmos/gfs.t12z.sfcf006.nc",
            "--include",
            "GFS_SFC_DATA/gfs.20230602/12/atmos/gfs.t12z.sfcf006.nc",
            "--include",
            "FV3GFS/gfs.20230602/12/atmos/gfs.t12z.atmf012.nc",
            "--include",
            "FV3GFS/gfs.20230602/12/atmos/gfs.t12z.sfcf012.nc",
            "--include",
            "GFS_SFC_DATA/gfs.20230602/12/atmos/gfs.t12z.sfcf012.nc",
            "--include",
            "FV3GFS/gfs.20230602/12/atmos/gfs.t12z.atmf018.nc",
            "--include",
            "FV3GFS/gfs.20230602/12/atmos/gfs.t12z.sfcf018.nc",
            "--include",
            "GFS_SFC_DATA/gfs.20230602/12/atmos/gfs.t12z.sfcf018.nc",
            "--include",
            "FV3GFS/gfs.20230602/12/atmos/gfs.t12z.atmf024.nc",
            "--include",
            "FV3GFS/gfs.20230602/12/atmos/gfs.t12z.sfcf024.nc",
            "--include",
            "GFS_SFC_DATA/gfs.20230602/12/atmos/gfs.t12z.sfcf024.nc",
            "--include",
            "GFS_SFC_DATA/gfs.20230602/12/atmos/gfs.t12z.sfcf003.nc",
            "--include",
            "GFS_SFC_DATA/gfs.20230602/12/atmos/gfs.t12z.sfcf009.nc",
            "--include",
            "GFS_SFC_DATA/gfs.20230602/12/atmos/gfs.t12z.sfcf015.nc",
            "--include",
            "GFS_SFC_DATA/gfs.20230602/12/atmos/gfs.t12z.sfcf021.nc",
            "--include",
            "GEFS_Aerosol/20230602/00/gfs.t00z.atmf000.nemsio",
            "--include",
            "GEFS_Aerosol/20230602/00/gfs.t00z.atmf006.nemsio",
            "--include",
            "GEFS_Aerosol/20230602/00/gfs.t00z.atmf012.nemsio",
            "--include",
            "GEFS_Aerosol/20230602/00/gfs.t00z.atmf018.nemsio",
            "--include",
            "GEFS_Aerosol/20230602/00/gfs.t00z.atmf024.nemsio",
            "--include",
            "GEFS_Aerosol/20230602/00/gfs.t00z.atmf030.nemsio",
            "--include",
            "GEFS_Aerosol/20230602/00/gfs.t00z.atmf036.nemsio",
            "s3://noaa-ufs-srw-pds/UFS-AQM",
            str(dst_dir),
        )
        try:
            assert actual == expected
        except AssertionError:
            print(f"{actual=}")
            diff = set(actual).symmetric_difference(set(expected))
            print(f"{diff=}")
            raise


class TestUseCase:

    def test_from_key(self, tmp_path: Path) -> None:
        """Test creating a use case context object."""
        use_case = UseCase.from_key(UseCaseKey.AEROMMA, dst_dir=tmp_path)
        print(use_case)
        assert isinstance(use_case, UseCaseAeromma)


class TestSRWFixedSyncRunner:

    def test_create_sync_command(self, tmp_path: Path) -> None:
        """Test an exact match for the AWS S3 sync command."""
        dst_dir = tmp_path / "output-for-this-test"
        ctx = SRWFixedContext(
            dst_dir=dst_dir,
            dry_run=True,
        )
        runner = SRWFixedSyncRunner(ctx)
        actual = runner._create_sync_cmd_()
        expected = (
            "aws",
            "s3",
            "sync",
            "--no-sign-request",
            "--dryrun",
            "--exclude",
            "*",
            "--include",
            "fix/*",
            "--include",
            "NaturalEarth/*",
            "s3://noaa-ufs-srw-pds/develop-20250702",
            str(dst_dir),
        )
        try:
            assert actual == expected
        except AssertionError:
            print(f"{actual=}")
            diff = set(actual).symmetric_difference(set(expected))
            print(f"{diff=}")
            raise
