import os
from pathlib import Path

import typer

from aqm_data_sync.core import UseCaseKey, Context, UseCase, S3SyncRunner

os.environ["NO_COLOR"] = "1"
app = typer.Typer(pretty_exceptions_enable=False)


@app.command()
def main(
    dst_dir: Path = typer.Option(..., "--dst-dir", help="Destination directory for sync."),
    first_cycle_date: str = typer.Option(
        None,
        "--first-cycle-date",
        help="First cycle date in yyyymmdd format. Required if --use-case is not provided.",
    ),
    fcst_hr: int = typer.Option(0, "--fcst-hr", help="Forecast hour."),
    last_cycle_date: str = typer.Option(
        None,
        "--last-cycle-date",
        help="Last cycle date in yyyymmdd format. If not provided, defaults to 24 hours after --first-cycle-date.",
    ),
    s3_root: str = typer.Option("s3://noaa-ufs-srw-pds/UFS-AQM", "--s3-root", help="S3 root path."),
    max_concurrent_requests: int = typer.Option(
        3, "--max-concurrent-requests", help="Max concurrent requests."
    ),
    dry_run: bool = typer.Option(False, "--dry-run", help="Dry run."),
    use_case: UseCaseKey = typer.Option(UseCaseKey.UNDEFINED, "--use-case", help="Use case."),
    snippet: bool = typer.Option(
        False,
        "--snippet",
        help="If provided, download data for a single forecast cycle loop (e.g. one day).",
    ),
) -> None:
    kwds = dict(
        first_cycle_date=first_cycle_date,
        dst_dir=dst_dir,
        fcst_hr=fcst_hr,
        last_cycle_date=last_cycle_date,
        s3_root=s3_root,
        max_concurrent_requests=max_concurrent_requests,
        dry_run=dry_run,
        snippet=snippet,
    )
    if use_case == UseCaseKey.UNDEFINED:
        ctx = Context(**kwds)
    else:
        ctx = UseCase.from_key(use_case, **kwds)
    runner = S3SyncRunner(ctx)
    runner.run()


if __name__ == "__main__":
    app()
