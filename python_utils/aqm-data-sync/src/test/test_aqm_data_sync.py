import os
import subprocess
from pathlib import Path

from typer.testing import CliRunner

from aqm_data_sync.aqm_data_sync_cli import app
from aqm_data_sync.core import UseCaseKey


def test_help() -> None:
    """Test that the help message can be displayed."""
    os.environ["TERMINAL_WIDTH"] = "100"
    cli_path = Path(__file__).parent.parent / "aqm_data_sync" / "aqm_data_sync_cli.py"
    subprocess.check_call(["python", str(cli_path), "--help"])
    for subcommand in ("time-varying", "srw-fixed"):
        subprocess.check_call(["python", str(cli_path), subcommand, "--help"])


def test_time_varying_use_case(tmp_path: Path) -> None:
    """Test the use case pathway for a snippet."""
    runner = CliRunner()

    args = [
        "time-varying",
        "--use-case",
        UseCaseKey.AEROMMA.value,
        "--dst-dir",
        str(tmp_path),
        "--dry-run",
        "--snippet",
    ]
    result = runner.invoke(app, args, catch_exceptions=False)
    print(result.output)
    assert result.exit_code == 0
