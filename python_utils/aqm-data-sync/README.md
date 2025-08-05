# aqm-data-sync

A command line utility to synchronize AQM datasets. Currently, tuned to specialized use cases related to Short-Range Weather App use cases. Only S3 download is supported.

# Installation

```shell
git clone -b develop https://github.com/NOAA-EMC/AQM-utils.git
cd AQM-utils/python_utils/aqm-data-sync
conda env create -f environment.yml
conda run -n aqm-data-sync pip install .
```

# Testing

```shell
pytest test
```

# Usage

```shell
conda run -n aqm-data-sync aqm-data-sync --help

Usage: aqm-data-sync [OPTIONS]

╭─ Options ────────────────────────────────────────────────────────────────────────────────────────╮
│ *  --dst-dir                        PATH                 Destination directory for sync.         │
│                                                          [default: None]                         │
│                                                          [required]                              │
│    --first-cycle-date               TEXT                 First cycle date in yyyymmdd format.    │
│                                                          Required if --use-case is not provided. │
│                                                          [default: None]                         │
│    --fcst-hr                        INTEGER              Forecast hour. [default: 0]             │
│    --last-cycle-date                TEXT                 Last cycle date in yyyymmdd format. If  │
│                                                          not provided, defaults to 24 hours      │
│                                                          after --first-cycle-date.               │
│                                                          [default: None]                         │
│    --s3-root                        TEXT                 S3 root path.                           │
│                                                          [default:                               │
│                                                          s3://noaa-ufs-srw-pds/UFS-AQM]          │
│    --max-concurrent-requests        INTEGER              Max concurrent requests. [default: 3]   │
│    --dry-run                                             Dry run.                                │
│    --use-case                       [UNDEFINED|AEROMMA]  Use case. [default: UNDEFINED]          │
│    --snippet                                             If provided, download data for a single │
│                                                          forecast cycle loop (e.g. one day).     │
│    --install-completion                                  Install completion for the current      │
│                                                          shell.                                  │
│    --show-completion                                     Show completion for the current shell,  │
│                                                          to copy it or customize the             │
│                                                          installation.                           │
│    --help                                                Show this message and exit.             │
╰──────────────────────────────────────────────────────────────────────────────────────────────────╯
```