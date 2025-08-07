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
Usage: aqm_data_sync_cli.py [OPTIONS] COMMAND [ARGS]...

┌─ Commands ───────────────────────────────────────────────────────────────────────────────────────┐
│ time-varying   Download time varying input data for UFS-AQM.                                     │
│ srw-fixed      Download SRW fixed data.                                                          │
└──────────────────────────────────────────────────────────────────────────────────────────────────┘

 Usage: aqm_data_sync_cli.py time-varying [OPTIONS]

 Download time varying input data for UFS-AQM.

┌─ Options ────────────────────────────────────────────────────────────────────────────────────────┐
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
│    --use-case                       [UNDEFINED|AEROMMA]  Use case. [default: UNDEFINED]          │
│    --max-concurrent-requests        INTEGER              Maximum number of concurrent requests.  │
│                                                          [default: 5]                            │
│    --dry-run                                             Dry run. Nothing will be materially     │
│                                                          synchronized.                           │
│    --snippet                                             If provided, download data for a single │
│                                                          forecast cycle loop (e.g. one day).     │
│    --help                                                Show this message and exit.             │
└──────────────────────────────────────────────────────────────────────────────────────────────────┘

 Usage: aqm_data_sync_cli.py srw-fixed [OPTIONS]

 Download SRW fixed data.

┌─ Options ────────────────────────────────────────────────────────────────────────────────────────┐
│ *  --dst-dir                        PATH     Destination directory for sync. [default: None]     │
│                                              [required]                                          │
│    --max-concurrent-requests        INTEGER  Maximum number of concurrent requests. [default: 5] │
│    --dry-run                                 Dry run. Nothing will be materially synchronized.   │
│    --help                                    Show this message and exit.                         │
└──────────────────────────────────────────────────────────────────────────────────────────────────┘
```