#!/bin/bash

#-----------------------------------------------------------------------------
#
# Batch launch for convert_airnow_csv.f90.
#
# Usage:
#
# 1.  Create output directories in advance, such as airnow/netcdf/2023.
#
# 2.  Set INPATH and OUTPATH below, for the desired file name patterns.
#     Paths may be relative or absolute.
#
# 3.  Go to the desired run directory, such as the following.
#     Directory must contain the executable and the setup script.
#
#     cd /home/Dave.Allured/airnow/convert/0205
#
# 4.  sbatch convert.airnow.job YYYYMMDD1 YYYYMMDD2
#
# Converts Airnow CSV hourly files to Netcdf daily files for
# one or more dates, as specified by the start and end dates
# on the batch command line.
#
#-----------------------------------------------------------------------------

#SBATCH --account=naqfc
#SBATCH --ntasks=1
#SBATCH --time=8:00:00
#SBATCH -J convert.airnow

echo "`date`: Job convert.airnow start."

YYYYMMDD1=$1
YYYYMMDD2=$2

if [ "x$YYYYMMDD1" == x ] || [ "x$YYYYMMDD2" == x ]; then
  echo "Invalid date.  Specify YYYYMMDD1 YYYYMMDD2 on batch command line."
  exit 1
fi

idir=/lfs/h2/emc/physics/noscrub/kai.wang/Bias_correction/airnow/csvtonetcdf/csv
odir=/lfs/h2/emc/physics/noscrub/kai.wang/Bias_correction/airnow/csvtonetcdf/netcdf
YY1=$(echo ${YYYYMMDD1} | cut -c1-4)
YY2=$(echo ${YYYYMMDD2} | cut -c1-4)
let ic=${YY1}
while [ ${ic} -le ${YY2} ]; do
    if [ ! -d /lfs/h2/emc/physics/noscrub/kai.wang/Bias_correction/airnow/csvtonetcdf/netcdf/${ic} ]; then
        mkdir -p /lfs/h2/emc/physics/noscrub/kai.wang/Bias_correction/airnow/csvtonetcdf/netcdf/${ic}
    fi
    ((ic++))
done

source setup.wcoss2.ifort.convert

## set INPATH  = airnow/csv/YYYY/YYYYMMDD/HourlyAQObs_YYYYMMDDHH.dat
## set OUTPATH = airnow/netcdf/YYYY/HourlyAQObs.YYYYMMDD.nc
export INPATH=${idir}/YYYY/YYYYMMDD/HourlyAQObs_YYYYMMDDHH.dat
export OUTPATH=${odir}/YYYY/YYYYMMDD/HourlyAQObs.YYYYMMDD.nc

./convert_airnow_csv $INPATH $OUTPATH $YYYYMMDD1 $YYYYMMDD2

echo ""
echo "`date`: Job convert.airnow complete."
