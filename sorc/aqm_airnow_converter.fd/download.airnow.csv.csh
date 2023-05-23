#!/bin/csh

#---------------------------------------------------------------------------
#
# download.airnow.csv.csh -- Download Airnow CSV data files.
#
# 2022-sep-01	Original version.  By Dave Allured, NOAA/PSL/CIRES.
#		Downloads both HourlyAQObs and HourlyData files.
# 2022-oct-07	Suppress downloading error page files for missing files.
#		Use curl --fail option.
#
# 2023-jan-08	Omit HourlyData files.  Download HourlyAQObs only.
# 2023-feb-01	Switch from loop to curl globbing.  See docs.
#		Add --retry due to flaky Hera connections.
#
# This version downloads a time range within a single year.
#
# Because of curl globbing, this version will be slow for
# incomplete dates.  This version is intended for archive
# building, not real time tracking.
#
# No checking for previous files is performed.
# Any previous files are overwritten.
# A mixture of old and new files is possible, if you don't
# take care to avoid this.
#
# Usage:
#
# 1.  Go to the top-level data directory containing the year directories.
#
# 2.  ./download.airnow.csv.csh YYYY MMDD1 MMDD2
#
#---------------------------------------------------------------------------

echo "`date`:  Start Airnow downloads."

if ( $# != 3 ) then
   echo '*** Need three parameters.'
   echo '*** Usage:  ./download.airnow.csv.csh YYYY MMDD1 MMDD2'
   exit 1
endif

set yyyy  = $1
set mmdd1 = $2
set mmdd2 = $3

# SAMPLE -- WORKS:
# http://files.airnowtech.org/airnow/2022/20220826/HourlyAQObs_2022082600.dat

# Use the Airnow virtual source URL, not the physical one.
# This one might continue to track if the physical data host is changed.

set prefix = "http://files.airnowtech.org/airnow"

set base   = "$cwd"		# top download directory = current directory

# Auto configure for the stupid date commands.
# There is no standard shell date incrementer that I could find, that is
# both portable and straightforward.  Cal is intricate.  Perl is not shell.

date -u -d '2020-1-1 +1 day' >& /dev/null	# works on linux/GNU, not mac
						# error status = 1 --> mac

set mac   = $status		# Having a bad day today.  No boolean invert
set linux = `expr 1 - $mac`	# operator in sight.  This instead, fragile
				# yet elegant.  Elegance wins today.

# Main loop over each date in requested time range.
# This loop could handle year crossings, but stay in one year for this version.

set yyyymmdd = ${yyyy}${mmdd1}			# make start and end full dates
set end_date = ${yyyy}${mmdd2}

while ( $yyyymmdd <= $end_date )
   echo ''
   echo ========== $yyyymmdd ==========		# progress display

   mkdir -p "$base/$yyyy/$yyyymmdd"
   cd       "$base/$yyyy/$yyyymmdd"

   set day_prefix = "$prefix/$yyyy/$yyyymmdd"

# Fetch 24 Airnow hourly files.  Use curl globbing.
# This will be slow for incomplete dates.

   curl -O -R --fail --retry 10 $day_prefix/HourlyAQObs_${yyyymmdd}'[00-23].dat'

# Advance to next calendar date.  Ouch.

   if ( $linux ) then
      set yyyymmdd = `date -u -d "$yyyymmdd +1 day" +%Y%m%d`	     # linux/GNU
   else
      set mmdd     = `echo $yyyymmdd | cut -c5-`
      set yyyymmdd = `date -j -u -v+1d +%Y%m%d ${mmdd}0400${yyyy}`   # mac/BSD
   endif
end

echo "`date`:  Download complete for $yyyy."
exit
