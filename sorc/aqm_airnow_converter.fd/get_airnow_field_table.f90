!-----------------------------------------------------------------------------
!
! get_airnow_field_table.f90 -- Decision table for AirNow CSV reader.
!
! 2023-jan-31	Original version.  By Dave Allured, NOAA/PSL/CIRES.
! 2023-feb-04	Change coordinates from save daily, to save hourly,
!		  to capture the many variations in the source data.
!
! The field column is sourced directly from EPA file documentation,
! HourlyAQObsFactSheet.pdf, update November 2021.
!
! Table is used to control processing in these routines:
!
!    convert_airnow_csv.f90
!    read_airnow_csv.f90
!    write_airnow_netcdf.f90
!
! Name and region field widths are oversized for possible
! future purposes.
!
! Field widths for type units are unspecified because they are
! common-valued, therefore attached directly to their associated
! output data variables as actual-length char attributes.
!
! Time = hourly:  Save 24 values per day, per site.
!
! Time = daily:   Merge and save only one value per day, per site.
!
! Consistency = yes:  Same value expected per site, across
! different dates and times.  Print warning when value changes.
!
! Consistency = merge:  Assign the "active" setting when value
! changes within the same date, and do not print a warning.
!
! Consistency = constant:  Expect a common value across all sites
! and all times.
!
! Note that caller sets string sizes for the output arrays.
! Length 30 is appropriate for the current table version.
!
!-----------------------------------------------------------------------------

module get__airnow_field_table

  integer, parameter :: len_table_item = 30	! length for string arguments

contains

subroutine get_airnow_field_table (field_names, field_types, daily, hourly, &
     required, consistency, merge_type, len_out, expected, units, &
     i_site_id, i_lat, i_lon, i_date, i_time, nfields)
  implicit none

! Output arguments.

  character(*), intent (out), allocatable :: field_names(:)   ! value and
  character(*), intent (out), allocatable :: field_types(:)   ! switch columns
  logical,      intent (out), allocatable :: daily(:)
  logical,      intent (out), allocatable :: hourly(:)
  logical,      intent (out), allocatable :: required(:)
  logical,      intent (out), allocatable :: consistency(:)
  logical,      intent (out), allocatable :: merge_type(:)
  integer,      intent (out), allocatable :: len_out(:)	      ! output lengths
  character(*), intent (out), allocatable :: expected(:)      ! expected value
  character(*), intent (out), allocatable :: units(:)	      ! associated units

  integer, intent (out) :: i_site_id, i_lat, i_lon	! convenience indices
  integer, intent (out) :: i_date, i_time		! into specific fields
  integer, intent (out) :: nfields			! no. of fields in table

! Local variables.

  character(len_table_item) col(5), base_name
  character line*80
  integer j, fi, fi2, ti

  character(*), parameter :: table(*) = (/ &
  'Field name      Type    Time    Required  Consistency  Width or Value   ', &
  'AQSID           char    daily   yes       yes          12               ', &
  'SiteName        char    daily   no        yes          80               ', &
  'Status          char    daily   no        merge        8                ', &
  'EPARegion       char    daily   no        yes          8                ', &
  'Latitude        double  hourly  yes       no                            ', &
  'Longitude       double  hourly  yes       no                            ', &
  'Elevation       float   hourly  no        no                            ', &
  'GMTOffset       float   daily   no        yes      (Some have fractions)', &
  'CountryCode     char    daily   no        yes          2                ', &
  'StateName       char    daily   no        yes          3  (Some are N/A)', &
  'ValidDate       verify  none    no        no                            ', &
  'ValidTime       verify  none    no        no                            ', &
  'DataSource      omit    none    no        no                            ', &
  'ReportingArea_PipeDelimited omit none no  no                            ', &
  'OZONE_AQI       float   hourly  no        no                            ', &
  'PM10_AQI        float   hourly  no        no                            ', &
  'PM25_AQI        float   hourly  no        no                            ', &
  'NO2_AQI         float   hourly  no        no                            ', &
  'OZONE_Measured  float   daily   no        merge  (These four are 0 or 1)', &
  'PM10_Measured   float   daily   no        merge                         ', &
  'PM25_Measured   float   daily   no        merge                         ', &
  'NO2_Measured    float   daily   no        merge                         ', &
  'PM25            float   hourly  yes       no                            ', &
  'PM25_Unit       unit    daily   no        constant     "UG/M3"          ', &
  'OZONE           float   hourly  yes       no                            ', &
  'OZONE_Unit      unit    daily   no        constant     "PPB"            ', &
  'NO2             float   hourly  no        no                            ', &
  'NO2_Unit        unit    daily   no        constant     "PPB"            ', &
  'CO              float   hourly  no        no                            ', &
  'CO_Unit         unit    daily   no        constant     "PPM"            ', &
  'SO2             float   hourly  no        no                            ', &
  'SO2_Unit        unit    daily   no        constant     "PPB"            ', &
  'PM10            float   hourly  no        no                            ', &
  'PM10_Unit       unit    daily   no        constant     "UG/M3"          '  /)

! Allocate result arrays.

  nfields = size (table) - 1	! determine table size; omit header line

  allocate (field_names(nfields), field_types(nfields))
  allocate (daily(nfields),       hourly(nfields))
  allocate (required(nfields),    consistency(nfields))
  allocate (merge_type(nfields),  len_out(nfields))
  allocate (expected(nfields),    units(nfields))

! Fully init arrays that are only partially filled.

  len_out(:)  = 0
  expected(:) = ' '

! Parse table lines using fortran free format reads.

! Note, slashes must be inside quoted strings.  This may also
! be true for asterisks in some cases.  See fortran specs.

  do fi = 1, nfields
    ti = fi + 1				! skip table's own header line
    line = table(ti)
    read (line, *) field_names(fi), field_types(fi), col(3:5)

! Convert some fields to switch settings.

    daily(fi)       = (col(3) == 'daily')
    hourly(fi)      = (col(3) == 'hourly')
    required(fi)    = (col(4) == 'yes')
    consistency(fi) = (col(5) == 'yes')
    merge_type(fi)  = (col(5) == 'merge')

! Get the sixth column for special cases only.

    if (field_types(fi) == 'char') &
      read (line, *) col(1:5), len_out(fi)

    if (col(5) == 'constant') &
      read (line, *) col(1:5), expected(fi)
  end do

! Get the units value associated with each data variable, if available.
! This is specific to the naming system in this particular AirNow format.

   units(:) = ' '			! default to blank = no units attribute

   do fi = 1, nfields
      j = index (field_names(fi), '_Unit ')	! find a Unit suffix
      if (j == 0) cycle
      base_name = field_names(fi)(1:j-1)	! parse out the base var name
      fi2 = findloc (field_names(:), base_name, dim=1)   ! find the data var
      if (fi2 == 0) cycle
      units(fi2) = expected(fi)		       ! assign units string to data var
   end do

! Get the convenience indices for specific fields.

  i_site_id = findloc (field_names(:), 'AQSID',     dim=1)
  i_lat     = findloc (field_names(:), 'Latitude',  dim=1)
  i_lon     = findloc (field_names(:), 'Longitude', dim=1)
  i_date    = findloc (field_names(:), 'ValidDate', dim=1)
  i_time    = findloc (field_names(:), 'ValidTime', dim=1)

end subroutine get_airnow_field_table
end module get__airnow_field_table
