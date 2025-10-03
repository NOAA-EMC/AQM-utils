#---------------------------------------------------------------------------
#
# 2014-may-10	Original make file for interpolate_update.f90.
#		By Dave Allured, NOAA/ESRL/PSD/CIRES.
# 2014-jul-16	Clean version for new release.
# 2016-jan-12	Upgrade to Dave's current fortran library structure.
# 2016-feb-10	Update for get_param_module.
# 2021-Jan-03   Modify for support RRFS-CMAQ
#
# This make file uses a single large compile command, rather
# than incremental compiles.  Environment variables are used to
# keep the make file generic for different compilers.
#
# Fortran source files are divided into three main groups in
# separate directories.  Order of compilation is managed with
# make macros that group and order the source files.  Within
# each bottom group,  file lists are mostly alphabetical,
# sometimes not, as needed.
#
#---------------------------------------------------------------------------

Prg      =  aqm_post_grib2_793

# Library directories.

Plib   =   ../../lib/sorc/lib.bias
Lib    =   ../../lib/sorc/lib.f90


# Command argument sequences.

NETCDF_LDFLAGS = -L${NETCDF}/lib -lnetcdf -lnetcdff
LIB_NETCDF = -I${NETCDF}/include $(NETCDF_LDFLAGS)

F90_CMD   = ftn

F90_FLAGS4 = -O3
#F90_FLAGS5 =

F90_FLAGS  = $(F90_FLAGS1) $(F90_FLAGS2)  \
             $(F90_FLAGS3) $(F90_FLAGS4)  \
             $(F90_FLAGS5)

Libraries =	$(LIB_NETCDF)
Link.f90  =	$(F90_CMD) $(F90_FLAGS)

F90_CONFIG =  f90_config.gfortran.f90

# Lib = General fortran support library.

Lib1 =		$(Lib)/config.f90 \
		$(Lib)/parse_varexp.f90		$(Lib)/stdlit.f90 \
		$(Lib)/read_netcdf_var.f90 \
		$(Lib)/string_utils.f90

Lib2 =		$(Lib)/date_utils.f90		$(Lib)/get_free_unit.f90 \
		$(Lib)/get_param_module.f90	$(Lib)/message_nolog.f90 \
		$(Lib)/netwrite3.no-udunits.f90	$(Lib)/read_line.f90


Lib3 =         $(Lib)/next_time.f90

#NETCDF_LIB  =  ${NETCDF_LDFLAGS}  ${NETCDF_LDFLAGS_F}
NETCDF_LIB  =  $(NETCDF_LDFLAGS)
W3_LIB      = ${W3NCO_LIB4} ${W3EMC_LIB4}

Lib4   =  \
          $(NETCDF_LIB) \
          ${G2_LIB4} \
          $(W3_LIB) \
          ${PNG_LIB} \
          ${BACIO_LIB4} \
          ${JASPER_LIB} \
          ${Z_LIB}

Subs.lib =	$(Lib1) $(Lib2) $(Lib3)

# Plib = Shared project support library.

Subs.plib =	$(Plib)/expand_filename.f90 \
		$(Plib)/gridlocation.f90 \
		$(Plib)/read_grid_coords.f90	$(Plib)/read_gridded_aqm.f90 \
		$(Plib)/read_gridded_vars.f90	$(Plib)/read_table_lines.f90

# Local support routines in main program directory.

#Subs.local =	interpolate.f90			read_config_file.f90 \
#		read_station_file.f90		write_interp_netcdf.f90

# Main program.

Subs.main =	$(Subs.lib) $(Subs.plib)

F90_CLEANUP = *.o *.mod *__genmod.f90

# Catch hidden include file dependencies that are not explicitly listed.
# This is overkill, but it is fast and good assurance.

Hidden =	$(Lib)/*.f90 $(Plib)/*.f90

$(Prg):	$(Prg).f90 $(Subs.main) $(Hidden) makefile
	$(Link.f90) $(Subs.main) $@.f90 $(Libraries) -o $@ $(Lib4)
	(rm $(F90_CLEANUP) ; echo) >& /dev/null
	@echo Good compile.

clean:
	@(rm $(F90_CLEANUP) *.o  *.dwarf $(Prg) ; echo) >& /dev/null
	@(rm -r *.dSYM ; echo) >& /dev/null
install:
	-cp $(Prg) ../../exec/.
clobber: clean
	-rm -f $(Prg)
