#---------------------------------------------------------------------------
#
# 2014-may-10	Original make file for interpolate_update.f90.
#		By Dave Allured, NOAA/ESRL/PSD/CIRES.
# 2014-jul-16	Clean version for new release.
# 2016-jan-12	Upgrade to Dave's current fortran library structure.
# 2016-feb-10	Update for get_param_module.
#
# 2017-apr-06	read_netcdf_var.f90 moves to standard F90 library.
# 2017-jun-09	get_command_args.f90 moves into local directory.
#
# 2019-may-17	Updates for new modules, etc.
# 2019-jul-30	Simplify for F2003.  Omit $F90_CONFIG.
# 2019-nov-06	WCOSS compatibility.  Add targets "install" and "clobber".
#
# 2022-apr-16	Add read_gridded_hourly.f90, etc. for RRFS-CMAQ.
# 2022-may-23	Add netcdf_sup.f90.
# 2022-may-27	Main program name change to aqm_bias_interpolate.f90.
#
# 2023-apr-09	Rename read_station_file.f90 to read_site_list.f90.
#		Omit get_free_unit.f90, no longer needed.
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

Prg =		aqm_bias_interpolate

# Library directories.

Plib =		$(LIB_BIAS)
Lib =		$(LIB_F90)

# Command argument sequences.

Libraries =	$(LIB_NETCDF)
Link.f90 =	$(F90_CMD) $(F90_FLAGS)

# Lib = General fortran support library.

Lib1 =		$(Lib)/config.f90		$(Lib)/stdlit.f90 \
		$(Lib)/string_utils.f90

Lib2 =		$(Lib)/date_utils.f90		$(Lib)/distance_btw_coords.f90 \
		$(Lib)/expand_filename.f90 \
		$(Lib)/get_param_module.f90	$(Lib)/message_nolog.f90 \
		$(Lib)/netcdf_sup.f90		$(Lib)/netwrite3.no-udunits.f90 \
		$(Lib)/read_line.f90		$(Lib)/read_netcdf_var.f90

Subs.lib =	$(Lib1) $(Lib2)

# Plib = Shared project support library.

Subs.plib =	$(Plib)/gridlocation.f90	$(Plib)/read_grid_coords.f90 \
		$(Plib)/read_gridded_aqm.f90	$(Plib)/read_gridded_hourly.f90 \
		$(Plib)/read_gridded_vars.f90	$(Plib)/read_table_lines.f90

# Local support routines in main program directory.

Subs.local =	derivatives.f90			get_command_args.interp.f90 \
		interpolate.f90			read_config_file_interp.f90 \
		read_site_list.f90		write_interp_netcdf.f90

# Main program.

Subs.main =	$(Subs.lib) $(Subs.plib) $(Subs.local)

# Catch hidden include file dependencies that are not explicitly listed.
# This is overkill, but it is fast and good assurance.

Hidden =	*.f90 $(Lib)/*.f90 $(Plib)/*.f90

$(Prg):	$(Prg).f90 $(Subs.main) $(Hidden) makefile
	$(Link.f90) $(Subs.main) $@.f90 $(Libraries) -o $@
	(rm $(F90_CLEANUP) ; echo) >& /dev/null
	@echo Good compile.

clean:
	@(rm $(F90_CLEANUP) *.o  *.dwarf $(Prg) ; echo) >& /dev/null
	@(rm -r *.dSYM ; echo) >& /dev/null

install:
	-cp $(Prg) $(INSTALL_DIR)

clobber: clean
	-rm -f $(Prg)
