#---------------------------------------------------------------------------
#
# 2014-jul-16	Make file for bias_correct main program.
#		By Dave Allured, NOAA/ESRL/PSD/CIRES.
#		Clean version for release.
# 2015-oct-29	Add support routines for bias threshold filter.
#
# 2016-jan-12	Upgrade to Dave's current fortran library structure.
# 2016-jan-20	Fix name conflict.  Change spread.f90 to spreading.f90.
# 2016-feb-04	Add anenmean_method and kfan_method.
# 2016-feb-05	Add $F90_PARALLEL, instead of compiler specific options.
# 2016-feb-08	Add get_param_int, get_param_yesno, write_test_files.
# 2016-feb-09	Update for get_param_module.
# 2016-feb-10	Split write_test_files into two routines.
# 2016-feb-15	Replace kf_analog.f90 with simplified x_analog.f90.
# 2016-feb-24	Remove $F90_PARALLEL, it is problematical.  Control parallel
#		  and optimizations exclusively through $F90_FLAGS.
#
# 2017-apr-04	Add *.f90 to catch hidden include files in main directory.
# 2017-apr-06	read_netcdf_var.f90 moves to standard F90 library.
# 2017-apr-11	Add library routine.
#		Rename x_analog.f90 to analog_ensemble.f90.
# 2017-apr-24	Add routines for predictor weighting.
#
# 2017-apr-29	Code restructuring for analog_control, etc.
# 2017-may-17	More support for predictor weighting.
# 2017-may-31	Add compute_weight_sets.f90.
# 2017-jun-02	Add blackout_obs_data.f90.
# 2017-jun-08	Move analog computational code into lib.analog.
#
# 2019-may-29	Add compute_wind.f90.
# 2019-jun-28	Add probability forecast module and support.
# 2019-aug-08	Reorganize.  Switch from $LIB_OPT_INTERP to $LIB_PROBABILITY.
# 2019-nov-06	WCOSS compatibility.  Add targets "install" and "clobber".
#
# 2020-jun-11	Add write_site_result_files.f90.
# 2020-jul-31	Add new routines for probability linear regression method.
# 2020-nov-10	Add new routines for config file reader.
#		Name change to find_analog.f90.
#
# 2021-apr-27	Add site blocking routines.
#
# 2022-apr-17	Add read_gridded_hourly.f90, etc. for RRFS-CMAQ.
# 2022-may-21	Add netcdf_sup.f90 to read units attributes.
# 2022-may-25	Add short_training_period.f90.
# 2022-may-31	Main program name change to aqm_bias_correct.f90.
# 2022-jun-02	Split write_corrected_netcdf into two versions.
#
# 2023-mar-27	Add print_omp_info.f90 with conditional $F90_OMP_STUBS.
# 2023-apr-04	Add read_obs_series_netcdf.f90, etc. for AirNow Netcdf support.
#
# This make file uses a single large compile command, rather
# than incremental compiles.  Environment variables are used to
# keep the make file generic for different compilers.
#
# Fortran source files are divided into four main groups in
# separate directories.  Order of compilation is managed with
# make macros that group and order the source files.  Within
# each bottom group, file lists are mostly alphabetical,
# sometimes not, as needed.
#
#---------------------------------------------------------------------------

Prg =		aqm_bias_correct

# Library directories.

Alib =		$(LIB_ANALOG)
Blib =		$(LIB_BIAS)
Lib =		$(LIB_F90)
Plib =		$(LIB_PROBABILITY)

# Command argument sequences.

Libraries =	$(LIB_BUFR) $(LIB_NETCDF)
Link.f90 =	$(F90_CMD) $(F90_FLAGS) $(F03_ALLOCATE)

# OpenMP support for gfortran serial mode.  Needed to add the prefix.

ifdef F90_OMP_STUBS
   OMP_STUBS = $(Lib)/$(F90_OMP_STUBS)
else
   OMP_STUBS =
endif

# Lib = General fortran support library.

Lib1 =		$(Lib)/config.f90		$(Lib)/stdlit.f90 \
		$(Lib)/string_utils.f90

Lib2 =		$(Lib)/binomial_coefficient.f90	$(Lib)/date_utils.f90 \
		$(Lib)/distance_btw_coords.f90	$(Lib)/expand_filename.f90 \
		$(Lib)/find_runs.f90		$(Lib)/get_free_unit.f90 \
		$(Lib)/get_param_module.f90	$(Lib)/message_nolog.f90 \
		$(Lib)/multisets.f90		$(Lib)/netcdf_sup.f90 \
		$(Lib)/netwrite3.no-udunits.f90	$(OMP_STUBS) \
		$(Lib)/print_omp_info.f90	$(Lib)/read_line.f90 \
		$(Lib)/read_netcdf_var.f90	$(Lib)/shell-sort.f90 \
		$(Lib)/std_dev.f90		$(Lib)/std_dev_grid.f90

Subs.lib =	$(Lib1) $(Lib2)

# Blib = Shared BC project support library.

Subs.blib =	$(Blib)/bufrlib_interface.f90	$(Blib)/gridlocation.f90 \
		$(Blib)/print_library_info.f90	$(Blib)/read_bufr_var.f90 \
		$(Blib)/read_grid_coords.f90	$(Blib)/read_gridded_aqm.f90 \
		$(Blib)/read_gridded_hourly.f90 \
		$(Blib)/read_gridded_vars.f90	$(Blib)/read_table_lines.f90 \
		$(Blib)/shell-sort.descending.f90

# Analog = Analog computational library.

Analog1 =	$(Alib)/compute_weight_sets.f90	$(Alib)/rmse.f90 \
		$(Alib)/stdevTNcirc.f90		$(Alib)/wind_dir_error.f90

Analog2 =	$(Alib)/find_analog.f90		$(Alib)/kf_luca.f90 \
		$(Alib)/short_training_period.f90 \
		$(Alib)/analog_ensemble.f90	$(Alib)/anenmean_method.f90 \
		$(Alib)/kfan_method.f90		$(Alib)/filter_dispatch.f90 \
		$(Alib)/weight_control.f90	$(Alib)/analog_control.f90

Subs.alib =	$(Analog1) $(Analog2)

# Plib = Probability forecast module.

Opt_Interp =	$(Plib)/haversine_km.f90	$(Plib)/jacobi.f90 \
		$(Plib)/lubksb.f90		$(Plib)/ludcmp.f90 \
		$(Plib)/xnorm_cdf.f90		$(Plib)/cdf_to_probability.f90 \
		$(Plib)/optimal_interp_analvar.f90

Probability1 =	$(Plib)/probability_type.f90 \
		$(Plib)/daily_averages.f90	$(Plib)/daily_max.f90 \
		$(Plib)/find_nearest_grid_points.f90 \
		$(Plib)/gridded_climatology.f90	$(Plib)/make_daily.f90 \
		$(Plib)/regress.f90		$(Plib)/site_climatology.f90 \
		$(Plib)/write_probability_netcdf.f90 \
		$(Plib)/write_probability_netcdf_lr.f90

Probability2 =	$(Plib)/make_climatologies.f90	$(Plib)/opt_interp_method.f90 \
		$(Plib)/regress_method.f90	$(Plib)/probability.f90

Subs.plib =	$(Opt_Interp) $(Probability1) $(Probability2)

# Local support routines in main program directory.

Local1 =	align_obs_to_forecasts.f90	blackout_obs_data.f90 \
		compute_wind.f90		convert_obs_units.f90 \
		get_command_args.main.f90	get_window_offsets.f90 \
		grid_distances.f90		qc_single_site.f90 \
		read_blocking_list.f90		site_blocking.f90

Local2 =	print_interp_summary.f90	read_var_table.f90 \
		read_config_file_main.f90	read_interp_forecasts.f90 \
		read_obs_series_bufr.f90	read_obs_file_netcdf.f90 \
		read_obs_series_netcdf.f90	read_obs_series.f90 \
		read_obs_qc.f90			read_predictor_weights.f90 \
		read_exception_list.f90		spread_bias.f90 \
		write_corrected_netcdf_rrfs.f90	write_corrected_netcdf_v1.f90 \
		write_predictor_weights.f90	write_site_list.f90 \
		write_site_result_files.f90	write_test_day_files.f90 \
		write_test_site_files.f90

Local3 =	spreading.f90

Subs.local =	$(Local1) $(Local2) $(Local3)

# Main program.

Subs.main =	$(Subs.lib) $(Subs.blib) $(Subs.alib) $(Subs.plib) $(Subs.local)

# Catch hidden include file dependencies that are not explicitly listed.
# This is overkill, but it is fast and good assurance.

Hidden =	*.f90 $(Lib)/*.f90 $(Alib)/*.f90 $(Blib)/*.f90 $(Plib)/*.f90

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
