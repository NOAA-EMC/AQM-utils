# 2023-feb-04	Original makefile version for convert_airnow_csv.f90.
#		By Dave Allured, NOAA/PSL/CIRES.

Prg =		convert_airnow_csv

Link.f90 =	$(F90_CMD) $(F90_FLAGS)

Lib =		$(LIB_F90)
Libraries =	$(LIB_NETCDF)

Subs.lib =	$(Lib)/config.f90		$(Lib)/stdlit.f90 \
		$(Lib)/string_utils.f90		$(Lib)/date_utils.f90 \
		$(Lib)/expand_filename.f90	$(Lib)/shell-sort.f90 \
		$(Lib)/netwrite3.no-udunits.f90

Subs.local =	get_command_args.convert.f90	get_airnow_field_table.f90 \
                verify_date_time.f90 		read_airnow_csv.f90 \
                write_airnow_netcdf.f90

Subs.all =	$(Subs.lib) $(Subs.local)

Hidden =	*.f90 $(Lib)/*.f90

$(Prg):	$(Prg).f90 $(Subs.all) $(Hidden) makefile
	$(Link.f90) $(Subs.all) $@.f90 $(Libraries) -o $@
	(rm *.mod ; echo) >& /dev/null
	@echo Good compile.

clean:
	@(rm *.mod *.o  *.dwarf $(Prg) ; echo) >& /dev/null
	@(rm -r *.dSYM ; echo) >& /dev/null
