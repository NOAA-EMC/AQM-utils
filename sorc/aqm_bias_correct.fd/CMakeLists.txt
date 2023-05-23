set(IOAPI_OFFSET_64 YES)

set(LIB_DIR "${CMAKE_SOURCE_DIR}/lib")
set(ALIB "${LIB_DIR}/lib.analog")
set(BLIB "${LIB_DIR}/lib.bias")
set(FLIB "${LIB_DIR}/lib.f90")
set(PLIB "${LIB_DIR}/lib.probability")

message("=== AQM library directory: ${LIB_DIR}")

list(APPEND SRC_FLIB1
    ${FLIB}/config.f90 
    ${FLIB}/stdlit.f90
    ${FLIB}/string_utils.f90
    )

list(APPEND SRC_FLIB2
    ${FLIB}/binomial_coefficient.f90	
    ${FLIB}/date_utils.f90
    ${FLIB}/distance_btw_coords.f90
    ${FLIB}/expand_filename.f90
    ${FLIB}/find_runs.f90
    ${FLIB}/get_free_unit.f90
    ${FLIB}/get_param_module.f90     
    ${FLIB}/message_nolog.f90
    ${FLIB}/multisets.f90
    ${FLIB}/netcdf_sup.f90
    ${FLIB}/netwrite3.no-udunits.f90 
    ${FLIB}/print_omp_info.f90
    ${FLIB}/read_line.f90
    ${FLIB}/read_netcdf_var.f90
    ${FLIB}/shell-sort.f90
    ${FLIB}/std_dev.f90
    ${FLIB}/std_dev_grid.f90
    )

list(APPEND SRC_BLIB
    ${BLIB}/bufrlib_interface.f90
    ${BLIB}/gridlocation.f90
    ${BLIB}/print_library_info.f90
    ${BLIB}/read_bufr_var.f90    
    ${BLIB}/read_grid_coords.f90
    ${BLIB}/read_gridded_aqm.f90
    ${BLIB}/read_gridded_hourly.f90
    ${BLIB}/read_gridded_vars.f90   
    ${BLIB}/read_table_lines.f90
    ${BLIB}/shell-sort.descending.f90
    )

list(APPEND SRC_ALIB1
    ${ALIB}/compute_weight_sets.f90 
    ${ALIB}/rmse.f90
    ${ALIB}/stdevTNcirc.f90         
    ${ALIB}/wind_dir_error.f90
    )

list(APPEND SRC_ALIB2       
    ${ALIB}/find_analog.f90         
    ${ALIB}/kf_luca.f90
    ${ALIB}/short_training_period.f90
    ${ALIB}/analog_ensemble.f90     
    ${ALIB}/anenmean_method.f90
    ${ALIB}/kfan_method.f90         
    ${ALIB}/filter_dispatch.f90
    ${ALIB}/weight_control.f90      
    ${ALIB}/analog_control.f90
    )

list(APPEND SRC_PLIB1
    ${PLIB}/haversine_km.f90        
    ${PLIB}/jacobi.f90
    ${PLIB}/lubksb.f90
    ${PLIB}/ludcmp.f90
    ${PLIB}/xnorm_cdf.f90
    ${PLIB}/cdf_to_probability.f90
    ${PLIB}/optimal_interp_analvar.f90
    )

list(APPEND SRC_PLIB2
    ${PLIB}/probability_type.f90
    ${PLIB}/daily_averages.f90
    ${PLIB}/daily_max.f90
    ${PLIB}/find_nearest_grid_points.f90
    ${PLIB}/gridded_climatology.f90 
    ${PLIB}/make_daily.f90
    ${PLIB}/regress.f90
    ${PLIB}/site_climatology.f90
    ${PLIB}/write_probability_netcdf.f90
    ${PLIB}/write_probability_netcdf_lr.f90
    )

list(APPEND SRC_PLIB3
    ${PLIB}/make_climatologies.f90
    ${PLIB}/opt_interp_method.f90
    ${PLIB}/regress_method.f90
    ${PLIB}/probability.f90
    )

list(APPEND SRC_LOCAL1
    align_obs_to_forecasts.f90
    blackout_obs_data.f90
    compute_wind.f90
    convert_obs_units.f90
    get_command_args.main.f90
    get_window_offsets.f90
    grid_distances.f90
    qc_single_site.f90
    read_blocking_list.f90
    site_blocking.f90
    )

list(APPEND SRC_LOCAL2
    print_interp_summary.f90
    read_var_table.f90
    read_config_file_main.f90
    read_interp_forecasts.f90
    read_obs_file_netcdf.f90
    read_obs_qc.f90
    read_obs_series.f90
    read_obs_series_bufr.f90
    read_obs_series_netcdf.f90
    read_predictor_weights.f90
    read_exception_list.f90
    spread_bias.f90
    write_corrected_netcdf_rrfs.f90
    write_corrected_netcdf_v1.f90
    write_predictor_weights.f90
    write_site_list.f90
    write_site_result_files.f90
    write_test_day_files.f90
    write_test_site_files.f90
    )

list(APPEND SRC_LOCAL3
    spreading.f90
    )

list(APPEND lib_src
    ${SRC_FLIB1}
    ${SRC_FLIB2}
    ${SRC_BLIB}
    ${SRC_ALIB1}
    ${SRC_ALIB2}
    ${SRC_PLIB1}
    ${SRC_PLIB2}
    ${SRC_PLIB3}
    ${SRC_LOCAL1}
    ${SRC_LOCAL2}
    ${SRC_LOCAL3}
    )

set(exe_name aqm_bias_correct)
set(lib_name aqm_bias_correct_lib)
set(exe_src aqm_bias_correct.f90)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS
      "-g -traceback -fp-model source -free -convert big_endian -qopenmp")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
  set(CMAKE_Fortran_FLAGS
      "-g -fbacktrace -ffree-form -ffree-line-length-none -fconvert=big-endian -fopenmp")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -ggdb -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans -ffpe-trap=invalid,zero,overflow -fbounds-check")
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 10)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch")
  endif()
endif()

add_library(${lib_name} STATIC ${lib_src})
add_executable(${exe_name} ${exe_src})

set(mod_dir "${CMAKE_CURRENT_BINARY_DIR}/mod")
set_target_properties(${lib_name} PROPERTIES Fortran_MODULE_DIRECTORY ${mod_dir})
target_include_directories(${lib_name} INTERFACE ${mod_dir})

target_link_libraries(
  ${lib_name}
  PUBLIC
  NetCDF::NetCDF_Fortran
  bufr::bufr_4
)

if(OpenMP_Fortran_FOUND)
  target_link_libraries(${exe_name} PUBLIC OpenMP::OpenMP_Fortran)
endif()

target_link_libraries(${exe_name} PRIVATE ${lib_name})

install(TARGETS ${exe_name})
