set(IOAPI_OFFSET_64 YES)

set(LIB_DIR "${CMAKE_SOURCE_DIR}/lib")
set(BLIB "${LIB_DIR}/lib.bias")
set(FLIB "${LIB_DIR}/lib.f90")

message("=== AQM library directory: ${LIB_DIR}")

list(APPEND SRC_FLIB1
    ${FLIB}/config.f90 
    ${FLIB}/stdlit.f90
    ${FLIB}/read_netcdf_var.f90
    ${FLIB}/string_utils.f90
    ${FLIB}/netcdf_sup.f90)

list(APPEND SRC_FLIB2
    ${FLIB}/date_utils.f90
    ${FLIB}/expand_filename.f90
    ${FLIB}/get_free_unit.f90
    ${FLIB}/get_param_module.f90     
    ${FLIB}/message_nolog.f90
    ${FLIB}/netwrite3.no-udunits.f90 
    ${FLIB}/next_time.f90
    ${FLIB}/read_line.f90)

list(APPEND SRC_BLIB
    ${BLIB}/gridlocation.f90
    ${BLIB}/read_grid_coords.f90
    ${BLIB}/read_gridded_aqm.f90
    ${BLIB}/read_gridded_hourly.f90
    ${BLIB}/read_gridded_vars.f90   
    ${BLIB}/read_table_lines.f90)

list(APPEND lib_src
    ${SRC_FLIB1}
    ${SRC_FLIB2}
    ${SRC_BLIB}
    )

set(exe_name aqm_post_grib2)
set(lib_name aqm_post_grib2_lib)
set(exe_src aqm_post_grib2.f90)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS
      "-g -traceback -fp-model source -free -convert big_endian")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
  set(CMAKE_Fortran_FLAGS
      "-g -fbacktrace -ffree-form -ffree-line-length-none -fconvert=big-endian")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -ggdb -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans -ffpe-trap=invalid,zero,overflow -fbounds-check")
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 10)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch")
  endif()
endif()

add_library(${lib_name} STATIC ${lib_src})
add_executable(${exe_name} ${exe_src})

target_link_libraries(
  ${lib_name}
  PUBLIC
  NetCDF::NetCDF_Fortran
  g2::g2_4
  w3emc::w3emc_4
  w3nco::w3nco_4
  bacio::bacio_4
  ${JASPER_LIBRARIES}
  PNG::PNG
  JPEG::JPEG
  ${ZLIB_LIBRARY}
)

target_link_libraries(${exe_name} PRIVATE ${lib_name})

install(TARGETS ${exe_name})
