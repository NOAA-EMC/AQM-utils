set(IOAPI_OFFSET_64 YES)

set(LIB_DIR "${CMAKE_SOURCE_DIR}/lib")
set(CLIB "${LIB_DIR}/lib.converter")

message("=== AQM library directory: ${LIB_DIR}")

list(APPEND SRC_CLIB
    ${CLIB}/config.f90
    ${CLIB}/stdlit.f90
    ${CLIB}/string_utils.f90
    ${CLIB}/date_utils.f90    
    ${CLIB}/expand_filename.f90
    ${CLIB}/shell-sort.f90
    ${CLIB}/netwrite3.no-udunits.f90
    )

list(APPEND SRC_LOCAL
    get_command_args.convert.f90
    get_airnow_field_table.f90
    verify_date_time.f90
    read_airnow_csv.f90
    write_airnow_netcdf.f90
    )

list(APPEND lib_src
    ${SRC_CLIB}
    ${SRC_LOCAL}
    )

set(exe_name convert_airnow_csv)
set(lib_name convert_airnow_csv_lib)
set(exe_src convert_airnow_csv.f90)

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

set(mod_dir "${CMAKE_CURRENT_BINARY_DIR}/mod")
set_target_properties(${lib_name} PROPERTIES Fortran_MODULE_DIRECTORY ${mod_dir})
target_include_directories(${lib_name} INTERFACE ${mod_dir})

target_link_libraries(
  ${lib_name}
  PUBLIC
  NetCDF::NetCDF_Fortran
)

target_link_libraries(${exe_name} PRIVATE ${lib_name})

install(TARGETS ${exe_name})
