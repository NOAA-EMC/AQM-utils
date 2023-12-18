# check environment
cmake_minimum_required(VERSION 3.15)

# set compiler
if (NOT DEFINED ENV{CMAKE_Fortran_COMPILER})
  message(FATAL_ERROR "CMAKE_Fortran_COMPILER is not defined")
endif()

set(CMAKE_Fortran_COMPILER $ENV{CMAKE_Fortran_COMPILER})

if(NOT DEFINED CMAKE_INSTALL_BINDIR)
  set(CMAKE_INSTALL_BINDIR exec)
endif()

if(NOT DEFINED BUILD_POST_STAT)
  set(BUILD_POST_STAT on)
endif()

# set the project name and version
project(
  AQM-utils
  VERSION 1.0.0
  LANGUAGES Fortran)

# User options.
option(OPENMP "use OpenMP threading" ON)

# include local macros
list (APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

# find external libraries
find_package(NetCDF REQUIRED COMPONENTS Fortran)
find_package(g2 REQUIRED)
find_package(w3emc REQUIRED)
find_package(w3nco REQUIRED)
find_package(bacio REQUIRED)

if(OPENMP)
  find_package(OpenMP REQUIRED COMPONENTS Fortran)
endif()

if(BUILD_POST_STAT)
  find_package(PNG REQUIRED)
  find_package(ZLIB REQUIRED)
  find_package(bufr REQUIRED)
  find_package(Jasper REQUIRED)
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -check all -ftrapuv")
endif()

# add project's subdirectories
add_subdirectory(sorc)
