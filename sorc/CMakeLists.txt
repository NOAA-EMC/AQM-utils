# This is the cmake build file for the src directory in AQM-utils

add_subdirectory(gefs2clbcs_para.fd)
if (BUILD_POST_STAT)
  add_subdirectory(aqm_post_grib2.fd)
  add_subdirectory(aqm_post_maxi_grib2.fd)
  add_subdirectory(aqm_post_bias_cor_grib2.fd)
  add_subdirectory(aqm_post_maxi_bias_cor_grib2.fd)
  add_subdirectory(aqm_bias_correct.fd)
  add_subdirectory(aqm_bias_interpolate.fd)
  add_subdirectory(convert_airnow_csv.fd)
endif()
