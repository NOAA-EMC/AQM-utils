help([[
This module loads libraries for building NEXUS on
the GMU ORC machine Hopper using GNU
]])

whatis([===[Loads libraries needed for building the UFS SRW App on Hopper ]===])

-- prepend_path("MODULEPATH","/contrib/sutils/modulefiles")
-- load("sutils")

-- This path should point to your HPCstack installation directory
local HPCstack="/opt/sw/other/apps/hpc-stack/"

-- Load HPC stack
prepend_path("MODULEPATH", pathJoin(HPCstack, "modulefiles/stack"))

-- load(pathJoin("cmake", os.getenv("cmake_ver") or "3.20.1"))
-- load(pathJoin("cmake", os.getenv("cmake_ver") or "3.22.1-6o"))

-- intel_ver=os.getenv("intel_ver") or "2022.1.2"
-- load(pathJoin("intel", intel_ver))

-- impi_ver=os.getenv("impi_ver") or "2022.1.2"
-- load(pathJoin("impi", impi_ver))

-- prepend_path("MODULEPATH","/scratch1/NCEPDEV/nems/role.epic/hpc-stack/libs/intel-2022.1.2/modulefiles/stack")

-- load(pathJoin("hpc", os.getenv("hpc_ver") or "1.2.0"))
-- load(pathJoin("hpc-intel", os.getenv("hpc_intel_ver") or "2022.1.2"))
-- load(pathJoin("hpc-impi", os.getenv("hpc_impi_ver") or "2022.1.2"))
-- 
-- load(pathJoin("hdf5", os.getenv("hdf5_ver") or "1.10.6"))
-- load(pathJoin("netcdf", os.getenv("netcdf_ver") or "4.7.4"))
-- load(pathJoin("esmf", os.getenv("esmf_ver") or "8.3.0b09"))

setenv("CMAKE_C_COMPILER","mpicc")
setenv("CMAKE_CXX_COMPILER","mpicxx")
setenv("CMAKE_Fortran_COMPILER","mpif90")
setenv("CMAKE_Platform","hera.gnu")
