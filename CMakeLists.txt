cmake_minimum_required(VERSION 3.10)

project(lenkf-rsm)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PROJECT_SOURCE_DIR}/cmake/modules/")

find_package(LAPACK REQUIRED)

enable_language(Fortran)

find_package(MPI REQUIRED)

if(MPI_Fortran_HAVE_F90_MODULE)
  add_definitions(-DHAVE_MPI_MODULE)
endif()

find_package(HDF5 COMPONENTS Fortran REQUIRED)

add_library(system_mpi SHARED system_mpi.F90)

set(SRC_ASSIM lenkf_rsm.f90)
add_library(assim SHARED ${SRC_ASSIM})
target_link_libraries(assim PRIVATE ${LAPACK_LIBRARIES})

function(configure_mpi_target target)
  target_compile_definitions(${target} PRIVATE ${MPI_Fortran_COMPILE_DEFINITIONS})
  target_include_directories(${target} PRIVATE ${MPI_Fortran_INCLUDE_DIRS})
  target_link_libraries(${target} PRIVATE ${MPI_Fortran_LINK_FLAGS})
  target_link_libraries(${target} PRIVATE ${MPI_Fortran_LIBRARIES})
endfunction()

add_library(random SHARED random.f90)

add_library(localization SHARED localization.f90)

set(SRC_ASSIM_PARALLEL assimilate.f90)
add_library(assim_parallel SHARED ${SRC_ASSIM_PARALLEL})
target_link_libraries(assim_parallel PRIVATE assim system_mpi)
configure_mpi_target(assim_parallel)

set(SRC_ASSIM_ADVECT1D advect1d_assimilate_interfaces.f90 advect1d_assimilate.f90)
add_executable(advect1d_assimilate ${SRC_ASSIM_ADVECT1D})
target_link_libraries(advect1d_assimilate PRIVATE assim_parallel random localization system_mpi ${HDF5_Fortran_LIBRARIES} ${HDF5_LIBRARIES})
configure_mpi_target(advect1d_assimilate)
set_source_files_properties(advect1d_assimilate_interfaces.F90 PROPERTIES COMPILE_FLAGS -cpp)

if(HDF5_Fortran_INCLUDE_DIRS)
  target_include_directories(advect1d_assimilate PRIVATE ${HDF5_Fortran_INCLUDE_DIRS})
elseif(HDF5_Fortran_INCLUDE_DIR)
  target_include_directories(advect1d_assimilate PRIVATE ${HDF5_Fortran_INCLUDE_DIR})
else()
  message(FATAL_ERROR "No HDF5 Fortran include directory found.")
endif()

set(SRC_ADVECT1D advect1d.f90)
add_library(advect1d SHARED ${SRC_ADVECT1D})

find_package(F2PY REQUIRED)

configure_file(animate.py animate.py COPYONLY)
configure_file(animate_mpi.py animate_mpi.py COPYONLY)
configure_file(advect1d.py advect1d.py COPYONLY)
configure_file(enkf.py enkf.py COPYONLY)
configure_file(libsuffix.py libsuffix.py COPYONLY)
configure_file(lenkf_rsm_py.py lenkf_rsm_py.py COPYONLY)
