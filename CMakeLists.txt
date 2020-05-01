cmake_minimum_required(VERSION 3.2.3)

project(lenkf-rsm)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PROJECT_SOURCE_DIR}/cmake/modules/")

find_package(LAPACK REQUIRED)

enable_language(Fortran)

find_package(MPI REQUIRED)

set(SRC_ASSIM lenkf_rsm.f90)
add_library(assim SHARED ${SRC_ASSIM})
target_link_libraries(assim PRIVATE ${LAPACK_LIBRARIES})

function(configure_mpi_target target)
  target_compile_definitions(${target} PRIVATE ${MPI_Fortran_COMPILE_DEFINITIONS})
  target_include_directories(${target} PRIVATE ${MPI_Fortran_INCLUDE_DIRS})
  target_link_libraries(${target} PRIVATE ${MPI_Fortran_LINK_FLAGS})
  target_link_libraries(${target} PRIVATE ${MPI_Fortran_LIBRARIES})
endfunction()

set(SRC_ASSIM_PARALLEL assimilate.f90)
add_library(assim_parallel SHARED ${SRC_ASSIM_PARALLEL})
target_link_libraries(assim_parallel PRIVATE assim)
configure_mpi_target(assim_parallel)

set(SRC_ASSIM_ADVECT1D advect1d_assimilate_interfaces.f90 advect1d_assimilate.f90)
add_executable(advect1d_assimilate ${SRC_ASSIM_ADVECT1D})
target_link_libraries(advect1d_assimilate PRIVATE assim_parallel)
configure_mpi_target(advect1d_assimilate)

set(SRC_ADVECT1D advect1d.f90)
add_library(advect1d SHARED ${SRC_ADVECT1D})

find_package(F2PY REQUIRED)

configure_file(animate.py animate.py COPYONLY)
configure_file(advect1d.py advect1d.py COPYONLY)
configure_file(enkf.py enkf.py COPYONLY)
configure_file(libsuffix.py libsuffix.py COPYONLY)
configure_file(lenkf_rsm_py.py lenkf_rsm_py.py COPYONLY)
