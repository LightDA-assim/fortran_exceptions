cmake_minimum_required(VERSION 3.2.3)

project(lenkf-rsm)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PROJECT_SOURCE_DIR}/cmake/modules/")

find_package(LAPACK REQUIRED)

enable_language(Fortran)

set(SRC_ASSIM lenkf_rsm.f90)

add_library(assim SHARED ${SRC_ASSIM})
target_link_libraries(assim PRIVATE ${LAPACK_LIBRARIES})

set(SRC_ADVECT1D advect1d.f90)

add_library(advect1d SHARED ${SRC_ADVECT1D})

find_package(F2PY REQUIRED)

configure_file(animate.py animate.py COPYONLY)
configure_file(advect1d.py advect1d.py COPYONLY)
configure_file(enkf.py enkf.py COPYONLY)
configure_file(libsuffix.py libsuffix.py COPYONLY)
configure_file(lenkf_rsm_py.py lenkf_rsm_py.py COPYONLY)
