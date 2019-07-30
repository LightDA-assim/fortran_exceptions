cmake_minimum_required(VERSION 3.10)

find_package(LAPACK REQUIRED)

enable_language(Fortran)

set(SRC_ASSIM lenkf_rsm.f90)

add_library(assim SHARED ${SRC_ASSIM})
target_link_libraries(assim PRIVATE ${LAPACK_LIBRARIES})

set(SRC_ADVECT1D advect1d.f90)

add_library(advect1d SHARED ${SRC_ADVECT1D})

configure_file(animate.py animate.py COPYONLY)
configure_file(advect1d.py advect1d.py COPYONLY)
configure_file(enkf.py enkf.py COPYONLY)
configure_file(libsuffix.py libsuffix.py COPYONLY)
