cmake_minimum_required(VERSION 3.10)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/modules/")

find_package(LAPACK REQUIRED)

enable_language(Fortran)

set(SRC_ASSIM lenkf_rsm.f90)

add_library(assim SHARED ${SRC_ASSIM})
target_link_libraries(assim PRIVATE ${LAPACK_LIBRARIES})

set(SRC_ADVECT1D advect1d.f90)

add_library(advect1d SHARED ${SRC_ADVECT1D})

find_package(F2PY REQUIRED)

set(dgemm_module_name "dgemm")
set(dgemm_sources ${CMAKE_CURRENT_SOURCE_DIR}/dgemm.pyf)
set(dgemm_module_file ${CMAKE_CURRENT_BINARY_DIR}/${dgemm_module_name}${PYTHONEXTENSION_MODULE_SUFFIX})
add_custom_target(${dgemm_module_name} ALL DEPENDS ${dgemm_module_file})

add_custom_command(
  OUTPUT ${dgemm_module_file}
  COMMAND ${F2PY_EXECUTABLE}
  -m ${dgemm_module_name}
  -c ${dgemm_sources}
  ${BLAS_LIBRARIES} ${BLAS_LINKER_FLAGS}
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

install(FILES ${dgemm_module_file} DESTINATION .)
configure_file(animate.py animate.py COPYONLY)
configure_file(advect1d.py advect1d.py COPYONLY)
configure_file(enkf.py enkf.py COPYONLY)
configure_file(libsuffix.py libsuffix.py COPYONLY)
configure_file(lenkf_rsm_py.py lenkf_rsm_py.py COPYONLY)
