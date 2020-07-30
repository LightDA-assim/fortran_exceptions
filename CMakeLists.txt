cmake_minimum_required(VERSION 3.10)

project(lenkf-rsm)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PROJECT_SOURCE_DIR}/cmake/modules/")

include(lenkf_functions)

find_package(LAPACK REQUIRED)

set(SYSTEM_MPI_SOURCES system_mpi.F90)
add_library(system_mpi SHARED ${SYSTEM_MPI_SOURCES})
set_target_properties(system_mpi PROPERTIES PUBLIC_HEADER "mpi_types.h")
configure_mpi_target(system_mpi)
target_include_directories(system_mpi PUBLIC ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR})

set(UTIL_SOURCES util.F90)
add_library(util STATIC ${UTIL_SOURCES})
target_include_directories(util PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_link_libraries(util PUBLIC system_mpi)
configure_mpi_target(util)

set(ASSIM_SOURCES lenkf_rsm.f90 lenkf_rsm_c.f90 dummy_assimilator.f90)
add_library(assim SHARED ${ASSIM_SOURCES})
target_link_libraries(assim PRIVATE ${LAPACK_LIBRARIES})
target_include_directories(assim PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

set(RANDOM_SOURCES random.f90 random_integer.f90)
add_library(random SHARED ${RANDOM_SOURCES})

add_library(localization SHARED localization.f90)

set(ASSIM_PARALLEL_SOURCES assimilate.F90 assimilation_batch_manager.F90 assimilation_model_interface.f90)
add_library(assim_parallel SHARED ${ASSIM_PARALLEL_SOURCES})
target_link_libraries(assim_parallel PRIVATE random assim system_mpi util)
configure_mpi_target(assim_parallel)
target_include_directories(assim_parallel PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

set(ADVECT1D_ASSIMILATE_INTERFACES_SOURCES advect1d_assimilate_interfaces.F90)
add_library(advect1d_assimilate_interfaces ${ADVECT1D_ASSIMILATE_INTERFACES_SOURCES})
target_link_libraries(advect1d_assimilate_interfaces PRIVATE assim assim_parallel localization system_mpi)
configure_hdf5_target(advect1d_assimilate_interfaces)
target_include_directories(advect1d_assimilate_interfaces PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

set(ADVECT1D_ASSIMILATE_SOURCES advect1d_assimilate.f90)
add_executable(advect1d_assimilate ${ADVECT1D_ASSIMILATE_SOURCES})
target_link_libraries(advect1d_assimilate PRIVATE assim assim_parallel localization system_mpi advect1d_assimilate_interfaces)
configure_mpi_target(advect1d_assimilate)
set_source_files_properties(advect1d_assimilate_interfaces.F90 PROPERTIES COMPILE_FLAGS -cpp)

set(ADVECT1D_SOURCES advect1d.f90)
add_library(advect1d SHARED ${ADVECT1D_SOURCES})

find_package(F2PY REQUIRED)

configure_file(animate.py animate.py COPYONLY)
configure_file(animate_mpi.py animate_mpi.py COPYONLY)
configure_file(advect1d.py advect1d.py COPYONLY)
configure_file(enkf.py enkf.py COPYONLY)
configure_file(libsuffix.py libsuffix.py COPYONLY)
configure_file(lenkf_rsm_py.py lenkf_rsm_py.py COPYONLY)

enable_testing()
add_subdirectory(tests)

# -------------------
# Build documentation
# -------------------

set ( SKIP_DOC_GEN FALSE CACHE BOOL
  "Disable building the API documentation with FORD" )
if ( NOT SKIP_DOC_GEN )
  find_program(FORD ford)

  if(FORD)

    set(DOC_DIR "${PROJECT_BINARY_DIR}/doc")
    set(PROJ_DIR "${PROJECT_SOURCE_DIR}")
    set(FORD_PROJECT_FILE "${CMAKE_CURRENT_BINARY_DIR}/lenkf-rsm.md")
    set(MACRO_FLAG "")
    string(REPLACE ";" "\ninclude:" FORD_PROJECT_INCLUDES "${MPI_Fortran_INCLUDE_DIRS};${PROJ_DIR}")

    configure_file(lenkf-rsm.md.in lenkf-rsm.md)

    # Pick the preprocessor to use based on the Fortran compiler
    if ( "${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel" )
      set ( FPP "fpp\n" )
    else ()
      set ( FPP "gfortran -E\n" ) # default to gfortran -E for gfortran and unsupported compilers
    endif ()
    file ( WRITE "${PROJECT_BINARY_DIR}/.PREPROCESSOR" "${FPP}" )

    # Dynamically generate the FORD outputs list
    message ( STATUS "Dynamically computing FORD output information..." )
    if ( NOT (DEFINED FORD_OUTPUTS_CACHED) )
      message ( STATUS "Running FORD to dynamically compute documentation outputs, this could take a while..." )
      execute_process ( COMMAND ${CMAKE_COMMAND} -E remove_directory ${DOC_DIR}
	COMMAND ${CMAKE_COMMAND} -E make_directory ${DOC_DIR}
	COMMAND "${FORD}" --debug -q ${MACRO_FLAG} -d "${PROJ_DIR}" -o "${DOC_DIR}" -p "${PAGE_DIR}" "${FORD_PROJECT_FILE}" OUTPUT_QUIET )
    else ()
      message ( STATUS "Re-using cached FORD outputs, rather than regenerating them" )
    endif()

    # Compile a list of FORD output files
    file ( GLOB_RECURSE FORD_OUTPUTS
      "${DOC_DIR}/*.*" )
    file ( GLOB_RECURSE FORD_CLEAN_OUTPUTS
      "${DOC_DIR}/*.*" )

    # Cache FORD outputs
    if ( (DEFINED FORD_OUTPUTS) AND ( NOT ( "${FORD_OUTPUTS}" STREQUAL "" ) ) )
      message ( STATUS "Caching FORD outputs" )
      set ( FORD_OUTPUTS_CACHED "${FORD_OUTPUTS}"
	CACHE STRING "variable containing FORD outputs to prevent rebuilding FORD docs" FORCE )
    endif ()
    message ( STATUS "Done dynamically computing FORD outputs." )

    foreach ( DOC_SRC_FILE ${PROJECT_BINARY_DIR}/lenkf-rsm.md ${SYSTEM_MPI_SOURCES} ${UTIL_SOURCES} ${ASSIM_SOURCES} ${ASSIM_PARALLEL_SOURCES} ${ADVECT1D_ASSIMILATE_INTERFACES_SOURCES} ${ADVECT1D_ASSIMILATE_SOURCES} ${ADVECT1D_SOURCES} ${PAGES_FILES} )
      list ( APPEND FORD_DEPENDS "${DOC_SRC_FILE}" )
    endforeach ()

    # Re-build FORD output if needed
    add_custom_command ( OUTPUT ${FORD_OUTPUTS_CACHED}
      COMMAND "${FORD}" --debug ${MACRO_FLAG} -d "${PROJ_DIR}" -o "${DOC_DIR}" "${FORD_PROJECT_FILE}"
      MAIN_DEPENDENCY "${FORD_PROJECT_FILE}"
      DEPENDS ${FORD_DEPENDS}
      COMMENT "Building HTML documentation for ${PROJECT_NAME} using FORD" )

    add_custom_target ( documentation ALL
      DEPENDS ${FORD_OUTPUTS_CACHED} )

    # Install documentation
    set ( INSTALL_API_DOCUMENTATION TRUE
      CACHE BOOL "Install FORD generated documentation?" )
    if ( INSTALL_API_DOCUMENTATION )
      if ( USE_GNU_INSTALL_CONVENTION )
	install ( DIRECTORY "${DOC_DIR}/" DESTINATION "${CMAKE_INSTALL_DOCDIR}" )
      else ()
	install ( DIRECTORY "${DOC_DIR}/" DESTINATION "${CMAKE_INSTALL_PREFIX}/${PACKAGE_VERSION}/doc" )
      endif ()

    else ()

      # FORD Not found
      message ( WARNING
	"FORD not found. Please set the CMake cache variable FORD to point to the installed FORD executable, and reconfigure or disable building the documentation. FORD can be installed from PYPI with `sudo pip install FORD` or from <https://github.com/cmacmackin/ford> If you do not wish to install FORD and build the JSON-Fortran documentation, then please set the CMake cache variable SKIP_DOC_GEN to TRUE." )

    endif ()
  endif ()

endif()
