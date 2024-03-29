cmake_minimum_required(VERSION 3.0.2)

project(fortran_exceptions)

enable_language(Fortran)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PROJECT_SOURCE_DIR}/cmake/modules/")

if(DEFINED CMAKE_Fortran_COMPILER_VERSION)
  set(Fortran_MODULE_DIRECTORY include/fortran_modules/${CMAKE_Fortran_COMPILER_ID}/${CMAKE_Fortran_COMPILER_VERSION})
else()
  set(Fortran_MODULE_DIRECTORY include/fortran_modules/${CMAKE_Fortran_COMPILER_ID})
endif()

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${Fortran_MODULE_DIRECTORY})

include(CMakePackageConfigHelpers)

add_subdirectory(src)

# Add all targets to the build-tree export set
export(TARGETS fortran_exceptions
  FILE "${PROJECT_BINARY_DIR}/fortran_exceptionsTargets.cmake")

# Export the package for use from the build-tree
# (this registers the build-tree with a global CMake-registry)
export(PACKAGE fortran_exceptions)

configure_package_config_file(${CMAKE_CURRENT_SOURCE_DIR}/Config.cmake.in
  "${CMAKE_CURRENT_BINARY_DIR}/fortran_exceptionsConfig.cmake"
  INSTALL_DESTINATION lib/cmake/fortran_exceptions
  )

install(FILES
          "${CMAKE_CURRENT_BINARY_DIR}/fortran_exceptionsConfig.cmake"
        DESTINATION lib/cmake/fortran_exceptions
)

install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION ${Fortran_MODULE_DIRECTORY})

install(EXPORT fortran_exceptions
  FILE fortran_exceptionsTargets.cmake
  DESTINATION lib/cmake/fortran_exceptions
)

# -------------------
# Build documentation
# -------------------

set ( SKIP_DOC_GEN FALSE CACHE BOOL
  "Disable building the API documentation with FORD" )
if ( NOT SKIP_DOC_GEN )
  find_program(FORD ford)

  find_package(PythonInterp)

  IF(NOT PYTHON_VERSION_STRING VERSION_LESS 3.5)
    set(MARKDOWN_SUPPORTED_PYTHON true)
  endif()

    if(NOT MARKDOWN_SUPPORTED_PYTHON)
    message(WARNING "Python version ${PYTHON_VERSION_STRING} is not supported by Python-Markdown, not attempting to build documentation.")
  endif()

  if(FORD AND MARKDOWN_SUPPORTED_PYTHON)

    set(DOC_ROOT "${PROJECT_BINARY_DIR}/doc")
    set(DOC_DIR "${DOC_ROOT}/fortran_exceptions")
    set(PROJ_DIR "${PROJECT_SOURCE_DIR}")
    set(FORD_PROJECT_FILE "${CMAKE_CURRENT_BINARY_DIR}/fortran_exceptions.md")
    set(MACRO_FLAG "")
    string(REPLACE ";" "\ninclude:" FORD_PROJECT_INCLUDES "${PROJ_DIR}/src")

    configure_file(fortran_exceptions.md.in fortran_exceptions.md)

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

    foreach ( DOC_SRC_FILE ${PROJECT_BINARY_DIR}/fortran_exceptions.md src/exceptions.f90)
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
      install ( DIRECTORY "${DOC_ROOT}/" DESTINATION share/doc )
    else ()

      # FORD Not found
      message ( WARNING
	"FORD not found. Please set the CMake cache variable FORD to point to the installed FORD executable, and reconfigure or disable building the documentation. FORD can be installed from PYPI with `sudo pip install FORD` or from <https://github.com/cmacmackin/ford> If you do not wish to install FORD and build the JSON-Fortran documentation, then please set the CMake cache variable SKIP_DOC_GEN to TRUE." )

    endif ()
  endif ()

endif()

function(list_transform_prepend var prefix)
    set(temp "")
    foreach(f ${${var}})
        list(APPEND temp "${prefix}${f}")
    endforeach()
    set(${var} "${temp}" PARENT_SCOPE)
endfunction()

find_program(FPRETTIFY_EXECUTABLE NAMES "fprettify" DOC "Path to fprettify")

if(NOT FPRETTIFY_EXECUTABLE)
  message(STATUS "fprettify not found.")
else()
  set(DO_FPRETTIFY "${FPRETTIFY_EXECUTABLE}" "--diff" "-i" "2" "-l" "81")

  set(ALL_SOURCES src/exceptions.f90)

  list_transform_prepend(ALL_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/)

  add_custom_target(
    fprettify
    COMMAND ${DO_FPRETTIFY}
    ${ALL_SOURCES}
    )

endif()

enable_testing()
add_subdirectory(tests)
