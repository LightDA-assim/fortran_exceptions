if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER 9)
    # Compiler doesn't support overriding finalizers
  else()
    # Compiler supports overriding finalizers
    add_definitions(-DOVERRIDABLE_FINALIZERS)
  endif()
else()
  # Compiler supports overriding finalizers
  add_definitions(-DOVERRIDABLE_FINALIZERS)
endif()

add_library(fortran_exceptions SHARED exceptions.f90)
target_include_directories(fortran_exceptions PUBLIC ${CMAKE_CURRENT_BINARY_DIR})
target_include_directories(fortran_exceptions INTERFACE
  $<INSTALL_INTERFACE:${Fortran_MODULE_DIRECTORY}>)
install(TARGETS fortran_exceptions EXPORT system_mpi)
