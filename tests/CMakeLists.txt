add_library(dummy_model_interfaces dummy_model_interfaces.F90)
target_link_libraries(dummy_model_interfaces PRIVATE assim_parallel system_mpi random)
configure_mpi_target(dummy_model_interfaces)

add_executable(dummy_assimilator_tests dummy_assimilator_tests.F90)
target_link_libraries(dummy_assimilator_tests PRIVATE assim)

add_library(model_interface_tests model_interface_tests.f90)
target_link_libraries(model_interface_tests PRIVATE assim_parallel system_mpi random)

add_executable(dummy_model_interface_tests dummy_model_interface_tests.f90)
target_link_libraries(dummy_model_interface_tests PRIVATE model_interface_tests system_mpi dummy_model_interfaces)

add_test(test_dummy_model_interface ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 4 ./dummy_model_interface_tests)

add_executable(advect1d_model_interface_tests advect1d_model_interface_tests.f90)
target_link_libraries(advect1d_model_interface_tests PRIVATE model_interface_tests system_mpi advect1d_assimilate_interfaces)
configure_mpi_target(advect1d_model_interface_tests)
configure_hdf5_target(advect1d_model_interface_tests)

add_test(test_advect1d_model_interface ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 4 ./advect1d_model_interface_tests)

add_test(test_dummy_assimilator dummy_assimilator_tests)

add_executable(batch_manager_tests batch_manager_tests.F90)
target_link_libraries(batch_manager_tests PRIVATE dummy_model_interfaces system_mpi assim assim_parallel)

add_test(test_batch_manager ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 4 ./batch_manager_tests)

add_executable(util_tests util_tests.f90)
target_link_libraries(util_tests util)

add_test(test_util ./util_tests)

add_library(exception_tests exception_tests.f90)
target_link_libraries(exception_tests util)

add_executable(exception_test_pass test_exception_pass.f90)
target_link_libraries(exception_test_pass exception_tests)

add_test(test_exception_pass exception_test_pass)

add_executable(exception_test_fail test_exception_fail.f90)
target_link_libraries(exception_test_fail exception_tests)

add_test(NAME test_exception_fail
  COMMAND ${CMAKE_COMMAND}
  -DCMD=$<TARGET_FILE:exception_test_fail>
  -DFAIL_PATTERN=Unhandled\ exception
  -DWILL_FAIL=1
  -P ${CMAKE_CURRENT_SOURCE_DIR}/test_and_check_regex.cmake)

add_executable(exception_test_fail_implicit test_exception_fail_implicit.f90)
target_link_libraries(exception_test_fail_implicit exception_tests)

add_test(NAME test_exception_fail_implicit
  COMMAND ${CMAKE_COMMAND}
  -DCMD=$<TARGET_FILE:exception_test_fail_implicit>
  -DFAIL_PATTERN=Unhandled\ exception
  -DWILL_FAIL=1
  -P ${CMAKE_CURRENT_SOURCE_DIR}/test_and_check_regex.cmake)

add_library(darray_tests darray_tests.F90)
target_link_libraries(darray_tests util)

add_executable(darray_test test_darray.F90)
target_link_libraries(darray_test PUBLIC darray_tests)
configure_mpi_target(darray_test)

add_test(test_darray darray_test ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 4)
