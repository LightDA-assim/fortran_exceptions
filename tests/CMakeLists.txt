
if(FPRETTIFY_EXECUTABLE)

  set(FPRETTIFY_CMD ${DO_FPRETTIFY})
  list(APPEND FPRETTIFY_CMD "${ALL_SOURCES}")
  string(REPLACE ";" "\;" FPRETTIFY_CMD "${FPRETTIFY_CMD}")
  add_test(NAME test_fprettify
    COMMAND ${CMAKE_COMMAND}
    -DCMD=${FPRETTIFY_CMD}
    -DFAIL_PATTERN=.
    -P ${CMAKE_CURRENT_SOURCE_DIR}/test_and_check_regex.cmake)
endif()

add_library(exception_tests exception_tests.f90)
target_link_libraries(exception_tests fortran_exceptions)

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

add_custom_target(all_tests ALL
  DEPENDS exception_tests exception_test_pass exception_test_fail
  exception_test_fail_implicit)

add_custom_command(
     TARGET all_tests
     COMMENT "Run tests"
     POST_BUILD
     WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
     COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure -C $<CONFIGURATION>
)
