
# Split command and args
message("CMD=${CMD}")
set(ARGS ${CMD})
list(GET ARGS 0 CMD_EXECUTABLE)
list(REMOVE_AT ARGS 0)

# Run the command
execute_process(COMMAND ${CMD_EXECUTABLE} ${ARGS}
  RESULT_VARIABLE CMD_RESULT
  OUTPUT_VARIABLE OUTPUT
  ERROR_VARIABLE OUTPUT)

message("${OUTPUT}")

# Check whether OUTPUT matches PASS_PATTERN
if(DEFINED PASS_PATTERN)
  string(REGEX MATCH "${PASS_PATTERN}" PASS_MATCH ${OUTPUT})
endif()

# Check whether OUTPUT matches FAIL_PATTERN
if(DEFINED FAIL_PATTERN AND OUTPUT)
  string(REGEX MATCH "${FAIL_PATTERN}" FAIL_MATCH ${OUTPUT})
endif()

if(${WILL_FAIL})

  message("${CMD} returned ${CMD_RESULT}")

  if(${CMD_RESULT} EQUAL 0)
    message(FATAL_ERROR "${CMD_EXECUTABLE} should have failed, but didn't")
  endif()

  if(DEFINED FAIL_PATTERN AND NOT FAIL_MATCH)
    message(FATAL_ERROR "${CMD_EXECUTABLE} output did not match pattern \"${FAIL_PATTERN}\"")
  endif()

  if(DEFINED PASS_PATTERN AND PASS_MATCH)
    message(FATAL_ERROR "${CMD_EXECUTABLE} output matched pattern ${PASS_PATTERN}")
  endif()

else()

  if(NOT ${CMD_RESULT} EQUAL 0)
    message(FATAL_ERROR "Error running ${CMD_EXECUTABLE}, exit status ${CMD_RESULT}")
  endif()

  if(DEFINED FAIL_PATTERN AND FAIL_MATCH)
    message(FATAL_ERROR "${CMD_EXECUTABLE} output matched pattern ${FAIL_PATTERN}")
  endif()

  if(DEFINED PASS_PATTERN AND NOT PASS_MATCH)
    message(FATAL_ERROR "${CMD_EXECUTABLE} output did not match pattern ${PASS_PATTERN}")
  endif()

endif()
