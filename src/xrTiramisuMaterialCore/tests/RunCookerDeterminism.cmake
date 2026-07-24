if(NOT DEFINED COOKER OR NOT EXISTS "${COOKER}")
    message(FATAL_ERROR "COOKER executable is missing: ${COOKER}")
endif()

if(NOT DEFINED INPUT OR NOT IS_DIRECTORY "${INPUT}")
    message(FATAL_ERROR "Material input directory is missing: ${INPUT}")
endif()

if(NOT DEFINED OUTPUT_DIR)
    message(FATAL_ERROR "OUTPUT_DIR is required")
endif()
if(NOT DEFINED TEST_DEBUG_FLAG OR NOT TEST_DEBUG_FLAG STREQUAL "-rdbg")
    message(FATAL_ERROR "Material cooker tests must run with TEST_DEBUG_FLAG=-rdbg")
endif()

file(MAKE_DIRECTORY "${OUTPUT_DIR}")
set(OUTPUT_A "${OUTPUT_DIR}/materials-a.xrm")
set(OUTPUT_B "${OUTPUT_DIR}/materials-b.xrm")

execute_process(
    COMMAND "${COOKER}" --input "${INPUT}" --output "${OUTPUT_A}" "${TEST_DEBUG_FLAG}"
    RESULT_VARIABLE RESULT_A
    OUTPUT_VARIABLE STDOUT_A
    ERROR_VARIABLE STDERR_A
)
if(NOT RESULT_A EQUAL 0)
    message(FATAL_ERROR "First cooker run failed (${RESULT_A})\n${STDOUT_A}\n${STDERR_A}")
endif()

execute_process(
    COMMAND "${COOKER}" --input "${INPUT}" --output "${OUTPUT_B}" "${TEST_DEBUG_FLAG}"
    RESULT_VARIABLE RESULT_B
    OUTPUT_VARIABLE STDOUT_B
    ERROR_VARIABLE STDERR_B
)
if(NOT RESULT_B EQUAL 0)
    message(FATAL_ERROR "Second cooker run failed (${RESULT_B})\n${STDOUT_B}\n${STDERR_B}")
endif()

file(SHA256 "${OUTPUT_A}" HASH_A)
file(SHA256 "${OUTPUT_B}" HASH_B)
if(NOT HASH_A STREQUAL HASH_B)
    message(FATAL_ERROR "Cooker output is not deterministic: ${HASH_A} != ${HASH_B}")
endif()

message(STATUS "Deterministic material bundle SHA-256: ${HASH_A}")
