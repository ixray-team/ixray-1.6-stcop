if(NOT DEFINED COOKER OR NOT EXISTS "${COOKER}")
    message(FATAL_ERROR "COOKER executable is missing: ${COOKER}")
endif()
if(NOT DEFINED INSPECTOR OR NOT EXISTS "${INSPECTOR}")
    message(FATAL_ERROR "Bundle inspector executable is missing: ${INSPECTOR}")
endif()
if(NOT DEFINED INPUT OR NOT IS_DIRECTORY "${INPUT}")
    message(FATAL_ERROR "Material input directory is missing: ${INPUT}")
endif()
if(NOT DEFINED OUTPUT)
    message(FATAL_ERROR "OUTPUT is required")
endif()
if(NOT DEFINED TEST_DEBUG_FLAG OR NOT TEST_DEBUG_FLAG STREQUAL "-rdbg")
    message(FATAL_ERROR "Material cooker tests must run with TEST_DEBUG_FLAG=-rdbg")
endif()

execute_process(
    COMMAND "${COOKER}" --input "${INPUT}" --output "${OUTPUT}" "${TEST_DEBUG_FLAG}"
    RESULT_VARIABLE COOK_RESULT
    OUTPUT_VARIABLE COOK_STDOUT
    ERROR_VARIABLE COOK_STDERR
)
if(NOT COOK_RESULT EQUAL 0)
    message(FATAL_ERROR "Cooker failed (${COOK_RESULT})\n${COOK_STDOUT}\n${COOK_STDERR}")
endif()
if(NOT COOK_STDOUT MATCHES "with [1-9][0-9]* DXIL/SPIR-V production/validation blob")
    message(FATAL_ERROR "Cooker did not report compiled shader blobs\n${COOK_STDOUT}")
endif()

execute_process(
    COMMAND "${INSPECTOR}" "${OUTPUT}" "${TEST_DEBUG_FLAG}"
    RESULT_VARIABLE INSPECT_RESULT
    OUTPUT_VARIABLE INSPECT_STDOUT
    ERROR_VARIABLE INSPECT_STDERR
)
if(NOT INSPECT_RESULT EQUAL 0)
    message(FATAL_ERROR "Cooked bundle inspection failed (${INSPECT_RESULT})\n${INSPECT_STDOUT}\n${INSPECT_STDERR}")
endif()

message(STATUS "${COOK_STDOUT}${INSPECT_STDOUT}")
