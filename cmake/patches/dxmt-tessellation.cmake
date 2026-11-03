# Applies the DXMT tessellation patch to the fetched DXMT checkout.
#
# FetchContent re-runs PATCH_COMMAND whenever it updates the checkout, so the
# script must tolerate an already patched tree (and a tree that was patched by
# hand while debugging).
set(PATCH "${CMAKE_CURRENT_LIST_DIR}/dxmt-tessellation.patch")

if(NOT EXISTS "${PATCH}")
    message(FATAL_ERROR "DXMT: ${PATCH} is missing")
endif()

execute_process(
    COMMAND git apply --reverse --check "${PATCH}"
    RESULT_VARIABLE already_applied
    OUTPUT_QUIET
    ERROR_QUIET
)

if(already_applied EQUAL 0)
    message(STATUS "DXMT: tessellation patch already applied")
    return()
endif()

execute_process(
    COMMAND git apply --ignore-whitespace "${PATCH}"
    RESULT_VARIABLE result
    ERROR_VARIABLE error
)

if(NOT result EQUAL 0)
    message(FATAL_ERROR "DXMT: failed to apply the tessellation patch\n${error}")
endif()

message(STATUS "DXMT: tessellation patch applied")
