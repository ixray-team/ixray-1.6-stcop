project(xrPlay)
set(XPLAY_SRC_FILES)

# Files import
file(GLOB_RECURSE XPLAY_SOURCE_ALL_FILES
    "*.*"
)

# File groups
source_group("kernel" FILES ${XPLAY_SOURCE_ALL_FILES})

# Apply list
list(APPEND XPLAY_SRC_FILES ${XPLAY_SOURCE_ALL_FILES})

# Remove borlands files
#list(FILTER CPIPE_SRC_FILES EXCLUDE REGEX "")

# xrCPU_Pipe project
add_executable(xrPlay WIN32 ${XPLAY_SOURCE_ALL_FILES})
target_include_directories(xrPlay PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrPlay PRIVATE "${IXRAY_SDK_INC}")

# project defines 
#target_compile_definitions(xrPlay PRIVATE XRPLAY_EXPORT)

add_compile_options(/fp:fast)
target_compile_definitions(xrPlay PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list 
target_link_libraries(xrPlay PUBLIC xrEngine)