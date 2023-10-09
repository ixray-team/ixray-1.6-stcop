project(xrEngine)
set(XPLAY_SRC_FILES)

# Files import
file(GLOB_RECURSE XPLAY_SOURCE_ALL_FILES
    "*.*"
)
file(GLOB_RECURSE XPLAY_SOURCE_CPP_FILES
    "*.cpp"
    "*.h"
)
file(GLOB_RECURSE XPLAY_SOURCE_RES_FILES
    "*.ico"
    "*.rc"
    "*.bmp"
)

# File groups
source_group("Source Files" FILES ${XPLAY_SOURCE_CPP_FILES})
source_group("Resources" FILES ${XPLAY_SOURCE_RES_FILES})

# Apply list
list(APPEND XPLAY_SRC_FILES ${XPLAY_SOURCE_ALL_FILES})

# Remove files
#list(FILTER CPIPE_SRC_FILES EXCLUDE REGEX "")

# xrEngine project
add_executable(xrEngine WIN32 ${XPLAY_SOURCE_ALL_FILES})
target_include_directories(xrEngine PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrEngine PRIVATE "${IXRAY_SDK_INC}")

# Project defines
#target_compile_definitions(xrEngine PRIVATE XRPLAY_EXPORT)

add_compile_options(/fp:fast)
target_compile_definitions(xrEngine PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrEngine PUBLIC xrAbstractions)
add_dependencies(xrEngine xrGame xrGameSpy xrRender_R4)
