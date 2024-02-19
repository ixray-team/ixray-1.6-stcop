project(xrEngine)
set(XPLAY_SRC_FILES)

# Files import
file(GLOB_RECURSE XPLAY_SOURCE_ALL_FILES CONFIGURE_DEPENDS
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

if (WIN32)
    add_dependencies(xrEngine xrGame xrRender_R4)
    if(IXRAY_USE_R1)
        add_dependencies(xrEngine xrRender_R1)
    endif()

    if(IXRAY_USE_R2)
        add_dependencies(xrEngine xrRender_R2)
    endif()
endif()

if(IXRAY_MP)
    add_dependencies(xrEngine xrGameSpy)
endif()
