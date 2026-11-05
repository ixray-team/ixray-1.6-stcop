project(MaxTools)

# Files import
file(GLOB_RECURSE MaxTools_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.cxx"
    "*.h"
)

set(MaxTools_SRC_FILES)

source_group("src" FILES ${MaxTools_SOURCE_ALL_FILES})

# Apply list
list(APPEND MaxTools_SRC_FILES ${MaxTools_SOURCE_ALL_FILES})

# Remove unused files
#list(FILTER MaxTools_SRC_FILES EXCLUDE REGEX "cl_raypick.cpp$")

# MaxTools project
add_library(MaxTools SHARED ${MaxTools_SRC_FILES})
target_include_directories(MaxTools PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(MaxTools PRIVATE "${IXR_3DS_MAX_SDK}/include")
target_link_directories(MaxTools PRIVATE "${IXR_3DS_MAX_SDK}/lib/x64/Release")
set_target_properties(MaxTools PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)

set_target_properties(
    MaxTools
    PROPERTIES
        OUTPUT_NAME "ixray_max_tools"
        SUFFIX ".dlu"
)

target_link_libraries(MaxTools PUBLIC PCore)
target_link_libraries(MaxTools PUBLIC geom)
target_link_libraries(MaxTools PUBLIC bmm)
target_link_libraries(MaxTools PUBLIC gfx)
target_link_libraries(MaxTools PUBLIC mesh)
target_link_libraries(MaxTools PUBLIC maxutil)
target_link_libraries(MaxTools PUBLIC maxscrpt)
target_link_libraries(MaxTools PUBLIC manipsys)
target_link_libraries(MaxTools PUBLIC paramblk2)
target_link_libraries(MaxTools PUBLIC core)
target_link_libraries(MaxTools PUBLIC comctl32)