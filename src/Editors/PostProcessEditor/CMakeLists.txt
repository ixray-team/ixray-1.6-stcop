project(PPEditor)
set(PPE__SRC_FILES)

# Files import
file(GLOB_RECURSE PPE__SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.*"
    "../../xrGame/PostprocessAnimator.*"
)
file(GLOB_RECURSE PPE__SOURCE_CPP_FILES
    "*.cpp"
    "*.h"
)
file(GLOB_RECURSE PPE__SOURCE_RES_FILES
    "*.ico"
    "*.rc"
    "*.bmp"
)
file(GLOB_RECURSE PPE__SOURCE_UI_FILES
    "UI*.cpp"
    "UI*.h"
)

# File groups
source_group("Source Files" FILES ${PPE__SOURCE_CPP_FILES})
source_group("Resources" FILES ${PPE__SOURCE_RES_FILES})
source_group("UI" FILES ${PPE__SOURCE_UI_FILES})

# Apply list
list(APPEND PPE__SRC_FILES ${PPE__SOURCE_ALL_FILES})

# Remove files
#list(FILTER CPIPE_SRC_FILES EXCLUDE REGEX "")

# PPEditor project
add_executable(PPEditor ${PPE__SOURCE_ALL_FILES})
target_include_directories(PPEditor PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(PPEditor PRIVATE "${IXRAY_SDK_INC}")
target_compile_definitions(PPEditor PRIVATE _PP_EDITOR_)

# Linker list
target_link_libraries(PPEditor PUBLIC xrAbstractions)
target_link_libraries(PPEditor PUBLIC xrEUI)