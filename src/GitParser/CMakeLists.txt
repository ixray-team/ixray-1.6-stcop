project(GitParser)

# Files import
file(GLOB_RECURSE GP_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
)

set(GP_SRC_FILES)

source_group("src" FILES ${GP_SOURCE_ALL_FILES})

# Apply list
list(APPEND GP_SRC_FILES ${GP_SOURCE_ALL_FILES})

# xrCDB project
add_executable(GitParser ${GP_SRC_FILES})

# Move precompiled header
set_target_properties(GitParser PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)