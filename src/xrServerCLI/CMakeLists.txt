project(xrServerCLI)
set(XRDS_SRC_FILES)

# Files import
file(GLOB_RECURSE XRDS_SOURCE_ALL_FILES
	"*.*"
)

# File groups
source_group("core" FILES ${XRDS_SOURCE_ALL_FILES})

# Apply list
list(APPEND XRDS_SRC_FILES ${XRDS_SOURCE_ALL_FILES})

# Remove files
#list(FILTER CPIPE_SRC_FILES EXCLUDE REGEX "")

# xrServerCLI project
add_executable(xrServerCLI WIN32 ${XRDS_SRC_FILES})
if (MSVC)
    target_link_options(xrServerCLI PRIVATE /SUBSYSTEM:CONSOLE)
endif()
target_include_directories(xrServerCLI PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrServerCLI PRIVATE "${IXRAY_SDK_INC}")

# Precompiled header
target_precompile_headers(xrServerCLI PRIVATE "stdafx.h")

# Project defines
target_compile_definitions(xrServerCLI PRIVATE xrServerCLI_EXPORT)

# Linker list
target_link_libraries(xrServerCLI PUBLIC xrEngineCore)
add_dependencies(xrServerCLI xrRender_DS0)
add_dependencies(xrServerCLI xrGame)
add_dependencies(xrServerCLI xrGameSpy)
