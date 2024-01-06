project(xrCore)
set(CORE_SRC_FILES)

# Files import
file(GLOB CORE_SOURCE_STL_FILES
    "*_std*.*"
    "*_stl*.*"
    "buffer_vector*.*"
    "intrusive_ptr*.*"
    "Fixed*.*"
)

file(GLOB CORE_SOURCE_MATH_FILES
    "vector.h"
    "_*.cpp"
    "_*.h"
)

file(GLOB CORE_SOURCE_LZO_FILES
    "lzo_*.*"
    "rt_lzo*.*"
)

file(GLOB CORE_SOURCE_LZ_FILES
    "LzHuf*.cpp"
    "lzhuf.h"
)

file(GLOB CORE_SOURCE_XML_FILES
    "XmlParser/*.cpp"
    "XmlParser/*.h"
)

file(GLOB CORE_SOURCE_MEMORY_FILES
    "xrMemory*.*"
    "memory_*.*"
    "*malloc*.*"
    "doug_lea_allocator.*"
)

file(GLOB CORE_SOURCE_DEBUG_FILES
    "xrDebug*.*"
    "log.*"
)

file(GLOB CORE_SOURCE_FS_FILES
    "FS*.*"
    "File*.*"
    "file*.*"
    "Locator*.*"
)

file(GLOB_RECURSE CORE_SOURCE_ALL_FILES CONFIGURE_DEPENDS
    "*.cpp"
    "*.c"
    "*.h"
)

file(GLOB CORE_SOURCE_RT_FILES "rt_c*.*")
file(GLOB CORE_SOURCE_PPMD_FILES "ppmd_*.*")
file(GLOB CORE_SOURCE_DISCORD_FILES "discord/*.*")
file(GLOB CORE_SOURCE_XR_API_FILES "API/*.*")
file(GLOB_RECURSE CORE_SOURCE_MEMORY_P3_FILES "ptmalloc3/*.*")

# NatVis config
file(GLOB_RECURSE CORE_NATVIS_FILES "../ixray.natvis")

source_group("ungrouped" FILES ${CORE_SOURCE_ALL_FILES})
source_group("core/api" FILES ${CORE_SOURCE_XR_API_FILES})
source_group("core/math" FILES ${CORE_SOURCE_MATH_FILES})
source_group("memory" FILES ${CORE_SOURCE_MEMORY_FILES})
source_group("memory/p3malloc" FILES ${CORE_SOURCE_MEMORY_P3_FILES})
source_group("debug" FILES ${CORE_SOURCE_DEBUG_FILES})
source_group("compressors/rt" FILES ${CORE_SOURCE_RT_FILES})
source_group("compressors/lz" FILES ${CORE_SOURCE_LZ_FILES})
source_group("compressors/lzo" FILES ${CORE_SOURCE_LZO_FILES})
source_group("compressors/ppmd" FILES ${CORE_SOURCE_PPMD_FILES})
source_group("core/fs" FILES ${CORE_SOURCE_FS_FILES})
source_group("core/stl" FILES ${CORE_SOURCE_STL_FILES})
source_group("core/XML" FILES ${CORE_SOURCE_XML_FILES})
source_group("discord" FILES ${CORE_SOURCE_DISCORD_FILES})
source_group("discord/api" FILES ${CORE_SOURCE_DISCORD_API_FILES})

# Apply list
list(APPEND CORE_SRC_FILES ${CORE_SOURCE_ALL_FILES})
list(APPEND CORE_SRC_FILES ${CORE_NATVIS_FILES})

# Remove borlands files
list(FILTER CORE_SRC_FILES EXCLUDE REGEX  "ELocatorAPI.*")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "LocatorAPI_Notifications.cpp$")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "xrMemory_subst_borland.*")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "FileSystem_borland.cpp")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "blackbox/*")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "ptmalloc3/t-test*.*")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "ptmalloc3/ptmalloc3.c")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "ptmalloc3/tst-independent-alloc.c")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "rt_lzo*.cpp")

# xrCore project
add_library(xrCore SHARED ${CORE_SRC_FILES})
target_include_directories(xrCore PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrCore PUBLIC "${IXRAY_SDK_INC}")
target_link_directories(xrCore PUBLIC "${IXRAY_SDK_LIB}")

if (IXRAY_ASAN)
    add_compile_definitions(PURE_ONLY)
endif()

# Move precompiled header
set_target_properties(xrCore PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
set_target_properties(xrCore PROPERTIES COMPILE_FLAGS "/Yustdafx.h")
set_source_files_properties(stdafx.cpp PROPERTIES COMPILE_FLAGS "/Ycstdafx.h")
target_precompile_headers(xrCore PUBLIC "stdafx.h")

file(GLOB_RECURSE CORE_SOURCE_PCH_FILES "stdafx.*")
file(GLOB_RECURSE CORE_SOURCE_ALL_C_FILES "*.c")

set_source_files_properties(${CORE_SOURCE_ALL_C_FILES} PROPERTIES SKIP_PRECOMPILE_HEADERS ON)
source_group("pch" FILES ${CORE_SOURCE_PCH_FILES})

# Project defines
target_compile_definitions(xrCore PRIVATE XRCORE_EXPORTS)
target_compile_definitions(xrCore PRIVATE _WINDOWS)
target_compile_definitions(xrCore PRIVATE _USRDLL)

## Disable user-cheats for Shipping build
target_compile_definitions(xrCore PUBLIC "$<$<CONFIG:Debug>:DEBUG_DRAW>")
target_compile_definitions(xrCore PUBLIC "$<$<CONFIG:RelWithDebInfo>:DEBUG_DRAW>")
target_compile_definitions(xrCore PUBLIC "$<$<CONFIG:Release>:MASTER_GOLD> $<$<CONFIG:Release>:DISABLE_PROFILER>")
if (DEVIXRAY_ENABLE_SHIPPING)
    target_compile_definitions(xrCore PUBLIC "$<$<CONFIG:Shipping>:MASTER_GOLD> $<$<CONFIG:Shipping>:DISABLE_PROFILER>")
    target_compile_definitions(xrCore PUBLIC "$<$<CONFIG:Shipping>:SHIPPING_BUILD>")
endif()

# Linker list
target_link_libraries(xrCore PRIVATE "dbghelp.lib")
target_link_libraries(xrCore PRIVATE "discord_game_sdk.dll.lib")
target_link_libraries(xrCore PRIVATE dxerr)
target_link_options(xrCore PUBLIC "$<$<CONFIG:DEBUG>:/SAFESEH:NO>")

add_compile_options(/fp:fast)
target_compile_definitions(xrCore PUBLIC "$<$<CONFIG:Debug>:DEBUG>")

# Nuget
set(CORE_MARL ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Marl.2023.11.29-open/)
set(CORE_OPT ${CMAKE_BINARY_DIR}/packages/ImeSense.Packages.Optick.1.4.0/)

add_custom_command(TARGET ${PROJECT_NAME}
    PRE_BUILD
    COMMAND ${NUGET_COMMAND} restore ${CMAKE_CURRENT_SOURCE_DIR}/Packages.config -SolutionDirectory ${CMAKE_BINARY_DIR}
)

add_custom_command(TARGET ${PROJECT_NAME}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CORE_MARL}native/bin/${CMAKE_VS_PLATFORM_NAME}/Release/marl.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CORE_OPT}native/bin/${CMAKE_VS_PLATFORM_NAME}/Release/OptickCore.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${IXRAY_SDK_BIN}/discord_game_sdk.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
)

# Marl
target_link_libraries(xrCore PUBLIC ${CORE_MARL}native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/marl.lib)
target_include_directories(xrCore PUBLIC "${CORE_MARL}/native/include/")

# Optick
target_link_libraries(xrCore PUBLIC ${CORE_OPT}native/lib/${CMAKE_VS_PLATFORM_NAME}/Release/OptickCore.lib)
target_include_directories(xrCore PUBLIC "${CORE_OPT}/native/include/")

# Get author name
execute_process(
    COMMAND git log -1 --pretty=format:%an
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    OUTPUT_VARIABLE AUTHOR
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
# Get current branch
execute_process(
    COMMAND git branch --show-current
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    OUTPUT_VARIABLE BRANCH
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
# Get hash of the latest commit
execute_process(
    COMMAND git log --pretty=format:%h -n 1
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    OUTPUT_VARIABLE HASH
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
# Define a variable for CI_BUILD
set(CI_BUILD 0)
# Create git_version.h file
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/git_version.h.in
    ${CMAKE_CURRENT_SOURCE_DIR}/git_version.h
)

if (IXRAY_USE_OPTICK)
    target_compile_definitions(xrCore PUBLIC "USE_OPTICK")
endif()

if (CI_IXRAY_UTILS OR CI_IXRAY_COMPRESSOR_ONLY)
    target_compile_definitions(xrCore PUBLIC "IXR_CI_BUILD")
endif()