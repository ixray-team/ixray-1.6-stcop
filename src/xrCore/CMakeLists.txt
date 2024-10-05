project(xrCore)
set(CORE_SRC_FILES)

# Files import
file(GLOB CORE_SOURCE_STL_FILES
    "*_std*.*"
    "*_stl*.*"
    "buffer_vector*.*"
    "RingBuffer.*"
    "fastdelegate.*"
    "intrusive_ptr*.*"
    "associative_vector*.*"
    "Fixed*.*"
)

file(GLOB CORE_SOURCE_MATH_FILES
    "vector.h"
    "_*.cpp"
    "_*.h"
)

file(GLOB CORE_SOURCE_LZ_FILES
    "LzHuf*.cpp"
    "lzhuf.h"
)

file(GLOB CORE_SOURCE_XML_FILES
    "XmlParser/*.cpp"
    "XmlParser/*.h"
)

file(GLOB CORE_SOURCE_STACK_FILES
    "StackTrace/*.h"
)

file(GLOB CORE_SOURCE_MEMORY_FILES
    "xrMemory*.*"
    "memory_*.*"
    "xrPool.*"
    "*malloc*.*"
    "doug_lea_allocator.*"
)

file(GLOB CORE_SOURCE_DEBUG_FILES
    "xrDebug*.*"
    "log.*"
)

file(GLOB CORE_SOURCE_STR_FILES
    "xrstring*.*"
    "shared_string.*"
    "xr_trims.*"
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
file(GLOB_RECURSE CORE_SOURCE_DISCORD_FILES "discord/*.*")
file(GLOB CORE_SOURCE_XR_API_FILES "API/*.*")
file(GLOB CORE_SOURCE_XR_API_PLATF_FILES "Platform/*.*")
file(GLOB CORE_SOURCE_XR_API_WIN_FILES "Platform/Windows/*.*")
file(GLOB CORE_SOURCE_XR_API_LIN_FILES "Platform/Linux/*.*")
file(GLOB_RECURSE CORE_SOURCE_MEMORY_P3_FILES "ptmalloc3/*.*")

# NatVis config
file(GLOB_RECURSE CORE_NATVIS_FILES "../ixray.natvis")
file(GLOB_RECURSE CORE_NUGET_FILES "Packages.config")

source_group("ungrouped" FILES ${CORE_SOURCE_ALL_FILES})
source_group("core/api" FILES ${CORE_SOURCE_XR_API_FILES})
source_group("core/math" FILES ${CORE_SOURCE_MATH_FILES})
source_group("core/string" FILES ${CORE_SOURCE_STR_FILES})
source_group("platform" FILES ${CORE_SOURCE_XR_API_PLATF_FILES})
source_group("platform/windows" FILES ${CORE_SOURCE_XR_API_WIN_FILES})
source_group("platform/linux" FILES ${CORE_SOURCE_XR_API_LIN_FILES})
source_group("memory" FILES ${CORE_SOURCE_MEMORY_FILES})
source_group("memory/p3malloc" FILES ${CORE_SOURCE_MEMORY_P3_FILES})
source_group("debug" FILES ${CORE_SOURCE_DEBUG_FILES})
source_group("compressors/rt" FILES ${CORE_SOURCE_RT_FILES})
source_group("compressors/lz" FILES ${CORE_SOURCE_LZ_FILES})
source_group("compressors/ppmd" FILES ${CORE_SOURCE_PPMD_FILES})
source_group("core/fs" FILES ${CORE_SOURCE_FS_FILES})
source_group("core/stl" FILES ${CORE_SOURCE_STL_FILES})
source_group("core/XML" FILES ${CORE_SOURCE_XML_FILES})
source_group("core/StackTrace" FILES ${CORE_SOURCE_STACK_FILES})
source_group("discord" FILES ${CORE_SOURCE_DISCORD_FILES})
source_group("discord/api" FILES ${CORE_SOURCE_DISCORD_API_FILES})

# Apply list
list(APPEND CORE_SRC_FILES ${CORE_SOURCE_ALL_FILES})
list(APPEND CORE_SRC_FILES ${CORE_NUGET_FILES})

if (MSVC)
	list(APPEND CORE_SRC_FILES ${CORE_NATVIS_FILES})
endif()

# Remove borlands files
list(FILTER CORE_SRC_FILES EXCLUDE REGEX  "ELocatorAPI.*")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "LocatorAPI_Notifications.cpp$")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "xrMemory_subst_borland.*")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "FileSystem_borland.cpp")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "blackbox/*")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "ptmalloc3/t-test*.*")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "ptmalloc3/ptmalloc3.c")
list(FILTER CORE_SRC_FILES EXCLUDE REGEX "ptmalloc3/tst-independent-alloc.c")

if (MSVC)
    list(FILTER CORE_SRC_FILES EXCLUDE REGEX "Platform/Linux/")
else()
    list(FILTER CORE_SRC_FILES EXCLUDE REGEX "Platform/Windows/")
    list(FILTER CORE_SRC_FILES EXCLUDE REGEX "discord/api/")
endif()

# xrCore project
add_library(xrCore SHARED ${CORE_SRC_FILES})
target_link_directories(xrCore PUBLIC ${IXRAY_SDK_LIB})

if(IXRAY_ASAN)
    target_compile_definitions(xrCore PRIVATE PURE_ONLY)
endif()

# Includes 
target_include_directories(xrCore PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(xrCore PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/../3rd party/lzo/include")
target_include_directories(xrCore PUBLIC ${IXRAY_SDK_INC})
target_include_directories(xrCore PUBLIC ${CMAKE_BINARY_DIR}/dep)
target_include_directories(xrCore PUBLIC ${CORE_DXMATH}/include/)
target_include_directories(xrCore PUBLIC ${CORE_SDL3}build/native/include/)
target_include_directories(xrCore PUBLIC ${IXR_TBB_INC})
target_include_directories(xrCore PUBLIC ${MIMALLOC}/build/native/include/mimalloc-2.1)
target_include_directories(xrCore PUBLIC ${LZO}/build/native/include/)

if (NOT WIN32)
    target_include_directories(xrCore PUBLIC ${IXRAY_SDK_INC}/linux)
endif()

# Move precompiled header
target_validate_pch(xrCore ${CMAKE_CURRENT_SOURCE_DIR})

# Project defines
target_compile_definitions(xrCore PRIVATE XRCORE_EXPORTS)
target_compile_definitions(xrCore PRIVATE _USRDLL)

## Disable user-cheats for Shipping build
target_compile_definitions(xrCore PUBLIC "$<$<CONFIG:Debug>:DEBUG>")
target_compile_definitions(xrCore PUBLIC "$<$<CONFIG:Debug>:DEBUG_DRAW>")
target_compile_definitions(xrCore PUBLIC "$<$<CONFIG:RelWithDebInfo>:DEBUG_DRAW>")
target_compile_definitions(xrCore PUBLIC "$<$<CONFIG:Release>:MASTER_GOLD>")
if (DEVIXRAY_ENABLE_SHIPPING)
    target_compile_definitions(xrCore PUBLIC "$<$<CONFIG:Shipping>:MASTER_GOLD>")
    target_compile_definitions(xrCore PUBLIC "$<$<CONFIG:Shipping>:SHIPPING_BUILD>")
endif()

# Profiler
if (IXRAY_PROFILER)
    target_include_directories(xrCore PUBLIC "${CORE_OPT}build/native/include/")
    target_link_directories(xrCore PUBLIC "${CORE_OPT}runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/")
    add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CORE_OPT}runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/OptickCore.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    )
    target_compile_definitions(xrCore PUBLIC "IXRAY_PROFILER")
    target_link_libraries(xrCore PUBLIC "OptickCore")
endif()

# Linker list
if(WIN32)
    if (NOT IXR_ARM_ENABLE)
        target_link_libraries(xrCore PRIVATE dxerr)
    endif()

    target_link_libraries(xrCore PRIVATE "dbghelp.lib")
    target_link_libraries(xrCore PRIVATE "winmm.lib")
    target_link_libraries(xrCore PRIVATE "faultrep.lib")

    target_link_libraries(xrCore PUBLIC ${CORE_SDL3}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/SDL3.lib)
    target_link_libraries(xrCore PUBLIC ${IXR_TBB_SDK}/runtimes/win7-${NUGET_PACKAGE_PLATFORM}/native/Release/tbb12.lib)
    target_link_libraries(xrCore PRIVATE ${CMAKE_BINARY_DIR}/dep/discord_gamesdk/lib/${NUGET_PACKAGE_PLATFORM_EX}/discord_game_sdk.dll.lib)
    target_link_libraries(xrCore PUBLIC ${LZO}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/lzo2.lib)
    target_link_libraries(xrCore PUBLIC ${MIMALLOC}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/mimalloc.lib)
elseif(NOT WIN32 AND CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    target_link_libraries(xrCore PUBLIC ${CORE_SDL3}/runtimes/linux-${NUGET_PACKAGE_PLATFORM}/native/Release/libSDL3.so.0.0.0)
    target_link_libraries(xrCore PUBLIC ${IXR_TBB_SDK}/runtimes/linux-${NUGET_PACKAGE_PLATFORM}/native/Release/libtbb.so.12.13)
    target_link_libraries(xrCore PUBLIC ${LZO}/runtimes/linux-${NUGET_PACKAGE_PLATFORM}/native/Release/liblzo2.so.2.0.0)
    target_link_libraries(xrCore PUBLIC ${MIMALLOC}/runtimes/linux-${NUGET_PACKAGE_PLATFORM}/native/Release/libmimalloc.so.2.1)
endif()

# Git Info
include(gitver.cmake)

# Copy SDK binaries
if(WIN32)
    add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CORE_SDL3}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/SDL3.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${IXR_TBB_SDK}/runtimes/win7-${NUGET_PACKAGE_PLATFORM}/native/Release/tbb12.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_BINARY_DIR}/dep/discord_gamesdk/lib/${NUGET_PACKAGE_PLATFORM_EX}/discord_game_sdk.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${LZO}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/lzo2.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${MIMALLOC}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/mimalloc.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${MIMALLOC}/runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/mimalloc-redirect${MIMALLOC_POSTFIX}.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
    )
elseif(NOT WIN32 AND CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CORE_SDL3}/runtimes/linux-${NUGET_PACKAGE_PLATFORM}/native/Release/libSDL3.so.0.0.0 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/libSDL3.so.0
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${IXR_TBB_SDK}/runtimes/linux-${NUGET_PACKAGE_PLATFORM}/native/Release/libtbb.so.12.13 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/libtbb.so.12
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${LZO}/runtimes/linux-${NUGET_PACKAGE_PLATFORM}/native/Release/liblzo2.so.2.0.0 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/liblzo2.so.2
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${MIMALLOC}/runtimes/linux-${NUGET_PACKAGE_PLATFORM}/native/Release/libmimalloc.so.2.1 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/libmimalloc.so.2
    )
endif()

# Set relative runpath
if(NOT WIN32 AND CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    set_target_properties(${PROJECT_NAME} PROPERTIES
        BUILD_RPATH ""
        BUILD_RPATH_USE_ORIGIN ON
    )
    set_target_properties(${PROJECT_NAME} PROPERTIES
        BUILD_RPATH "$ORIGIN"
    )
endif()
