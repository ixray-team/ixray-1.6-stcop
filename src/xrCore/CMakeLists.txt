project(xrCore)
set(CORE_SRC_FILES)

file(GLOB_RECURSE CORE_SOURCE_ALL_FILES CONFIGURE_DEPENDS "*.*")

# NatVis и NuGet-конфиги
file(GLOB_RECURSE CORE_NATVIS_FILES "../ixray.natvis")

foreach(_file IN LISTS CORE_SOURCE_ALL_FILES)
	file(RELATIVE_PATH _rel "${CMAKE_CURRENT_SOURCE_DIR}" "${_file}")
	get_filename_component(_dir "${_rel}" DIRECTORY)

	if(_dir)
		string(REPLACE "/" "\\" _group "${_dir}")
	else()
		set(_group "ungrouped")
	endif()

	source_group("${_group}" FILES "${_file}")
endforeach()

list(APPEND CORE_SRC_FILES ${CORE_SOURCE_ALL_FILES})

if(MSVC)
	list(APPEND CORE_SRC_FILES ${CORE_NATVIS_FILES})
endif()

if (MSVC)
	list(FILTER CORE_SRC_FILES EXCLUDE REGEX "Platform/Linux/")
	list(FILTER CORE_SRC_FILES EXCLUDE REGEX "Platform/BSD/")
	list(FILTER CORE_SRC_FILES EXCLUDE REGEX "Platform/MacOS/")
else()
	list(FILTER CORE_SRC_FILES EXCLUDE REGEX "Platform/Windows/")
	list(FILTER CORE_SRC_FILES EXCLUDE REGEX "discord/api/")

	if(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
		list(FILTER CORE_SRC_FILES EXCLUDE REGEX "Platform/BSD/")
		list(FILTER CORE_SRC_FILES EXCLUDE REGEX "Platform/MacOS/")
	else()
		list(FILTER CORE_SRC_FILES EXCLUDE REGEX "Platform/Linux/")
	endif()
endif()

# xrCore project
add_library(xrCore SHARED ${CORE_SRC_FILES})
target_link_directories(xrCore PUBLIC ${IXRAY_SDK_LIB})

if(IXRAY_ASAN)
	target_compile_definitions(xrCore PRIVATE PURE_ONLY)
endif()

if (IXRAY_DISCORD_RPC)
	target_compile_definitions(xrCore PRIVATE IXR_ENABLE_DISCORD)
endif()

# Includes 
target_include_directories(xrCore PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(xrCore PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/..")
target_include_directories(xrCore PUBLIC ${EXTERNAL_INCLUDE_DIR})
target_include_directories(xrCore PUBLIC ${CMAKE_BINARY_DIR}/dep)

if (NOT WIN32)
	target_include_directories(xrCore PUBLIC ${EXTERNAL_INCLUDE_DIR}/linux)
endif()

# Move precompiled header
target_validate_pch(xrCore ${CMAKE_CURRENT_SOURCE_DIR})

# Project defines
target_compile_definitions(xrCore PRIVATE XRCORE_EXPORTS)
target_compile_definitions(xrCore PRIVATE _USRDLL)

# Profiler
if (IXRAY_PROFILER AND NOT IXRAY_PROFILER_TRACY)
	target_include_directories(xrCore PUBLIC "${CORE_OPT}build/native/include/")
	target_link_directories(xrCore PUBLIC "${CORE_OPT}runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/")
	add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
		COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CORE_OPT}runtimes/win-${NUGET_PACKAGE_PLATFORM}/native/Release/OptickCore.dll ${IXRAY_OUTPUT}
	)
	target_compile_definitions(xrCore PUBLIC "IXRAY_PROFILER")
	target_link_libraries(xrCore PUBLIC "OptickCore")
elseif (IXRAY_PROFILER_TRACY)
	target_compile_definitions(xrCore PUBLIC "IXRAY_PROFILER_TRACY")
	target_link_libraries(xrCore PUBLIC TracyClient)
endif()

# Linker list
target_link_libraries(xrCore PUBLIC SDL3-shared)
target_link_libraries(xrCore PUBLIC TBB::tbb)
target_link_libraries(xrCore PUBLIC LZO::lzo2)
target_link_libraries(xrCore PRIVATE yaml-cpp)

if(WIN32)
	if(NOT IXR_ARM_ENABLE AND NOT CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
		target_link_libraries(xrCore PRIVATE dxerr)
	endif()

	target_link_libraries(xrCore PRIVATE DiscordSDK::discord)
	target_link_libraries(xrCore PRIVATE WindowsSDK::WinMM)
	target_link_libraries(xrCore PRIVATE WindowsSDK::DBGHELP)
	target_link_libraries(xrCore PRIVATE WindowsSDK::FAULTREP)
endif()

# Git Info
include(gitver.cmake)

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

target_include_directories(xrCore PUBLIC "${CMAKE_SOURCE_DIR}/src/3rd-party/embree/include/")
target_link_libraries(xrCore PUBLIC embree)

if(WIN32)
    add_custom_command(TARGET xrCore POST_BUILD
    	COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_BINARY_DIR}/src/3rd-party/embree/$<CONFIG>/embree4.dll ${IXRAY_OUTPUT}
    )
else ()
    add_custom_command(TARGET xrCore POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_BINARY_DIR}/src/3rd-party/embree/$<CONFIG>/libembree4.so ${IXRAY_OUTPUT}
    )
endif()
