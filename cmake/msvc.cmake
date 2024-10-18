# Global options
set(CMAKE_CXX_FLAGS_DEBUG "/MD")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /UMBCS /D_UNICODE /DUNICODE")

# Win32 Extensions
if (CMAKE_SIZEOF_VOID_P EQUAL 4)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /LARGEADDRESSAWARE")
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} /LARGEADDRESSAWARE")
    ADD_DEFINITIONS(/arch:SSE2)
endif()

# Apply definitions
add_compile_definitions(_WINDOWS)

# Enable gcc/clang style for MSVC
add_compile_options(/permissive- /fp:fast /wd4073 /wd4390 /wd4273 /sdl /wd4566 /wd4297)
string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
add_compile_options("$<$<CONFIG:DEBUG>:/Od>" "$<$<CONFIG:DEBUG>:/MD>" "/Ob1")
add_compile_options("$<$<CONFIG:RELEASE>:/Ot>"  "$<$<CONFIG:RELEASE>:/Ob2>" "$<$<CONFIG:RELWITHDEBINFO>:/wd4577>")

add_compile_options($<$<CXX_COMPILER_ID:MSVC>:/MP>)
add_compile_options(/wd4595 /wd4996 /wd4005)
add_link_options("$<$<CONFIG:DEBUG>:/SAFESEH:NO>")
add_compile_options("$<$<CONFIG:RELEASE>:/wd4530>" "$<$<CONFIG:DEBUG>:/wd4275>" "$<$<CONFIG:DEBUG>:/wd4251>" "$<$<CONFIG:RELWITHDEBINFO>:/wd4530>")

## Exceptions...
add_compile_options("/EHsc")

## Edit and Continue mode
if (IXRAY_ASAN)
    add_compile_options("$<$<CONFIG:DEBUG>:/Zi>" "$<$<CONFIG:RELWITHDEBINFO>:/Zi>" "$<$<CONFIG:RELEASE>:/Zi>")
else()
    add_compile_options("$<$<CONFIG:DEBUG>:/ZI>" "$<$<CONFIG:RELWITHDEBINFO>:/Zi>" "$<$<CONFIG:RELEASE>:/Zi>")
endif()

if(${CMAKE_GENERATOR_PLATFORM} MATCHES "arm64")
    set(IXR_ARM_ENABLE ON)
    add_compile_options(/Zc:preprocessor)
else()
    set(IXR_ARM_ENABLE OFF)
endif()

# Setup build patches
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# Hack for COPY
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_EX ${CMAKE_BINARY_DIR}/bin/$<CONFIG>/)

# Other 
function(target_validate_pch target target_path)
	set_target_properties(${target} PROPERTIES DISABLE_PRECOMPILE_HEADERS ON)
	set_target_properties(${target} PROPERTIES COMPILE_FLAGS "/Yustdafx.h")
	set_source_files_properties(stdafx.cpp PROPERTIES COMPILE_FLAGS "/Ycstdafx.h")
	target_precompile_headers(${target} PRIVATE "stdafx.h")

	file(GLOB_RECURSE CORE_SOURCE_PCH_FILES "${target_path}/stdafx.*")
	file(GLOB_RECURSE CORE_SOURCE_ALL_C_FILES "${target_path}/*.c")

	set_source_files_properties(${CORE_SOURCE_ALL_C_FILES} PROPERTIES SKIP_PRECOMPILE_HEADERS ON)
	source_group("pch" FILES ${CORE_SOURCE_PCH_FILES})
endfunction()