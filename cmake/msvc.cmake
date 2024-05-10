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
add_compile_options(/permissive- /fp:fast /wd4073 /wd4390 /wd4273 /sdl)
string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
add_compile_options("$<$<CONFIG:DEBUG>:/Od>" "$<$<CONFIG:DEBUG>:/MD>" "/Ob1" "$<$<CONFIG:DEBUG>:/EHsc>")
add_compile_options("$<$<CONFIG:RELEASE>:/Ot>"  "$<$<CONFIG:RELEASE>:/Ob2>" "$<$<CONFIG:RELWITHDEBINFO>:/wd4577>")

add_compile_options($<$<CXX_COMPILER_ID:MSVC>:/MP>)
add_compile_options(/wd4595 /wd4996 /wd4005)
add_link_options("$<$<CONFIG:DEBUG>:/SAFESEH:NO>")
add_compile_options("$<$<CONFIG:RELEASE>:/wd4530>" "$<$<CONFIG:DEBUG>:/wd4275>" "$<$<CONFIG:DEBUG>:/wd4251>" "$<$<CONFIG:RELWITHDEBINFO>:/wd4530>")

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