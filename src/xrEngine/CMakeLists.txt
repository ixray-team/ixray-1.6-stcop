project(xrEngine)
set(ENGINE_SRC_FILES)

# Files import
file(GLOB_RECURSE ENGINE_SOURCE_INTERFACE_FILES
    "I*.cpp"
	"I*.h"
)

file(GLOB_RECURSE ENGINE_SOURCE_ALL_FILES
    "ai*.cpp"
	"ai*.h"
)

file(GLOB_RECURSE ENGINE_SOURCE_DEVICE_FILES
    "device*.*"
	"Device*.*"
)

file(GLOB_RECURSE ENGINE_SOURCE_ENV_FILES
    "Environment*.*"
)

file(GLOB_RECURSE ENGINE_SOURCE_WEDIT_FILES
    "editor*.*"
)

file(GLOB_RECURSE ENGINE_SOURCE_ALL_FILES
    "*.cpp"
    "*.c"
	"*.h"
)

# File groups
source_group("core" FILES ${ENGINE_SOURCE_ALL_FILES})
source_group("interfaces" FILES ${ENGINE_SOURCE_INTERFACE_FILES})
source_group("lua" FILES ${ENGINE_SOURCE_LUA_FILES})
source_group("device" FILES ${ENGINE_SOURCE_DEVICE_FILES})
source_group("weather_editor" FILES ${ENGINE_SOURCE_WEDIT_FILES})
source_group("environment" FILES ${ENGINE_SOURCE_ENV_FILES})

# Apply list
list(APPEND ENGINE_SRC_FILES ${ENGINE_SOURCE_ALL_FILES})

# Remove borlands files
#list(FILTER ENGINE_SRC_FILES EXCLUDE REGEX "")

# xrEngine project
add_executable(xrEngine WIN32 ${ENGINE_SRC_FILES})
target_include_directories(xrEngine PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_include_directories(xrEngine PUBLIC "${IXRAY_SDK_INC}")

# PCH
target_precompile_headers(xrEngine PUBLIC "stdafx.h")

# project defines 
target_compile_definitions(xrEngine PRIVATE ENGINE_BUILD)

add_compile_options(/fp:fast)
target_compile_definitions(xrEngine PRIVATE "$<$<CONFIG:Debug>:DEBUG>")

# Linker list
target_link_libraries(xrEngine PUBLIC xrCore)
target_link_libraries(xrEngine PUBLIC xrCDB)
target_link_libraries(xrEngine PUBLIC xrAPI)
target_link_libraries(xrEngine PUBLIC xrSound)
target_link_libraries(xrEngine PUBLIC luajit)
target_link_libraries(xrEngine PUBLIC luabind)
target_link_libraries(xrEngine PUBLIC xrNetServer)
target_link_libraries(xrEngine PUBLIC "Winmm.lib")
target_link_libraries(xrEngine PUBLIC "Vfw32.lib")

target_link_libraries(xrEngine PUBLIC ${IXRAY_SDK_LIB}/FreeImage.lib)

# Nuget
add_custom_command(TARGET ${PROJECT_NAME}
    PRE_BUILD
    COMMAND ${CMAKE_HOME_DIRECTORY}/nuget restore ${CMAKE_CURRENT_SOURCE_DIR}/Packages.config -SolutionDirectory ${CMAKE_CACHEFILE_DIR}
)

## Theora
target_link_libraries(xrEngine PUBLIC ${CMAKE_CACHEFILE_DIR}/packages/ImeSense.Packages.LibTheora.1.1.1.3/native/lib/Win32/Release/libtheora.lib)
target_include_directories(xrEngine PUBLIC "${CMAKE_CACHEFILE_DIR}/packages/ImeSense.Packages.LibTheora.1.1.1.3/native/include/")

## LibOGG
target_link_libraries(xrEngine PUBLIC ${CMAKE_CACHEFILE_DIR}/packages/ImeSense.Packages.LibOgg.1.3.5.4/native/lib/Win32/Release/libogg.lib)
target_include_directories(xrEngine PUBLIC "${CMAKE_CACHEFILE_DIR}/packages/ImeSense.Packages.LibOgg.1.3.5.4/native/include/")
