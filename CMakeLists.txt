cmake_minimum_required(VERSION 3.14)
project(IXRay)

# CMake options
option(IXRAY_UTILS "Apply utils to solution" OFF)
option(IXRAY_MP "Apply multiplayer xrGame and multiplayer utilities to solution" OFF)
option(IXRAY_COMPRESSOR_ONLY "Build only compressor" OFF)

# Output target
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# VS options
set(CMAKE_EXECUTABLE_ENABLE_EXPORTS ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_FLAGS_DEBUG "/MD")

string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
add_compile_options("$<$<CONFIG:RELWITHDEBINFO>:/Ob2>" "$<$<CONFIG:DEBUG>:/EHsc>"
                    "$<$<CONFIG:RELWITHDEBINFO>:/Ot>" "$<$<CONFIG:RELWITHDEBINFO>:/wd4577>")
add_compile_options($<$<CXX_COMPILER_ID:MSVC>:/MP>)

# Edit and Continue mode
add_compile_options("$<$<CONFIG:DEBUG>:/ZI>" "$<$<CONFIG:RELWITHDEBINFO>:/Zi>")

# Configure NuGet
find_program(NUGET_COMMAND nuget)
if(NOT NUGET_COMMAND)
    message("NuGet not found in PATH!")
    message("Downloading NuGet...")
    if(NOT EXISTS "${CMAKE_BINARY_DIR}/dep/nuget")
        execute_process(COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_BINARY_DIR}/dep/nuget")
        file(DOWNLOAD https://dist.nuget.org/win-x86-commandline/latest/nuget.exe
             "${CMAKE_BINARY_DIR}/dep/nuget/nuget.exe")
    endif()
    set(NUGET_COMMAND "${CMAKE_BINARY_DIR}/dep/nuget/nuget.exe")
    message("NuGet downloaded: ${NUGET_COMMAND}")
else()
    message("NuGet found: ${NUGET_COMMAND}")
endif()

# Configure dependencies
set(RENDERDOC_API "${CMAKE_CURRENT_SOURCE_DIR}/src/Dependencies/renderdoc")

# SDK folders
set(IXRAY_SDK_LIB ${CMAKE_CURRENT_SOURCE_DIR}/sdk/libraries/)
set(IXRAY_SDK_INC ${CMAKE_CURRENT_SOURCE_DIR}/sdk/include/)
set(IXRAY_SDK_BIN ${CMAKE_CURRENT_SOURCE_DIR}/sdk/binaries/)

# Solution folders
set(IXRAY_FOLDER_UTILS FOLDER "Utils")
set(IXRAY_FOLDER_ENGINE FOLDER "Engine")
set(IXRAY_FOLDER_RENDER FOLDER "Renders")
set(IXRAY_FOLDER_GAME FOLDER "Game")
set(IXRAY_FOLDER_3RD FOLDER "3rd Party")

# 3rd libraries
add_subdirectory("src/3rd party/luabind")
add_subdirectory("src/3rd party/ode")
add_subdirectory("src/3rd party/crypto")
add_subdirectory("src/3rd party/cximage")

# Engine libraries
add_subdirectory("src/xrCore")
add_subdirectory("src/Layers/xrAPI")
add_subdirectory("src/xrCDB")
add_subdirectory("src/xrSound")
add_subdirectory("src/xrNetServer")
add_subdirectory("src/xrEngine")
add_subdirectory("src/xrCPU_Pipe")
add_subdirectory("src/xrPhysics")
add_subdirectory("src/xrParticles")
add_subdirectory("src/Layers/xrRenderPC_R1")
add_subdirectory("src/Layers/xrRenderPC_R2")
add_subdirectory("src/Layers/xrRenderPC_R4")
add_subdirectory("src/xrGame")
add_subdirectory("src/xrGameSpy")
add_subdirectory("src/xrPlay")

if(IXRAY_MP)
    add_subdirectory("src/xrServer")
endif()

# Engine Utils
if(IXRAY_UTILS OR IXRAY_COMPRESSOR_ONLY)
    add_subdirectory("src/utils/xrCompress")
endif()

if(IXRAY_UTILS)
    add_subdirectory("src/utils/xrDXT")
    add_subdirectory("src/utils/xrQSlim")
    add_subdirectory("src/utils/xrLC_Light")
    #add_subdirectory("src/utils/xrLC_LightStab")
    add_subdirectory("src/utils/xrDO_Light")
    add_subdirectory("src/utils/xrLC")
    add_subdirectory("src/utils/ETools")
    add_subdirectory("src/utils/xrSE_Factory")
    add_subdirectory("src/utils/xrAI")

    if(IXRAY_MP)
        add_subdirectory("src/utils/mp_balancer")
        add_subdirectory("src/utils/mp_configs_verifyer")
        add_subdirectory("src/utils/mp_screenshots_info")
        add_subdirectory("src/utils/mp_gpprof_server")
    endif()
endif()

# Move libraries
set_target_properties(xrCore PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrAPI PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrCDB PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrSound PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrNetServer PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrCPU_Pipe PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrPhysics PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrParticles PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrAbstractions PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrGame PROPERTIES ${IXRAY_FOLDER_GAME})
set_target_properties(xrGameSpy PROPERTIES ${IXRAY_FOLDER_GAME})
set_target_properties(xrRender_R1 PROPERTIES ${IXRAY_FOLDER_RENDER})
set_target_properties(xrRender_R2 PROPERTIES ${IXRAY_FOLDER_RENDER})
set_target_properties(xrRender_R4 PROPERTIES ${IXRAY_FOLDER_RENDER})


if(IXRAY_UTILS)
    if(NOT IXRAY_COMPRESSOR_ONLY)
        set_target_properties(xrCompress PROPERTIES ${IXRAY_FOLDER_UTILS})
    endif()

    set_target_properties(xrDXT PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(QSlim PROPERTIES ${IXRAY_FOLDER_UTILS})
    #set_target_properties(xrLC_LightStab PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(xrLC_Light PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(xrDO_Light PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(xrLC PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(ETools PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(xrSE_Factory PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(xrAI PROPERTIES ${IXRAY_FOLDER_UTILS})

    if(IXRAY_MP)
        set_target_properties(mp_balancer PROPERTIES ${IXRAY_FOLDER_UTILS})
        set_target_properties(mp_configs_verifyer PROPERTIES ${IXRAY_FOLDER_UTILS})
        set_target_properties(mp_screenshots_info PROPERTIES ${IXRAY_FOLDER_UTILS})
        set_target_properties(mp_gpprof_server PROPERTIES ${IXRAY_FOLDER_UTILS})
    endif()
endif()

# Move 3rd
set_target_properties(luabind PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(ODE PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(crypto PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(CxImage PROPERTIES ${IXRAY_FOLDER_3RD})

# CMake targets
set_property(GLOBAL PROPERTY USE_FOLDERS ON)
set(PREDEFINED_TARGETS_FOLDER "CustomTargets")

# Set default project
if(NOT IXRAY_COMPRESSOR_ONLY)
    set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT xrEngine)
else()
    set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT xrCompress)
    set_target_properties(xrEngine xrAPI xrCDB xrSound xrCPU_Pipe xrPhysics xrParticles xrGame xrGameSpy xrRender_R1 xrRender_R2 xrRender_R4 xrAbstractions xrNetServer PROPERTIES EXCLUDE_FROM_ALL 1 EXCLUDE_FROM_DEFAULT_BUILD 1)
    set_target_properties(ODE luabind crypto CxImage PROPERTIES EXCLUDE_FROM_ALL 1 EXCLUDE_FROM_DEFAULT_BUILD 1)

    set_target_properties(xrEngine PROPERTIES ${IXRAY_FOLDER_ENGINE})

    if(IXRAY_UTILS)
        set_target_properties(QSlim xrDXT xrLC_Light xrDO_Light xrLC ETools xrSE_Factory xrAI PROPERTIES EXCLUDE_FROM_ALL 1 EXCLUDE_FROM_DEFAULT_BUILD 1)

        if (IXRAY_MP)
            set_target_properties(mp_configs_verifyer mp_balancer mp_screenshots_info mp_gpprof_server PROPERTIES EXCLUDE_FROM_ALL 1 EXCLUDE_FROM_DEFAULT_BUILD 1)
        endif()
    endif()
endif()
