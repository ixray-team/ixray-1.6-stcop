cmake_minimum_required(VERSION 3.16)
project(IXray)

# CMake options
option(IXRAY_UTILS "Apply utils to solution" OFF)
option(IXRAY_MP "Apply mp xrGame (Soon) and MP utils to solution" OFF)

# Output target
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bins)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# VS options
set(CMAKE_EXECUTABLE_ENABLE_EXPORTS ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /MD")

string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
add_compile_options("$<$<CONFIG:RELWITHDEBINFO>:/Ob2>" "$<$<CONFIG:DEBUG>:/EHsc>"
                    "$<$<CONFIG:RELWITHDEBINFO>:/Ot>" "$<$<CONFIG:RELWITHDEBINFO>:/wd4577>")

add_compile_options($<$<CXX_COMPILER_ID:MSVC>:/MP>)
add_compile_options("$<$<CONFIG:Debug>:/MD>")

# Edit and Continue mode 
add_compile_options("$<$<CONFIG:DEBUG>:/ZI>" "$<$<CONFIG:RELWITHDEBINFO>:/Zi>")

# SDK Folders
set(IXRAY_SDK_LIB ${CMAKE_CURRENT_SOURCE_DIR}/sdk/libraries/)
set(IXRAY_SDK_INC ${CMAKE_CURRENT_SOURCE_DIR}/sdk/include/)
set(IXRAY_SDK_BIN ${CMAKE_CURRENT_SOURCE_DIR}/sdk/binaries/)

# Solution folders
set(IXRAY_FOLDER_ENGINE FOLDER "Engine")
set(IXRAY_FOLDER_UTILS FOLDER "Utils")
set(IXRAY_FOLDER_RENDER FOLDER "Renders")
set(IXRAY_FOLDER_GAME FOLDER "Game")
set(IXRAY_FOLDER_3RD FOLDER "3rd Party")

add_subdirectory("src")

# Move libraries
set_target_properties(xrCore PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrAPI PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrCDB PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrSound PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrNetServer PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrCPU_Pipe PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrPhysics PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrParticles PROPERTIES ${IXRAY_FOLDER_ENGINE})

set_target_properties(xrGame PROPERTIES ${IXRAY_FOLDER_GAME})
set_target_properties(xrGameSpy PROPERTIES ${IXRAY_FOLDER_GAME})

set_target_properties(RenderNew PROPERTIES ${IXRAY_FOLDER_RENDER})
set_target_properties(xrRender_R1 PROPERTIES ${IXRAY_FOLDER_RENDER})
set_target_properties(xrRender_R2 PROPERTIES ${IXRAY_FOLDER_RENDER})
set_target_properties(xrRender_R4 PROPERTIES ${IXRAY_FOLDER_RENDER})

if (IXRAY_UTILS)
    set_target_properties(xrDXT PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(QSlim PROPERTIES ${IXRAY_FOLDER_UTILS})
    #set_target_properties(xrLC_LightStab PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(xrLC_Light PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(xrDO_Light PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(xrLC PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(xrCompressor PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(ETools PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(xrSE_Factory PROPERTIES ${IXRAY_FOLDER_UTILS})
    set_target_properties(xrAI PROPERTIES ${IXRAY_FOLDER_UTILS})
    if (IXRAY_MP)
        set_target_properties(mp_balancer PROPERTIES ${IXRAY_FOLDER_UTILS})
        set_target_properties(mp_configs_verifyer PROPERTIES ${IXRAY_FOLDER_UTILS})
        set_target_properties(mp_screenshots_info PROPERTIES ${IXRAY_FOLDER_UTILS})
        set_target_properties(mp_gpprof_server PROPERTIES ${IXRAY_FOLDER_UTILS})
    endif()
endif()

# Move 3rd
set_target_properties(NRI_D3D11 PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(NRI_D3D12 PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(NRI_Shared PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(NRI_Validation PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(NRI_VK PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(NRI PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(imgui PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(TracyClient PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(luajit PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(luabind PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(ODE PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(crypto PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(CxImage PROPERTIES ${IXRAY_FOLDER_3RD})

# Cmake trash
set_property(GLOBAL PROPERTY USE_FOLDERS ON)
set(PREDEFINED_TARGETS_FOLDER "CustomTargets")

# Set default project
set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT xrEngine)