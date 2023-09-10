cmake_minimum_required(VERSION 3.8)
project(IXRay)

# CMake options
option(IXRAY_UTILS "Apply utils to solution" OFF)
set(CMAKE_EXECUTABLE_ENABLE_EXPORTS ON)

# VS options
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_EXTENSIONS ON)
set(CMAKE_CXX_FLAGS_DEBUG "/MD")

string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
add_compile_options("$<$<CONFIG:RELWITHDEBINFO>:/Ob2>" "$<$<CONFIG:DEBUG>:/EHsc>"
                    "$<$<CONFIG:RELWITHDEBINFO>:/Ot>" "$<$<CONFIG:RELWITHDEBINFO>:/wd4577>")
add_compile_options($<$<CXX_COMPILER_ID:MSVC>:/MP>)

# SDK folders
set(IXRAY_SDK_LIB ${CMAKE_CURRENT_SOURCE_DIR}/sdk/libraries/)
set(IXRAY_SDK_INC ${CMAKE_CURRENT_SOURCE_DIR}/sdk/include/)
set(IXRAY_SDK_BIN ${CMAKE_CURRENT_SOURCE_DIR}/sdk/binaries/)

# Solution folders
set(IXRAY_FOLDER_ENGINE FOLDER "Engine")
set(IXRAY_FOLDER_RENDER FOLDER "Renders")
set(IXRAY_FOLDER_3RD FOLDER "3rd Party")

# 3rd libraries
add_subdirectory("src/3rd party/luajit")
add_subdirectory("src/3rd party/luabind")
add_subdirectory("src/3rd party/ode")

# Engine libraries
add_subdirectory("src/Layers/xrAPI")
add_subdirectory("src/xrCore")
add_subdirectory("src/xrCDB")
add_subdirectory("src/xrSound")
add_subdirectory("src/xrNetServer")
add_subdirectory("src/xrEngine")
add_subdirectory("src/xrCPU_Pipe")
add_subdirectory("src/xrPhysics")
add_subdirectory("src/xrParticles")
add_subdirectory("src/Layers/xrRenderPC_R1")

# Move libraries
set_target_properties(xrCore PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrAPI PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrCDB PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrSound PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrEngine PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrNetServer PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrCPU_Pipe PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrPhysics PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrParticles PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrRender_R1 PROPERTIES ${IXRAY_FOLDER_RENDER})

# Move dependencies
set_target_properties(luajit PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(luabind PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(ODE PROPERTIES ${IXRAY_FOLDER_3RD})

# CMake targets
set_property(GLOBAL PROPERTY USE_FOLDERS ON)
set(PREDEFINED_TARGETS_FOLDER "CustomTargets")

# Set default project
set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT xrEngine)
