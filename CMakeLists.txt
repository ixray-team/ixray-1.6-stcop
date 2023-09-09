cmake_minimum_required(VERSION 3.8)
project(IXRay)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_EXTENSIONS ON)
set(CMAKE_CXX_FLAGS_DEBUG "/MD")

string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
add_compile_options(/MP "$<$<CONFIG:RELWITHDEBINFO>:/Ob2>" "$<$<CONFIG:DEBUG>:/EHsc>"
                    "$<$<CONFIG:RELWITHDEBINFO>:/Ot>" "$<$<CONFIG:RELWITHDEBINFO>:/wd4577>")

# Solution folders
set(IXRAY_FOLDER_ENGINE FOLDER "Engine")

# Engine libraries
add_subdirectory("src/Layers/xrAPI")
add_subdirectory("src/xrCore")
add_subdirectory("src/xrCDB")
add_subdirectory("src/xrSound")

# Move libraries
set_target_properties(xrCore PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrAPI PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrCDB PROPERTIES ${IXRAY_FOLDER_ENGINE})
set_target_properties(xrSound PROPERTIES ${IXRAY_FOLDER_ENGINE})

# CMake targets
set_property(GLOBAL PROPERTY USE_FOLDERS ON)
set(PREDEFINED_TARGETS_FOLDER "CustomTargets")
