include(FetchContent)

set(BOX3D_SAMPLES OFF CACHE BOOL "" FORCE)
set(BOX3D_UNIT_TESTS OFF CACHE BOOL "" FORCE)
set(BOX3D_BENCHMARKS OFF CACHE BOOL "" FORCE)
set(BOX3D_DOCS OFF CACHE BOOL "" FORCE)
set(BOX3D_VALIDATE OFF CACHE BOOL "" FORCE)
set(BOX3D_PROFILE OFF CACHE BOOL "" FORCE)

FetchContent_Declare(
    box3d
    GIT_REPOSITORY https://github.com/erincatto/box3d.git
    GIT_TAG v0.1.0
    GIT_SHALLOW TRUE
)

# Box3D links statically into xrPhysics.
set(IXRAY_SAVED_BUILD_SHARED_LIBS ${BUILD_SHARED_LIBS})
set(BUILD_SHARED_LIBS OFF)
FetchContent_MakeAvailable(box3d)
set(BUILD_SHARED_LIBS ${IXRAY_SAVED_BUILD_SHARED_LIBS})

if(TARGET box3d)
    set_target_properties(box3d PROPERTIES FOLDER "3rd Party" POSITION_INDEPENDENT_CODE ON)
    # Box3D forces the static CRT; the engine uses the DLL CRT in every configuration.
    set_target_properties(box3d PROPERTIES MSVC_RUNTIME_LIBRARY "MultiThreadedDLL")
endif()
