# Solution folders
set(IXRAY_FOLDER_3RD FOLDER "3rd Party")

# 3rd libraries
if(NOT IXRAY_COMPRESSOR_ONLY)
    add_subdirectory("ode")
    add_subdirectory("crypto")
    add_subdirectory("NvTriStrip")
    add_subdirectory("IKAN")

    set(EMBREE_TUTORIALS OFF CACHE BOOL "" FORCE)
    set(EMBREE_TESTING OFF CACHE BOOL "" FORCE)
    set(EMBREE_ISPC_SUPPORT OFF CACHE BOOL "" FORCE)
    set(EMBREE_STATIC_LIB OFF CACHE BOOL "" FORCE)
    set(EMBREE_MAX_ISA "SSE2" CACHE STRING "" FORCE)
    set(EMBREE_ISA_SSE2 ON CACHE BOOL "" FORCE)

    add_subdirectory("embree")
    
    if (IXRAY_ENABLE_RESONANCEAUDIO)
        add_subdirectory("resonance-audio")
    endif()
endif()

add_subdirectory("imgui")

if(WIN32 AND NOT CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    add_subdirectory("dxerr")
endif()

if(NOT IXRAY_COMPRESSOR_ONLY)
    if(IXRAY_UTILS OR IXRAY_EDITORS)
        add_subdirectory("MagicSoftware/FreeMagic/Source")
        add_subdirectory("NvMender2003")
		
        set_target_properties(NvMender2003 PROPERTIES ${IXRAY_FOLDER_3RD})
        set_target_properties(MagicFM PROPERTIES ${IXRAY_FOLDER_3RD})
    endif()
endif()

if(DEVIXRAY_ENABLE_OGL_RENDER AND NOT IXRAY_COMPRESSOR_ONLY)
    add_subdirectory("glew")
endif()

# Move 3rd
if(NOT IXRAY_COMPRESSOR_ONLY)
	set_target_properties(embree PROPERTIES ${IXRAY_FOLDER_3RD})
    set_target_properties(ODE PROPERTIES ${IXRAY_FOLDER_3RD})
    set_target_properties(IKAN PROPERTIES ${IXRAY_FOLDER_3RD})
    set_target_properties(NvTriStrip PROPERTIES ${IXRAY_FOLDER_3RD})
    set_target_properties(crypto PROPERTIES ${IXRAY_FOLDER_3RD})
    set_target_properties(lunasvg PROPERTIES ${IXRAY_FOLDER_3RD})
    set_target_properties(pffft PROPERTIES ${IXRAY_FOLDER_3RD})
    set_target_properties(plutovg PROPERTIES ${IXRAY_FOLDER_3RD})
    
    if (IXRAY_ENABLE_RESONANCEAUDIO)
        set_target_properties(ResonanceAudioStatic PROPERTIES ${IXRAY_FOLDER_3RD})
        set_target_properties(SadieHrtfsObj PROPERTIES ${IXRAY_FOLDER_3RD})
        set_target_properties(ResonanceAudioObj PROPERTIES ${IXRAY_FOLDER_3RD})
    endif()
endif()

set_target_properties(imgui PROPERTIES ${IXRAY_FOLDER_3RD})

if(WIN32 AND NOT CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    set_target_properties(dxerr PROPERTIES ${IXRAY_FOLDER_3RD})
endif()
