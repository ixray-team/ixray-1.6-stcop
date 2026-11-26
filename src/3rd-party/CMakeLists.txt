# Solution folders
set(IXRAY_FOLDER_3RD FOLDER "3rd Party")

# 3rd libraries
if(NOT IXRAY_COMPRESSOR_ONLY)
    add_subdirectory("ode")
    add_subdirectory("crypto")
    add_subdirectory("NvTriStrip")
    add_subdirectory("IKAN")

    # Windows uses the prebuilt package from cmake/modules/Embree.cmake
    if(NOT WIN32)
        set(EMBREE_TUTORIALS OFF CACHE BOOL "" FORCE)
        set(EMBREE_TESTING OFF CACHE BOOL "" FORCE)
        set(EMBREE_ISPC_SUPPORT OFF CACHE BOOL "" FORCE)
        set(EMBREE_STATIC_LIB OFF CACHE BOOL "" FORCE)
    	if(APPLE)
    		set(EMBREE_ISA_NEON ON CACHE BOOL "" FORCE)
    		set(EMBREE_ISA_NEON2X ON CACHE BOOL "" FORCE)
    		set(EMBREE_ISA_SSE2 OFF CACHE BOOL "" FORCE)
    		set(EMBREE_ISA_SSE42 OFF CACHE BOOL "" FORCE)
    		set(EMBREE_ISA_AVX OFF CACHE BOOL "" FORCE)
    		set(EMBREE_ISA_AVX2 OFF CACHE BOOL "" FORCE)
    		set(EMBREE_ISA_AVX512 OFF CACHE BOOL "" FORCE)
    		set(EMBREE_MAX_ISA "NEON" CACHE STRING "" FORCE)
    	else()
    		set(EMBREE_MAX_ISA "SSE2" CACHE STRING "" FORCE)
    		set(EMBREE_ISA_SSE2 ON CACHE BOOL "" FORCE)
    	endif()

        # Embree is a heavy ray-query library: build it optimized in Debug too.
        # Root options force /Od /Ob1 /ZI, which override embree's own /Ox, so swap them for this subdirectory only.
        get_directory_property(IxrDirCompileOptions COMPILE_OPTIONS)
        if(MSVC)
            set(IxrEmbreeCompileOptions ${IxrDirCompileOptions})
            list(REMOVE_ITEM IxrEmbreeCompileOptions "$<$<CONFIG:DEBUG>:/Od>" "/Ob1" "$<$<CONFIG:DEBUG>:/ZI>")
            list(APPEND IxrEmbreeCompileOptions "$<$<CONFIG:DEBUG>:/O2>" "$<$<CONFIG:DEBUG>:/Ob2>" "$<$<CONFIG:DEBUG>:/Oi>" "$<$<CONFIG:DEBUG>:/Zi>")
            set_directory_properties(PROPERTIES COMPILE_OPTIONS "${IxrEmbreeCompileOptions}")
        endif()

        add_subdirectory("embree")

        set_directory_properties(PROPERTIES COMPILE_OPTIONS "${IxrDirCompileOptions}")
        foreach(EmbreeTarget embree lexers math simd sys tasking)
            if(TARGET ${EmbreeTarget})
                set_target_properties(${EmbreeTarget} PROPERTIES FOLDER "3rd Party/Embree Lib")
            endif()
        endforeach()
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
    if(NOT WIN32)
        set_target_properties(embree PROPERTIES ${IXRAY_FOLDER_3RD})
    endif()
    set_target_properties(ODE PROPERTIES ${IXRAY_FOLDER_3RD})
    set_target_properties(IKAN PROPERTIES ${IXRAY_FOLDER_3RD})
    set_target_properties(NvTriStrip PROPERTIES ${IXRAY_FOLDER_3RD})
    set_target_properties(crypto PROPERTIES ${IXRAY_FOLDER_3RD})
    set_target_properties(lunasvg PROPERTIES ${IXRAY_FOLDER_3RD})
    set_target_properties(pffft PROPERTIES ${IXRAY_FOLDER_3RD})
    set_target_properties(plutovg PROPERTIES ${IXRAY_FOLDER_3RD})
endif()

set_target_properties(imgui PROPERTIES ${IXRAY_FOLDER_3RD})

if(WIN32 AND NOT CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    set_target_properties(dxerr PROPERTIES ${IXRAY_FOLDER_3RD})
endif()
