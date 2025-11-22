# Solution folders
set(IXRAY_FOLDER_3RD FOLDER "3rd Party")

# 3rd libraries
if(NOT IXRAY_COMPRESSOR_ONLY)
    add_subdirectory("ode")
    add_subdirectory("crypto")
    # resonance-audio
    if (IXRAY_ENABLE_RESONANCEAUDIO)
        set(RESONANCE_ROOT ${CMAKE_SOURCE_DIR}/src/3rd-party/resonance-audio)
        set(THIRD_PARTY ${RESONANCE_ROOT}/third_party)

        if(WIN32)
            set(CLONE_SCRIPT "${THIRD_PARTY}/clone_core_deps.bat")
            set(SHELL_EXECUTABLE "cmd.exe")
            set(SHELL_ARGS "/C")
        else()
            set(CLONE_SCRIPT "${THIRD_PARTY}/clone_core_deps.sh")
            set(SHELL_EXECUTABLE "bash")
            set(SHELL_ARGS "")
        endif()

        message(STATUS "Cloning Resonance Audio core dependencies...")

        execute_process(
            COMMAND ${SHELL_EXECUTABLE} ${SHELL_ARGS} "${CLONE_SCRIPT}"
            WORKING_DIRECTORY "${THIRD_PARTY}"
            RESULT_VARIABLE RES_CLONE
            OUTPUT_VARIABLE OUT_CLONE
            ERROR_VARIABLE ERR_CLONE
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_STRIP_TRAILING_WHITESPACE
        )

        if(NOT RES_CLONE EQUAL 0)
            message(WARNING "=== Resonance Audio dependency cloning failed ===")
            message(WARNING "Exit code: ${RES_CLONE}")
            message(WARNING "Error output:\n${ERR_CLONE}")
            message(WARNING "Standard output:\n${OUT_CLONE}")
            message(FATAL_ERROR "Failed to clone Resonance Audio core dependencies.")
        else()
            message(STATUS "Resonance Audio core dependencies cloned successfully.")
        endif()

        if(NOT RES_CLONE EQUAL 0)
            message(FATAL_ERROR "Failed to clone core dependencies:\n${ERR_CLONE}")
        else()
            message(STATUS "Core dependencies cloned:\n${OUT_CLONE}")
        endif()

        set(BUILD_RESONANCE_AUDIO_API ON CACHE BOOL "Build Resonance Audio API")
        add_subdirectory("resonance-audio")
    endif()
endif()

add_subdirectory("imgui")
add_subdirectory("opcode")

if(WIN32)
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
    set_target_properties(ODE PROPERTIES ${IXRAY_FOLDER_3RD})
    set_target_properties(crypto PROPERTIES ${IXRAY_FOLDER_3RD})
    set_target_properties(lunasvg PROPERTIES ${IXRAY_FOLDER_3RD})
    set_target_properties(plutovg PROPERTIES ${IXRAY_FOLDER_3RD})
    
    if (IXRAY_ENABLE_RESONANCEAUDIO)
        set_target_properties(ResonanceAudioStatic PROPERTIES ${IXRAY_FOLDER_3RD})
        set_target_properties(SadieHrtfsObj PROPERTIES ${IXRAY_FOLDER_3RD})
        set_target_properties(PffftObj PROPERTIES ${IXRAY_FOLDER_3RD})
        set_target_properties(ResonanceAudioObj PROPERTIES ${IXRAY_FOLDER_3RD})
    endif()
endif()

set_target_properties(imgui PROPERTIES ${IXRAY_FOLDER_3RD})
set_target_properties(opcode-prosectors PROPERTIES ${IXRAY_FOLDER_3RD})

if(WIN32)
    set_target_properties(dxerr PROPERTIES ${IXRAY_FOLDER_3RD})
endif()
