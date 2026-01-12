if(NOT TARGET copy_all_dlls)
    add_custom_target(copy_all_dlls ALL)
    set_target_properties(copy_all_dlls PROPERTIES FOLDER "Pre-Build")
endif()

function(add_imported_lib name include_dir lib_path dll_paths)
    add_library(${name} INTERFACE IMPORTED)
    set_target_properties(${name} PROPERTIES
        INTERFACE_INCLUDE_DIRECTORIES "${include_dir}"
        INTERFACE_LINK_LIBRARIES "${lib_path}"
    )

    foreach(dll_file IN LISTS dll_paths)
        if(EXISTS "${dll_file}")
            set(target_dir "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>")
            add_custom_command(TARGET copy_all_dlls PRE_BUILD
                COMMAND ${CMAKE_COMMAND} -E make_directory "${target_dir}"
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                        "${dll_file}" "${target_dir}/"
            )
        endif()
    endforeach()

    add_dependencies(${name} copy_all_dlls)
endfunction()

function(target_copy_dependency TARGET_NAME FILE_PATH)
    if(WIN32)
        add_custom_command(TARGET ${TARGET_NAME} POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
                    ${FILE_PATH}
                    ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>/
        )
    else()
        get_filename_component(FILE_NAME ${FILE_PATH} NAME)
    
        add_custom_command(TARGET ${TARGET_NAME} POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E echo "Copying ${FILE_NAME}"
            COMMAND ${CMAKE_COMMAND} -E copy_if_different 
                ${FILE_PATH} 
                $<TARGET_FILE_DIR:${TARGET_NAME}>/${FILE_NAME}
            COMMENT "Copying ${FILE_NAME} to output directory"
        )
    endif()
endfunction()

function(download_single_header url filename)
    set(full_path "${EXTERNAL_INCLUDE_DIR}${filename}")
    
    if(NOT EXISTS "${full_path}")
        message(STATUS "Downloading ${filename}...")
        
        file(DOWNLOAD 
            "${url}" 
            "${full_path}"
            TLS_VERIFY ON
            STATUS download_status
            TIMEOUT 60
        )
        
        list(GET download_status 0 status_code)
        if(NOT status_code EQUAL 0)
            message(WARNING "Failed to download ${filename}")
        else()
            message(STATUS "Successfully downloaded ${filename}")
        endif()
    endif()
endfunction()

find_package(PkgConfig QUIET)