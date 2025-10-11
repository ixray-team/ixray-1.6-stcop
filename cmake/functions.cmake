if(NOT TARGET copy_all_dlls)
    add_custom_target(copy_all_dlls ALL
        #COMMENT "Copying all DLLs"
    )
    set_target_properties(copy_all_dlls PROPERTIES FOLDER "Pre-Build")
endif()

function(add_imported_lib name include_dir lib_path dll_path)
    add_library(${name} INTERFACE IMPORTED)
    set_target_properties(${name} PROPERTIES
        INTERFACE_INCLUDE_DIRECTORIES "${include_dir}"
        INTERFACE_LINK_LIBRARIES "${lib_path}"
    )

    if(EXISTS "${dll_path}")
        set(target_dir "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/$<CONFIG>")

        add_custom_command(TARGET copy_all_dlls PRE_BUILD
            COMMAND ${CMAKE_COMMAND} -E make_directory "${target_dir}"
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
                    "${dll_path}" "${target_dir}/"
            #COMMENT "Copying ${dll_path} to ${target_dir}"
        )

        add_dependencies(${name} copy_all_dlls)
    endif()
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

find_package(PkgConfig QUIET)