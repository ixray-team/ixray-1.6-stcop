function(generate_manifest source destination)
    if(NOT WIN32 AND NOT MSVC)
        return()
    endif()

    configure_file(
        ${source}
        ${destination}
    )
endfunction()

function(embed_manifest target_name target_type manifest_file)
    if(NOT WIN32 AND NOT MSVC)
        return()
    endif()

    if(target_type STREQUAL "EXE")
        set(id_value 1)
    elseif(target_type STREQUAL "DLL")
        set(id_value 2)
    else()
        message(FATAL_ERROR "Invalid target type: ${target_type}. Expected 'EXE' or 'DLL'!")
    endif()

    set_target_properties(${target_name}
        PROPERTIES
            LINK_FLAGS "/MANIFEST:EMBED,ID=${id_value} /MANIFESTINPUT:${manifest_file}"
    )
endfunction()
