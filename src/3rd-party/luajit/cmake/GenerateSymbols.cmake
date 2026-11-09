function(generate_symbols target_name)
    if(NOT LINUX AND NOT CMAKE_C_COMPILER_ID MATCHES "GNU|Clang" OR APPLE)
        return()
    endif()

    # Extract symbols
    add_custom_command(
        TARGET ${target_name}
        POST_BUILD
        COMMENT "Extracting symbols from $<TARGET_FILE_NAME:${target_name}>..."
        COMMAND
            ${CMAKE_OBJCOPY}
            --only-keep-debug
            $<TARGET_FILE:${target_name}>
            $<TARGET_FILE_DIR:${target_name}>/$<TARGET_FILE_NAME:${target_name}>.debug
    )
    # Strip symbols
    add_custom_command(
        TARGET ${target_name}
        POST_BUILD
        COMMENT "Stripping $<TARGET_FILE_NAME:${target_name}>..."
        COMMAND
            ${CMAKE_OBJCOPY}
            --strip-all $<TARGET_FILE:${target_name}>
    )
    # Link debug module
    add_custom_command(
        TARGET ${target_name}
        POST_BUILD
        COMMENT "Linking symbols to $<TARGET_FILE_NAME:${target_name}>..."
        COMMAND
            ${CMAKE_OBJCOPY}
            --add-gnu-debuglink=$<TARGET_FILE_DIR:${target_name}>/$<TARGET_FILE_NAME:${target_name}>.debug
            $<TARGET_FILE:${target_name}>
    )
endfunction()
