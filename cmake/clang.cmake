# General build options
add_compile_options(-Wno-switch -Wno-unused-result)

if (WIN32)
    add_compile_options(--Wno-microsoft-template-shadow)
else()
    add_compile_options(-fms-extensions -Wno-null-dereference -Wno-nonnull -Wno-format-truncation -Wno-attributes -Wno-format)
endif()

if (NOT WIN32)
    find_program(LLD_PROGRAM lld)
    if(LLD_PROGRAM)
        add_link_options("-fuse-ld=lld")
        link_directories(/usr/lib64 /usr/local/lib64)
    endif(LLD_PROGRAM)
endif()

# Setup build patches
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin/$<CONFIG>)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin/$<CONFIG>)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib/$<CONFIG>)

# Hack for COPY
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_EX ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})

# Other 
function(target_validate_pch target target_path)
endfunction()