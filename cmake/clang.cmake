# General build options
add_compile_options(-Wno-switch -Wno-unused-result)

if (WIN32)
    add_compile_options(--Wno-microsoft-template-shadow)
else()
    add_compile_options(-fms-extensions)
    add_compile_options(-fexperimental-library) # stacktrace
    add_compile_options(
        -Wno-null-dereference -Wno-nonnull -Wno-format-truncation -Wno-attributes -Wno-format -Wno-inconsistent-missing-override -Wno-undefined-inline
        -Wno-inline-new-delete -Wno-implicit-exception-spec-mismatch -Wno-unknown-warning-option -Wno-tautological-constant-out-of-range-compare
        -Wno-macro-redefined -Wno-int-to-pointer-cast
    )
    add_compile_options(-msse4.2) # crc32

    # Use libc++ only if headers are present (some distros don't install libc++ by default).
    if(EXISTS "/usr/include/c++/v1/new" OR EXISTS "/usr/include/c++/v1/iostream")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++") # ranges
    else()
        message(STATUS "libc++ headers not found in /usr/include/c++/v1; not adding -stdlib=libc++ flag")
    endif()
    add_compile_definitions(_GNU_SOURCE) # mremap
    
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