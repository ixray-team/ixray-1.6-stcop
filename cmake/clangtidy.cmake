option(IXRAY_CLANG_TIDY "Run clang-tidy while building (slow, naming checks only)" OFF)

if (WIN32)
    set(CMAKE_VS_GLOBALS "EnableClangTidyCodeAnalysis=true")
    set(CMAKE_CXX_CLANG_TIDY_EXPORT_FIXES_DIR "${CMAKE_BINARY_DIR}/clang-tidy")
else()
    # Homebrew keeps clang-tidy next to llvm-config instead of on PATH, and the project
    # already requires llvm@15 for DXMT.
    find_program(CLANG_TIDY_EXE clang-tidy
        HINTS /opt/homebrew/opt/llvm@15/bin /opt/homebrew/opt/llvm/bin /usr/local/opt/llvm/bin)

    if(NOT CLANG_TIDY_EXE)
        message(STATUS "clang-tidy not found, linting skipped (brew install llvm@15)")
    elseif(NOT IXRAY_CLANG_TIDY)
        message(STATUS "clang-tidy found: ${CLANG_TIDY_EXE} (disabled, pass -DIXRAY_CLANG_TIDY=ON to run it)")
    else()
        set(CMAKE_CXX_CLANG_TIDY ${CLANG_TIDY_EXE} -checks=readability-identifier-naming)
        message(STATUS "clang-tidy enabled: ${CLANG_TIDY_EXE}")
    endif()
endif()
