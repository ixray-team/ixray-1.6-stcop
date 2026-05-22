# Debian / Ubuntu package check

find_program(APT_EXECUTABLE apt REQUIRED)

# ------------------------------------------------------------------------------
# Detect latest available clang version
# ------------------------------------------------------------------------------

execute_process(
    COMMAND bash -c "apt-cache search '^clang-[0-9]+$' | sed 's/clang-//' | sort -V | tail -n1"
    OUTPUT_VARIABLE CLANG_VERSION
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

if (NOT CLANG_VERSION)
    set(CLANG_VERSION "18")
endif()

message(STATUS "Detected Clang version: ${CLANG_VERSION}")

# ------------------------------------------------------------------------------
# Package list
# ------------------------------------------------------------------------------

set(REQUIRED_PACKAGES
    "clang-${CLANG_VERSION}"
    "lldb-${CLANG_VERSION}"
    "lld-${CLANG_VERSION}"
    "libc++-${CLANG_VERSION}-dev"
    "libc++abi-${CLANG_VERSION}-dev"

    "libx11-dev"
    "libxext-dev"
    "libxrandr-dev"
    "libxcursor-dev"
    "libxfixes-dev"
    "libxi-dev"
    "libxinerama-dev"
    "libxss-dev"
    "libwayland-dev"
    "libdrm-dev"
    "libgbm-dev"
    "libudev-dev"
    "libpipewire-0.3-dev"
    "libibus-1.0-dev"
    "libdbus-1-dev"
    "libtbb-dev"
    "liblzo2-2"
    "liblzo2-dev"
    "libogg-dev"
    "meson"
    "glslang-tools"
    "libvulkan-dev"
    "spirv-headers"
)

# ------------------------------------------------------------------------------
# Package check
# ------------------------------------------------------------------------------

message(STATUS "Checking required Debian/Ubuntu packages...")

foreach(Package IN LISTS REQUIRED_PACKAGES)

    execute_process(
        COMMAND dpkg -s ${Package}
        RESULT_VARIABLE PACKAGE_FOUND
        OUTPUT_QUIET
        ERROR_QUIET
    )

    if (PACKAGE_FOUND EQUAL 0)
        message(STATUS "[OK] ${Package}")
    else()
        message(ERROR "[MISSING] ${Package}")
    endif()

endforeach()