# Arch Linux / pacman package check

find_program(PACMAN_EXECUTABLE pacman REQUIRED)

set(REQUIRED_PACKAGES
    clang
    lldb
    lld
    libc++
    libc++abi

    libx11
    libxext
    libxrandr
    libxcursor
    libxfixes
    libxi
    libxinerama
    libxss
    wayland
    libdrm
    mesa
    systemd-libs
    libpipewire
    libibus
    dbus
    onetbb
    lzo
    libogg
    meson
    glslang
    vulkan-headers
    vulkan-icd-loader
    spirv-headers
)

message(STATUS "Checking required Arch Linux packages...")

foreach(Package IN LISTS REQUIRED_PACKAGES)

    # pacman -T resolves "provides", so split or renamed packages are still found
    execute_process(
        COMMAND pacman -T ${Package}
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
