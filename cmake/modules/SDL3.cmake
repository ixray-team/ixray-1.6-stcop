include(FetchContent)

FetchContent_Declare(
    SDL3
    URL https://github.com/libsdl-org/SDL/archive/refs/tags/release-3.2.22.tar.gz
    DOWNLOAD_EXTRACT_TIMESTAMP TRUE
)

set(SDL_SHARED ON CACHE BOOL "" FORCE)
set(SDL_STATIC OFF CACHE BOOL "" FORCE)
set(SDL_TEST_LIBRARY OFF CACHE BOOL "" FORCE)
set(SDL_REVISION "3.2.22" CACHE STRING "SDL revision" FORCE)

# Subsystems the engine and tools actually use:
#   Video + Events  - window, input, clipboard, cursors, message boxes, IME
#   Audio           - xrSound backend and voice recorder
#   Joystick/Gamepad, HIDAPI, Sensor - GamepadService (DualSense HID, gyro, rumble)
#   Render          - SDL_Renderer for xrCompress, xrLevelBuilder, xrCformConvert, SQLGen
#   Vulkan (Linux, DXVK WSI), Metal (macOS, DXMT)
# Everything else is disabled. Disabled APIs stay exported as stubs.
set(SDL_GPU OFF CACHE BOOL "" FORCE)
set(SDL_CAMERA OFF CACHE BOOL "" FORCE)
set(SDL_HAPTIC OFF CACHE BOOL "" FORCE)
set(SDL_POWER OFF CACHE BOOL "" FORCE)
set(SDL_DIALOG OFF CACHE BOOL "" FORCE)

set(SDL_OPENGL OFF CACHE BOOL "" FORCE)
set(SDL_OPENGLES OFF CACHE BOOL "" FORCE)
set(SDL_OPENVR OFF CACHE BOOL "" FORCE)

# SDL_Renderer backends: tools only need one hardware driver (software is always built)
set(SDL_RENDER_D3D OFF CACHE BOOL "" FORCE)
set(SDL_RENDER_D3D12 OFF CACHE BOOL "" FORCE)
set(SDL_RENDER_GPU OFF CACHE BOOL "" FORCE)
set(SDL_RENDER_VULKAN OFF CACHE BOOL "" FORCE)

set(SDL_DISKAUDIO OFF CACHE BOOL "" FORCE)
set(SDL_DUMMYAUDIO OFF CACHE BOOL "" FORCE)
set(SDL_DUMMYCAMERA OFF CACHE BOOL "" FORCE)
set(SDL_VIRTUAL_JOYSTICK OFF CACHE BOOL "" FORCE)
set(SDL_HIDAPI_LIBUSB OFF CACHE BOOL "" FORCE)

set(SDL_EXAMPLES OFF CACHE BOOL "" FORCE)
set(SDL_TESTS OFF CACHE BOOL "" FORCE)
set(SDL_INSTALL OFF CACHE BOOL "" FORCE)

FetchContent_MakeAvailable(SDL3)

set_target_properties(SDL_uclibc PROPERTIES FOLDER "3rd Party")
set_target_properties(SDL3-shared PROPERTIES FOLDER "3rd Party")

# SDL Image
download_single_header(
    "https://raw.githubusercontent.com/libsdl-org/SDL_image/refs/heads/main/include/SDL3_image/SDL_image.h"
    "SDL_Ext/SDL_image.h"
)