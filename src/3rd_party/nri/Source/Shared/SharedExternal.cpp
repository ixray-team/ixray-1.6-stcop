/*
Copyright (c) 2022, NVIDIA CORPORATION. All rights reserved.

NVIDIA CORPORATION and its licensors retain all intellectual property
and proprietary rights in and to this software, related documentation
and any modifications thereto. Any use, reproduction, disclosure or
distribution of this software and related documentation without an express
license agreement from NVIDIA CORPORATION is strictly prohibited.
*/

#include "SharedExternal.h"

#include <cstdarg>

#if _WIN32
    #include <windows.h>
#else
    #include <csignal>
#endif

constexpr std::array<const char*, uint32_t(nri::Message::TYPE_ERROR) + 1> MESSAGE_TYPE_NAME =
{
    "INFO",
    "WARNING",
    "ERROR"
};

constexpr std::array<const char*, uint32_t(nri::GraphicsAPI::VULKAN) + 1> GRAPHICS_API_NAME =
{
    "D3D11",
    "D3D12",
    "VULKAN"
};

Log::Log(nri::GraphicsAPI graphicsAPI, const nri::CallbackInterface& callbackInterface) :
    m_GraphicsAPI(graphicsAPI),
    m_CallbackInterface(callbackInterface)
{
}

void Log::ReportMessage(nri::Message message, const char* format, ...) const
{
    const char* messageTypeName = MESSAGE_TYPE_NAME[(size_t)message];
    const char* graphicsAPIName = GRAPHICS_API_NAME[(size_t)m_GraphicsAPI];

    char buffer[4096];
    int written = snprintf(buffer, GetCountOf(buffer), "[nri(%s)::%s] -- ", graphicsAPIName, messageTypeName);

    va_list	argptr;
    va_start(argptr, format);
    written += vsnprintf(buffer + written, GetCountOf(buffer) - written, format, argptr);
    va_end(argptr);

    const int end = std::min(written, (int)GetCountOf(buffer) - 2);
    buffer[end] = '\n';
    buffer[end + 1] = '\0';

    if (m_CallbackInterface.MessageCallback != nullptr)
        m_CallbackInterface.MessageCallback(m_CallbackInterface.userArg, buffer, message);

    if (message == nri::Message::TYPE_ERROR && m_CallbackInterface.AbortExecution != nullptr)
        m_CallbackInterface.AbortExecution(m_CallbackInterface.userArg);
}

void ConvertCharToWchar(const char* in, wchar_t* out, size_t outLength)
{
    if (outLength == 0)
        return;

    for (size_t i = 0; i < outLength - 1 && *in; i++)
        *out++ = *in++;

    *out = 0;
}

void ConvertWcharToChar(const wchar_t* in, char* out, size_t outLength)
{
    if (outLength == 0)
        return;

    for (size_t i = 0; i < outLength - 1 && *in; i++)
        *out++ = char(*in++);

    *out = 0;
}

#if defined(_WIN32)

#include <winerror.h>

nri::Result GetResultFromHRESULT(long result)
{
    if (SUCCEEDED(result))
        return nri::Result::SUCCESS;

    if (result == E_INVALIDARG || result == E_POINTER || result == E_HANDLE)
        return nri::Result::INVALID_ARGUMENT;

    if (result == DXGI_ERROR_UNSUPPORTED)
        return nri::Result::UNSUPPORTED;

    if (result == DXGI_ERROR_DEVICE_REMOVED || result == DXGI_ERROR_DEVICE_RESET)
        return nri::Result::DEVICE_LOST;

    if (result == E_OUTOFMEMORY)
        return nri::Result::OUT_OF_MEMORY;

    return nri::Result::FAILURE;
}
#endif

constexpr std::array<uint32_t, (size_t)nri::Format::MAX_NUM> TEXEL_BLOCK_WIDTH = {
    0, // UNKNOWN

    1, // R8_UNORM
    1, // R8_SNORM
    1, // R8_UINT
    1, // R8_SINT

    1, // RG8_UNORM
    1, // RG8_SNORM
    1, // RG8_UINT
    1, // RG8_SINT

    1, // BGRA8_UNORM
    1, // BGRA8_SRGB

    1, // RGBA8_UNORM
    1, // RGBA8_SNORM
    1, // RGBA8_UINT
    1, // RGBA8_SINT
    1, // RGBA8_SRGB

    1, // R16_UNORM
    1, // R16_SNORM
    1, // R16_UINT
    1, // R16_SINT
    1, // R16_SFLOAT

    1, // RG16_UNORM
    1, // RG16_SNORM
    1, // RG16_UINT
    1, // RG16_SINT
    1, // RG16_SFLOAT

    1, // RGBA16_UNORM
    1, // RGBA16_SNORM
    1, // RGBA16_UINT
    1, // RGBA16_SINT
    1, // RGBA16_SFLOAT

    1, // R32_UINT
    1, // R32_SINT
    1, // R32_SFLOAT

    1, // RG32_UINT
    1, // RG32_SINT
    1, // RG32_SFLOAT

    1, // RGB32_UINT
    1, // RGB32_SINT
    1, // RGB32_SFLOAT

    1, // RGBA32_UINT
    1, // RGBA32_SINT
    1, // RGBA32_SFLOAT

    1, // R10_G10_B10_A2_UNORM
    1, // R10_G10_B10_A2_UINT
    1, // R11_G11_B10_UFLOAT
    1, // R9_G9_B9_E5_UFLOAT

    4, // BC1_RGBA_UNORM
    4, // BC1_RGBA_SRGB
    4, // BC2_RGBA_UNORM
    4, // BC2_RGBA_SRGB
    4, // BC3_RGBA_UNORM
    4, // BC3_RGBA_SRGB
    4, // BC4_R_UNORM
    4, // BC4_R_SNORM
    4, // BC5_RG_UNORM
    4, // BC5_RG_SNORM
    4, // BC6H_RGB_UFLOAT
    4, // BC6H_RGB_SFLOAT
    4, // BC7_RGBA_UNORM
    4, // BC7_RGBA_SRGB

    // DEPTH_STENCIL_ATTACHMENT views
    1, // D16_UNORM
    1, // D24_UNORM_S8_UINT
    1, // D32_SFLOAT
    1, // D32_SFLOAT_S8_UINT_X24

    // Depth-stencil specific SHADER_RESOURCE views
    0, // R24_UNORM_X8
    0, // X24_R8_UINT
    0, // X32_R8_UINT_X24
    0, // R32_SFLOAT_X8_X24
};

uint32_t GetTexelBlockWidth(nri::Format format)
{
    return TEXEL_BLOCK_WIDTH[(size_t)format];
}

constexpr std::array<uint32_t, (size_t)nri::Format::MAX_NUM> TEXEL_BLOCK_SIZE = {
    1, // UNKNOWN

    1, // R8_UNORM
    1, // R8_SNORM
    1, // R8_UINT
    1, // R8_SINT

    2, // RG8_UNORM
    2, // RG8_SNORM
    2, // RG8_UINT
    2, // RG8_SINT

    4, // BGRA8_UNORM
    4, // BGRA8_SRGB

    4, // RGBA8_UNORM
    4, // RGBA8_SNORM
    4, // RGBA8_UINT
    4, // RGBA8_SINT
    4, // RGBA8_SRGB

    2, // R16_UNORM
    2, // R16_SNORM
    2, // R16_UINT
    2, // R16_SINT
    2, // R16_SFLOAT

    4, // RG16_UNORM
    4, // RG16_SNORM
    4, // RG16_UINT
    4, // RG16_SINT
    4, // RG16_SFLOAT

    8, // RGBA16_UNORM
    8, // RGBA16_SNORM
    8, // RGBA16_UINT
    8, // RGBA16_SINT
    8, // RGBA16_SFLOAT

    4, // R32_UINT
    4, // R32_SINT
    4, // R32_SFLOAT

    8, // RG32_UINT
    8, // RG32_SINT
    8, // RG32_SFLOAT

    12, // RGB32_UINT
    12, // RGB32_SINT
    12, // RGB32_SFLOAT

    16, // RGBA32_UINT
    16, // RGBA32_SINT
    16, // RGBA32_SFLOAT

    4, // R10_G10_B10_A2_UNORM
    4, // R10_G10_B10_A2_UINT
    4, // R11_G11_B10_UFLOAT
    4, // R9_G9_B9_E5_UFLOAT

    8, // BC1_RGBA_UNORM
    8, // BC1_RGBA_SRGB
    16, // BC2_RGBA_UNORM
    16, // BC2_RGBA_SRGB
    16, // BC3_RGBA_UNORM
    16, // BC3_RGBA_SRGB
    8, // BC4_R_UNORM
    8, // BC4_R_SNORM
    16, // BC5_RG_UNORM
    16, // BC5_RG_SNORM
    16, // BC6H_RGB_UFLOAT
    16, // BC6H_RGB_SFLOAT
    16, // BC7_RGBA_UNORM
    16, // BC7_RGBA_SRGB

    // DEPTH_STENCIL_ATTACHMENT views
    2, // D16_UNORM
    4, // D24_UNORM_S8_UINT
    4, // D32_SFLOAT
    8, // D32_SFLOAT_S8_UINT_X24

    // Depth-stencil specific SHADER_RESOURCE views
    0, // R24_UNORM_X8
    0, // X24_R8_UINT
    0, // X32_R8_UINT_X24
    0, // R32_SFLOAT_X8_X24
};

uint32_t GetTexelBlockSize(nri::Format format)
{
    return TEXEL_BLOCK_SIZE[(size_t)format];
}

constexpr std::array<nri::Format, 100> DXGI_FORMAT_TABLE =
{
    nri::Format::UNKNOWN,                              // DXGI_FORMAT_UNKNOWN = 0,

    nri::Format::UNKNOWN,                              // DXGI_FORMAT_R32G32B32A32_TYPELESS = 1,
    nri::Format::RGBA32_SFLOAT,                        // DXGI_FORMAT_R32G32B32A32_FLOAT = 2,
    nri::Format::RGBA32_UINT,                          // DXGI_FORMAT_R32G32B32A32_UINT = 3,
    nri::Format::RGBA32_SINT,                          // DXGI_FORMAT_R32G32B32A32_SINT = 4,

    nri::Format::UNKNOWN,                              // DXGI_FORMAT_R32G32B32_TYPELESS = 5,
    nri::Format::RGB32_SFLOAT,                         // DXGI_FORMAT_R32G32B32_FLOAT = 6,
    nri::Format::RGB32_UINT,                           // DXGI_FORMAT_R32G32B32_UINT = 7,
    nri::Format::RGB32_SINT,                           // DXGI_FORMAT_R32G32B32_SINT = 8,

    nri::Format::UNKNOWN,                              // DXGI_FORMAT_R16G16B16A16_TYPELESS = 9,
    nri::Format::RGBA16_SFLOAT,                        // DXGI_FORMAT_R16G16B16A16_FLOAT = 10,
    nri::Format::RGBA16_UNORM,                         // DXGI_FORMAT_R16G16B16A16_UNORM = 11,
    nri::Format::RGBA16_UINT,                          // DXGI_FORMAT_R16G16B16A16_UINT = 12,
    nri::Format::RGBA16_SNORM,                         // DXGI_FORMAT_R16G16B16A16_SNORM = 13,
    nri::Format::RGBA16_SINT,                          // DXGI_FORMAT_R16G16B16A16_SINT = 14,

    nri::Format::UNKNOWN,                              // DXGI_FORMAT_R32G32_TYPELESS = 15,
    nri::Format::RG32_SFLOAT,                          // DXGI_FORMAT_R32G32_FLOAT = 16,
    nri::Format::RG32_UINT,                            // DXGI_FORMAT_R32G32_UINT = 17,
    nri::Format::RGB32_SINT,                           // DXGI_FORMAT_R32G32_SINT = 18,
    nri::Format::UNKNOWN,                              // DXGI_FORMAT_R32G8X24_TYPELESS = 19,
    nri::Format::D32_SFLOAT_S8_UINT_X24,               // DXGI_FORMAT_D32_FLOAT_S8X24_UINT = 20,
    nri::Format::R32_SFLOAT_X8_X24,                    // DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS = 21,
    nri::Format::X32_R8_UINT_X24,                      // DXGI_FORMAT_X32_TYPELESS_G8X24_UINT = 22,

    nri::Format::UNKNOWN,                              // DXGI_FORMAT_R10G10B10A2_TYPELESS = 23,
    nri::Format::R10_G10_B10_A2_UNORM,                 // DXGI_FORMAT_R10G10B10A2_UNORM = 24,
    nri::Format::R10_G10_B10_A2_UINT,                  // DXGI_FORMAT_R10G10B10A2_UINT = 25,
    nri::Format::R11_G11_B10_UFLOAT,                   // DXGI_FORMAT_R11G11B10_FLOAT = 26,

    nri::Format::UNKNOWN,                              // DXGI_FORMAT_R8G8B8A8_TYPELESS = 27,
    nri::Format::RGBA8_UNORM,                          // DXGI_FORMAT_R8G8B8A8_UNORM = 28,
    nri::Format::RGBA8_SRGB,                           // DXGI_FORMAT_R8G8B8A8_UNORM_SRGB = 29,
    nri::Format::RGBA8_UINT,                           // DXGI_FORMAT_R8G8B8A8_UINT = 30,
    nri::Format::RGBA8_SNORM,                          // DXGI_FORMAT_R8G8B8A8_SNORM = 31,
    nri::Format::RGBA8_SINT,                           // DXGI_FORMAT_R8G8B8A8_SINT = 32,

    nri::Format::UNKNOWN,                              // DXGI_FORMAT_R16G16_TYPELESS = 33,
    nri::Format::RG16_SFLOAT,                          // DXGI_FORMAT_R16G16_FLOAT = 34,
    nri::Format::RG16_UNORM,                           // DXGI_FORMAT_R16G16_UNORM = 35,
    nri::Format::RG16_UINT,                            // DXGI_FORMAT_R16G16_UINT = 36,
    nri::Format::RG16_SNORM,                           // DXGI_FORMAT_R16G16_SNORM = 37,
    nri::Format::RG16_SINT,                            // DXGI_FORMAT_R16G16_SINT = 38,

    nri::Format::UNKNOWN,                              // DXGI_FORMAT_R32_TYPELESS = 39,
    nri::Format::D32_SFLOAT,                           // DXGI_FORMAT_D32_FLOAT = 40,
    nri::Format::R32_SFLOAT,                           // DXGI_FORMAT_R32_FLOAT = 41,
    nri::Format::R32_UINT,                             // DXGI_FORMAT_R32_UINT = 42,
    nri::Format::R32_SINT,                             // DXGI_FORMAT_R32_SINT = 43,
    nri::Format::UNKNOWN,                              // DXGI_FORMAT_R24G8_TYPELESS = 44,
    nri::Format::D24_UNORM_S8_UINT,                    // DXGI_FORMAT_D24_UNORM_S8_UINT = 45,
    nri::Format::R24_UNORM_X8,                         // DXGI_FORMAT_R24_UNORM_X8_TYPELESS = 46,
    nri::Format::X24_R8_UINT,                          // DXGI_FORMAT_X24_TYPELESS_G8_UINT = 47,

    nri::Format::UNKNOWN,                              // DXGI_FORMAT_R8G8_TYPELESS = 48,
    nri::Format::RG8_UNORM,                            // DXGI_FORMAT_R8G8_UNORM = 49,
    nri::Format::RG8_UINT,                             // DXGI_FORMAT_R8G8_UINT = 50,
    nri::Format::RG8_SNORM,                            // DXGI_FORMAT_R8G8_SNORM = 51,
    nri::Format::RG8_SINT,                             // DXGI_FORMAT_R8G8_SINT = 52,

    nri::Format::UNKNOWN,                              // DXGI_FORMAT_R16_TYPELESS = 53,
    nri::Format::R16_SFLOAT,                           // DXGI_FORMAT_R16_FLOAT = 54,
    nri::Format::D16_UNORM,                            // DXGI_FORMAT_D16_UNORM = 55,
    nri::Format::R16_UNORM,                            // DXGI_FORMAT_R16_UNORM = 56,
    nri::Format::R16_UINT,                             // DXGI_FORMAT_R16_UINT = 57,
    nri::Format::R16_SNORM,                            // DXGI_FORMAT_R16_SNORM = 58,
    nri::Format::R16_SINT,                             // DXGI_FORMAT_R16_SINT = 59,

    nri::Format::UNKNOWN,                              // DXGI_FORMAT_R8_TYPELESS = 60,
    nri::Format::R8_UNORM,                             // DXGI_FORMAT_R8_UNORM = 61,
    nri::Format::R8_UINT,                              // DXGI_FORMAT_R8_UINT = 62,
    nri::Format::R8_SNORM,                             // DXGI_FORMAT_R8_SNORM = 63,
    nri::Format::R8_SINT,                              // DXGI_FORMAT_R8_SINT = 64,
    nri::Format::UNKNOWN,                              // DXGI_FORMAT_A8_UNORM = 65,

    nri::Format::UNKNOWN,                              // DXGI_FORMAT_R1_UNORM = 66,
    nri::Format::R9_G9_B9_E5_UFLOAT,                   // DXGI_FORMAT_R9G9B9E5_SHAREDEXP = 67,
    nri::Format::UNKNOWN,                              // DXGI_FORMAT_R8G8_B8G8_UNORM = 68,
    nri::Format::UNKNOWN,                              // DXGI_FORMAT_G8R8_G8B8_UNORM = 69,
    nri::Format::UNKNOWN,                              // DXGI_FORMAT_BC1_TYPELESS = 70,
    nri::Format::BC1_RGBA_UNORM,                       // DXGI_FORMAT_BC1_UNORM = 71,
    nri::Format::BC1_RGBA_SRGB,                        // DXGI_FORMAT_BC1_UNORM_SRGB = 72,
    nri::Format::UNKNOWN,                              // DXGI_FORMAT_BC2_TYPELESS = 73,
    nri::Format::BC2_RGBA_UNORM,                       // DXGI_FORMAT_BC2_UNORM = 74,
    nri::Format::BC2_RGBA_SRGB,                        // DXGI_FORMAT_BC2_UNORM_SRGB = 75,
    nri::Format::UNKNOWN,                              // DXGI_FORMAT_BC3_TYPELESS = 76,
    nri::Format::BC3_RGBA_UNORM,                       // DXGI_FORMAT_BC3_UNORM = 77,
    nri::Format::BC3_RGBA_SRGB,                        // DXGI_FORMAT_BC3_UNORM_SRGB = 78,
    nri::Format::UNKNOWN,                              // DXGI_FORMAT_BC4_TYPELESS = 79,
    nri::Format::BC4_R_UNORM,                          // DXGI_FORMAT_BC4_UNORM = 80,
    nri::Format::BC4_R_SNORM,                          // DXGI_FORMAT_BC4_SNORM = 81,
    nri::Format::UNKNOWN,                              // DXGI_FORMAT_BC5_TYPELESS = 82,
    nri::Format::BC5_RG_UNORM,                         // DXGI_FORMAT_BC5_UNORM = 83,
    nri::Format::BC5_RG_SNORM,                         // DXGI_FORMAT_BC5_SNORM = 84,
    nri::Format::UNKNOWN,                              // DXGI_FORMAT_B5G6R5_UNORM = 85,
    nri::Format::UNKNOWN,                              // DXGI_FORMAT_B5G5R5A1_UNORM = 86,
    nri::Format::BGRA8_UNORM,                          // DXGI_FORMAT_B8G8R8A8_UNORM = 87,
    nri::Format::UNKNOWN,                              // DXGI_FORMAT_B8G8R8X8_UNORM = 88,
    nri::Format::UNKNOWN,                              // DXGI_FORMAT_R10G10B10_XR_BIAS_A2_UNORM = 89,
    nri::Format::UNKNOWN,                              // DXGI_FORMAT_B8G8R8A8_TYPELESS = 90,
    nri::Format::BGRA8_SRGB,                           // DXGI_FORMAT_B8G8R8A8_UNORM_SRGB = 91,
    nri::Format::UNKNOWN,                              // DXGI_FORMAT_B8G8R8X8_TYPELESS = 92,
    nri::Format::UNKNOWN,                              // DXGI_FORMAT_B8G8R8X8_UNORM_SRGB = 93,
    nri::Format::UNKNOWN,                              // DXGI_FORMAT_BC6H_TYPELESS = 94,
    nri::Format::BC6H_RGB_UFLOAT,                      // DXGI_FORMAT_BC6H_UF16 = 95,
    nri::Format::BC6H_RGB_SFLOAT,                      // DXGI_FORMAT_BC6H_SF16 = 96,
    nri::Format::UNKNOWN,                              // DXGI_FORMAT_BC7_TYPELESS = 97,
    nri::Format::BC7_RGBA_UNORM,                       // DXGI_FORMAT_BC7_UNORM = 98,
    nri::Format::BC7_RGBA_SRGB,                        // DXGI_FORMAT_BC7_UNORM_SRGB = 99,
};

nri::Format DXGIFormatToNRIFormat(uint32_t dxgiFormat)
{
    if (dxgiFormat < DXGI_FORMAT_TABLE.size())
        return DXGI_FORMAT_TABLE[dxgiFormat];

    return nri::Format::UNKNOWN;
}

// TODO: add packed formats and depth-specific
constexpr std::array<nri::Format, 261> VK_FORMAT_TABLE = {
    nri::Format::UNKNOWN, // VK_FORMAT_UNDEFINED = 0,
    nri::Format::UNKNOWN, // VK_FORMAT_R4G4_UNORM_PACK8 = 1,
    nri::Format::UNKNOWN, // VK_FORMAT_R4G4B4A4_UNORM_PACK16 = 2,
    nri::Format::UNKNOWN, // VK_FORMAT_B4G4R4A4_UNORM_PACK16 = 3,
    nri::Format::UNKNOWN, // VK_FORMAT_R5G6B5_UNORM_PACK16 = 4,
    nri::Format::UNKNOWN, // VK_FORMAT_B5G6R5_UNORM_PACK16 = 5,
    nri::Format::UNKNOWN, // VK_FORMAT_R5G5B5A1_UNORM_PACK16 = 6,
    nri::Format::UNKNOWN, // VK_FORMAT_B5G5R5A1_UNORM_PACK16 = 7,
    nri::Format::UNKNOWN, // VK_FORMAT_A1R5G5B5_UNORM_PACK16 = 8,
    nri::Format::R8_UNORM, // VK_FORMAT_R8_UNORM = 9,
    nri::Format::R8_SNORM, // VK_FORMAT_R8_SNORM = 10,
    nri::Format::UNKNOWN, // VK_FORMAT_R8_USCALED = 11,
    nri::Format::UNKNOWN, // VK_FORMAT_R8_SSCALED = 12,
    nri::Format::R8_UINT, // VK_FORMAT_R8_UINT = 13,
    nri::Format::R8_SINT, // VK_FORMAT_R8_SINT = 14,
    nri::Format::UNKNOWN, // VK_FORMAT_R8_SRGB = 15,
    nri::Format::RG8_UNORM, // VK_FORMAT_R8G8_UNORM = 16,
    nri::Format::RG8_SNORM, // VK_FORMAT_R8G8_SNORM = 17,
    nri::Format::UNKNOWN, // VK_FORMAT_R8G8_USCALED = 18,
    nri::Format::UNKNOWN, // VK_FORMAT_R8G8_SSCALED = 19,
    nri::Format::RG8_UINT, // VK_FORMAT_R8G8_UINT = 20,
    nri::Format::RG8_SINT, // VK_FORMAT_R8G8_SINT = 21,
    nri::Format::UNKNOWN, // VK_FORMAT_R8G8_SRGB = 22,
    nri::Format::UNKNOWN, // VK_FORMAT_R8G8B8_UNORM = 23,
    nri::Format::UNKNOWN, // VK_FORMAT_R8G8B8_SNORM = 24,
    nri::Format::UNKNOWN, // VK_FORMAT_R8G8B8_USCALED = 25,
    nri::Format::UNKNOWN, // VK_FORMAT_R8G8B8_SSCALED = 26,
    nri::Format::UNKNOWN, // VK_FORMAT_R8G8B8_UINT = 27,
    nri::Format::UNKNOWN, // VK_FORMAT_R8G8B8_SINT = 28,
    nri::Format::UNKNOWN, // VK_FORMAT_R8G8B8_SRGB = 29,
    nri::Format::UNKNOWN, // VK_FORMAT_B8G8R8_UNORM = 30,
    nri::Format::UNKNOWN, // VK_FORMAT_B8G8R8_SNORM = 31,
    nri::Format::UNKNOWN, // VK_FORMAT_B8G8R8_USCALED = 32,
    nri::Format::UNKNOWN, // VK_FORMAT_B8G8R8_SSCALED = 33,
    nri::Format::UNKNOWN, // VK_FORMAT_B8G8R8_UINT = 34,
    nri::Format::UNKNOWN, // VK_FORMAT_B8G8R8_SINT = 35,
    nri::Format::UNKNOWN, // VK_FORMAT_B8G8R8_SRGB = 36,
    nri::Format::RGBA8_UNORM, // VK_FORMAT_R8G8B8A8_UNORM = 37,
    nri::Format::RGBA8_SNORM, // VK_FORMAT_R8G8B8A8_SNORM = 38,
    nri::Format::UNKNOWN, // VK_FORMAT_R8G8B8A8_USCALED = 39,
    nri::Format::UNKNOWN, // VK_FORMAT_R8G8B8A8_SSCALED = 40,
    nri::Format::RGBA8_UINT, // VK_FORMAT_R8G8B8A8_UINT = 41,
    nri::Format::RGBA8_SINT, // VK_FORMAT_R8G8B8A8_SINT = 42,
    nri::Format::RGBA8_SRGB, // VK_FORMAT_R8G8B8A8_SRGB = 43,
    nri::Format::BGRA8_UNORM, // VK_FORMAT_B8G8R8A8_UNORM = 44,
    nri::Format::UNKNOWN, // VK_FORMAT_B8G8R8A8_SNORM = 45,
    nri::Format::UNKNOWN, // VK_FORMAT_B8G8R8A8_USCALED = 46,
    nri::Format::UNKNOWN, // VK_FORMAT_B8G8R8A8_SSCALED = 47,
    nri::Format::UNKNOWN, // VK_FORMAT_B8G8R8A8_UINT = 48,
    nri::Format::UNKNOWN, // VK_FORMAT_B8G8R8A8_SINT = 49,
    nri::Format::BGRA8_SRGB, // VK_FORMAT_B8G8R8A8_SRGB = 50,
    nri::Format::UNKNOWN, // VK_FORMAT_A8B8G8R8_UNORM_PACK32 = 51,
    nri::Format::UNKNOWN, // VK_FORMAT_A8B8G8R8_SNORM_PACK32 = 52,
    nri::Format::UNKNOWN, // VK_FORMAT_A8B8G8R8_USCALED_PACK32 = 53,
    nri::Format::UNKNOWN, // VK_FORMAT_A8B8G8R8_SSCALED_PACK32 = 54,
    nri::Format::UNKNOWN, // VK_FORMAT_A8B8G8R8_UINT_PACK32 = 55,
    nri::Format::UNKNOWN, // VK_FORMAT_A8B8G8R8_SINT_PACK32 = 56,
    nri::Format::UNKNOWN, // VK_FORMAT_A8B8G8R8_SRGB_PACK32 = 57,
    nri::Format::UNKNOWN, // VK_FORMAT_A2R10G10B10_UNORM_PACK32 = 58,
    nri::Format::UNKNOWN, // VK_FORMAT_A2R10G10B10_SNORM_PACK32 = 59,
    nri::Format::UNKNOWN, // VK_FORMAT_A2R10G10B10_USCALED_PACK32 = 60,
    nri::Format::UNKNOWN, // VK_FORMAT_A2R10G10B10_SSCALED_PACK32 = 61,
    nri::Format::UNKNOWN, // VK_FORMAT_A2R10G10B10_UINT_PACK32 = 62,
    nri::Format::UNKNOWN, // VK_FORMAT_A2R10G10B10_SINT_PACK32 = 63,
    nri::Format::R10_G10_B10_A2_UNORM, // VK_FORMAT_A2B10G10R10_UNORM_PACK32 = 64,
    nri::Format::UNKNOWN, // VK_FORMAT_A2B10G10R10_SNORM_PACK32 = 65,
    nri::Format::UNKNOWN, // VK_FORMAT_A2B10G10R10_USCALED_PACK32 = 66,
    nri::Format::UNKNOWN, // VK_FORMAT_A2B10G10R10_SSCALED_PACK32 = 67,
    nri::Format::R10_G10_B10_A2_UINT, // VK_FORMAT_A2B10G10R10_UINT_PACK32 = 68,
    nri::Format::UNKNOWN, // VK_FORMAT_A2B10G10R10_SINT_PACK32 = 69,
    nri::Format::R16_UNORM, // VK_FORMAT_R16_UNORM = 70,
    nri::Format::R16_SNORM, // VK_FORMAT_R16_SNORM = 71,
    nri::Format::UNKNOWN, // VK_FORMAT_R16_USCALED = 72,
    nri::Format::UNKNOWN, // VK_FORMAT_R16_SSCALED = 73,
    nri::Format::R16_UINT, // VK_FORMAT_R16_UINT = 74,
    nri::Format::R16_SINT, // VK_FORMAT_R16_SINT = 75,
    nri::Format::R16_SFLOAT, // VK_FORMAT_R16_SFLOAT = 76,
    nri::Format::RG16_UNORM, // VK_FORMAT_R16G16_UNORM = 77,
    nri::Format::RG16_SNORM, // VK_FORMAT_R16G16_SNORM = 78,
    nri::Format::UNKNOWN, // VK_FORMAT_R16G16_USCALED = 79,
    nri::Format::UNKNOWN, // VK_FORMAT_R16G16_SSCALED = 80,
    nri::Format::RG16_UINT, // VK_FORMAT_R16G16_UINT = 81,
    nri::Format::RG16_SINT, // VK_FORMAT_R16G16_SINT = 82,
    nri::Format::RG16_SFLOAT, // VK_FORMAT_R16G16_SFLOAT = 83,
    nri::Format::UNKNOWN, // VK_FORMAT_R16G16B16_UNORM = 84,
    nri::Format::UNKNOWN, // VK_FORMAT_R16G16B16_SNORM = 85,
    nri::Format::UNKNOWN, // VK_FORMAT_R16G16B16_USCALED = 86,
    nri::Format::UNKNOWN, // VK_FORMAT_R16G16B16_SSCALED = 87,
    nri::Format::UNKNOWN, // VK_FORMAT_R16G16B16_UINT = 88,
    nri::Format::UNKNOWN, // VK_FORMAT_R16G16B16_SINT = 89,
    nri::Format::UNKNOWN, // VK_FORMAT_R16G16B16_SFLOAT = 90,
    nri::Format::RGBA16_UNORM, // VK_FORMAT_R16G16B16A16_UNORM = 91,
    nri::Format::RGBA16_SNORM, // VK_FORMAT_R16G16B16A16_SNORM = 92,
    nri::Format::UNKNOWN, // VK_FORMAT_R16G16B16A16_USCALED = 93,
    nri::Format::UNKNOWN, // VK_FORMAT_R16G16B16A16_SSCALED = 94,
    nri::Format::RGBA16_UINT, // VK_FORMAT_R16G16B16A16_UINT = 95,
    nri::Format::RGBA16_SINT, // VK_FORMAT_R16G16B16A16_SINT = 96,
    nri::Format::RGBA16_SFLOAT, // VK_FORMAT_R16G16B16A16_SFLOAT = 97,
    nri::Format::R32_UINT, // VK_FORMAT_R32_UINT = 98,
    nri::Format::R32_SINT, // VK_FORMAT_R32_SINT = 99,
    nri::Format::R32_SFLOAT, // VK_FORMAT_R32_SFLOAT = 100,
    nri::Format::RG32_UINT, // VK_FORMAT_R32G32_UINT = 101,
    nri::Format::RG32_SINT, // VK_FORMAT_R32G32_SINT = 102,
    nri::Format::RG32_SFLOAT, // VK_FORMAT_R32G32_SFLOAT = 103,
    nri::Format::RGB32_UINT, // VK_FORMAT_R32G32B32_UINT = 104,
    nri::Format::RGB32_SINT, // VK_FORMAT_R32G32B32_SINT = 105,
    nri::Format::RGB32_SFLOAT, // VK_FORMAT_R32G32B32_SFLOAT = 106,
    nri::Format::RGBA32_UINT, // VK_FORMAT_R32G32B32A32_UINT = 107,
    nri::Format::RGBA32_SINT, // VK_FORMAT_R32G32B32A32_SINT = 108,
    nri::Format::RGBA32_SFLOAT, // VK_FORMAT_R32G32B32A32_SFLOAT = 109,
    nri::Format::UNKNOWN, // VK_FORMAT_R64_UINT = 110,
    nri::Format::UNKNOWN, // VK_FORMAT_R64_SINT = 111,
    nri::Format::UNKNOWN, // VK_FORMAT_R64_SFLOAT = 112,
    nri::Format::UNKNOWN, // VK_FORMAT_R64G64_UINT = 113,
    nri::Format::UNKNOWN, // VK_FORMAT_R64G64_SINT = 114,
    nri::Format::UNKNOWN, // VK_FORMAT_R64G64_SFLOAT = 115,
    nri::Format::UNKNOWN, // VK_FORMAT_R64G64B64_UINT = 116,
    nri::Format::UNKNOWN, // VK_FORMAT_R64G64B64_SINT = 117,
    nri::Format::UNKNOWN, // VK_FORMAT_R64G64B64_SFLOAT = 118,
    nri::Format::UNKNOWN, // VK_FORMAT_R64G64B64A64_UINT = 119,
    nri::Format::UNKNOWN, // VK_FORMAT_R64G64B64A64_SINT = 120,
    nri::Format::UNKNOWN, // VK_FORMAT_R64G64B64A64_SFLOAT = 121,
    nri::Format::R11_G11_B10_UFLOAT, // VK_FORMAT_B10G11R11_UFLOAT_PACK32 = 122,
    nri::Format::R9_G9_B9_E5_UFLOAT, // VK_FORMAT_E5B9G9R9_UFLOAT_PACK32 = 123,
    nri::Format::UNKNOWN, // VK_FORMAT_D16_UNORM = 124,
    nri::Format::UNKNOWN, // VK_FORMAT_X8_D24_UNORM_PACK32 = 125,
    nri::Format::UNKNOWN, // VK_FORMAT_D32_SFLOAT = 126,
    nri::Format::UNKNOWN, // VK_FORMAT_S8_UINT = 127,
    nri::Format::UNKNOWN, // VK_FORMAT_D16_UNORM_S8_UINT = 128,
    nri::Format::UNKNOWN, // VK_FORMAT_D24_UNORM_S8_UINT = 129,
    nri::Format::UNKNOWN, // VK_FORMAT_D32_SFLOAT_S8_UINT = 130,
    nri::Format::UNKNOWN, // VK_FORMAT_BC1_RGB_UNORM_BLOCK = 131,
    nri::Format::UNKNOWN, // VK_FORMAT_BC1_RGB_SRGB_BLOCK = 132,
    nri::Format::UNKNOWN, // VK_FORMAT_BC1_RGBA_UNORM_BLOCK = 133,
    nri::Format::UNKNOWN, // VK_FORMAT_BC1_RGBA_SRGB_BLOCK = 134,
    nri::Format::UNKNOWN, // VK_FORMAT_BC2_UNORM_BLOCK = 135,
    nri::Format::UNKNOWN, // VK_FORMAT_BC2_SRGB_BLOCK = 136,
    nri::Format::UNKNOWN, // VK_FORMAT_BC3_UNORM_BLOCK = 137,
    nri::Format::UNKNOWN, // VK_FORMAT_BC3_SRGB_BLOCK = 138,
    nri::Format::UNKNOWN, // VK_FORMAT_BC4_UNORM_BLOCK = 139,
    nri::Format::UNKNOWN, // VK_FORMAT_BC4_SNORM_BLOCK = 140,
    nri::Format::UNKNOWN, // VK_FORMAT_BC5_UNORM_BLOCK = 141,
    nri::Format::UNKNOWN, // VK_FORMAT_BC5_SNORM_BLOCK = 142,
    nri::Format::UNKNOWN, // VK_FORMAT_BC6H_UFLOAT_BLOCK = 143,
    nri::Format::UNKNOWN, // VK_FORMAT_BC6H_SFLOAT_BLOCK = 144,
    nri::Format::UNKNOWN, // VK_FORMAT_BC7_UNORM_BLOCK = 145,
    nri::Format::UNKNOWN, // VK_FORMAT_BC7_SRGB_BLOCK = 146,
    nri::Format::UNKNOWN, // VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK = 147,
    nri::Format::UNKNOWN, // VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK = 148,
    nri::Format::UNKNOWN, // VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK = 149,
    nri::Format::UNKNOWN, // VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK = 150,
    nri::Format::UNKNOWN, // VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK = 151,
    nri::Format::UNKNOWN, // VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK = 152,
    nri::Format::UNKNOWN, // VK_FORMAT_EAC_R11_UNORM_BLOCK = 153,
    nri::Format::UNKNOWN, // VK_FORMAT_EAC_R11_SNORM_BLOCK = 154,
    nri::Format::UNKNOWN, // VK_FORMAT_EAC_R11G11_UNORM_BLOCK = 155,
    nri::Format::UNKNOWN, // VK_FORMAT_EAC_R11G11_SNORM_BLOCK = 156,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_4x4_UNORM_BLOCK = 157,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_4x4_SRGB_BLOCK = 158,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_5x4_UNORM_BLOCK = 159,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_5x4_SRGB_BLOCK = 160,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_5x5_UNORM_BLOCK = 161,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_5x5_SRGB_BLOCK = 162,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_6x5_UNORM_BLOCK = 163,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_6x5_SRGB_BLOCK = 164,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_6x6_UNORM_BLOCK = 165,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_6x6_SRGB_BLOCK = 166,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_8x5_UNORM_BLOCK = 167,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_8x5_SRGB_BLOCK = 168,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_8x6_UNORM_BLOCK = 169,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_8x6_SRGB_BLOCK = 170,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_8x8_UNORM_BLOCK = 171,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_8x8_SRGB_BLOCK = 172,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_10x5_UNORM_BLOCK = 173,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_10x5_SRGB_BLOCK = 174,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_10x6_UNORM_BLOCK = 175,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_10x6_SRGB_BLOCK = 176,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_10x8_UNORM_BLOCK = 177,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_10x8_SRGB_BLOCK = 178,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_10x10_UNORM_BLOCK = 179,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_10x10_SRGB_BLOCK = 180,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_12x10_UNORM_BLOCK = 181,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_12x10_SRGB_BLOCK = 182,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_12x12_UNORM_BLOCK = 183,
    nri::Format::UNKNOWN, // VK_FORMAT_ASTC_12x12_SRGB_BLOCK = 184,
    nri::Format::UNKNOWN, // VK_FORMAT_G8B8G8R8_422_UNORM = 1000156000,
    nri::Format::UNKNOWN, // VK_FORMAT_B8G8R8G8_422_UNORM = 1000156001,
    nri::Format::UNKNOWN, // VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM = 1000156002,
    nri::Format::UNKNOWN, // VK_FORMAT_G8_B8R8_2PLANE_420_UNORM = 1000156003,
    nri::Format::UNKNOWN, // VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM = 1000156004,
    nri::Format::UNKNOWN, // VK_FORMAT_G8_B8R8_2PLANE_422_UNORM = 1000156005,
    nri::Format::UNKNOWN, // VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM = 1000156006,
    nri::Format::UNKNOWN, // VK_FORMAT_R10X6_UNORM_PACK16 = 1000156007,
    nri::Format::UNKNOWN, // VK_FORMAT_R10X6G10X6_UNORM_2PACK16 = 1000156008,
    nri::Format::UNKNOWN, // VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16 = 1000156009,
    nri::Format::UNKNOWN, // VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16 = 1000156010,
    nri::Format::UNKNOWN, // VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16 = 1000156011,
    nri::Format::UNKNOWN, // VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16 = 1000156012,
    nri::Format::UNKNOWN, // VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16 = 1000156013,
    nri::Format::UNKNOWN, // VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16 = 1000156014,
    nri::Format::UNKNOWN, // VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16 = 1000156015,
    nri::Format::UNKNOWN, // VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16 = 1000156016,
    nri::Format::UNKNOWN, // VK_FORMAT_R12X4_UNORM_PACK16 = 1000156017,
    nri::Format::UNKNOWN, // VK_FORMAT_R12X4G12X4_UNORM_2PACK16 = 1000156018,
    nri::Format::UNKNOWN, // VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16 = 1000156019,
    nri::Format::UNKNOWN, // VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16 = 1000156020,
    nri::Format::UNKNOWN, // VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16 = 1000156021,
    nri::Format::UNKNOWN, // VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16 = 1000156022,
    nri::Format::UNKNOWN, // VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16 = 1000156023,
    nri::Format::UNKNOWN, // VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16 = 1000156024,
    nri::Format::UNKNOWN, // VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16 = 1000156025,
    nri::Format::UNKNOWN, // VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16 = 1000156026,
    nri::Format::UNKNOWN, // VK_FORMAT_G16B16G16R16_422_UNORM = 1000156027,
    nri::Format::UNKNOWN, // VK_FORMAT_B16G16R16G16_422_UNORM = 1000156028,
    nri::Format::UNKNOWN, // VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM = 1000156029,
    nri::Format::UNKNOWN, // VK_FORMAT_G16_B16R16_2PLANE_420_UNORM = 1000156030,
    nri::Format::UNKNOWN, // VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM = 1000156031,
    nri::Format::UNKNOWN, // VK_FORMAT_G16_B16R16_2PLANE_422_UNORM = 1000156032,
    nri::Format::UNKNOWN, // VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM = 1000156033,
    nri::Format::UNKNOWN, // VK_FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG = 1000054000,
    nri::Format::UNKNOWN, // VK_FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG = 1000054001,
    nri::Format::UNKNOWN, // VK_FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG = 1000054002,
    nri::Format::UNKNOWN, // VK_FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG = 1000054003,
    nri::Format::UNKNOWN, // VK_FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG = 1000054004,
    nri::Format::UNKNOWN, // VK_FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG = 1000054005,
    nri::Format::UNKNOWN, // VK_FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG = 1000054006,
    nri::Format::UNKNOWN, // VK_FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG = 1000054007,
    nri::Format::UNKNOWN, // VK_FORMAT_G8B8G8R8_422_UNORM_KHR = VK_FORMAT_G8B8G8R8_422_UNORM,
    nri::Format::UNKNOWN, // VK_FORMAT_B8G8R8G8_422_UNORM_KHR = VK_FORMAT_B8G8R8G8_422_UNORM,
    nri::Format::UNKNOWN, // VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM_KHR = VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM,
    nri::Format::UNKNOWN, // VK_FORMAT_G8_B8R8_2PLANE_420_UNORM_KHR = VK_FORMAT_G8_B8R8_2PLANE_420_UNORM,
    nri::Format::UNKNOWN, // VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM_KHR = VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM,
    nri::Format::UNKNOWN, // VK_FORMAT_G8_B8R8_2PLANE_422_UNORM_KHR = VK_FORMAT_G8_B8R8_2PLANE_422_UNORM,
    nri::Format::UNKNOWN, // VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM_KHR = VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM,
    nri::Format::UNKNOWN, // VK_FORMAT_R10X6_UNORM_PACK16_KHR = VK_FORMAT_R10X6_UNORM_PACK16,
    nri::Format::UNKNOWN, // VK_FORMAT_R10X6G10X6_UNORM_2PACK16_KHR = VK_FORMAT_R10X6G10X6_UNORM_2PACK16,
    nri::Format::UNKNOWN, // VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16_KHR = VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16,
    nri::Format::UNKNOWN, // VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16_KHR = VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16,
    nri::Format::UNKNOWN, // VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16_KHR = VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16,
    nri::Format::UNKNOWN, // VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16_KHR = VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16,
    nri::Format::UNKNOWN, // VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16_KHR = VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16,
    nri::Format::UNKNOWN, // VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16_KHR = VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16,
    nri::Format::UNKNOWN, // VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16_KHR = VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16,
    nri::Format::UNKNOWN, // VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16_KHR = VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16,
    nri::Format::UNKNOWN, // VK_FORMAT_R12X4_UNORM_PACK16_KHR = VK_FORMAT_R12X4_UNORM_PACK16,
    nri::Format::UNKNOWN, // VK_FORMAT_R12X4G12X4_UNORM_2PACK16_KHR = VK_FORMAT_R12X4G12X4_UNORM_2PACK16,
    nri::Format::UNKNOWN, // VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16_KHR = VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16,
    nri::Format::UNKNOWN, // VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16_KHR = VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16,
    nri::Format::UNKNOWN, // VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16_KHR = VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16,
    nri::Format::UNKNOWN, // VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16_KHR = VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16,
    nri::Format::UNKNOWN, // VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16_KHR = VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16,
    nri::Format::UNKNOWN, // VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16_KHR = VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16,
    nri::Format::UNKNOWN, // VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16_KHR = VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16,
    nri::Format::UNKNOWN, // VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16_KHR = VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16,
    nri::Format::UNKNOWN, // VK_FORMAT_G16B16G16R16_422_UNORM_KHR = VK_FORMAT_G16B16G16R16_422_UNORM,
    nri::Format::UNKNOWN, // VK_FORMAT_B16G16R16G16_422_UNORM_KHR = VK_FORMAT_B16G16R16G16_422_UNORM,
    nri::Format::UNKNOWN, // VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM_KHR = VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM,
    nri::Format::UNKNOWN, // VK_FORMAT_G16_B16R16_2PLANE_420_UNORM_KHR = VK_FORMAT_G16_B16R16_2PLANE_420_UNORM,
    nri::Format::UNKNOWN, // VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM_KHR = VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM,
    nri::Format::UNKNOWN, // VK_FORMAT_G16_B16R16_2PLANE_422_UNORM_KHR = VK_FORMAT_G16_B16R16_2PLANE_422_UNORM,
    nri::Format::UNKNOWN  // VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM_KHR = VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM,
};

nri::Format VKFormatToNRIFormat(uint32_t format)
{
    if (format < VK_FORMAT_TABLE.size())
        return VK_FORMAT_TABLE[format];

    return nri::Format::UNKNOWN;
}

static void MessageCallback(void* userArg, const char* message, nri::Message messageType)
{
    MaybeUnused(userArg);
    MaybeUnused(messageType);

    fprintf(stderr, "%s", message);
#if _WIN32
    OutputDebugStringA(message);
#endif
}

static void AbortExecution(void* userArg)
{
    MaybeUnused(userArg);

#if _WIN32
    DebugBreak();
#else
    raise(SIGTRAP);
#endif
}

void CheckAndSetDefaultCallbacks(nri::CallbackInterface& callbackInterface)
{
    if (callbackInterface.MessageCallback == nullptr)
        callbackInterface.MessageCallback = MessageCallback;

    if (callbackInterface.AbortExecution == nullptr)
        callbackInterface.AbortExecution = AbortExecution;
}