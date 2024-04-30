#pragma once

#define RHI_MAKEFOURCC(ch0, ch1, ch2, ch3)                              \
                ((u32)(u8)(ch0) | ((u32)(u8)(ch1) << 8) |       \
                ((u32)(u8)(ch2) << 16) | ((u32)(u8)(ch3) << 24 ))

enum ERHITextureFormat
{
	FMT_UNKNOWN,

	FMT_R8G8B8,
	FMT_A8R8G8B8,
	FMT_A8B8G8R8,
	FMT_X8R8G8B8,

	FMT_R5G6B5,
	FMT_G16R16,
	FMT_A16B16G16R16,
	FMT_L8,
	FMT_V8U8,
	FMT_Q8W8V8U8,
	FMT_V16U16,
	FMT_D24X8,
	FMT_D32F_LOCKABLE,
	FMT_G16R16F,
	FMT_A16B16G16R16F,
	FMT_R32F,
	FMT_R16F,
	FMT_A32B32G32R32F,

	FMT_UYVY                 = RHI_MAKEFOURCC('U', 'Y', 'V', 'Y'),
    FMT_R8G8_B8G8            = RHI_MAKEFOURCC('R', 'G', 'B', 'G'),
    FMT_YUY2                 = RHI_MAKEFOURCC('Y', 'U', 'Y', '2'),
    FMT_G8R8_G8B8            = RHI_MAKEFOURCC('G', 'R', 'G', 'B'),
    FMT_DXT1                 = RHI_MAKEFOURCC('D', 'X', 'T', '1'),
    FMT_DXT2                 = RHI_MAKEFOURCC('D', 'X', 'T', '2'),
    FMT_DXT3                 = RHI_MAKEFOURCC('D', 'X', 'T', '3'),
    FMT_DXT4                 = RHI_MAKEFOURCC('D', 'X', 'T', '4'),
    FMT_DXT5                 = RHI_MAKEFOURCC('D', 'X', 'T', '5'),
};