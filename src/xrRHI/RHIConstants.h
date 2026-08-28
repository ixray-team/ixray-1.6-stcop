#pragma once

constexpr u32 RHI_VERTEX_TEXTURESAMPLER = 256;
constexpr u32 RHI_REQ_TEXTURE2D_U_OR_V_DIMENSION = 16384;
constexpr u32 RHI_SHADERS_TYPE_SIZE = 6;
constexpr u32 RHI_MAX_RENDER_TARGETS = 6;
constexpr u32 RHI_MAX_CONSTANT_BUFFERS = 22;
constexpr u32 RHI_APPEND_ALIGNED_ELEMENT = 0xffffffff;

// Legacy shit
constexpr u32 RHI_FVF_RESERVED0        = 0x001;
constexpr u32 RHI_FVF_POSITION_MASK    = 0x400E;
constexpr u32 RHI_FVF_XYZ              = 0x002;
constexpr u32 RHI_FVF_XYZRHW           = 0x004;
constexpr u32 RHI_FVF_XYZB1            = 0x006;
constexpr u32 RHI_FVF_XYZB2            = 0x008;
constexpr u32 RHI_FVF_XYZB3            = 0x00a;
constexpr u32 RHI_FVF_XYZB4            = 0x00c;
constexpr u32 RHI_FVF_XYZB5            = 0x00e;
constexpr u32 RHI_FVF_XYZW             = 0x4002;

constexpr u32 RHI_FVF_NORMAL           = 0x010;
constexpr u32 RHI_FVF_PSIZE            = 0x020;
constexpr u32 RHI_FVF_DIFFUSE          = 0x040;
constexpr u32 RHI_FVF_SPECULAR         = 0x080;

constexpr u32 RHI_FVF_TEXCOUNT_MASK    = 0xf00;
constexpr u32 RHI_FVF_TEXCOUNT_SHIFT   = 8;
constexpr u32 RHI_FVF_TEX0             = 0x000;
constexpr u32 RHI_FVF_TEX1             = 0x100;
constexpr u32 RHI_FVF_TEX2             = 0x200;
constexpr u32 RHI_FVF_TEX3             = 0x300;
constexpr u32 RHI_FVF_TEX4             = 0x400;
constexpr u32 RHI_FVF_TEX5             = 0x500;
constexpr u32 RHI_FVF_TEX6             = 0x600;
constexpr u32 RHI_FVF_TEX7             = 0x700;
constexpr u32 RHI_FVF_TEX8             = 0x800;

constexpr u32 RHI_FVF_LASTBETA_UBYTE4   = 0x1000;
constexpr u32 RHI_FVF_LASTBETA_D3DCOLOR = 0x8000;
constexpr u32 RHI_FVF_RESERVED2			= 0x6000;

constexpr u32 RHI_COMMONSHADER_SAMPLER_SLOT_COUNT = 16;