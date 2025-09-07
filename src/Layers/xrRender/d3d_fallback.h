// Minimal Direct3D type fallbacks for non-D3D builds (e.g. Vulkan on Linux)
// Guards: only active when XR_FORCE_NO_D3D is defined and we don't have real D3D headers.
#pragma once
#if defined(XR_FORCE_NO_D3D)
#ifndef XR_D3D_FALLBACK
#define XR_D3D_FALLBACK 1

#include <cstdint>
using u8  = unsigned char;
using u16 = unsigned short;
using u32 = unsigned int;
using u64 = unsigned long long;
using BOOL = int;

struct ID3DBaseTexture{}; struct ID3DTexture2D: ID3DBaseTexture {}; struct ID3DTexture3D: ID3DBaseTexture {}; 
struct ID3DRenderTargetView{}; struct ID3DDepthStencilView{}; struct ID3DVertexBuffer{}; struct ID3DIndexBuffer{}; 
struct ID3DVertexShader{}; struct ID3DPixelShader{}; struct ID3DState{}; struct ID3DBlob{}; struct ID3DInputLayout{}; 
struct ID3DGeometryShader{}; struct ID3D11HullShader{}; struct ID3D11DomainShader{}; struct ID3D11ComputeShader{}; 
struct ID3DShaderResourceView{}; struct ID3D11UnorderedAccessView{}; struct ID3DQuery{}; struct IDirect3DVertexDeclaration9{};

// Simplified DX9 vertex element structure (layout identifiers only)
struct D3DVERTEXELEMENT9 { u16 Stream; u16 Offset; u8 Type; u8 Method; u8 Usage; u8 UsageIndex; };

// Emulate minimal enums
enum D3DSTENCILOP { D3DSTENCILOP_KEEP, D3DSTENCILOP_REPLACE, D3DSTENCILOP_INCR, D3DSTENCILOP_DECR };
// Pixel formats placeholder
enum D3DFORMAT { D3DFMT_UNKNOWN = 0 };
struct D3D_TEXTURE2D_DESC { u32 Width{0}; u32 Height{0}; u32 MipLevels{1}; u32 ArraySize{1}; u32 Format{0}; };

// Pool enum used by stats_manager
enum _D3DPOOL { D3DPOOL_DEFAULT=0, D3DPOOL_MANAGED=1, D3DPOOL_SYSTEMMEM=2, D3DPOOL_SCRATCH=3 }; 

// Resource usage placeholder
enum D3D_USAGE { D3D_USAGE_DEFAULT = 0 };

// Sampler base for vertex texture constant offset
#ifndef D3DVERTEXTEXTURESAMPLER0
#define D3DVERTEXTEXTURESAMPLER0 0
#endif

// Lock flags used in code paths (streaming). Values arbitrary.
#ifndef D3DLOCK_DISCARD
#define D3DLOCK_DISCARD 0x1000
#endif
#ifndef D3DLOCK_NOOVERWRITE
#define D3DLOCK_NOOVERWRITE 0x2000
#endif

#endif // XR_D3D_FALLBACK
#endif // XR_FORCE_NO_D3D
