#ifndef DX9BACKEND_H
#define DX9BACKEND_H

#include "../xrRender/xrBackend/R_IBackend.h"
#include "dx9r_constants_cache.h"

struct Buffer_DX9
{
	IDirect3DVertexBuffer9* pVB;
	IDirect3DIndexBuffer9* pIB;
};

struct Texture_DX9
{
	IDirect3DTexture9* pTex;
};

class CBackend_DX9 : public CBackendBase
{
public:
	enum	MaxTextures
	{
		mtMaxPixelShaderTextures = 16,
		mtMaxVertexShaderTextures = 4,
	};

public:
	CBackend_DX9();
	~CBackend_DX9();

	IVertexBuffer*		CreateVertexBuffer(byte* data, u32 length, u32 stride, ResourceUsage usage) override;
	IIndexBuffer*		CreateIndexBuffer(byte* data, u32 length, ResourceUsage usage) override;
	ITexture2D*			CreateTexture2D(const TextureDesc* pDesc, byte* data, u32 length) override;

	void				set_Constants(R_constant_table* C) override;
	void				set_Textures(STextureList* T) override;
	void				set_Element(ShaderElement* S, u32	pass = 0) override;

	CTexture*			get_ActiveTexture(u32 stage) override;

	void				set_Vertices(IVertexBuffer* _vb, u32 _vb_stride) override;
	void				set_Indices(IIndexBuffer* _ib) override;
	void				set_Scissor(Irect* rect = NULL) override;

	void				Render(PRIMITIVETYPE T, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC) override;
	void				Render(PRIMITIVETYPE T, u32 startV, u32 PC) override;

public:
	// Pixel/Vertex constants
	ALIGN(16)	R_constants			constants;
	R_constant_table*				ctable;

	// Lists-expanded
	CTexture* textures_ps[mtMaxPixelShaderTextures];	// stages
	CTexture* textures_vs[mtMaxVertexShaderTextures];	// 4 vs
};

extern CBackend_DX9 backend_dx9_impl;

inline D3DPRIMITIVETYPE GetD3DPrimitiveType(PRIMITIVETYPE pt)
{
	return (D3DPRIMITIVETYPE)pt;

	switch (pt)
	{
	case PT_POINTLIST:
		return D3DPT_POINTLIST;
	case PT_LINELIST:
		return D3DPT_LINELIST;
	case PT_LINESTRIP:
		return D3DPT_LINESTRIP;
	case PT_TRIANGLELIST:
		return D3DPT_TRIANGLELIST;
	case PT_TRIANGLESTRIP:
		return D3DPT_TRIANGLESTRIP;
	case PT_TRIANGLEFAN:
		return D3DPT_TRIANGLEFAN;
	}

	FATAL("Unknowed PRIMITIVETYPE");
	return (D3DPRIMITIVETYPE)0;
}

inline D3DFORMAT GetD3DFormat(PixelFormat format)
{
	// #TODO: Floating-point formats

	switch (format)
	{
	case FMT_R8G8B8:
		return D3DFMT_R8G8B8;
	case FMT_R8G8B8A8:
		return D3DFMT_A8R8G8B8;
	//case FMT_R32G32B32F:
	//	return D3DFMT_A32B32G32R32F;
	//case FMT_R32G32B32A32F:
	//	return D3DFMT_A32B32G32R32F;
	case FMT_DEPTH32F:
		return D3DFMT_D32F_LOCKABLE;
	case FMT_DEPTH24_STENCIL_8:
		return D3DFMT_D24S8;

	}

	FATAL("Unknowed PixelFormat");
	return D3DFMT_UNKNOWN;
}

#endif // !DX9BACKEND_H
