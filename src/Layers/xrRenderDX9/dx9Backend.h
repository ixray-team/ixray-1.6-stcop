#ifndef DX9BACKEND_H
#define DX9BACKEND_H

#include "../xrRender/xrBackend/R_IBackend.h"
#include "dx9r_constants_cache.h"
#include <d3d9types.h>

struct Buffer_DX9
{
	IDirect3DVertexBuffer9* pVB;
	IDirect3DIndexBuffer9* pIB;

	static void Destroy(IGraphicsResource* pGraphicsResource);
};

struct Texture_DX9
{
	IDirect3DTexture9* pTex;

	static void Destroy(IGraphicsResource* pGraphicsResource);
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

	IVertexBuffer*		CreateVertexBuffer(void* data, u32 length, u32 stride, ResourceUsage usage) override;
	IIndexBuffer*		CreateIndexBuffer(void* data, u32 length, ResourceUsage usage) override;
	
	// Unified texture creation
	IBaseTexture*		CreateTexture(const TextureDesc* pDesc, const SUBRESOURCE_DATA* pSubresource) override;

	bool				MapBuffer(IGraphicsResource* pResource, u32 Subresource, Mapping MapType, u32 MapFlags, MAPPED_SUBRESOURCE* pMappedResource) override;
	void				UnmapBuffer(IGraphicsResource* pResource, u32 Subresource) override;

	void				set_Constants(R_constant_table* _C) override;
	void				set_Textures(STextureList* _T) override;
	void				set_Element(ShaderElement* S, u32	pass = 0) override;

	CTexture*			get_ActiveTexture(u32 stage) override;

	void				set_Vertices(IVertexBuffer* _vb, u32 _vb_stride)  override;
	void				set_Indices(IIndexBuffer* _ib) override;
	void				set_Stencil(u32 _enable, u32 _func = D3DCMP_ALWAYS, u32 _ref = 0x00, u32 _mask = 0x00, u32 _writemask = 0x00, u32 _fail = D3DSTENCILOP_KEEP, u32 _pass = D3DSTENCILOP_KEEP, u32 _zfail = D3DSTENCILOP_KEEP)  override;
	void				set_Z(u32 _enable) override;
	void				set_ZFunc(u32 _func)  override;
	void				set_AlphaRef(u32 _value) override;
	void				set_ColorWriteEnable(u32 _mask = COLORWRITEENABLE_RED | COLORWRITEENABLE_GREEN | COLORWRITEENABLE_BLUE | COLORWRITEENABLE_ALPHA) override;
	void				set_CullMode(u32 _mode)  override;
	void				set_ClipPlanes(u32 _enable, Fplane* _planes = NULL, u32 count = 0) override;
	void				set_ClipPlanes(u32 _enable, Fmatrix* _xform = NULL, u32 fmask = 0xff)  override;
	void				set_Scissor(Irect* rect = NULL)  override;

	void				Render(PRIMITIVETYPE T, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC) override;
	void				Render(PRIMITIVETYPE T, u32 startV, u32 PC) override;

	// Device create / destroy / frame signaling
	void				RestoreQuadIBData() override;	// Igor: is used to test bug with rain, particles corruption
	void				CreateQuadIB() override;
	void				OnFrameBegin() override;
	void				OnFrameEnd() override;
	void				OnDeviceCreate() override;
	void				OnDeviceDestroy() override;

	void				Invalidate();

public:
	// Pixel/Vertex constants
	//ALIGN(16)	R_constants_DX9			constants;
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
