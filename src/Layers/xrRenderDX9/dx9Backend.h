#ifndef DX9BACKEND_H
#define DX9BACKEND_H

#include "../xrRender/xrBackend/R_IBackend.h"

struct Buffer_DX9
{
	IDirect3DVertexBuffer9* pVB;
	IDirect3DIndexBuffer9* pIB;
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

	void				set_Vertices(IVertexBuffer* _vb, u32 _vb_stride) override;
	void				set_Indices(IIndexBuffer* _ib) override;
	void				set_Scissor(Irect* rect = NULL) override;

	void				Render(PRIMITIVETYPE T, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC) override;
	void				Render(PRIMITIVETYPE T, u32 startV, u32 PC) override;

public:
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

#endif // !DX9BACKEND_H
