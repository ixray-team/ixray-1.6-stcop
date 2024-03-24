#ifndef DX9BACKEND_H
#define DX9BACKEND_H

#include "../xrRender/xrBackend/R_IBackend.h"

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

	IVertexBuffer* CreateVertexBuffer(byte* data, u32 length, u32 stride, ResourceUsage usage) override;
	IIndexBuffer* CreateIndexBuffer(byte* data, u32 length, ResourceUsage usage) override;

	void set_Vertices(IVertexBuffer* _vb, u32 _vb_stride) override;
	void set_Indices(IIndexBuffer* _ib) override;
	void set_Scissor(Irect* rect = NULL) override;

public:
	// Lists-expanded
	CTexture* textures_ps[mtMaxPixelShaderTextures];	// stages
	CTexture* textures_vs[mtMaxVertexShaderTextures];	// 4 vs
};

extern CBackend_DX9 backend_dx9_impl;

#endif // !DX9BACKEND_H
