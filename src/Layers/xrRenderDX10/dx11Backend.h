#ifndef DX11BACKEND_H
#define DX11BACKEND_H

#include "../xrRender/xrBackend/R_IBackend.h"

class CBackend_DX11 : public CBackendBase
{
public:
	enum	MaxTextures
	{
		//	Actually these values are 128
		mtMaxPixelShaderTextures = 16,
		mtMaxVertexShaderTextures = 4,
		mtMaxGeometryShaderTextures = 16,
		mtMaxHullShaderTextures = 16,
		mtMaxDomainShaderTextures = 16,
		mtMaxComputeShaderTextures = 16,
	};
	enum
	{
		MaxCBuffers = 14
	};

public:
	CBackend_DX11();
	~CBackend_DX11();

	IVertexBuffer*		CreateVertexBuffer(byte* data, u32 length, u32 stride, ResourceUsage usage) override;
	IIndexBuffer*		CreateIndexBuffer(byte* data, u32 length, ResourceUsage usage) override;

	void				set_Vertices(IVertexBuffer* _vb, u32 _vb_stride) override;
	void				set_Indices(IIndexBuffer* _ib) override;
	void				set_Scissor(Irect* rect = NULL) override;

	void				Render(PRIMITIVETYPE T, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC) override;
	void				Render(PRIMITIVETYPE T, u32 startV, u32 PC) override;


	// CBackend DX11
	void				ApplyPrimitieTopology(D3D_PRIMITIVE_TOPOLOGY Topology);

private:
	// Lists-expanded
	CTexture* textures_ps[mtMaxPixelShaderTextures];	// stages
	CTexture* textures_vs[mtMaxVertexShaderTextures];	// 4 vs
	CTexture* textures_gs[mtMaxGeometryShaderTextures];	// 4 vs
	CTexture* textures_hs[mtMaxHullShaderTextures];		// 4 vs
	CTexture* textures_ds[mtMaxDomainShaderTextures];	// 4 vs
	CTexture* textures_cs[mtMaxComputeShaderTextures];	// 4 vs

	// DX11 State
	D3D_PRIMITIVE_TOPOLOGY			m_PrimitiveTopology;
};

extern CBackend_DX11 backend_dx11_impl;



#endif // !DX11BACKEND_H
