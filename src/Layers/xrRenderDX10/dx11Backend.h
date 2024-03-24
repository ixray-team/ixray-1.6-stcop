#ifndef DX11BACKEND_H
#define DX11BACKEND_H

#include "../xrRender/xrBackend/R_IBackend.h"
#include "dx10r_constants_cache.h"

struct Buffer_DX11
{
	ID3D11Buffer* pBuffer;
};

struct Texture_DX11
{
	ID3D11Texture1D* pTex1D;
	ID3D11Texture2D* pTex2D;
	ID3D11Texture3D* pTex3D;
	ID3D11ShaderResourceView* pSRV;
};

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


	// CBackend DX11
	void			ApplyVertexLayout();
	void			ApplyRTandZB();
	void			ApplyPrimitieTopology(D3D_PRIMITIVE_TOPOLOGY Topology);
	bool			CBuffersNeedUpdate(ref_cbuffer	buf1[MaxCBuffers], ref_cbuffer	buf2[MaxCBuffers], u32& uiMin, u32& uiMax);

private:
	// Pixel/Vertex constants
	ALIGN(16)	R_constants			constants;
	R_constant_table*				ctable;

	R_LOD							LOD;

	ref_cbuffer						m_aVertexConstants[MaxCBuffers];
	ref_cbuffer						m_aPixelConstants[MaxCBuffers];
	ref_cbuffer						m_aGeometryConstants[MaxCBuffers];
	ref_cbuffer						m_aHullConstants[MaxCBuffers];
	ref_cbuffer						m_aDomainConstants[MaxCBuffers];
	ref_cbuffer						m_aComputeConstants[MaxCBuffers];
	D3D_PRIMITIVE_TOPOLOGY			m_PrimitiveTopology;
	ID3DInputLayout*				m_pInputLayout;
	DWORD							dummy0;	//	Padding to avoid warning	
	DWORD							dummy1;	//	Padding to avoid warning	
	DWORD							dummy2;	//	Padding to avoid warning	

	// Lists-expanded
	CTexture* textures_ps[mtMaxPixelShaderTextures];	// stages
	CTexture* textures_vs[mtMaxVertexShaderTextures];	// 4 vs
	CTexture* textures_gs[mtMaxGeometryShaderTextures];	// 4 vs
	CTexture* textures_hs[mtMaxHullShaderTextures];		// 4 vs
	CTexture* textures_ds[mtMaxDomainShaderTextures];	// 4 vs
	CTexture* textures_cs[mtMaxComputeShaderTextures];	// 4 vs

};

extern CBackend_DX11 backend_dx11_impl;

inline DXGI_FORMAT GetDXGIFormat(PixelFormat format)
{
	switch (format)
	{
	case FMT_R8G8B8:
	case FMT_R8G8B8A8:
		return DXGI_FORMAT_R8G8B8A8_UNORM;
	case FMT_R32G32B32F:
		return DXGI_FORMAT_R32G32B32_FLOAT;
	case FMT_R32G32B32A32F:
		return DXGI_FORMAT_R32G32B32A32_FLOAT;
	case FMT_DEPTH32F:
		return DXGI_FORMAT_D32_FLOAT;
	case FMT_DEPTH24_STENCIL_8:
		return DXGI_FORMAT_D24_UNORM_S8_UINT;
	default:
		break;
	}

	FATAL("Unkonwed PixelFormat");
	return DXGI_FORMAT_UNKNOWN;
}

#endif // !DX11BACKEND_H
