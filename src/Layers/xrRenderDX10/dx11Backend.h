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

	IVertexBuffer*		CreateVertexBuffer(void* data, u32 length, u32 stride, ResourceUsage usage) override;
	IIndexBuffer*		CreateIndexBuffer(void* data, u32 length, ResourceUsage usage) override;
	ITexture2D*			CreateTexture2D(const TextureDesc* pDesc, byte* data, u32 length) override;

	// Buffer Mapping
	bool				MapBuffer(IGraphicsResource* pResource, u32 Subresource, Mapping MapType, u32 MapFlags, MAPPED_SUBRESOURCE* pMappedResource) override;
	void				UnmapBuffer(IGraphicsResource* pResource, u32 Subresource) override;

	void				set_Constants(R_constant_table* C) override;
	void				set_Textures(STextureList* T) override;
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

	// #TODO: make me virtual
	void				Invalidate();

	// #TODO: make me private
public:
	//// Pixel/Vertex constants
	//ALIGN(16)	R_constants			constants;
	//R_constant_table*				ctable;

	//R_LOD							LOD;

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

	// Old DX11 Backend stuff
	// Needs to be rewriten
private:

	void			ApplyVertexLayout();
	void			ApplyRTandZB();
	void			ApplyPrimitieTopology(D3D_PRIMITIVE_TOPOLOGY Topology);
	bool			CBuffersNeedUpdate(ref_cbuffer	buf1[MaxCBuffers], ref_cbuffer	buf2[MaxCBuffers], u32& uiMin, u32& uiMax);

//	bool					m_bChangedRTorZB;
};

extern CBackend_DX11 backend_dx11_impl;

inline DXGI_FORMAT GetDXGIFormat(PixelFormat format)
{
	switch (format)
	{
	case FMT_R8G8B8:
	case FMT_R8G8B8A8:
		return DXGI_FORMAT_R8G8B8A8_UNORM;
	case FMT_R32G32F:
		return DXGI_FORMAT_R32G32_FLOAT;
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

inline D3D11_MAP GetD3DMap(Mapping map)
{
	switch (map)
	{
	case Mapping::MAP_READ:
		return D3D11_MAP_READ;
	case Mapping::MAP_WRITE:
		return D3D11_MAP_WRITE;
	case Mapping::MAP_READ_WRITE:
		return D3D11_MAP_READ_WRITE;
	case Mapping::MAP_WRITE_DISCARD:
		return D3D11_MAP_WRITE_DISCARD;
	case Mapping::MAP_WRITE_NO_OVERWRITE:
		return D3D11_MAP_WRITE_NO_OVERWRITE;
	}

	FATAL("Unkonwed Mapping");
	return (D3D11_MAP)0;
}

IC D3D_PRIMITIVE_TOPOLOGY TranslateTopology(D3DPRIMITIVETYPE T)
{
	static	D3D_PRIMITIVE_TOPOLOGY translateTable[] =
	{
		D3D_PRIMITIVE_TOPOLOGY_UNDEFINED,		//	None
		D3D_PRIMITIVE_TOPOLOGY_POINTLIST,		//	D3DPT_POINTLIST = 1,
		D3D_PRIMITIVE_TOPOLOGY_LINELIST,		//	D3DPT_LINELIST = 2,
		D3D_PRIMITIVE_TOPOLOGY_LINESTRIP,		//	D3DPT_LINESTRIP = 3,
		D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST,	//	D3DPT_TRIANGLELIST = 4,
		D3D_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP,	//	D3DPT_TRIANGLESTRIP = 5,
		D3D_PRIMITIVE_TOPOLOGY_UNDEFINED,		//	D3DPT_TRIANGLEFAN = 6,
	};

	VERIFY(T < sizeof(translateTable) / sizeof(translateTable[0]));
	VERIFY(T >= 0);

	D3D_PRIMITIVE_TOPOLOGY	result = translateTable[T];

	VERIFY(result != D3D_PRIMITIVE_TOPOLOGY_UNDEFINED);

	return result;
}

IC u32 GetIndexCount(D3DPRIMITIVETYPE T, u32 iPrimitiveCount)
{
	switch (T)
	{
	case D3DPT_POINTLIST:
		return iPrimitiveCount;
	case D3DPT_LINELIST:
		return iPrimitiveCount * 2;
	case D3DPT_LINESTRIP:
		return iPrimitiveCount + 1;
	case D3DPT_TRIANGLELIST:
		return iPrimitiveCount * 3;
	case D3DPT_TRIANGLESTRIP:
		return iPrimitiveCount + 2;
	default: NODEFAULT;
#ifdef DEBUG
		return 0;
#endif // #ifdef DEBUG
	}
}

#endif // !DX11BACKEND_H
