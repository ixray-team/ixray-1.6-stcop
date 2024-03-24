#ifndef R_IBACKEND_H
#define R_IBACKEND_H

#pragma once

#include "R_IConstants_cache.h"
#include "../FVF.h"
#include "../dxDebugRender.h"
#include "../R_DStreams.h"
#include "../R_Backend_xform.h"
#include "../R_Backend_hemi.h"
#include "../R_Backend_tree.h"

#ifdef USE_DX11
#include "../../xrRenderPC_R4/R_Backend_LOD.h"
#include "../../xrRenderDX10/StateManager/dx10SamplerStateCache.h"
#include "../../xrRenderDX10/StateManager/dx10StateCache.h"
#include "../../xrRenderDX10/StateManager/dx10StateManager.h"
#endif // USE_DX11


// https://wickedengine.net/2021/05/06/graphics-api-abstraction/

// Primitives supported by draw-primitive API
enum PRIMITIVETYPE {
	PT_POINTLIST = 1,
	PT_LINELIST = 2,
	PT_LINESTRIP = 3,
	PT_TRIANGLELIST = 4,
	PT_TRIANGLESTRIP = 5,
	PT_TRIANGLEFAN = 6,
	PT_FORCE_DWORD = 0x7fffffff, /* force 32-bit size enum */
};

/* Flags to construct RS_COLORWRITEENABLE */
#define COLORWRITEENABLE_RED     (1L<<0)
#define COLORWRITEENABLE_GREEN   (1L<<1)
#define COLORWRITEENABLE_BLUE    (1L<<2)
#define COLORWRITEENABLE_ALPHA   (1L<<3)

// ResourceUsage mapping and usage:
//	D3DPOOL_MANAGED - ResourceUsage::IMMUTABLE
//	D3DPOOL_DEFAULT - ResourceUsage::DYNAMIC

enum class ResourceUsage
{
	UNKNOWN,
	IMMUTABLE,	// Static resource, will never change
	DYNAMIC		// Dynamic resource, CPU Write, update on any Unmap
};

enum class Mapping
{
	MAP_READ,
	MAP_WRITE,
	MAP_READ_WRITE,
	MAP_WRITE_DISCARD,
	MAP_WRITE_NO_OVERWRITE
};

enum class TextureType
{
	TEXTURE_1D,
	TEXTURE_2D,
	TEXTURE_3D
};

struct TextureDesc
{
	TextureType textureType;
	u32 width = 1;
	u32 height = 1;
	u32 mipmapLevel = 0;
	PixelFormat format;
	bool renderTargetUsage;
};

//////////////////
// D3D9 Copy-paste

/* Vertex Buffer Description */
struct VERTEXBUFFER_DESC
{
	D3DFORMAT           Format;
	D3DRESOURCETYPE     Type;
	DWORD               Usage;
	D3DPOOL             Pool;
	UINT                Size;

	DWORD               FVF;

};

/* Index Buffer Description */
struct INDEXBUFFER_DESC
{
	D3DFORMAT           Format;
	D3DRESOURCETYPE     Type;
	DWORD               Usage;
	D3DPOOL             Pool;
	UINT                Size;
};

struct MAPPED_SUBRESOURCE {
	void* pData;
	u32 RowPitch;
	u32 DepthPitch;
};

///		detailed statistic
struct	R_statistics_element {
	u32		verts, dips;
	ICF		void	add(u32 _verts) { verts += _verts; dips++; }
};

struct	R_statistics {
	R_statistics_element		s_static;
	R_statistics_element		s_flora;
	R_statistics_element		s_flora_lods;
	R_statistics_element		s_details;
	R_statistics_element		s_ui;
	R_statistics_element		s_dynamic;
	R_statistics_element		s_dynamic_sw;
	R_statistics_element		s_dynamic_inst;
	R_statistics_element		s_dynamic_1B;
	R_statistics_element		s_dynamic_2B;
	R_statistics_element		s_dynamic_3B;
	R_statistics_element		s_dynamic_4B;
};

struct backend_stats
{
	u32								polys;
	u32								verts;
	u32								calls;
	u32								vs;
	u32								ps;
#ifdef	DEBUG
	u32								decl;
	u32								vb;
	u32								ib;
	u32								states;			// Number of times the shader-state changes
	u32								textures;		// Number of times the shader-tex changes
	u32								matrices;		// Number of times the shader-xform changes
	u32								constants;		// Number of times the shader-consts changes
#endif
	u32								xforms;
	u32								target_rt;
	u32								target_zb;

	R_statistics					r;
};

class IGraphicsResource
{
public:
	virtual ~IGraphicsResource() = default;
	inline bool IsValid() const { return m_InternalResource != nullptr; }
	
	std::shared_ptr<void> m_InternalResource;
	
	// IUnknown interface
	u64 AddRef();
	u64 Release();
};

inline u64 IGraphicsResource::AddRef()
{
	return 0;
}

inline u64 IGraphicsResource::Release()
{
	return 0;
}

///////////////////////////////////////////////////////////

// nasral, ydalit'

HRESULT VertexBuffer_Lock(IGraphicsResource* pGraphicsResource, UINT OffsetToLock, UINT SizeToLock, void** ppbData, DWORD Flags);
HRESULT VertexBuffer_Unlock(IGraphicsResource* pGraphicsResource);

HRESULT IndexBuffer_Lock(IGraphicsResource* pGraphicsResource, UINT OffsetToLock, UINT SizeToLock, void** ppbData, DWORD Flags);
HRESULT IndexBuffer_Unlock(IGraphicsResource* pGraphicsResource);

///////////////////////////////////////////////////////////

class IBufferBase : public IGraphicsResource
{

};

class IVertexBuffer : public IBufferBase
{
public:
	// nasral, ydalit'

	HRESULT Lock(UINT OffsetToLock, UINT SizeToLock, void** ppbData, DWORD Flags);
	HRESULT Unlock();

	void GetDesc(VERTEXBUFFER_DESC* pDesc);
	VERTEXBUFFER_DESC m_Desc;
};

inline HRESULT IVertexBuffer::Lock(UINT OffsetToLock, UINT SizeToLock, void** ppbData, DWORD Flags)
{
	return VertexBuffer_Lock(this, OffsetToLock, SizeToLock, ppbData, Flags);
}

inline HRESULT IVertexBuffer::Unlock()
{
	return VertexBuffer_Unlock(this);
}

inline void IVertexBuffer::GetDesc(VERTEXBUFFER_DESC* pDesc)
{
	R_ASSERT(pDesc);
	*pDesc = m_Desc;
}

class IIndexBuffer : public IGraphicsResource
{
public:
	// nasral, ydalit'

	HRESULT Lock(UINT OffsetToLock, UINT SizeToLock, void** ppbData, DWORD Flags);
	HRESULT Unlock();

	void GetDesc(INDEXBUFFER_DESC* pDesc);
	INDEXBUFFER_DESC m_Desc;
};

inline HRESULT IIndexBuffer::Lock(UINT OffsetToLock, UINT SizeToLock, void** ppbData, DWORD Flags)
{
	return IndexBuffer_Lock(this, OffsetToLock, SizeToLock, ppbData, Flags);
}

inline HRESULT IIndexBuffer::Unlock()
{
	return IndexBuffer_Unlock(this);
}

inline void IIndexBuffer::GetDesc(INDEXBUFFER_DESC* pDesc)
{
	R_ASSERT(pDesc);
	*pDesc = m_Desc;
}

///////////////////////////////////////////////////////////
// Texture stuff

class ITexture2D : public IGraphicsResource
{
public:

};

class IVertexShader : public IGraphicsResource
{
};

class IPixelShader : public IGraphicsResource
{
};

#pragma warning(push)
#pragma warning(disable:4324)
class ECORE_API CBackendBase
{
public:
	IVertexBuffer*					vb;
	IIndexBuffer*					ib;
	u32								vb_stride;

	// Shaders/State
	ID3DState*						state;
	ID3DPixelShader*				ps;
	ID3DVertexShader*				vs;
	ID3DGeometryShader*				gs;
	ID3D11HullShader*				hs;
	ID3D11DomainShader*				ds;
	ID3D11ComputeShader*			cs;

#ifdef DEBUG
	LPCSTR							ps_name;
	LPCSTR							vs_name;

	LPCSTR							gs_name;
	LPCSTR							hs_name;
	LPCSTR							ds_name;
	LPCSTR							cs_name;
#endif 

protected:
	// Render-targets
	ID3DRenderTargetView*			pRT[4];
	ID3DDepthStencilView*			pZB;
	ID3DBlob*						m_pInputSignature;
	bool							m_bChangedRTorZB;

public:
	CBackendBase();
	virtual ~CBackendBase();

	virtual IVertexBuffer*		CreateVertexBuffer(void* data, u32 length, u32 stride, ResourceUsage usage) = 0;
	virtual IIndexBuffer*		CreateIndexBuffer(void* data, u32 length, ResourceUsage usage) = 0;
	virtual ITexture2D*			CreateTexture2D(const TextureDesc* pDesc, byte* data, u32 length) = 0;

	// Buffer Mapping
	virtual bool				MapBuffer(IGraphicsResource* pResource, u32 Subresource, Mapping MapType, u32 MapFlags, MAPPED_SUBRESOURCE* pMappedResource) = 0;
	virtual void				UnmapBuffer(IGraphicsResource* pResource, u32 Subresource) = 0;

	virtual CTexture* get_ActiveTexture(u32 stage) = 0;

	IC  float					get_width();
	IC  float					get_height();
	IC  float					get_target_width();
	IC  float					get_target_height();

	// API
	IC	void					set_xform(u32 ID, const Fmatrix& M);
	IC	void					set_xform_world(const Fmatrix& M);
	IC	void					set_xform_view(const Fmatrix& M);
	IC	void					set_xform_project(const Fmatrix& M);
	IC	const Fmatrix&			get_xform_world();
	IC	const Fmatrix&			get_xform_view();
	IC	const Fmatrix&			get_xform_project();

	// Render Target API (#TODO: REFACTOR !!!)
	IC	void					set_RT(ID3DRenderTargetView* RT, u32 ID = 0);
	IC	void					set_ZB(ID3DDepthStencilView* ZB);
	IC	ID3DRenderTargetView*	get_RT(u32 ID = 0);
	IC	ID3DDepthStencilView*	get_ZB();

	IC	void					get_ConstantDirect(shared_str& n, u32 DataSize, void** pVData, void** pGData, void** pPData);

	virtual	void				set_Constants(R_constant_table* C) = 0;
	IC		void				set_Constants(ref_ctable& C_) { set_Constants(&*C_); }

	virtual void				set_Textures(STextureList* T) = 0;
	IC		void				set_Textures(ref_texture_list& T_) { set_Textures(&*T_); }

	virtual	void				set_Element(ShaderElement* S, u32	pass = 0) = 0;
	IC		void				set_Element(ref_selement& S, u32	pass = 0) { set_Element(&*S, pass); }

	IC	void					set_Shader(Shader* S, u32 pass = 0);
	IC	void					set_Shader(ref_shader& S, u32 pass = 0) { set_Shader(&*S, pass); }

	ICF	void					set_States(ID3DState* _state);
	ICF	void					set_States(ref_state& _state) { set_States(_state->state); }

#ifdef USE_DX11
	ICF  void					set_Format(SDeclaration* _decl);
#else //USE_DX11
	//ICF  void					set_Format(IDirect3DVertexDeclaration9* _decl);
#endif


	ICF void					set_VS(ref_vs& _vs);
#ifdef USE_DX11
	ICF void					set_VS(SVS* _vs);
protected:	//	In DX10 we need input shader signature which is stored in ref_vs
#endif //USE_DX11
	ICF void					set_VS(ID3DVertexShader* _vs, LPCSTR _n = 0);
#ifdef USE_DX11
public:
#endif //USE_DX11

	ICF void					set_PS(ID3DPixelShader* _ps, LPCSTR _n = 0);
	ICF void					set_PS(ref_ps& _ps) { set_PS(_ps->ps, _ps->cName.c_str()); }

#ifdef USE_DX11
	ICF void					set_GS(ID3DGeometryShader* _gs, LPCSTR _n = 0);
	ICF void					set_GS(ref_gs& _gs) { set_GS(_gs->gs, _gs->cName.c_str()); }

	ICF void					set_HS(ID3D11HullShader* _hs, LPCSTR _n = 0);
	ICF void					set_HS(ref_hs& _hs) { set_HS(_hs->sh, _hs->cName.c_str()); }

	ICF void					set_DS(ID3D11DomainShader* _ds, LPCSTR _n = 0);
	ICF void					set_DS(ref_ds& _ds) { set_DS(_ds->sh, _ds->cName.c_str()); }

	ICF void					set_CS(ID3D11ComputeShader* _cs, LPCSTR _n = 0);
	ICF void					set_CS(ref_cs& _cs) { set_CS(_cs->sh, _cs->cName.c_str()); }
#endif //USE_DX11

	virtual	void				set_Vertices(IVertexBuffer* _vb, u32 _vb_stride) = 0;
	virtual	void				set_Indices(IIndexBuffer* _ib) = 0;
	inline void					set_Geometry(SGeometry* _geom);
	inline void					set_Geometry(ref_geom& _geom) { set_Geometry(&*_geom); }
	virtual void				set_Stencil(u32 _enable, u32 _func = D3DCMP_ALWAYS, u32 _ref = 0x00, u32 _mask = 0x00, u32 _writemask = 0x00, u32 _fail = D3DSTENCILOP_KEEP, u32 _pass = D3DSTENCILOP_KEEP, u32 _zfail = D3DSTENCILOP_KEEP) = 0;
	virtual void				set_Z(u32 _enable) = 0;
	virtual void				set_ZFunc(u32 _func) = 0;
	virtual void				set_AlphaRef(u32 _value) = 0;
	virtual void				set_ColorWriteEnable(u32 _mask = COLORWRITEENABLE_RED | COLORWRITEENABLE_GREEN | COLORWRITEENABLE_BLUE | COLORWRITEENABLE_ALPHA) = 0;
	virtual void				set_CullMode(u32 _mode) = 0;
	u32							get_CullMode() { return cull_mode; }
	virtual void				set_ClipPlanes(u32 _enable, Fplane* _planes = NULL, u32 count = 0) = 0;
	virtual void				set_ClipPlanes(u32 _enable, Fmatrix* _xform = NULL, u32 fmask = 0xff) = 0;
	virtual void				set_Scissor(Irect* rect = NULL) = 0;
	
	// constants
	ICF	ref_constant			get_c(LPCSTR		n)		{ if (ctable)	return ctable->get(n); else return 0; }
	ICF	ref_constant			get_c(shared_str&	n)		{ if (ctable)	return ctable->get(n); else return 0; }

	// constants - direct (fast)
	ICF	void					set_c(R_constant* C_, const Fmatrix& A)										{ if (C_)		constants->set(C_, A);				}
	ICF	void					set_c(R_constant* C_, const Fvector4& A)									{ if (C_)		constants->set(C_, A);				}
	ICF	void					set_c(R_constant* C_, float x, float y, float z, float w)					{ if (C_)		constants->set(C_, x, y, z, w);		}
	ICF	void					set_ca(R_constant* C_, u32 e, const Fmatrix& A)								{ if (C_)		constants->seta(C_, e, A);			}
	ICF	void					set_ca(R_constant* C_, u32 e, const Fvector4& A)							{ if (C_)		constants->seta(C_, e, A);			}
	ICF	void					set_ca(R_constant* C_, u32 e, float x, float y, float z, float w)			{ if (C_)		constants->seta(C_, e, x, y, z, w); }

	// doesn't support on DX9
	ICF	void					set_c(R_constant* C_, float A)												{ if (C_)		constants->set(C_, A);				}
	ICF	void					set_c(R_constant* C_, int A)												{ if (C_)		constants->set(C_, A);				}

	// constants - LPCSTR (slow)
	ICF	void					set_c(LPCSTR n, const Fmatrix& A)							{ if (ctable)	set_c(&*ctable->get(n), A); }
	ICF	void					set_c(LPCSTR n, const Fvector4& A)							{ if (ctable)	set_c(&*ctable->get(n), A); }
	ICF	void					set_c(LPCSTR n, float x, float y, float z, float w)			{ if (ctable)	set_c(&*ctable->get(n), x, y, z, w); }
	ICF	void					set_ca(LPCSTR n, u32 e, const Fmatrix& A)					{ if (ctable)	set_ca(&*ctable->get(n), e, A); }
	ICF	void					set_ca(LPCSTR n, u32 e, const Fvector4& A)					{ if (ctable)	set_ca(&*ctable->get(n), e, A); }
	ICF	void					set_ca(LPCSTR n, u32 e, float x, float y, float z, float w) { if (ctable)	set_ca(&*ctable->get(n), e, x, y, z, w); }

	// doesn't support on DX9
	ICF	void					set_c(LPCSTR n, float A)									{ if (ctable)	set_c(&*ctable->get(n), A); }
	ICF	void					set_c(LPCSTR n, int A)										{ if (ctable)	set_c(&*ctable->get(n), A); }


	// constants - shared_str (average)
	ICF	void					set_c(shared_str& n, const Fmatrix& A)								{ if (ctable)	set_c(&*ctable->get(n), A); }
	ICF	void					set_c(shared_str& n, const Fvector4& A)								{ if (ctable)	set_c(&*ctable->get(n), A); }
	ICF	void					set_c(shared_str& n, float x, float y, float z, float w)			{ if (ctable)	set_c(&*ctable->get(n), x, y, z, w); }
	ICF	void					set_ca(shared_str& n, u32 e, const Fmatrix& A)						{ if (ctable)	set_ca(&*ctable->get(n), e, A); }
	ICF	void					set_ca(shared_str& n, u32 e, const Fvector4& A)						{ if (ctable)	set_ca(&*ctable->get(n), e, A); }
	ICF	void					set_ca(shared_str& n, u32 e, float x, float y, float z, float w)	{ if (ctable)	set_ca(&*ctable->get(n), e, x, y, z, w); }

	// doesn't support on DX9
	ICF	void					set_c(shared_str& n, float A)										{ if (ctable)	set_c(&*ctable->get(n), A); }
	ICF	void					set_c(shared_str& n, int A)											{ if (ctable)	set_c(&*ctable->get(n), A); }

	virtual	void				Render(PRIMITIVETYPE T, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC) = 0;
	virtual	void				Render(PRIMITIVETYPE T, u32 startV, u32 PC) = 0;

	// doesn't support on DX9
	ICF	void					Compute(UINT ThreadGroupCountX, UINT ThreadGroupCountY, UINT ThreadGroupCountZ);

	// Device create / destroy / frame signaling
	virtual void				RestoreQuadIBData() = 0;	// Igor: is used to test bug with rain, particles corruption
	virtual void				CreateQuadIB() = 0;
	virtual void				OnFrameBegin() = 0;
	virtual void				OnFrameEnd() = 0;
	virtual void				OnDeviceCreate() = 0;
	virtual void				OnDeviceDestroy() = 0;

	ICF	bool					is_TessEnabled();

	inline void					get_Stats(backend_stats* pStats);

	// #TODO: LAZY, get_Stats are useless in several cases
public:
	backend_stats					stat;

	// #TODO: get_Hemi, get_Tree, get_XForms
public:
	// Dynamic geometry streams
	_VertexStream					Vertex;
	_IndexStream					Index;
	IIndexBuffer*					QuadIB;
	IIndexBuffer*					old_QuadIB;
	IIndexBuffer*					CuboidIB;
	R_xforms						xforms;
	R_hemi							hemi;
	R_tree							tree;
#ifdef USE_DX11
	R_LOD							LOD;
#endif

	SDeclaration*					decl;

protected:

	u32								stencil_enable;
	u32								stencil_func;
	u32								stencil_ref;
	u32								stencil_mask;
	u32								stencil_writemask;
	u32								stencil_fail;
	u32								stencil_pass;
	u32								stencil_zfail;
	u32								colorwrite_mask;
	u32								cull_mode;
	u32								z_enable;
	u32								z_func;
	u32								alpha_ref;

protected:
	// Lists
	STextureList*					TextureList;
	SMatrixList*					MatrixList;
	SConstantList*					ConstantList;

	// Initialized on backend init
	R_IConstants*					constants;
	R_constant_table*				ctable;

};
#pragma warning(pop)

extern ECORE_API CBackendBase* g_rbackend;

// #TODO: REMOVE DX10 HACK!!!
void SRVSManager_Apply();

#define RCACHE_REMAPPING
#ifdef RCACHE_REMAPPING
#define RCache (*g_rbackend)
#endif

IC float CBackendBase::get_width()
{
	return (float)RDEVICE.Width;
}

IC float CBackendBase::get_height()
{
	return (float)RDEVICE.Height;
}

IC float CBackendBase::get_target_width()
{
	return (float)RDEVICE.TargetWidth;
}

IC float CBackendBase::get_target_height()
{
	return (float)RDEVICE.TargetHeight;
}

IC void	CBackendBase::set_xform(u32 ID, const Fmatrix& M_)
{
	stat.xforms++;
	//	TODO: DX10: Implement CBackend::set_xform
}
IC void CBackendBase::set_xform_world(const Fmatrix& M_)
{
	xforms.set_W(M_);
}
IC void CBackendBase::set_xform_view(const Fmatrix& M_)
{
	xforms.set_V(M_);
}
IC void CBackendBase::set_xform_project(const Fmatrix& M_)
{
	xforms.set_P(M_);
}
IC const Fmatrix& CBackendBase::get_xform_world() { return xforms.get_W(); }
IC const Fmatrix& CBackendBase::get_xform_view() { return xforms.get_V(); }
IC const Fmatrix& CBackendBase::get_xform_project() { return xforms.get_P(); }

#ifdef USE_DX11
IC void CBackendBase::set_RT(ID3DRenderTargetView* RT, u32 ID)
{
	if (RT != pRT[ID])
	{
		PGO(Msg("PGO:setRT"));
		stat.target_rt++;
		pRT[ID] = RT;

		//	Mark RT array dirty
		//	Reset all RT's here to allow RT to be bounded as input
		if (!m_bChangedRTorZB)
			RContext->OMSetRenderTargets(0, 0, 0);

		m_bChangedRTorZB = true;
	}
}

IC void CBackendBase::set_ZB(ID3DDepthStencilView* ZB)
{
	if (ZB != pZB)
	{
		PGO(Msg("PGO:setZB"));
		stat.target_zb++;
		pZB = ZB;

		//	Reset all RT's here to allow RT to be bounded as input
		if (!m_bChangedRTorZB)
			RContext->OMSetRenderTargets(0, 0, 0);

		m_bChangedRTorZB = true;
	}
}

IC ID3DRenderTargetView* CBackendBase::get_RT(u32 ID)
{
	VERIFY((ID >= 0) && (ID < 4));

	return pRT[ID];
}

IC ID3DDepthStencilView* CBackendBase::get_ZB()
{
	return pZB;
}

IC void CBackendBase::get_ConstantDirect(shared_str& n, u32 DataSize, void** pVData, void** pGData, void** pPData)
{
	ref_constant C_ = get_c(n);

	if (C_)
		constants->access_direct(&*C_, DataSize, pVData, pGData, pPData);
	else
	{
		if (pVData)	*pVData = 0;
		if (pGData)	*pGData = 0;
		if (pPData)	*pPData = 0;
	}
}

#else
#error "No implementation for RT API !!!"
#endif // USE_DX11

IC void CBackendBase::set_Geometry(SGeometry* _geom)
{
	set_Format(&*_geom->dcl);
	set_Vertices(_geom->vb, _geom->vb_stride);
	set_Indices(_geom->ib);
}

IC void CBackendBase::get_Stats(backend_stats* pStats)
{
	R_ASSERT(pStats);
	*pStats = stat;
}

IC void CBackendBase::set_Shader(Shader* S, u32 pass)
{
	set_Element(S->E[0], pass);
}

ICF void CBackendBase::set_States(ID3DState* _state)
{
	//	DX10 Manages states using it's own algorithm. Don't mess with it.
#ifndef USE_DX11
	if (state != _state)
#endif //USE_DX11
	{
		PGO(Msg("PGO:state_block"));
#ifdef DEBUG
		stat.states++;
#endif
		state = _state;
		state->Apply();
	}
}

ICF void CBackendBase::set_Format(SDeclaration* _decl)
{
	if (decl != _decl)
	{
		PGO(Msg("PGO:v_format:%x", _decl));
#ifdef DEBUG
		stat.decl++;
#endif
		decl = _decl;
	}
}

ICF void CBackendBase::set_VS(ref_vs& _vs)
{
	m_pInputSignature = _vs->signature->signature;
	set_VS(_vs->vs, _vs->cName.c_str());
}

ICF void CBackendBase::set_VS(SVS* _vs)
{
	m_pInputSignature = _vs->signature->signature;
	set_VS(_vs->vs, _vs->cName.c_str());
}

ICF void CBackendBase::set_VS(ID3DVertexShader* _vs, LPCSTR _n)
{
	if (vs != _vs)
	{
		PGO(Msg("PGO:Vshader:%x", _vs));
		stat.vs++;
		vs = _vs;

		RContext->VSSetShader(vs, 0, 0);

#ifdef DEBUG
		vs_name = _n;
#endif
	}
}

ICF void CBackendBase::set_PS(ID3DPixelShader* _ps, LPCSTR _n)
{
	if (ps != _ps)
	{
		PGO(Msg("PGO:Pshader:%x", _ps));
		stat.ps++;
		ps = _ps;

		RContext->PSSetShader(ps, 0, 0);


#ifdef DEBUG
		ps_name = _n;
#endif
	}
}

ICF void CBackendBase::set_GS(ID3DGeometryShader* _gs, LPCSTR _n)
{
	if (gs != _gs)
	{
		PGO(Msg("PGO:Gshader:%x", _ps));
		//	TODO: DX10: Get statistics for G Shader change

		gs = _gs;

		RContext->GSSetShader(gs, 0, 0);

#ifdef DEBUG
		gs_name = _n;
#endif
	}
}

ICF void CBackendBase::set_HS(ID3D11HullShader* _hs, LPCSTR _n)
{
	if (hs != _hs)
	{
		PGO(Msg("PGO:Hshader:%x", _ps));
		//	TODO: DX10: Get statistics for H Shader change

		hs = _hs;
		RContext->HSSetShader(hs, 0, 0);

#ifdef DEBUG
		hs_name = _n;
#endif
	}
}

ICF void CBackendBase::set_DS(ID3D11DomainShader* _ds, LPCSTR _n)
{
	if (ds != _ds)
	{
		PGO(Msg("PGO:Dshader:%x", _ps));
		//	TODO: DX10: Get statistics for D Shader change

		ds = _ds;
		RContext->DSSetShader(ds, 0, 0);

#ifdef DEBUG
		ds_name = _n;
#endif
	}
}

ICF void CBackendBase::set_CS(ID3D11ComputeShader* _cs, LPCSTR _n)
{
	if (cs != _cs)
	{
		PGO(Msg("PGO:Cshader:%x", _ps));
		//	TODO: DX10: Get statistics for D Shader change
		//stat.cs			++;
		cs = _cs;
		RContext->CSSetShader(cs, 0, 0);

#ifdef DEBUG
		cs_name = _n;
#endif
	}
}

ICF bool CBackendBase::is_TessEnabled()
{
#ifdef USE_DX11
	return true;
#else
	return false;
#endif // USE_DX11
}

ICF void CBackendBase::Compute(UINT ThreadGroupCountX, UINT ThreadGroupCountY, UINT ThreadGroupCountZ)
{
#ifdef USE_DX11
	stat.calls++;

	SRVSManager_Apply();
	StateManager.Apply();
	//	State manager may alter constants
	constants->flush();
	RContext->Dispatch(ThreadGroupCountX, ThreadGroupCountY, ThreadGroupCountZ);
#else
#error "IMPLEMENT AND MADE PURE VIRTUAL !!!"
#endif
}

class ECORE_API CDebugRenderHelper
{
public:
	// Debug render
	inline void dbg_DP(D3DPRIMITIVETYPE pt, ref_geom geom, u32 vBase, u32 pc);
	inline void dbg_DIP(D3DPRIMITIVETYPE pt, ref_geom geom, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC);

#ifdef USE_DX11
	//	TODO: DX10: Implement this.
	IC void	dbg_SetRS(D3DRENDERSTATETYPE p1, u32 p2)
	{
		VERIFY(!"Not implemented");
	}
	IC void	dbg_SetSS(u32 sampler, D3DSAMPLERSTATETYPE type, u32 value)
	{
		VERIFY(!"Not implemented");
	}
#else //USE_DX11
	IC void	dbg_SetRS(D3DRENDERSTATETYPE p1, u32 p2)
	{
		CHK_DX(RDevice->SetRenderState(p1, p2));
	}
	IC void	dbg_SetSS(u32 sampler, D3DSAMPLERSTATETYPE type, u32 value)
	{
		CHK_DX(RDevice->SetSamplerState(sampler, type, value));
	}
#endif
#ifdef DEBUG_DRAW
	IC void dbg_DrawAABB(Fvector& T_, float sx, float sy, float sz, u32 C_) { Fvector half_dim;	half_dim.set(sx, sy, sz); Fmatrix	TM;	TM.translate(T_); dbg_DrawOBB(TM, half_dim, C_); }
	IC void dbg_DrawOBB(Fmatrix& T, Fvector& half_dim, u32 C);
	IC void dbg_DrawTRI(Fmatrix& T_, Fvector* p, u32 C_) { dbg_DrawTRI(T_, p[0], p[1], p[2], C_); }
	IC void dbg_DrawTRI(Fmatrix& T, Fvector& p1, Fvector& p2, Fvector& p3, u32 C);
	IC void dbg_DrawLINE(Fmatrix& T, Fvector& p1, Fvector& p2, u32 C);
	IC void dbg_DrawEllipse(Fmatrix& T, u32 C);
#endif
};

extern ECORE_API CDebugRenderHelper DebugRenderHelper;

inline void CDebugRenderHelper::dbg_DP(D3DPRIMITIVETYPE pt, ref_geom geom, u32 vBase, u32 pc)
{
	g_rbackend->set_Geometry(geom);
	g_rbackend->Render((PRIMITIVETYPE)pt, vBase, pc);
}

inline void CDebugRenderHelper::dbg_DIP(D3DPRIMITIVETYPE pt, ref_geom geom, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC)
{
	g_rbackend->set_Geometry(geom);
	g_rbackend->Render((PRIMITIVETYPE)pt, baseV, startV, countV, startI, PC);
}


#ifdef DEBUG_DRAW
#define RGBA_GETALPHA(rgb)      ((rgb) >> 24)
void CDebugRenderHelper::dbg_DrawOBB(Fmatrix& T_, Fvector& half_dim, u32 C_)
{
	Fmatrix mL2W_Transform, mScaleTransform;

	mScaleTransform.scale(half_dim);
	mL2W_Transform.mul_43(T_, mScaleTransform);

	Fvector aabb[8] = {};
	aabb[0] = Fvector(-1, -1, -1); // 0
	aabb[1] = Fvector(-1, +1, -1); // 1
	aabb[2] = Fvector(+1, +1, -1); // 2
	aabb[3] = Fvector(+1, -1, -1); // 3
	aabb[4] = Fvector(-1, -1, +1); // 4
	aabb[5] = Fvector(-1, +1, +1); // 5
	aabb[6] = Fvector(+1, +1, +1); // 6
	aabb[7] = Fvector(+1, -1, +1); // 7

	const u32 aabb_id[12 * 2] = {
		0,1,  1,2,  2,3,  3,0,  4,5,  5,6,  6,7,  7,4,  1,5,  2,6,  3,7,  0,4
	};

	for (size_t i = 0; i < 8; i++) {
		mL2W_Transform.transform(aabb[i]);
	}

	DebugRenderImpl.add_lines(aabb, 8, aabb_id, 12, bgr2rgb(C_));
}
void CDebugRenderHelper::dbg_DrawTRI(Fmatrix& T_, Fvector& p1, Fvector& p2, Fvector& p3, u32 C_)
{
	Fvector	tri[3] = {};
	tri[0] = p1;
	tri[1] = p2;
	tri[2] = p3;

	const u32 pairs[3 * 2] = {
		0, 1, 1, 2, 2, 0
	};

	for (size_t i = 0; i < 3; i++) {
		T_.transform(tri[i]);
	}

	DebugRenderImpl.add_lines(tri, 3, pairs, 3, bgr2rgb(C_));
}

void CDebugRenderHelper::dbg_DrawLINE(Fmatrix& T_, Fvector& p1, Fvector& p2, u32 C_)
{
	Fvector line[2] = {};
	line[0] = p1;
	line[1] = p2;

	const u32 pairs[2] = {
		0, 1
	};

	for (size_t i = 0; i < 2; i++) {
		T_.transform(line[i]);
	}

	DebugRenderImpl.add_lines(line, 2, pairs, 1, bgr2rgb(C_));
}
void CDebugRenderHelper::dbg_DrawEllipse(Fmatrix& T_, u32 C_)
{
	float gVertices[] =
	{
		0.0000f,0.0000f,1.0000f,  0.0000f,0.3827f,0.9239f,  -0.1464f,0.3536f,0.9239f,
			-0.2706f,0.2706f,0.9239f,  -0.3536f,0.1464f,0.9239f,  -0.3827f,0.0000f,0.9239f,
			-0.3536f,-0.1464f,0.9239f,  -0.2706f,-0.2706f,0.9239f,  -0.1464f,-0.3536f,0.9239f,
			0.0000f,-0.3827f,0.9239f,  0.1464f,-0.3536f,0.9239f,  0.2706f,-0.2706f,0.9239f,
			0.3536f,-0.1464f,0.9239f,  0.3827f,0.0000f,0.9239f,  0.3536f,0.1464f,0.9239f,
			0.2706f,0.2706f,0.9239f,  0.1464f,0.3536f,0.9239f,  0.0000f,0.7071f,0.7071f,
			-0.2706f,0.6533f,0.7071f,  -0.5000f,0.5000f,0.7071f,  -0.6533f,0.2706f,0.7071f,
			-0.7071f,0.0000f,0.7071f,  -0.6533f,-0.2706f,0.7071f,  -0.5000f,-0.5000f,0.7071f,
			-0.2706f,-0.6533f,0.7071f,  0.0000f,-0.7071f,0.7071f,  0.2706f,-0.6533f,0.7071f,
			0.5000f,-0.5000f,0.7071f,  0.6533f,-0.2706f,0.7071f,  0.7071f,0.0000f,0.7071f,
			0.6533f,0.2706f,0.7071f,  0.5000f,0.5000f,0.7071f,  0.2706f,0.6533f,0.7071f,
			0.0000f,0.9239f,0.3827f,  -0.3536f,0.8536f,0.3827f,  -0.6533f,0.6533f,0.3827f,
			-0.8536f,0.3536f,0.3827f,  -0.9239f,0.0000f,0.3827f,  -0.8536f,-0.3536f,0.3827f,
			-0.6533f,-0.6533f,0.3827f,  -0.3536f,-0.8536f,0.3827f,  0.0000f,-0.9239f,0.3827f,
			0.3536f,-0.8536f,0.3827f,  0.6533f,-0.6533f,0.3827f,  0.8536f,-0.3536f,0.3827f,
			0.9239f,0.0000f,0.3827f,  0.8536f,0.3536f,0.3827f,  0.6533f,0.6533f,0.3827f,
			0.3536f,0.8536f,0.3827f,  0.0000f,1.0000f,0.0000f,  -0.3827f,0.9239f,0.0000f,
			-0.7071f,0.7071f,0.0000f,  -0.9239f,0.3827f,0.0000f,  -1.0000f,0.0000f,0.0000f,
			-0.9239f,-0.3827f,0.0000f,  -0.7071f,-0.7071f,0.0000f,  -0.3827f,-0.9239f,0.0000f,
			0.0000f,-1.0000f,0.0000f,  0.3827f,-0.9239f,0.0000f,  0.7071f,-0.7071f,0.0000f,
			0.9239f,-0.3827f,0.0000f,  1.0000f,0.0000f,0.0000f,  0.9239f,0.3827f,0.0000f,
			0.7071f,0.7071f,0.0000f,  0.3827f,0.9239f,0.0000f,  0.0000f,0.9239f,-0.3827f,
			-0.3536f,0.8536f,-0.3827f,  -0.6533f,0.6533f,-0.3827f,  -0.8536f,0.3536f,-0.3827f,
			-0.9239f,0.0000f,-0.3827f,  -0.8536f,-0.3536f,-0.3827f,  -0.6533f,-0.6533f,-0.3827f,
			-0.3536f,-0.8536f,-0.3827f,  0.0000f,-0.9239f,-0.3827f,  0.3536f,-0.8536f,-0.3827f,
			0.6533f,-0.6533f,-0.3827f,  0.8536f,-0.3536f,-0.3827f,  0.9239f,0.0000f,-0.3827f,
			0.8536f,0.3536f,-0.3827f,  0.6533f,0.6533f,-0.3827f,  0.3536f,0.8536f,-0.3827f,
			0.0000f,0.7071f,-0.7071f,  -0.2706f,0.6533f,-0.7071f,  -0.5000f,0.5000f,-0.7071f,
			-0.6533f,0.2706f,-0.7071f,  -0.7071f,0.0000f,-0.7071f,  -0.6533f,-0.2706f,-0.7071f,
			-0.5000f,-0.5000f,-0.7071f,  -0.2706f,-0.6533f,-0.7071f,  0.0000f,-0.7071f,-0.7071f,
			0.2706f,-0.6533f,-0.7071f,  0.5000f,-0.5000f,-0.7071f,  0.6533f,-0.2706f,-0.7071f,
			0.7071f,0.0000f,-0.7071f,  0.6533f,0.2706f,-0.7071f,  0.5000f,0.5000f,-0.7071f,
			0.2706f,0.6533f,-0.7071f,  0.0000f,0.3827f,-0.9239f,  -0.1464f,0.3536f,-0.9239f,
			-0.2706f,0.2706f,-0.9239f,  -0.3536f,0.1464f,-0.9239f,  -0.3827f,0.0000f,-0.9239f,
			-0.3536f,-0.1464f,-0.9239f,  -0.2706f,-0.2706f,-0.9239f,  -0.1464f,-0.3536f,-0.9239f,
			0.0000f,-0.3827f,-0.9239f,  0.1464f,-0.3536f,-0.9239f,  0.2706f,-0.2706f,-0.9239f,
			0.3536f,-0.1464f,-0.9239f,  0.3827f,0.0000f,-0.9239f,  0.3536f,0.1464f,-0.9239f,
			0.2706f,0.2706f,-0.9239f,  0.1464f,0.3536f,-0.9239f,  0.0000f,0.0000f,-1.0000f
	};
	u32 gFaces[224 * 3] =
	{
			0,1,2, 0,2,3, 0,3,4, 0,4,5, 0,5,6, 0,6,7, 0,7,8, 0,8,9, 0,9,10,
			0,10,11, 0,11,12, 0,12,13, 0,13,14, 0,14,15, 0,15,16, 0,16,1, 1,17,18, 1,18,2,
			2,18,19, 2,19,3, 3,19,20, 3,20,4, 4,20,21, 4,21,5, 5,21,22, 5,22,6, 6,22,23,
			6,23,7, 7,23,24, 7,24,8, 8,24,25, 8,25,9, 9,25,26, 9,26,10, 10,26,27, 10,27,11,
			11,27,28, 11,28,12, 12,28,29, 12,29,13, 13,29,30, 13,30,14, 14,30,31, 14,31,15, 15,31,32,
			15,32,16, 16,32,17, 16,17,1, 17,33,34, 17,34,18, 18,34,35, 18,35,19, 19,35,36, 19,36,20,
			20,36,37, 20,37,21, 21,37,38, 21,38,22, 22,38,39, 22,39,23, 23,39,40, 23,40,24, 24,40,41,
			24,41,25, 25,41,42, 25,42,26, 26,42,43, 26,43,27, 27,43,44, 27,44,28, 28,44,45, 28,45,29,
			29,45,46, 29,46,30, 30,46,47, 30,47,31, 31,47,48, 31,48,32, 32,48,33, 32,33,17, 33,49,50,
			33,50,34, 34,50,51, 34,51,35, 35,51,52, 35,52,36, 36,52,53, 36,53,37, 37,53,54, 37,54,38,
			38,54,55, 38,55,39, 39,55,56, 39,56,40, 40,56,57, 40,57,41, 41,57,58, 41,58,42, 42,58,59,
			42,59,43, 43,59,60, 43,60,44, 44,60,61, 44,61,45, 45,61,62, 45,62,46, 46,62,63, 46,63,47,
			47,63,64, 47,64,48, 48,64,49, 48,49,33, 49,65,66, 49,66,50, 50,66,67, 50,67,51, 51,67,68,
			51,68,52, 52,68,69, 52,69,53, 53,69,70, 53,70,54, 54,70,71, 54,71,55, 55,71,72, 55,72,56,
			56,72,73, 56,73,57, 57,73,74, 57,74,58, 58,74,75, 58,75,59, 59,75,76, 59,76,60, 60,76,77,
			60,77,61, 61,77,78, 61,78,62, 62,78,79, 62,79,63, 63,79,80, 63,80,64, 64,80,65, 64,65,49,
			65,81,82, 65,82,66, 66,82,83, 66,83,67, 67,83,84, 67,84,68, 68,84,85, 68,85,69, 69,85,86,
			69,86,70, 70,86,87, 70,87,71, 71,87,88, 71,88,72, 72,88,89, 72,89,73, 73,89,90, 73,90,74,
			74,90,91, 74,91,75, 75,91,92, 75,92,76, 76,92,93, 76,93,77, 77,93,94, 77,94,78, 78,94,95,
			78,95,79, 79,95,96, 79,96,80, 80,96,81, 80,81,65, 81,97,98, 81,98,82, 82,98,99, 82,99,83,
			83,99,100, 83,100,84, 84,100,101, 84,101,85, 85,101,102, 85,102,86, 86,102,103, 86,103,87, 87,103,104,
			87,104,88, 88,104,105, 88,105,89, 89,105,106, 89,106,90, 90,106,107, 90,107,91, 91,107,108, 91,108,92,
			92,108,109, 92,109,93, 93,109,110, 93,110,94, 94,110,111, 94,111,95, 95,111,112, 95,112,96, 96,112,97,
			96,97,81, 113,98,97, 113,99,98, 113,100,99, 113,101,100, 113,102,101, 113,103,102, 113,104,103, 113,105,104,
			113,106,105, 113,107,106, 113,108,107, 113,109,108, 113,110,109, 113,111,110, 113,112,111, 113,97,112
	};

	const int vcnt = sizeof(gVertices) / (sizeof(float) * 3);
	Fvector verts[vcnt] = {};
	for (int i = 0; i < vcnt; i++) {
		int k = i * 3;
		verts[i] = Fvector(gVertices[k], gVertices[k + 1], gVertices[k + 2]);
		T_.transform(verts[i]);
	}

	u32 pairs[224 * 6] = {};
	for (size_t i = 0; i < 224 * 3; i += 3) {
		pairs[i * 2] = gFaces[i];
		pairs[i * 2 + 1] = gFaces[i + 1];
		pairs[i * 2 + 2] = gFaces[i + 1];
		pairs[i * 2 + 3] = gFaces[i + 2];
		pairs[i * 2 + 4] = gFaces[i + 2];
		pairs[i * 2 + 5] = gFaces[i];
	}

	DebugRenderImpl.add_lines(verts, vcnt, pairs, 224 * 3, bgr2rgb(C_));
}
#endif

#endif