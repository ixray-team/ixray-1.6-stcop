#pragma once

#include "R_DStreams.h"
#include "r_constants_cache.h"
#include "R_Backend_xform.h"
#include "R_Backend_hemi.h"
#include "R_Backend_tree.h"
#include "../../xrRHI/RHIEnums.h"
#include "../../xrRHI/RHITopologyUtils.h"

#ifdef USE_DX11
#	include "..\xrRenderPC_R4\r_backend_lod.h"
#endif

#include "FVF.h"

const	u32		CULL_CCW			= D3DCULL_CCW;
const	u32		CULL_CW				= D3DCULL_CW;
const	u32		CULL_NONE			= D3DCULL_NONE;

///		detailed statistic
struct	R_statistics_element	{
	u32		verts,dips;
	ICF		void	add			(u32 _verts)	{ verts+=_verts; dips++; }
};
struct	R_statistics			{
	R_statistics_element		s_static		;
	R_statistics_element		s_flora			;
	R_statistics_element		s_flora_lods	;
	R_statistics_element		s_details		;
	R_statistics_element		s_ui			;
	R_statistics_element		s_dynamic		;
	R_statistics_element		s_dynamic_sw	;
	R_statistics_element		s_dynamic_inst	;
	R_statistics_element		s_dynamic_1B	;
	R_statistics_element		s_dynamic_2B	;
	R_statistics_element		s_dynamic_3B	;
	R_statistics_element		s_dynamic_4B	;
};

#pragma warning(push)
#pragma warning(disable:4324)
class  ECORE_API CBackend
{
public:
#ifdef USE_DX11
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
		MaxCBuffers	= 22
	};
#else //USE_DX11
	enum	MaxTextures
	{
		mtMaxPixelShaderTextures = 16,
		mtMaxVertexShaderTextures = 4,
	};
#endif
	


public:            
	// Dynamic geometry streams
	_VertexStream					Vertex;
	_IndexStream					Index;

	IRHIBuffer* QuadIB;
	IRHIBuffer* old_QuadIB;
	IRHIBuffer* CuboidIB;

	R_xforms						xforms;
	R_hemi							hemi;
	R_tree							tree;
#ifdef USE_DX11
	R_LOD							LOD;
#endif

#ifdef USE_DX11
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
#endif
private:
	// Vertices/Indices/etc
#ifdef USE_DX11
	SDeclaration*					decl;
#else //USE_DX11
	IDirect3DVertexDeclaration9*	decl;
#endif
	IRHIBuffer* vb;
	IRHIBuffer* ib;
	u32								vb_stride;

	// Pixel/Vertex constants
	ALIGN(16)	R_constants			constants;
	R_constant_table*				ctable;

	// Shaders/State
	ID3DState*						state;

	// Lists
	STextureList*					T;
	SMatrixList*					M;
	SConstantList*					C;

	// Lists-expanded
	CTexture*						textures_ps	[mtMaxPixelShaderTextures];	// stages
	CTexture*						textures_vs	[mtMaxVertexShaderTextures];	// 4 vs
#ifdef USE_DX11
	CTexture*						textures_gs	[mtMaxGeometryShaderTextures];	// 4 vs
	CTexture*						textures_hs	[mtMaxHullShaderTextures];	// 4 vs
	CTexture*						textures_ds	[mtMaxDomainShaderTextures];	// 4 vs
	CTexture*						textures_cs	[mtMaxComputeShaderTextures];	// 4 vs
#endif //USE_DX11
#ifdef _EDITOR
	CMatrix*						matrices	[8	];	// matrices are supported only for FFP
#endif

	void							Invalidate	();
public:
	struct _stats
	{
		u32								polys;
		u32								verts;
		u32								calls;
		u32								xforms;
		u32								target_zb;

		R_statistics					r	;
	}									stat;
public:
	IC	CTexture*					get_ActiveTexture			(u32 stage)
	{
		if (stage<CTexture::rstVertex)			return textures_ps[stage];
		else if (stage<CTexture::rstGeometry)	return textures_vs[stage-CTexture::rstVertex];
#ifdef USE_DX11
		else if (stage<CTexture::rstHull)	return textures_gs[stage-CTexture::rstGeometry];
		else if (stage<CTexture::rstDomain) return textures_hs[stage-CTexture::rstHull];
		else if (stage<CTexture::rstCompute) return textures_ds[stage-CTexture::rstDomain];
		else if (stage<CTexture::rstInvalid) return textures_cs[stage-CTexture::rstCompute];
		else
		{
			VERIFY(!"Invalid texture stage");
			return 0;
		}
#else //USE_DX11
		VERIFY(!"Invalid texture stage");
		return 0;
#endif
	}

#ifdef USE_DX11
	IC	void						get_ConstantDirect	(shared_str& n, u32 DataSize, void** pVData, void** pGData, void** pPData);
#else //USE_DX11
	IC	R_constant_array&			get_ConstantCache_Vertex	()			{ return constants.a_vertex;	}
	IC	R_constant_array&			get_ConstantCache_Pixel		()			{ return constants.a_pixel;		}
#endif

	IC  float							get_width();
	IC  float							get_height();	
	IC  float							get_target_width();
	IC  float							get_target_height();

	// API
	IC	void						set_xform			(u32 ID, const Fmatrix& M);
	IC	void						set_xform_world		(const Fmatrix& M);
	IC	void						set_xform_view		(const Fmatrix& M);
	IC	void						set_xform_project	(const Fmatrix& M);

	IC	void						set_xform_world_old	(const Fmatrix& M);
	IC	void						set_xform_view_old	(const Fmatrix& M);
	IC	void						set_xform_project_old (const Fmatrix& M);

	IC	const Fmatrix&				get_xform_world		();
	IC	const Fmatrix&				get_xform_view		();
	IC	const Fmatrix&				get_xform_project	();

	IC	const Fmatrix&				get_xform_world_old	();
	IC	const Fmatrix&				get_xform_view_old	();
	IC	const Fmatrix&				get_xform_project_old ();

	IC	void						set_RT				(IRHIRenderTargetView* RT, u32 ID=0);
	IC	IRHIRenderTargetView*		get_RT				(u32 ID=0);

	IC	void						set_Constants		(R_constant_table* C);
	IC	void						set_Constants		(ref_ctable& C_)						{ set_Constants(C_ ? &*C_ : nullptr);			}

		void						set_Textures		(STextureList* T);
	IC	void						set_Textures		(ref_texture_list& T_)				{ set_Textures(T_ ? &*T_ : nullptr);			}

#ifdef _EDITOR
	IC	void						set_Matrices		(SMatrixList* M);
	IC	void						set_Matrices		(ref_matrix_list& M)				{ set_Matrices(M ? &*M : nullptr);			}
#endif

	IC	void						set_Element			(ShaderElement* S, u32	pass=0);
	IC	void						set_Element			(ref_selement& S, u32	pass=0)		{ set_Element(S ? &*S : nullptr,pass);		}

	IC	void						set_Shader			(Shader* S, u32 pass=0);
	IC	void						set_Shader			(ref_shader& S, u32 pass=0)			{ set_Shader(S ? &*S : nullptr,pass);			}

	ICF	void						set_States			(ID3DState* _state);
	ICF	void						set_States			(ref_state& _state)					{ set_States(_state->state);	}

#ifdef USE_DX11
	ICF  void						set_Format			(SDeclaration* _decl);
#else //USE_DX11
	ICF  void						set_Format			(IDirect3DVertexDeclaration9* _decl);
#endif

	ICF void						set_PS				(const ref_ps& _ps)					{ GRHI->SetShader(_ps->ps, ERHI_SHADER_TYPE::PS); }
#ifdef USE_DX11
	ICF void						set_GS				(const ref_gs& _gs)					{ GRHI->SetShader(_gs->gs, ERHI_SHADER_TYPE::GS); }
	ICF void						set_HS				(const ref_hs& _hs)					{ GRHI->SetShader(_hs->sh, ERHI_SHADER_TYPE::HS); }
	ICF void						set_DS				(const ref_ds& _ds)					{ GRHI->SetShader(_ds->sh, ERHI_SHADER_TYPE::DS); }
	ICF void						set_CS				(const ref_cs& _cs)					{ GRHI->SetShader(_cs->sh, ERHI_SHADER_TYPE::CS); }
#endif

#ifdef USE_DX11
	ICF	bool						is_TessEnabled		();
#else
	ICF	bool						is_TessEnabled		() {return false;}
#endif

	ICF void						set_VS				(ref_vs& _vs);
#ifdef USE_DX11
	ICF void						set_VS				(SVS* _vs);
protected:	//	In DX10 we need input shader signature which is stored in ref_vs
#endif //USE_DX11

#ifdef USE_DX11
public:
#endif //USE_DX11

		void						set_Vertices		(IRHIBuffer* _vb, u32 _vb_stride);
		void						set_Indices			(IRHIBuffer* _ib);
	ICF void						set_Geometry		(SGeometry* _geom);
	ICF void						set_Geometry		(ref_geom& _geom)					{	set_Geometry(_geom ? &*_geom : nullptr);		}
	IC  void						set_Stencil			(u32 _enable, u32 _func=D3DCMP_ALWAYS, u32 _ref=0x00, u32 _mask=0x00, u32 _writemask=0x00, u32 _fail=D3DSTENCILOP_KEEP, u32 _pass=D3DSTENCILOP_KEEP, u32 _zfail=D3DSTENCILOP_KEEP);
	IC  void						set_Z				(u32 _enable);
	IC  void						set_ZFunc			(u32 _func);
	IC  void						set_AlphaRef		(u32 _value);
	IC  void						set_ColorWriteEnable(u32 _mask = D3DCOLORWRITEENABLE_RED | D3DCOLORWRITEENABLE_GREEN | D3DCOLORWRITEENABLE_BLUE | D3DCOLORWRITEENABLE_ALPHA);
	IC  void						set_CullMode		(u32 _mode);
	IC  u32							get_CullMode		(){return GRHI->StateManager->GetCullMode();}
	IC	void						set_Scissor			(Irect*	rect=NULL);

	// constants
	ICF	ref_constant				get_c				(LPCSTR			n)													{ if (ctable) return ctable->get(n); return 0;}
	ICF	ref_constant				get_c				(shared_str&	n)													{ if (ctable) return ctable->get(n); return 0;}

	// constants - direct (fast)
	ICF	void						set_c				(RHIShaderConstant* C_, const Fmatrix& A)									{ if (C_)		constants.set(C_,A);					}
	ICF	void						set_c				(RHIShaderConstant* C_, const Fvector4& A)									{ if (C_)		constants.set(C_,A);					}
	ICF	void						set_c				(RHIShaderConstant* C_, float x, float y, float z, float w)					{ if (C_)		constants.set(C_,x,y,z,w);			}
	ICF	void						set_ca				(RHIShaderConstant* C_, u32 e, const Fmatrix& A)							{ if (C_)		constants.seta(C_,e,A);				}
	ICF	void						set_ca				(RHIShaderConstant* C_, u32 e, const Fvector4& A)							{ if (C_)		constants.seta(C_,e,A);				}
	ICF	void						set_ca				(RHIShaderConstant* C_, u32 e, float x, float y, float z, float w)			{ if (C_)		constants.seta(C_,e,x,y,z,w);		}
#ifdef USE_DX11
	ICF	void						set_c				(RHIShaderConstant* C_, float A)											{ if (C_)		constants.set(C_,A);					}
	ICF	void						set_c				(RHIShaderConstant* C_, int A)												{ if (C_)		constants.set(C_,A);					}
#endif //USE_DX11


	// constants - LPCSTR (slow)
	ICF	void						set_c				(LPCSTR n, const Fmatrix& A)										{ if(!ctable) return; ref_constant c = ctable->get(n);  set_c(c ? &*c : nullptr,A);		}
	ICF	void						set_c				(LPCSTR n, const Fvector4& A)										{ if(!ctable) return; ref_constant c = ctable->get(n);  set_c(c ? &*c : nullptr,A);		}
	ICF	void						set_c				(LPCSTR n, float x, float y, float z, float w)						{ if(!ctable) return; ref_constant c = ctable->get(n);  set_c(c ? &*c : nullptr,x,y,z,w);	}
	ICF	void						set_ca				(LPCSTR n, u32 e, const Fmatrix& A)									{ if(!ctable) return; ref_constant c = ctable->get(n);  set_ca(c ? &*c : nullptr,e,A);		}
	ICF	void						set_ca				(LPCSTR n, u32 e, const Fvector4& A)								{ if(!ctable) return; ref_constant c = ctable->get(n);  set_ca(c ? &*c : nullptr,e,A);		}
	ICF	void						set_ca				(LPCSTR n, u32 e, float x, float y, float z, float w)				{ if(!ctable) return; ref_constant c = ctable->get(n);  set_ca(c ? &*c : nullptr,e,x,y,z,w);}
#ifdef USE_DX11																															 
	ICF	void						set_c				(LPCSTR n, float A)													{ if(!ctable) return; ref_constant c = ctable->get(n);  set_c(c ? &*c : nullptr,A);		}
	ICF	void						set_c				(LPCSTR n, int A)													{ if(!ctable) return; ref_constant c = ctable->get(n);  set_c(c ? &*c : nullptr,A);		}
#endif //USE_DX11

	// constants - shared_str (average)
	ICF	void						set_c				(shared_str& n, const Fmatrix& A)									{ if(!ctable) return; ref_constant c = ctable->get(n); set_c(c ? &*c : nullptr,A);			}
	ICF	void						set_c				(shared_str& n, const Fvector4& A)									{ if(!ctable) return; ref_constant c = ctable->get(n); set_c(c ? &*c : nullptr,A);			}
	ICF	void						set_c				(shared_str& n, float x, float y, float z, float w)					{ if(!ctable) return; ref_constant c = ctable->get(n); set_c(c ? &*c : nullptr,x,y,z,w);	}
	ICF	void						set_ca				(shared_str& n, u32 e, const Fmatrix& A)							{ if(!ctable) return; ref_constant c = ctable->get(n); set_ca(c ? &*c : nullptr,e,A);		}
	ICF	void						set_ca				(shared_str& n, u32 e, const Fvector4& A)							{ if(!ctable) return; ref_constant c = ctable->get(n); set_ca(c ? &*c : nullptr,e,A);		}
	ICF	void						set_ca				(shared_str& n, u32 e, float x, float y, float z, float w)			{ if(!ctable) return; ref_constant c = ctable->get(n); set_ca(c ? &*c : nullptr,e,x,y,z,w);}
#ifdef USE_DX11
	ICF	void						set_c				(shared_str& n, float A)											{ if(!ctable) return; ref_constant c = ctable->get(n); set_c(c ? &*c : nullptr,A);		}
	ICF	void						set_c				(shared_str& n, int A)												{ if(!ctable) return; ref_constant c = ctable->get(n); set_c(c ? &*c : nullptr,A);		}
#endif //USE_DX11

	ICF	void						Render				(ERHI_PRIMITIVE_TOPOLOGY topology, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC);
	ICF	void						Render				(ERHI_PRIMITIVE_TOPOLOGY topology, u32 startV, u32 PC);

#ifdef USE_DX11
	ICF	void						Compute				(UINT ThreadGroupCountX, UINT ThreadGroupCountY, UINT ThreadGroupCountZ);
	ICF void						Render_noIA			(u32 iVertexCount);
	ICF	void						RenderInstancedIndexed(ERHI_PRIMITIVE_TOPOLOGY T, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC, u32 instanceCount, u32 startInstanceLocation);
#endif //USE_DX11

	// Device create / destroy / frame signaling
	void							CreateQuadIB		();
	void							OnFrameBegin		();
	void							OnFrameEnd			();
	void							OnDeviceCreate		();
	void							OnDeviceDestroy		();

	// Debug render
	void dbg_DP						(ERHI_PRIMITIVE_TOPOLOGY pt, ref_geom geom, u32 vBase, u32 pc);
	void dbg_DIP					(ERHI_PRIMITIVE_TOPOLOGY pt, ref_geom geom, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC);
#ifdef USE_DX11
	//	TODO: DX10: Implement this.
	IC void	dbg_SetRS				(D3DRENDERSTATETYPE p1, u32 p2)
	{ VERIFY(!"Not implemented"); }
	IC void	dbg_SetSS				(u32 sampler, D3DSAMPLERSTATETYPE type, u32 value)
	{ VERIFY(!"Not implemented"); }
#else //USE_DX11
	IC void	dbg_SetRS				(D3DRENDERSTATETYPE p1, u32 p2)
	{ CHK_DX(RDevice->SetRenderState(p1,p2)); }
	IC void	dbg_SetSS				(u32 sampler, D3DSAMPLERSTATETYPE type, u32 value)
	{ CHK_DX(RDevice->SetSamplerState(sampler,type,value)); }
#endif
#ifdef DEBUG_DRAW
	IC void dbg_DrawAABB			(Fvector& T_, float sx, float sy, float sz, u32 C_)						{	Fvector half_dim;	half_dim.set(sx,sy,sz); Fmatrix	TM;	TM.translate(T_); dbg_DrawOBB(TM,half_dim,C_);	}
	void dbg_DrawOBB				(Fmatrix& T, Fvector& half_dim, u32 C);
	IC void dbg_DrawTRI				(Fmatrix& T_, Fvector* p, u32 C_)											{	dbg_DrawTRI(T_,p[0],p[1],p[2],C_);	}
	void dbg_DrawTRI				(Fmatrix& T, Fvector& p1, Fvector& p2, Fvector& p3, u32 C);
	void dbg_DrawLINE				(Fmatrix& T, Fvector& p1, Fvector& p2, u32 C);
	void dbg_DrawEllipse			(Fmatrix& T, u32 C);
#endif
	void	DrawTriangleFan(ERHI_PRIMITIVE_TOPOLOGY pt, ref_geom geom, u32 vBase, u32 pc);

	CBackend()						{	Invalidate(); };

#ifdef USE_DX11
private:
	//	DirectX 10 internal functionality
	void	ApplyVertexLayout();
	bool	CBuffersNeedUpdate(ref_cbuffer	buf1[MaxCBuffers], ref_cbuffer	buf2[MaxCBuffers], u32	&uiMin, u32	&uiMax);

private:
	ID3DBlob*				m_pInputSignature;

	bool					m_bChangedRTorZB;
#endif //USE_DX11
};
#pragma warning(pop)

extern  ECORE_API CBackend RCache;

#ifndef _EDITOR
#	include "D3DUtils.h"
#endif

IC void	CBackend::set_Scissor(Irect* R)
{
	GRHI->SetScissorRect(R);
}

IC void CBackend::set_Stencil(u32 _enable, u32 _func, u32 _ref, u32 _mask, u32 _writemask, u32 _fail, u32 _pass, u32 _zfail)
{
	GRHI->StateManager->SetStencil(_enable, _func, _ref, _mask, _writemask, _fail, _pass, _zfail);
}

IC void CBackend::set_Z(u32 _enable)
{
	GRHI->StateManager->SetDepthEnable(_enable);
}

IC void CBackend::set_ZFunc(u32 _func)
{
	GRHI->StateManager->SetDepthFunc(_func);
}

IC void CBackend::set_AlphaRef(u32 _value)
{
	GRHI->StateManager->SetAlphaRef(_value);
}

IC void	CBackend::set_ColorWriteEnable(u32 _mask)
{
	GRHI->StateManager->SetColorWriteEnable(_mask);
}

ICF void CBackend::set_CullMode(u32 _mode)
{
	GRHI->StateManager->SetCullMode(_mode);
}
