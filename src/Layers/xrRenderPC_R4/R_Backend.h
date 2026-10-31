#pragma once
#include "RenderAPI.h"

#include "R_DStreams.h"
#include "r_constants.h"
#include "R_Backend_xform.h"
#include "ShaderConstants.h"
#include "../../xrRHI/RHIEnums.h"
#include "../../xrRHI/RHITopologyUtils.h"

#include "FVF.h"

/// detailed statistic
struct R_statistics_element
{
	u32 verts,dips;
	ICF void add(u32 _verts)
	{
		verts+=_verts; dips++; 
	}
};

struct R_statistics
{
	R_statistics_element s_static;
	R_statistics_element s_flora;
	R_statistics_element s_flora_lods;
	R_statistics_element s_details;
	R_statistics_element s_ui;
	R_statistics_element s_dynamic;
	R_statistics_element s_dynamic_sw;
	R_statistics_element s_dynamic_inst;
	R_statistics_element s_dynamic_1B;
	R_statistics_element s_dynamic_2B;
	R_statistics_element s_dynamic_3B;
	R_statistics_element s_dynamic_4B;
};

#pragma warning(push)
#pragma warning(disable:4324)
class RENDER_API CBackend
{
public:
	enum MaxTextures
	{
		//	Actually these values are 128
		mtMaxPixelShaderTextures = 16,
		mtMaxVertexShaderTextures = 4,
		mtMaxGeometryShaderTextures = 16,
		mtMaxHullShaderTextures = 16,
		mtMaxDomainShaderTextures = 16,
		mtMaxComputeShaderTextures = 16,
	};

public:            
	// Dynamic geometry streams
	_VertexStream Vertex;
	_IndexStream Index;

	IRHIBuffer* QuadIB;
	IRHIBuffer* old_QuadIB;
	IRHIBuffer* CuboidIB;

	R_xforms						xforms;
	CShaderConstants				Constants;

	ERHI_PRIMITIVE_TOPOLOGY			m_PrimitiveTopology;
	RHIObject*				m_pInputLayout;
	DWORD							dummy0;	//	Padding to avoid warning	
	DWORD							dummy1;	//	Padding to avoid warning	
	DWORD							dummy2;	//	Padding to avoid warning	
private:
	// Vertices/Indices/etc
	SDeclaration*					decl;
	IRHIBuffer* vb;
	IRHIBuffer* ib;
	u32								vb_stride;

	// Pixel/Vertex constants
	R_constant_table*				ctable;

	// Shaders/State
	dx10State*						state;

	// Lists
	STextureList*					T;
	SMatrixList*					M;
	SConstantList*					C;

	// Lists-expanded
	CTexture*						textures_ps	[mtMaxPixelShaderTextures];	// stages
	CTexture*						textures_vs	[mtMaxVertexShaderTextures];	// 4 vs
	CTexture*						textures_gs	[mtMaxGeometryShaderTextures];	// 4 vs
	CTexture*						textures_hs	[mtMaxHullShaderTextures];	// 4 vs
	CTexture*						textures_ds	[mtMaxDomainShaderTextures];	// 4 vs
	CTexture*						textures_cs	[mtMaxComputeShaderTextures];	// 4 vs
	CMatrix*						matrices	[8	];	// matrices are supported only for FFP

	// Cached Device.IsEditorMode(); enables editor-only per-pass state (texture matrices).
	bool							EditorMode = false;

public:
	void Invalidate();
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
	void						unbind_cs_textures			();
	IC	CTexture*					get_ActiveTexture			(u32 stage)
	{
		if (stage<CTexture::rstVertex)			return textures_ps[stage];
		else if (stage<CTexture::rstGeometry)	return textures_vs[stage-CTexture::rstVertex];
		else if (stage<CTexture::rstHull)	return textures_gs[stage-CTexture::rstGeometry];
		else if (stage<CTexture::rstDomain) return textures_hs[stage-CTexture::rstHull];
		else if (stage<CTexture::rstCompute) return textures_ds[stage-CTexture::rstDomain];
		else if (stage<CTexture::rstInvalid) return textures_cs[stage-CTexture::rstCompute];
		else
		{
			VERIFY(!"Invalid texture stage");
			return 0;
		}
	}

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

	IC	void						set_Matrices		(SMatrixList* M);
	IC	void						set_Matrices		(ref_matrix_list& M)				{ set_Matrices(M ? &*M : nullptr);			}

	IC	void						set_Element			(ShaderElement* S, u32	pass=0);
	IC	void						set_Element			(ref_selement& S, u32	pass=0)		{ set_Element(S ? &*S : nullptr,pass);		}

	IC	void						set_Shader			(Shader* S, u32 pass=0);
	IC	void						set_Shader			(ref_shader& S, u32 pass=0)			{ set_Shader(S ? &*S : nullptr,pass);			}

	ICF	void						set_States			(dx10State* _state);
	ICF	void						set_States			(ref_state& _state)					{ set_States(_state->state);	}

	ICF  void						set_Format			(SDeclaration* _decl);

	ICF void						set_PS				(const ref_ps& _ps)					{ GRHI->SetShader(_ps->ps, ERHI_SHADER_TYPE::PS); }
	ICF void						set_GS				(const ref_gs& _gs)					{ GRHI->SetShader(_gs->gs, ERHI_SHADER_TYPE::GS); }
	ICF void						set_HS				(const ref_hs& _hs)					{ GRHI->SetShader(_hs->sh, ERHI_SHADER_TYPE::HS); }
	ICF void						set_DS				(const ref_ds& _ds)					{ GRHI->SetShader(_ds->sh, ERHI_SHADER_TYPE::DS); }
	ICF void						set_CS				(const ref_cs& _cs)					{ GRHI->SetShader(_cs->sh, ERHI_SHADER_TYPE::CS); }

	ICF void						set_VS				(ref_vs& _vs);
	ICF void						set_VS				(SVS* _vs);
	// Vertex shader of the current pass (resources a visual binds itself, e.g. instance buffers)
	ICF SVS*						get_VS				() const								{ return CurrentVS; }
	// Raw views bypass set_Textures, so the slot is forgotten and rebound by the next texture list
	ICF void						set_VSResource		(u32 Slot, IRHIShaderResourceView* View)	{ if (Slot < mtMaxVertexShaderTextures) textures_vs[Slot] = nullptr; GRHI->ShaderResourceCache->SetVSResource(Slot, View); }
	ICF void						set_CSResource		(u32 Slot, IRHIShaderResourceView* View)	{ if (Slot < mtMaxComputeShaderTextures) textures_cs[Slot] = nullptr; GRHI->ShaderResourceCache->SetCSResource(Slot, View); }
protected:	//	In DX10 we need input shader signature which is stored in ref_vs

public:

		void						set_Vertices		(IRHIBuffer* _vb, u32 _vb_stride);
		void						set_Indices			(IRHIBuffer* _ib);
	ICF void						set_Geometry		(SGeometry* _geom);
	ICF void						set_Geometry		(ref_geom& _geom)					{	set_Geometry(_geom ? &*_geom : nullptr);		}
	IC  void						set_Stencil			(u32 _enable, u32 _func=D3DCMP_ALWAYS, u32 _ref=0x00, u32 _mask=0x00, u32 _writemask=0x00, u32 _fail=D3DSTENCILOP_KEEP, u32 _pass=D3DSTENCILOP_KEEP, u32 _zfail=D3DSTENCILOP_KEEP);
	IC  void						set_Z				(u32 _enable);
	IC  void						set_ZFunc			(u32 _func);
	IC  void						set_AlphaRef		(u32 _value);
	IC  void						set_ColorWriteEnable(u32 _mask = D3DCOLORWRITEENABLE_RED | D3DCOLORWRITEENABLE_GREEN | D3DCOLORWRITEENABLE_BLUE | D3DCOLORWRITEENABLE_ALPHA);
	IC	void						set_Scissor			(Irect*	rect= nullptr);
	ICF	void						set_ScissorOverride	(bool Override)							{ GRHI->StateManager->OverrideScissoring(Override, true); }
	ICF	void						set_FillModeOverride(bool Override, u32 Mode)				{ GRHI->StateManager->OverrideFillMode(Override, Mode); }
	ICF	void						set_CullMode		(ERHI_CULLMODE Mode)					{ GRHI->StateManager->SetCullMode(Mode); }
	ICF	ERHI_CULLMODE				get_CullMode		() const								{ return GRHI->StateManager->GetCullMode(); }
	ICF	void						set_StencilRef		(u32 Ref)								{ GRHI->StateManager->SetStencilRef(Ref); }
	ICF	void						set_RenderState		(u32 State, u32 Value)					{ GRHI->StateManager->SetRenderState(State, Value); }

	// constants
	ICF	ref_constant				get_c				(const char*			n)													{ if (ctable) return ctable->get(n); return nullptr;}
	ICF	ref_constant				get_c				(shared_str&	n)													{ if (ctable) return ctable->get(n); return nullptr;}

	ICF	void						Render				(ERHI_PRIMITIVE_TOPOLOGY topology, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC);
	ICF	void						Render				(ERHI_PRIMITIVE_TOPOLOGY topology, u32 startV, u32 PC);

	ICF	void						Compute				(UINT ThreadGroupCountX, UINT ThreadGroupCountY, UINT ThreadGroupCountZ);
	ICF void						Render_noIA			(u32 iVertexCount);
	ICF	void						RenderInstancedIndexed(ERHI_PRIMITIVE_TOPOLOGY T, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC, u32 instanceCount, u32 startInstanceLocation);
	ICF	void						RenderInstanced		(ERHI_PRIMITIVE_TOPOLOGY T, u32 startV, u32 PC, u32 instanceCount);
	ICF void						FlushConstants() { Constants.Flush(); }

	// Device create / destroy / frame signaling
	void							CreateQuadIB		();
	void							OnFrameBegin		();
	void							OnFrameEnd			();
	void							OnDeviceCreate		();
	void							OnDeviceDestroy		();

	// Debug render
	void dbg_DP						(ERHI_PRIMITIVE_TOPOLOGY pt, ref_geom geom, u32 vBase, u32 pc);
	void dbg_DIP					(ERHI_PRIMITIVE_TOPOLOGY pt, ref_geom geom, u32 baseV, u32 startV, u32 countV, u32 startI, u32 PC);
	//	TODO: DX10: Implement this.
	IC void	dbg_SetRS				(D3DRENDERSTATETYPE p1, u32 p2)
	{ VERIFY(!"Not implemented"); }
	IC void	dbg_SetSS				(u32 sampler, D3DSAMPLERSTATETYPE type, u32 value)
	{ VERIFY(!"Not implemented"); }
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

private:
	//	DirectX 10 internal functionality
	void	ApplyVertexLayout();

private:
	RHIBlob*				m_pInputSignature;
	SVS*					CurrentVS = nullptr;

	bool					m_bChangedRTorZB;
};
#pragma warning(pop)

extern  RENDER_API CBackend RCache;

#include "D3DUtils.h"

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