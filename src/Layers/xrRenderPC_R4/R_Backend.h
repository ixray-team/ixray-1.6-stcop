#pragma once
#include "RenderAPI.h"

#include "R_DStreams.h"
#include "r_constants.h"
#include "ShaderConstants.h"
#include "GeometryHeap.h"
#include "../../xrRHI/RHIEnums.h"
#include "../../xrRHI/RHITopologyUtils.h"

#include "FVF.h"

struct GfxView
{
	Fmatrix View = Fidentity;
	Fmatrix Project = Fidentity;
	Fmatrix ViewOld = Fidentity;
	Fmatrix ProjectOld = Fidentity;
};

#pragma warning(push)
#pragma warning(disable:4324)
class RENDER_API GfxCmdList
{
public:
	enum MaxTextures
	{
		mtMaxPixelShaderTextures = u32(ERHI_MAX_TEXTURE::PixelShaderTextures),
		mtMaxComputeShaderTextures = u32(ERHI_MAX_TEXTURE::ComputeShaderTextures),
	};

public:            
	// Dynamic geometry streams
	VertexRing Vertex;
	_VertexStream LegacyVertex{ 256 };
	_IndexStream Index;

	IRHIBuffer* IotaVB = nullptr;
	ref_declaration PulledDecl;
	GeometryHeap* Heap = nullptr;

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
	RHIObject* PulledLayout = nullptr;
	EGeomSource GeomSource = EGeomSource::Legacy;
	u32 GeomPage = 0;

	// Pixel/Vertex constants
	R_constant_table*				ctable;

	// Pipeline state: the pass state plus the overrides recorded since SetStates
	GfxPipelineState*				state;
	RHIRasterizerDesc				Raster = {};
	RHIDepthStencilDesc				Depth = {};
	RHIBlendDesc					Blend = {};
	void*							States[3] = {};
	u32								StencilRef = 0;
	bool							StatesDirty = true;
	bool							ScissorOverride = false;
	bool							FillOverride = false;
	ERHI_FILL_MODE					FillOverrideMode = RHI_FILL_SOLID;

	// Lists
	STextureList*					T;
	SMatrixList*					M;

	// Lists-expanded
	CTexture*						Textures[6][mtMaxPixelShaderTextures] = {};
	u64								TextureMask[6] = {};
	CMatrix*						matrices	[8	];	// matrices are supported only for FFP

	// Cached Device.IsEditorMode(); enables editor-only per-pass state (texture matrices).
	bool							EditorMode = false;

public:
	void Invalidate();
	struct
	{
		u32							Calls = 0;
		u32							Polys = 0;
	}								Stats;
public:
	void						UnbindComputeTextures			();
	IC	CTexture*					GetTexture			(u32 stage)
	{
		VERIFY(stage < CTexture::rstInvalid);
		return Textures[stage / CTexture::rstVertex][stage % CTexture::rstVertex];
	}

	IC static float						GetWidth();
	IC static float						GetHeight();
	IC static float						GetTargetWidth();
	IC static float						GetTargetHeight();

	// API
		void						SetView				(const GfxView& View);
	ICF	void						SetView				(const Fmatrix& View, const Fmatrix& Project)	{ SetView({ View, Project, ViewState.ViewOld, ViewState.ProjectOld }); }
	ICF	const GfxView&				GetView				() const								{ return ViewState; }
		void						SetObjectTransform	(const Fmatrix& World, const Fmatrix& WorldOld = Fidentity);
	ICF	const Fmatrix&				GetObjectTransform	() const								{ return World; }
	ICF	const Fmatrix&				GetWorldViewProject	() const								{ return WorldViewProject; }

		void						SetRenderTargets	(std::initializer_list<IRHIRenderTargetView*> Targets, IRHIDepthStencilView* Depth);
	ICF	void						SetRenderTarget		(IRHIRenderTargetView* View, u32 Index = 0, bool Force = false)	{ GRHI->SetRenderTargetView(View, Index, Force); }
	ICF	IRHIRenderTargetView*		GetRenderTarget		(u32 Index = 0) const					{ return GRHI->GetRenderTargetView(Index); }
	ICF	void						SetDepthStencil		(IRHIDepthStencilView* View, bool Force = false)	{ GRHI->SetDepthStencilView(View, Force); }
	ICF	IRHIDepthStencilView*		GetDepthStencil		() const								{ return GRHI->GetDepthStencilView(); }
	ICF	void						ApplyRenderTargets	()										{ GRHI->ApplyRenderTargetChange(); }
	ICF	void						SetUAV				(IRHIUnorderedAccessView* View, u32 Index, bool Force = false)	{ GRHI->SetUnorderedAccessViews(View, Index, Force); }
	ICF	void						SetComputeUAVs		(u32 Start, u32 Count, IRHIUnorderedAccessView* const* Views, const u32* InitialCounts = nullptr)	{ GRHI->SetComputeUAVs(Start, Count, Views, InitialCounts); }
	ICF	void						SetComputeResources	(u32 Start, u32 Count, IRHIShaderResourceView* const* Views)	{ GRHI->SetComputeResources(Start, Count, Views); }
	ICF	void						SetViewport			(RHIViewport& Viewport)					{ GRHI->SetViewport(Viewport); }
	ICF	void						ClearTarget			(IRHIRenderTargetView* Target, ERTColor Color = ERTColor::Transparent)	{ GRHI->ClearTarget(Target, Color); }
	ICF	void						ClearTarget			(IRHIRenderTargetView* Target, const float* Color)	{ GRHI->ClearTarget(Target, Color); }
	ICF	void						ClearDepthStencil	(IRHIDepthStencilView* View, ERHI_CLEAR_TARGET Flags, float Depth, u8 Stencil)	{ GRHI->ClearDepthStencil(View, Flags, Depth, Stencil); }
	ICF	void						CopySurface			(IRHISurface* Dest, IRHISurface* Source)	{ GRHI->CopySurface(Dest, Source); }
	ICF	void						CopySurface			(IRHIRenderTargetView* Dest, IRHIRenderTargetView* Source)	{ GRHI->CopySurface(Dest, Source); }
	ICF	void						GenerateMips		(IRHIShaderResourceView* View)			{ GRHI->GenerateMips(View); }
	ICF	void						BeginQuery			(RHIObject* Query)						{ GRHI->BeginQuery(Query); }
	ICF	void						EndQuery			(RHIObject* Query)						{ GRHI->EndQuery(Query); }

		void						SetConstantTable		(R_constant_table* C);
	IC	void						SetConstantTable		(ref_ctable& C_)						{ SetConstantTable(C_ ? &*C_ : nullptr);			}

		void						SetTextures		(STextureList* T);
	IC	void						SetTextures		(ref_texture_list& T_)				{ SetTextures(T_ ? &*T_ : nullptr);			}

	IC	void						SetMatrices		(SMatrixList* M);
	IC	void						SetMatrices		(ref_matrix_list& M)				{ SetMatrices(M ? &*M : nullptr);			}

	IC	void						SetPipeline		(SPass& P);
	IC	void						SetPass			(SPass& P);
	IC	void						SetPass			(ShaderElement* S, u32	pass=0);
	IC	void						SetPass			(ref_selement& S, u32	pass=0)		{ SetPass(S ? &*S : nullptr,pass);		}

	IC	void						SetPass			(Shader* S, u32 pass=0);
	IC	void						SetPass			(ref_shader& S, u32 pass=0)			{ SetPass(S ? &*S : nullptr,pass);			}

	ICF	void						SetStates			(GfxPipelineState* _state);
	ICF	void						SetStates			(ref_state& _state)					{ SetStates(_state->state);	}

	ICF  void						SetFormat			(SDeclaration* _decl);

	ICF void						SetShader			(RHIObject* Shader, ERHI_SHADER_TYPE Type)	{ GRHI->SetShader(Shader, Type); }
	ICF void						SetPS				(const ref_ps& _ps)					{ GRHI->SetShader(_ps->ps, ERHI_SHADER_TYPE::PS); }
	ICF void						SetGS				(const ref_gs& _gs)					{ GRHI->SetShader(_gs->gs, ERHI_SHADER_TYPE::GS); }
	ICF void						SetHS				(const ref_hs& _hs)					{ GRHI->SetShader(_hs->sh, ERHI_SHADER_TYPE::HS); }
	ICF void						SetDS				(const ref_ds& _ds)					{ GRHI->SetShader(_ds->sh, ERHI_SHADER_TYPE::DS); }
	ICF void						SetCS				(const ref_cs& _cs)					{ GRHI->SetShader(_cs->sh, ERHI_SHADER_TYPE::CS); }

	ICF void						SetVS				(ref_vs& _vs);
	ICF void						SetVS				(SVS* _vs);
	// Vertex shader of the current pass (resources a visual binds itself, e.g. instance buffers)
	ICF SVS*						GetVS				() const								{ return CurrentVS; }
	// Raw views bypass set_Textures, so the slot is forgotten and rebound by the next texture list
		void						SetResource			(ERHI_SHADER_TYPE Stage, u32 Slot, IRHIShaderResourceView* View);
protected:	//	In DX10 we need input shader signature which is stored in ref_vs

public:

		void						SetVertices		(IRHIBuffer* _vb, u32 _vb_stride);
		void						SetIndices			(IRHIBuffer* _ib);
	ICF void						SetGeometry		(SGeometry* _geom);
	ICF void						SetGeometry		(ref_geom& _geom)					{	SetGeometry(_geom ? &*_geom : nullptr);		}
	ICF	RHIRasterizerDesc&			EditRaster			()										{ StatesDirty = true; States[0] = nullptr; return Raster; }
	ICF	RHIDepthStencilDesc&		EditDepth			()										{ StatesDirty = true; States[1] = nullptr; return Depth; }
	ICF	RHIBlendDesc&				EditBlend			()										{ StatesDirty = true; States[2] = nullptr; return Blend; }
		void						SetStencil			(u32 Enable, u32 Func = D3DCMP_ALWAYS, u32 Ref = 0x00, u32 Mask = 0x00, u32 WriteMask = 0x00, u32 Fail = D3DSTENCILOP_KEEP, u32 Pass = D3DSTENCILOP_KEEP, u32 ZFail = D3DSTENCILOP_KEEP);
	ICF	void						SetStencilRef		(u32 Ref)								{ StatesDirty |= StencilRef != Ref; StencilRef = Ref; }
	ICF	void						SetDepthEnable		(u32 Enable)							{ if (Depth.DepthEnable != !!Enable) EditDepth().DepthEnable = !!Enable; }
	ICF	void						SetDepthFunc		(u32 Func)								{ if (Depth.DepthFunc != ERHI_COMPARISON(Func)) EditDepth().DepthFunc = ERHI_COMPARISON(Func); }
	ICF	void						SetDepthWrite		(bool Write)							{ EditDepth().DepthWriteMask = Write ? RHI_DEPTH_WRITE_MASK_ALL : RHI_DEPTH_WRITE_MASK_ZERO; }
		void						SetBlend			(bool Enable, u32 Src, u32 Dst);
		void						SetColorWriteEnable	(u32 Mask = D3DCOLORWRITEENABLE_RED | D3DCOLORWRITEENABLE_GREEN | D3DCOLORWRITEENABLE_BLUE | D3DCOLORWRITEENABLE_ALPHA);
	ICF	void						SetCullMode			(ERHI_CULLMODE Mode)					{ if (Raster.CullMode != ERHI_CULL_MODE(Mode)) EditRaster().CullMode = ERHI_CULL_MODE(Mode); }
	ICF	ERHI_CULLMODE				GetCullMode			() const								{ return ERHI_CULLMODE(Raster.CullMode); }
	ICF	void						SetAlphaRef			(u32 Ref)								{ Constants.Write<CBMaterial>().m_AlphaRef = Ref / 255.0f; }
		void						SetScissor			(Irect* Rect = nullptr);
		void						SetScissorOverride	(bool Override);
		void						SetFillModeOverride	(bool Override, u32 Mode);

	ICF	CTexture*					GetPassTexture		(u32 Stage) const
	{
		if (T)
		{
			for (const auto& [Slot, Texture] : *T)
			{
				if (Slot == Stage)
				{
					return Texture._get();
				}
			}
		}
		return nullptr;
	}

	ICF	void						Draw				(ERHI_PRIMITIVE_TOPOLOGY Topology, u32 VertexCount, u32 StartVertex);
	ICF	void						DrawIndexed			(ERHI_PRIMITIVE_TOPOLOGY Topology, u32 IndexCount, u32 StartIndex, u32 BaseVertex);
	ICF	void						DrawInstanced		(ERHI_PRIMITIVE_TOPOLOGY Topology, u32 VertexCount, u32 InstanceCount, u32 StartVertex, u32 StartInstance = 0);
	ICF	void						DrawIndexedInstanced(ERHI_PRIMITIVE_TOPOLOGY Topology, u32 IndexCount, u32 InstanceCount, u32 StartIndex, u32 BaseVertex, u32 StartInstance);
	ICF	void						DrawQuads			(u32 Quads, u32 BaseVertex);
	ICF	void						DrawNoInput			(u32 VertexCount);
	ICF	void						Dispatch			(u32 GroupsX, u32 GroupsY, u32 GroupsZ);

	// Device create / destroy / frame signaling
	void							OnFrameBegin		();
	void							OnFrameEnd			();
	void							OnDeviceCreate		();
	void							OnDeviceDestroy		();

	GfxCmdList()						{	Invalidate(); };

private:
	void	ApplyVertexLayout();
	void	CreatePulledLayout();
	void	FlushStates();
	ICF bool PrepareDraw(ERHI_PRIMITIVE_TOPOLOGY Topology, u32 Count, u32 Instances, bool Indexed);
	ICF void SetDrawBase(u32 VertexBase, u32 Flags);
	ICF bool SetPulledBase(u32 VertexBase);
	void	UpdateObject();

	GfxView					ViewState;
	Fmatrix					World = Fidentity;
	Fmatrix					WorldOld = Fidentity;
	Fmatrix					WorldViewProject = Fidentity;

private:
	RHIBlob*				m_pInputSignature;
	SVS*					CurrentVS = nullptr;

	bool					m_bChangedRTorZB;
};
#pragma warning(pop)

#include "D3DUtils.h"