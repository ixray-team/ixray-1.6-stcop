#pragma once

#include "../../xrEngine/Render.h"
#include "../../xrCore/Collision/ISpatial.h"
#include "../../xrCore/Containers/_stl_extensions.h"
#include "r__dsgraph_types.h"

#ifdef RENDER_API_IMPORTS
class CSector;
class CPortal;
#else
#include "r__sector.h"
#endif

#include "r__portal_traverser.h"

// Everything a collection reads about its view. A graph collected on a worker owns a copy,
// because FrameMove and the UI change the camera, the environment and the thresholds under it.
struct GraphView
{
	u32 Phase = 0;
	Fvector CameraPosition{};
	float Fov = 90.f;
	float ViewportNear = 0.f;
	bool HasEnvironment = false;
	float FarPlane = 0.f;
	float FogDistance = 0.f;
	float SsaDiscard = 0.f;
	float SsaLodA = 0.f;
	float SsaLodB = 0.f;
	float DetailTextureRange = 0.f;
	float LodMu[5] = {};

	IRender_Sector* Sector = nullptr;
	CFrustum Frustum;
	Fmatrix Transform = Fidentity;
	Fvector Origin{};
	u32 TraverseOptions = 0;
	bool PassMask[3] = { true, false, false };	// deferred, forward, wallmarks
	bool CalculateSkeletons = false;
	bool Distortion = false;
	bool ZPrefill = false;
	bool TrackStats = false;

	void SetSubspace(IRender_Sector* NewSector, const Fmatrix& Combined, const Fvector& NewOrigin)
	{
		Sector = NewSector;
		Transform = Combined;
		Origin = NewOrigin;
		Frustum.CreateFromMatrix(Transform, FRUSTUM_P_ALL & ~FRUSTUM_P_NEAR);
	}
};

class R_dsgraph_structure;

struct GraphCollectJob
{
	R_dsgraph_structure* Graph;
	GraphView View;
};

class GfxCmdList;

//////////////////////////////////////////////////////////////////////////
// common part of interface implementation for all D3D renderers		//
//////////////////////////////////////////////////////////////////////////
class R_dsgraph_structure : public IDSGraphManager
{
public:
	struct SpotShadowBuildContext
	{
		bool CollectOnly = true;
		bool IsDepthCacheBuild = false;
		xr_vector<ref_texture> PendingTextures;
		xr_vector<dxRender_Visual*> CachedCasters;
		xr_vector<dxRender_Visual*> PerFrameCasters;
		xr_vector<std::pair<ref_texture, u64>> Textures;
		const char* RejectionReason = nullptr;
		shared_str RejectedTexture;
		xr_string PerFrameReason;
	};

	SpotShadowBuildContext* SpotShadowBuild = nullptr;
	bool SpotShadowPass = false;
	const xr_hash_set<dxRender_Visual*>* SpotShadowCachedCasters = nullptr;
	u32 SpotShadowCachedSkips = 0;
	u32 SpotShadowDuplicateSkips = 0;
	u32 SpotShadowStaticItems = 0;
	u32 SpotShadowDynamicItems = 0;

	CFrustum* View = nullptr;
	GfxCmdList* Backend = nullptr;

	// GraphMain follows the live device state until it is described by a view of its own
	GraphView ViewParams;
	bool OwnsView = false;

	u32 ViewPhase() const;

	// The collection of any view: static geometry can run on a worker, dynamic objects cannot
	void Build(const GraphView& NewView);
	void BuildFull(const GraphView& NewView, CObject* Ignore = nullptr);
	void Begin(const GraphView& NewView);
	void Traverse();
	void CaptureStatic();
	void CollectDynamic();
	void CaptureDynamic(CObject* Ignore = nullptr);

	IRenderable* val_pObject;
	Fmatrix* val_pTransform;
	bool val_bHUD;
	bool val_bUI;
	bool val_bInvisible;

	bool														pmask[3];
	bool														CollectForwardTogether = false;	// both priorities in one collection

	// Dynamic scene graph
	// Indexed by priority / 2; a packet's key holds its pass
	R_dsgraph::StaticList										StaticPasses[2];
	R_dsgraph::DynamicList										DynamicPasses[2];
	bool HasPasses(u32 Priority) const { return !StaticPasses[Priority].empty() || !DynamicPasses[Priority].empty(); }

	// Drawn back to front
	R_dsgraph::DynamicList										mapSorted;
	R_dsgraph::DynamicList										mapHUDSorted;
	R_dsgraph::DynamicList										mapUISorted;

	// Drawn in insertion order
	R_dsgraph::DynamicList										mapHUD;
	R_dsgraph::DynamicList										mapDistort;
	R_dsgraph::DynamicList										mapUI;
	R_dsgraph::DynamicList										mapUIEmissive;
	R_dsgraph::DynamicList										mapWmark;
	R_dsgraph::DynamicList										mapEmissive;
	R_dsgraph::DynamicList										mapHUDEmissive;
	R_dsgraph::DynamicList										mapHUDScopeMask;
	R_dsgraph::DynamicList										mapHUDDistort;
	R_dsgraph::LodList											mapLOD;

	void SortStatic();
	// Moves what a static-only collection of the same view produced into this graph
	void TakeStatic(R_dsgraph_structure& From);

	xr_vector<int,render_alloc<int> >									lstLODgroups;
	xr_vector<ISpatialShared>				lstRenderables;
	xr_vector<ISpatialShared>				lstRenderablesMain;
	xr_vector<ISpatialShared>				lstSpatial	;
	xr_vector<dxRender_Visual*,render_alloc<dxRender_Visual*> >			lstVisuals	;

	// Dynamic objects of a sub-space pass, in traversal order
	struct SubspaceItem
	{
		IRenderable*	Renderable;
		CFrustum*		Frustum;
	};
	xr_vector<SubspaceItem>										SubspaceDynamic;

	bool b_loaded;
	CPortalTraverser PortalTraverser;
	xr_hash_set<void*> private_visuals;

public:
public:
	R_dsgraph_structure	()
	{
		val_pObject			= NULL	;
		val_pTransform		= NULL	;
		val_bHUD			= false	;
		val_bUI				= false	;
		val_bInvisible		= false	;
		r_pmask				(true,true);
		b_loaded			= false	;
	};

	void r_dsgraph_destroy()
	{
		lstLODgroups.clear();
		lstRenderables.clear();
		lstSpatial.clear();
		lstVisuals.clear();

		for (u32 i = 0; i < 2; ++i)
		{
			StaticPasses[i] = {};
			DynamicPasses[i] = {};
		}
		for (R_dsgraph::DynamicList* List : { &mapSorted, &mapHUDSorted, &mapUISorted, &mapHUD, &mapDistort, &mapUI, &mapUIEmissive, &mapWmark, &mapEmissive, &mapHUDEmissive, &mapHUDScopeMask, &mapHUDDistort })
		{
			*List = {};
		}
		mapLOD = {};
	}

	void		r_dsgraph_clear_aux();
	void		r_dsgraph_clear_passes();
	void		add_Static(dxRender_Visual* pVisual, u32 planes);

	// IDSGraphManager
	void add_Static(IRenderVisual* Visual, CFrustum& Frustum, u32 Planes) override;
	void add_Dynamic(IRenderVisual* Visual, Fmatrix* Xform, bool IgnoreOptimize = false, bool Force = false) override;
	void set_Object(IRenderable* O = nullptr) override;
	IRenderable* get_Object() const override { return val_pObject; }
	void set_HUD(bool V = false) override { val_bHUD = V; }
	bool get_HUD() const override { return val_bHUD; }
	void set_Invisible(bool V = false) override { val_bInvisible = V; }
	void set_UI(bool V = false) override { val_bUI = V; }
	void CollectSpotShadowStatic(dxRender_Visual* Visual, u32 Planes);
	void		add_leafs_Dynamic(dxRender_Visual* pVisual, bool IgnoreObject = false, bool Force = false); // if detected node's full visibility

	void		r_pmask											(bool deffered = false, bool forward = false, bool wallmarks = false) { pmask[0] = deffered; pmask[1] = forward; pmask[2] = wallmarks; }

	void		r_dsgraph_insert_dynamic						(dxRender_Visual	*pVisual, Fvector& Center, bool Force = false);
	void		r_dsgraph_insert_static							(dxRender_Visual	*pVisual);

	void		r_dsgraph_render_graph							(u32	_priority,	bool _clear=true);
	void		r_dsgraph_render_ui								();
	void		r_dsgraph_render_sorted_ui						();
	void		r_dsgraph_render_hud							();
	void		r_dsgraph_render_hud_ui							();
	void		r_dsgraph_render_lods							(bool	_setup_zb,	bool _clear);
	void		r_dsgraph_render_sorted							(bool hud_render = true);
	void		r_dsgraph_render_sorted_hud						();
	void		r_dsgraph_render_emissive						();
	void		r_dsgraph_render_scope							();
	void		r_dsgraph_render_wmarks							();
	void		r_dsgraph_render_distort						();
	void		r_dsgraph_render_R1_box							(IRender_Sector* _sector, Fbox& _bb, int _element);

	void detectSectors_sphere(CSector* sector, xr_vector<IRender_Sector*>& m_sectors, const Fsphere& sphere);
	void detectSectors_frustum(CSector* sector, xr_vector<IRender_Sector*>& m_sectors, CFrustum* _frustum);

private:
	void		CollectSubspaceDynamic							();

public:
	virtual u32						memory_usage			()
	{
		return	(0);
	}
};
