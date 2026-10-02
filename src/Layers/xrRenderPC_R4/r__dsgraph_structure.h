#pragma once

#include "../../xrEngine/Render.h"
#include "../../xrCore/Collision/ISpatial.h"
#include "../../xrCore/Containers/_stl_extensions.h"
#include "r__dsgraph_types.h"
#include "r__sector.h"

struct R_CullTLS
{
	bool active = false;
	u32 phase = 0;
	float ssa_discard = 0.f;
	float ssa_lod_a = 0.f;
	float ssa_lod_b = 0.f;
};

extern thread_local R_CullTLS g_r_cull_tls;

//////////////////////////////////////////////////////////////////////////
// feedback	for receiving visuals										//
//////////////////////////////////////////////////////////////////////////
class R_feedback
{
public:
	virtual void rfeedback_static(dxRender_Visual* V) = 0;
};

//////////////////////////////////////////////////////////////////////////
// common part of interface implementation for all D3D renderers		//
//////////////////////////////////////////////////////////////////////////
class R_dsgraph_structure
{
public:
	CFrustum* View = nullptr;
	IRenderable*												val_pObject;
	Fmatrix*													val_pTransform;
	Fmatrix*													val_pLocalTransform;
	bool														val_bHUD;
	bool														val_bUI;
	bool														val_bInvisible;
	bool														val_bRecordMP;		// record nearest for multi-pass

	R_feedback*													val_feedback;		// feedback for geometry being rendered
	u32															val_feedback_breakp;// breakpoint

	// One counter for every graph. Visuals store a single vis.marker,
	// so per-graph counters alias and drop casters between passes.
	inline static u32											marker = 0;
	bool														pmask[3];

	// Dynamic scene graph
	R_dsgraph::mapNormalPasses_T								mapNormalPasses	[2]	;	// 2==(priority/2)
	R_dsgraph::mapMatrixPasses_T								mapMatrixPasses	[2]	;
	R_dsgraph::mapSorted_T										mapSorted;
	R_dsgraph::mapHUD_T											mapHUD;
	R_dsgraph::mapLOD_T											mapLOD;
	R_dsgraph::mapSorted_T										mapDistort;
	R_dsgraph::mapHUD_T											mapHUDSorted;

	R_dsgraph::mapHUD_T											mapUI;
	R_dsgraph::mapHUD_T											mapUISorted;
	R_dsgraph::mapSorted_T										mapUIEmissive;
	R_dsgraph::mapSorted_T										mapWmark;			// sorted
	R_dsgraph::mapSorted_T										mapEmissive;
	R_dsgraph::mapSorted_T										mapHUDEmissive;
	R_dsgraph::mapHUD_T											mapHUDScopeMask;
	R_dsgraph::mapSorted_T										mapHUDDistort;

	xr_vector<R_dsgraph::_LodItem,render_alloc<R_dsgraph::_LodItem> >	lstLODs		;
	xr_vector<int,render_alloc<int> >									lstLODgroups;
	xr_vector<ISpatialShared>				lstRenderables;
	xr_vector<ISpatialShared>				lstRenderablesMain;
	xr_vector<ISpatialShared>				lstSpatial	;
	xr_vector<dxRender_Visual*,render_alloc<dxRender_Visual*> >			lstVisuals	;

	xr_vector<dxRender_Visual*,render_alloc<dxRender_Visual*> >			lstRecorded	;

	u32															counter_S	;
	u32															counter_D	;
	bool b_loaded;
	CPortalTraverser PortalTraverser;
	bool private_marker = false;
	xr_hash_set<void*> private_visuals;

public:
				void					set_Feedback			(R_feedback*V, u32	id)			{ val_feedback_breakp = id; val_feedback = V;		}
				void					get_Counters			(u32&	s,	u32& d)				{ s=counter_S; d=counter_D;			}
				void					clear_Counters			()								{ counter_S=counter_D=0; 			}

public:
	R_dsgraph_structure	()
	{
		val_pObject			= NULL	;
		val_pTransform		= NULL	;
		val_bHUD			= false	;
		val_bUI				= false	;
		val_bInvisible		= false	;
		val_bRecordMP		= false	;
		val_feedback		= 0;
		val_feedback_breakp	= 0;
		r_pmask				(true,true);
		b_loaded			= false	;
	};

	void r_dsgraph_destroy()
	{
		lstLODs.clear();
		lstLODgroups.clear();
		lstRenderables.clear();
		lstSpatial.clear();
		lstVisuals.clear();

		lstRecorded.clear();

		for (int i = 0; i < SHADER_PASSES_MAX; ++i)
		{
			mapNormalPasses[0][i].destroy();
			mapNormalPasses[1][i].destroy();
			mapMatrixPasses[0][i].destroy();
			mapMatrixPasses[1][i].destroy();
		}
		mapSorted.destroy();
		mapHUD.destroy();
		mapUI.destroy();
		mapLOD.destroy();
		mapDistort.destroy();
		mapHUDSorted.destroy();
		mapHUDDistort.destroy();
		mapUISorted.destroy();

		mapWmark.destroy();
		mapEmissive.destroy();
		mapHUDEmissive.destroy();
		mapUIEmissive.destroy();
	}

	void		r_dsgraph_clear_aux();
	void		r_dsgraph_clear_passes();
	void		add_Static(dxRender_Visual* pVisual, u32 planes);
	void		add_leafs_Dynamic(dxRender_Visual* pVisual, bool IgnoreObject = false); // if detected node's full visibility

	void		r_pmask											(bool deffered = false, bool forward = false, bool wallmarks = false) { pmask[0] = deffered; pmask[1] = forward; pmask[2] = wallmarks; }

	void		r_dsgraph_insert_dynamic						(dxRender_Visual	*pVisual, Fvector& Center);
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
	void		r_dsgraph_render_subspace						(IRender_Sector* _sector, CFrustum* _frustum, Fmatrix& mCombined, Fvector& _cop, bool _dynamic, bool _precise_portals=false, CObject*O=nullptr );
	void		r_dsgraph_render_subspace						(IRender_Sector* _sector, Fmatrix& mCombined, Fvector& _cop, bool _dynamic, bool _precise_portals=false, CObject*O=nullptr );
	void		r_dsgraph_render_R1_box							(IRender_Sector* _sector, Fbox& _bb, int _element);

	void detectSectors_sphere(CSector* sector, xr_vector<IRender_Sector*>& m_sectors, const Fsphere& sphere);
	void detectSectors_frustum(CSector* sector, xr_vector<IRender_Sector*>& m_sectors, CFrustum* _frustum);

public:
	virtual		u32						memory_usage			()
	{
		return	(0);
	}
};
