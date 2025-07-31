#pragma once

#include "../../xrEngine/Render.h"
#include "../../xrCore/Collision/ISpatial.h"
#include "r__dsgraph_types.h"
#include "r__sector.h"

//////////////////////////////////////////////////////////////////////////
// feedback	for receiving visuals										//
//////////////////////////////////////////////////////////////////////////
class	R_feedback
{
public:
	virtual		void	rfeedback_static	(dxRender_Visual*	V)		= 0;
};

//////////////////////////////////////////////////////////////////////////
// common part of interface implementation for all D3D renderers		//
//////////////////////////////////////////////////////////////////////////
class	R_dsgraph_structure										: public IRender_interface, public pureFrame
{
public:
	IRenderable*												val_pObject;
	Fmatrix*													val_pTransform;
	BOOL														val_bHUD;
	BOOL														val_bInvisible;
	BOOL														val_bRecordMP;		// record nearest for multi-pass

	R_feedback*													val_feedback;		// feedback for geometry being rendered
	u32															val_feedback_breakp;// breakpoint

	u32															phase;
	u32															marker;
	bool														pmask[3];
public:
	// Dynamic scene graph
	//R_dsgraph::mapNormal_T										mapNormal	[2]		;	// 2==(priority/2)
	R_dsgraph::mapNormalPasses_T								mapNormalPasses	[2]	;	// 2==(priority/2)
	//R_dsgraph::mapMatrix_T										mapMatrix	[2]		;
	R_dsgraph::mapMatrixPasses_T								mapMatrixPasses	[2]	;
	R_dsgraph::mapSorted_T										mapSorted;
	R_dsgraph::mapHUD_T											mapHUD;
	R_dsgraph::mapLOD_T											mapLOD;
	R_dsgraph::mapSorted_T										mapDistort;
	R_dsgraph::mapHUD_T											mapHUDSorted;

#if RENDER!=R_R1
	R_dsgraph::mapSorted_T										mapWmark;			// sorted
	R_dsgraph::mapSorted_T										mapEmissive;
	R_dsgraph::mapSorted_T										mapHUDEmissive;
#endif

	xr_vector<R_dsgraph::_LodItem,render_alloc<R_dsgraph::_LodItem> >	lstLODs		;
	xr_vector<int,render_alloc<int> >									lstLODgroups;
	xr_vector<ISpatialShared>				lstRenderables;
	xr_vector<ISpatialShared>				lstRenderablesMain;
	xr_vector<ISpatialShared>				lstSpatial	;
	xr_vector<dxRender_Visual*,render_alloc<dxRender_Visual*> >			lstVisuals	;

	xr_vector<dxRender_Visual*,render_alloc<dxRender_Visual*> >			lstRecorded	;

	u32															counter_S	;
	u32															counter_D	;
	BOOL														b_loaded	;
public:
	virtual		void					set_Transform			(Fmatrix*	M	)				{ VERIFY(M);	val_pTransform = M;	}
	virtual		void					set_HUD					(BOOL 		V	)				{ val_bHUD		= V;				}
	virtual		BOOL					get_HUD					()								{ return		val_bHUD;			}
	virtual		void					set_Invisible			(BOOL 		V	)				{ val_bInvisible= V;				}
				void					set_Feedback			(R_feedback*V, u32	id)			{ val_feedback_breakp = id; val_feedback = V;		}
				void					get_Counters			(u32&	s,	u32& d)				{ s=counter_S; d=counter_D;			}
				void					clear_Counters			()								{ counter_S=counter_D=0; 			}
public:
	R_dsgraph_structure	()
	{
		val_pObject			= NULL	;
		val_pTransform		= NULL	;
		val_bHUD			= FALSE	;
		val_bInvisible		= FALSE	;
		val_bRecordMP		= FALSE	;
		val_feedback		= 0;
		val_feedback_breakp	= 0;
		marker				= 0;
		r_pmask				(true,true);
		b_loaded			= FALSE	;
	};

	void		r_dsgraph_destroy()
	{
		lstLODs.clear			();
		lstLODgroups.clear		();
		lstRenderables.clear	();
		lstSpatial.clear		();
		lstVisuals.clear		();

		lstRecorded.clear		();

		//mapNormal[0].destroy	();
		//mapNormal[1].destroy	();
		//mapMatrix[0].destroy	();
		//mapMatrix[1].destroy	();
		for (int i=0; i<SHADER_PASSES_MAX; ++i)
		{
			mapNormalPasses[0][i].destroy	();
			mapNormalPasses[1][i].destroy	();
			mapMatrixPasses[0][i].destroy	();
			mapMatrixPasses[1][i].destroy	();
		}
		mapSorted.destroy		();
		mapHUD.destroy			();
		mapLOD.destroy			();
		mapDistort.destroy		();
		mapHUDSorted.destroy();

#if RENDER!=R_R1
		mapWmark.destroy		();
		mapEmissive.destroy		();
		mapHUDEmissive.destroy	();
#endif
	}

	void		r_pmask											(bool deffered = false, bool forward = false, bool wallmarks = false) { pmask[0] = deffered; pmask[1] = forward; pmask[2] = wallmarks; }

	void		r_dsgraph_insert_dynamic						(dxRender_Visual	*pVisual, Fvector& Center);
	void		r_dsgraph_insert_static							(dxRender_Visual	*pVisual);

	void		r_dsgraph_render_graph							(u32	_priority,	bool _clear=true);
	void		r_dsgraph_render_hud							();
	void		r_dsgraph_render_hud_ui							();
	void		r_dsgraph_render_lods							(bool	_setup_zb,	bool _clear);
	void		r_dsgraph_render_sorted							();
	void		r_dsgraph_render_emissive						();
	void		r_dsgraph_render_wmarks							();
	void		r_dsgraph_render_distort						();
	void		r_dsgraph_render_subspace						(IRender_Sector* _sector, CFrustum* _frustum, Fmatrix& mCombined, Fvector& _cop, BOOL _dynamic, BOOL _precise_portals=FALSE, CObject*O=nullptr );
	void		r_dsgraph_render_subspace						(IRender_Sector* _sector, Fmatrix& mCombined, Fvector& _cop, BOOL _dynamic, BOOL _precise_portals=FALSE, CObject*O=nullptr );
	void		r_dsgraph_render_R1_box							(IRender_Sector* _sector, Fbox& _bb, int _element);


public:
	virtual		u32						memory_usage			()
	{
		return	(0);
	}
};
