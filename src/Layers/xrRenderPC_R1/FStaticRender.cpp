// CRender.cpp: implementation of the CRender class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include <d3dcompiler.h>

#include "../xrRenderDX9/dx9ShaderUtils.h"

#include "../../xrEngine/igame_persistent.h"
#include "../../xrEngine/environment.h"
#include "../xrRender/fbasicvisual.h"
#include "../../xrEngine/CustomHUD.h"
#include "../../xrEngine/xr_object.h"
#include "../../xrEngine/fmesh.h"
#include "../xrRender/SkeletonCustom.h"
#include "../xrRender/lighttrack.h"
#include "../xrRender/dxRenderDeviceRender.h"
#include "../xrRender/dxWallMarkArray.h"
#include "../xrRender/dxUIShader.h"
#include "../../xrCore/git_version.h"

using	namespace		R_dsgraph;

CRender													RImplementation;

//////////////////////////////////////////////////////////////////////////
ShaderElement*			CRender::rimp_select_sh_dynamic	(dxRender_Visual	*pVisual, float cdist_sq)
{
	switch (phase)		{
	case PHASE_NORMAL:	return (RImplementation.L_Projector->shadowing()?pVisual->shader->E[SE_R1_NORMAL_HQ]:pVisual->shader->E[SE_R1_NORMAL_LQ])._get();
	case PHASE_POINT:	return pVisual->shader->E[SE_R1_LPOINT]._get();
	case PHASE_SPOT:	return pVisual->shader->E[SE_R1_LSPOT]._get();
	default:			NODEFAULT;
	}
#ifdef DEBUG
	return	0;
#endif
}
//////////////////////////////////////////////////////////////////////////
ShaderElement*			CRender::rimp_select_sh_static	(dxRender_Visual	*pVisual, float cdist_sq)
{
	switch (phase)		{
	case PHASE_NORMAL:	return (((_sqrt(cdist_sq) - pVisual->vis.sphere.R)<44)?pVisual->shader->E[SE_R1_NORMAL_HQ]:pVisual->shader->E[SE_R1_NORMAL_LQ])._get();
	case PHASE_POINT:	return pVisual->shader->E[SE_R1_LPOINT]._get();
	case PHASE_SPOT:	return pVisual->shader->E[SE_R1_LSPOT]._get();
	default:			NODEFAULT;
	}
#ifdef DEBUG
	return	0;
#endif
}

//////////////////////////////////////////////////////////////////////////
void					CRender::create					()
{
	L_DB				= 0;
	L_Shadows			= 0;
	L_Projector			= 0;

	Device.seqFrame.Add	(this,REG_PRIORITY_HIGH+0x12345678);

	// c-setup
	dxRenderDeviceRender::Instance().Resources->RegisterConstantSetup("L_dynamic_pos",		&r1_dlight_binder_PR);
	dxRenderDeviceRender::Instance().Resources->RegisterConstantSetup("L_dynamic_color",	&r1_dlight_binder_color);
	dxRenderDeviceRender::Instance().Resources->RegisterConstantSetup("L_dynamic_xform",	&r1_dlight_binder_xform);


	// distortion
	u32 v_dev	= CAP_VERSION(Caps.raster_major, Caps.raster_minor);
	u32 v_need	= CAP_VERSION(1,4);

	o.distortion = v_dev >= v_need && !Core.ParamsData.test(ECoreParams::nodistort);
	Msg("* distortion: %s, dev(%d),need(%d)", o.distortion ? "used" : "unavailable", v_dev, v_need);

	//	Color mapping
	o.color_mapping = v_dev >= v_need && !Core.ParamsData.test(ECoreParams::nocolormap);
	Msg("* color_mapping: %s, dev(%d),need(%d)", o.color_mapping ? "used" : "unavailable", v_dev, v_need);

	m_skinning					= -1;

	// disasm
	o.disasm					= Core.ParamsData.test(ECoreParams::disasm);
	o.forceskinw				= Core.ParamsData.test(ECoreParams::skinw);
	o.no_detail_textures		= !ps_r2_ls_flags.test(R1FLAG_DETAIL_TEXTURES);
	c_ldynamic_props			= "L_dynamic_props";

	m_bMakeAsyncSS				= false;

//---------
	Target						= new CRenderTarget		();
//---------
	//
	Models						= new CModelPool		();
	L_Dynamic					= new CLightR_Manager	();
	PSLibrary.OnCreate			();
//.	HWOCC.occq_create			(occq_size);

	xrRender_apply_tf			();
	::PortalTraverser.initialize();
}

void CRender::destroy()
{
	m_bMakeAsyncSS = false;
	::PortalTraverser.destroy();
	//.	HWOCC.occq_destroy			();
	PSLibrary.OnDestroy();

	xr_delete(L_Dynamic);
	xr_delete(Models);

	//*** Components
	xr_delete(Target);
	Device.seqFrame.Remove(this);

	r_dsgraph_destroy();
}

void CRender::reset_begin()
{
	if (b_loaded)
	{
		Details->Unload();
		xr_delete(Details);
	}

	xr_delete(Target);
}

void CRender::reset_end()
{
	xrRender_apply_tf			();
//.	HWOCC.occq_create			(occq_size);
	if (b_loaded)
	{
		Details = new CDetailManager();
		Details->Load();
	}

	Target						=	new CRenderTarget	();

	if (L_Projector)			L_Projector->invalidate		();

	// Set this flag true to skip the first render frame,
	// that some data is not ready in the first frame (for example device camera position)
	m_bFirstFrameAfterReset = true;
}

void					CRender::OnFrame				()
{
	Models->DeleteQueue	();
}

// Implementation
IRender_ObjectSpecific*	CRender::ros_create				(IRenderable* parent)					{ return new CROS_impl();			}
void					CRender::ros_destroy			(IRender_ObjectSpecific* &p)			{ xr_delete(p);							}
IRenderVisual*			CRender::model_Create			(LPCSTR name, IReader* data)			{ return Models->Create(name,data);		}
IRenderVisual*			CRender::model_CreateChild		(LPCSTR name, IReader* data)			{ return Models->CreateChild(name,data);}
IRenderVisual*			CRender::model_Duplicate		(IRenderVisual* V)						{ return Models->Instance_Duplicate((dxRender_Visual*)V);	}
void					CRender::model_Delete			(IRenderVisual* &V, BOOL bDiscard)		
{ 
	dxRender_Visual* pVisual = (dxRender_Visual*)V;
	Models->Delete(pVisual, bDiscard);
	V = 0;
}
IRender_DetailModel*	CRender::model_CreateDM			(IReader*F)
{
	CDetail*	D		= new CDetail ();
	D->Load				(F);
	return D;
}
void					CRender::model_Delete			(IRender_DetailModel* & F)
{
	if (F)
	{
		CDetail*	D	= (CDetail*)F;
		D->Unload		();
		xr_delete		(D);
		F				= nullptr;
	}
}
IRenderVisual*			CRender::model_CreatePE			(LPCSTR name)	
{ 
	PS::CPEDef*	SE		= PSLibrary.FindPED	(name);		R_ASSERT3(SE,"Particle effect doesn't exist",name);
	return				Models->CreatePE	(SE);
}

IRenderVisual*			CRender::model_CreateParticles	(LPCSTR name)	
{ 
	PS::CPEDef*	SE		= PSLibrary.FindPED	(name);
	if (SE) return		Models->CreatePE	(SE);
	else{
		PS::CPGDef*	SG	= PSLibrary.FindPGD	(name);		R_ASSERT3(SG,"Particle effect or group doesn't exist",name);
		return			Models->CreatePG	(SG);
	}
}
void					CRender::models_Prefetch		()					{ Models->Prefetch	();}
void					CRender::models_Clear			(BOOL b_complete)	{ Models->ClearPool	(b_complete);}

ref_shader				CRender::getShader				(int id)			{ VERIFY(id<int(Shaders.size()));	return Shaders[id];	}
IRender_Portal*			CRender::getPortal				(int id)			{ VERIFY(id<int(Portals.size()));	return Portals[id];	}
IRender_Sector*			CRender::getSector				(int id)			{ VERIFY(id<int(Sectors.size()));	return Sectors[id];	}
IRender_Sector*			CRender::getSectorActive		()					{ return pLastSector;									}
IRenderVisual*			CRender::getVisual				(int id)			{ VERIFY(id<int(Visuals.size()));	return Visuals[id];	}
D3DVERTEXELEMENT9*		CRender::getVB_Format			(int id)			{ VERIFY(id<int(DCL.size()));		return DCL[id].begin();	}
IDirect3DVertexBuffer9*	CRender::getVB					(int id)			{ VERIFY(id<int(VB.size()));		return VB[id];		}
IDirect3DIndexBuffer9*	CRender::getIB					(int id)			{ VERIFY(id<int(IB.size()));		return IB[id];		}
IRender_Target*			CRender::getTarget				()					{ return Target;										}
FSlideWindowItem*		CRender::getSWI					(int id)			{ VERIFY(id<int(SWIs.size()));		return &SWIs[id];	}

CRender::SurfaceParams CRender::getSurface(const char* nameTexture)
{
	auto texture = DEV->_CreateTexture(nameTexture);
	SurfaceParams surface = {};
	surface.Surface = texture->pSurface;
	surface.w = texture->get_Width();
	surface.h = texture->get_Height();

	return surface;
}

IRender_Light*			CRender::light_create			()					{ return L_DB->Create();								}

IRender_Glow*			CRender::glow_create			()					{ return new CGlow();								}

void					CRender::flush					()					{ r_dsgraph_render_graph	(0);						}

BOOL					CRender::occ_visible			(vis_data& P)		{ return HOM.visible(P);								}
BOOL					CRender::occ_visible			(sPoly& P)			{ return HOM.visible(P);								}
BOOL					CRender::occ_visible			(Fbox& P)			{ return HOM.visible(P);								}
ENGINE_API	extern BOOL g_bRendering;
void					CRender::add_Visual				(IRenderVisual* V, bool ignore_opt)
{
	VERIFY				(g_bRendering);
	add_leafs_Dynamic	((dxRender_Visual*)V, ignore_opt);
}
void					CRender::add_Geometry			(IRenderVisual* V ){ add_Static((dxRender_Visual*)V,View->getMask());						}
void					CRender::add_StaticWallmark		(ref_shader& S, const Fvector& P, float s, CDB::TRI* T, Fvector* verts)
{
	if (T->suppress_wm)	return;
	VERIFY2							(_valid(P) && _valid(s) && T && verts && (s>EPS_L), "Invalid static wallmark params");
	Wallmarks->AddStaticWallmark	(T,verts,P,&*S,s);
}

void CRender::add_StaticWallmark			(IWallMarkArray *pArray, const Fvector& P, float s, CDB::TRI* T, Fvector* V)
{
	dxWallMarkArray *pWMA = (dxWallMarkArray *)pArray;
	ref_shader *pShader = pWMA->dxGenerateWallmark();
	if (pShader) add_StaticWallmark		(*pShader, P, s, T, V);
}

void CRender::add_StaticWallmark			(const wm_shader& S, const Fvector& P, float s, CDB::TRI* T, Fvector* V)
{
	dxUIShader* pShader = (dxUIShader*)&*S;
	add_StaticWallmark		(pShader->hShader, P, s, T, V);
}

void					CRender::clear_static_wallmarks	()
{
	if (Wallmarks)
		Wallmarks->clear				();
}

void					CRender::add_SkeletonWallmark	(intrusive_ptr<CSkeletonWallmark> wm)
{
	Wallmarks->AddSkeletonWallmark				(wm);
}
void					CRender::add_SkeletonWallmark	(const Fmatrix* xf, CKinematics* obj, ref_shader& sh, const Fvector& start, const Fvector& dir, float size)
{
	Wallmarks->AddSkeletonWallmark				(xf, obj, sh, start, dir, size);
}
void					CRender::add_SkeletonWallmark	(const Fmatrix* xf, IKinematics* obj, IWallMarkArray *pArray, const Fvector& start, const Fvector& dir, float size)
{
	dxWallMarkArray *pWMA = (dxWallMarkArray *)pArray;
	ref_shader *pShader = pWMA->dxGenerateWallmark();
	if (pShader) add_SkeletonWallmark(xf, (CKinematics*)obj, *pShader, start, dir, size);
}
void					CRender::add_Occluder			(Fbox2&	bb_screenspace	)
{
	VERIFY					(_valid(bb_screenspace));
	HOM.occlude				(bb_screenspace);
}

#include "../../xrEngine/PS_instance.h"
void					CRender::set_Object				(IRenderable*		O )	
{
	VERIFY					(g_bRendering);
	val_pObject				= O;		// nullptr is OK, trust me :)
	if (val_pObject)		{
		VERIFY(dynamic_cast<CObject*>(O)||dynamic_cast<CPS_Instance*>(O));
		if (O->renderable.pROS) { VERIFY(dynamic_cast<CROS_impl*>(O->renderable.pROS)); }
	}
	if (PHASE_NORMAL==phase)	{
		if (L_Shadows)
			L_Shadows->set_object	(O);
		
		if (L_Projector)
			L_Projector->set_object	(O);
	} else {
		if (L_Shadows)
			L_Shadows->set_object(0);

		if (L_Projector)
			L_Projector->set_object	(0);
	}
}
void					CRender::apply_object			(IRenderable*		O )
{
	if (0==O)			return	;
	if (PHASE_NORMAL==phase	&& O->renderable_ROS())		{
		CROS_impl& LT		= *((CROS_impl*)O->renderable.pROS);
		VERIFY(dynamic_cast<CObject*>(O)||dynamic_cast<CPS_Instance*>(O));
		VERIFY(dynamic_cast<CROS_impl*>(O->renderable.pROS));
		float o_hemi		= 0.5f*LT.get_hemi						();
		float o_sun			= 0.5f*LT.get_sun						();
		RCache.set_c		(c_ldynamic_props,o_sun,o_sun,o_sun,o_hemi);
		// shadowing
		if ((LT.shadow_recv_frame==Device.dwFrame) && O->renderable_ShadowReceive())	
			RImplementation.L_Projector->setup	(LT.shadow_recv_slot);
	}
}

// Misc
float					g_fSCREEN;
static	BOOL			gm_Nearer	= 0;

IC		void			gm_SetNearer		(BOOL bNearer)
{
	if (bNearer	!= gm_Nearer)
	{
		gm_Nearer	= bNearer;
		if (gm_Nearer)	RImplementation.rmNear	();
		else			RImplementation.rmNormal();
	}
}

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
CRender::CRender	()
:m_bFirstFrameAfterReset(false)
{
}

CRender::~CRender()
{
	for (auto& it : SWIs) {
		xr_free(it.sw);
		it.sw = nullptr;
		it.count = 0;
	}
	SWIs.clear();
}

extern float		r_ssaDISCARD;
extern float		r_ssaDONTSORT;
extern float		r_ssaLOD_A,			r_ssaLOD_B;
extern float		r_ssaGLOD_start,	r_ssaGLOD_end;
extern float		r_ssaHZBvsTEX;

ICF bool			pred_sp_sort		(ISpatial* _1, ISpatial* _2)
{
	float	d1		= _1->spatial.sphere.P.distance_to_sqr(Device.vCameraPosition);
	float	d2		= _2->spatial.sphere.P.distance_to_sqr(Device.vCameraPosition);
	return	d1<d2;
}

void CRender::Calculate				()
{
	Device.Statistic->RenderCALC.Begin();

	// Transfer to global space to avoid deep pointer access
	IRender_Target* T				=	getTarget	();
	float	fov_factor				=	_sqr		(90.f / Device.fFOV);
	g_fSCREEN						=	float(T->get_width()*T->get_height())*fov_factor*(EPS_S+ps_r__LOD);
	r_ssaDISCARD					=	_sqr(ps_r__ssaDISCARD)		/g_fSCREEN;
	r_ssaDONTSORT					=	_sqr(ps_r__ssaDONTSORT/3)	/g_fSCREEN;
	r_ssaLOD_A						=	_sqr(ps_r1_ssaLOD_A/3)		/g_fSCREEN;
	r_ssaLOD_B						=	_sqr(ps_r1_ssaLOD_B/3)		/g_fSCREEN;
	r_ssaGLOD_start					=	_sqr(ps_r__GLOD_ssa_start/3)/g_fSCREEN;
	r_ssaGLOD_end					=	_sqr(ps_r__GLOD_ssa_end/3)	/g_fSCREEN;
	r_ssaHZBvsTEX					=	_sqr(ps_r__ssaHZBvsTEX/3)	/g_fSCREEN;

	// Frustum & HOM rendering
	ViewBase.CreateFromMatrix		(Device.mFullTransform,FRUSTUM_P_LRTB|FRUSTUM_P_FAR);
	View							= 0;
	if (!ps_r2_ls_flags.test(R2FLAG_EXP_MT_CALC))	{
		HOM.Enable									();
		HOM.Render									(ViewBase);
	}
	gm_SetNearer					(FALSE);
	phase							= PHASE_NORMAL;

	// Detect camera-sector
	if (!vLastCameraPos.similar(Device.vCameraPosition,EPS_S)) 
	{
		CSector* pSector		= (CSector*)detectSector(Device.vCameraPosition);
		if (pSector && (pSector!=pLastSector))
			g_pGamePersistent->OnSectorChanged( translateSector(pSector) );

		if (0==pSector) pSector = pLastSector;
		pLastSector = pSector;
		vLastCameraPos.set(Device.vCameraPosition);
	}

	// Check if camera is too near to some portal - if so force DualRender
	if (rmPortals) 
	{
		Fvector box_radius;		box_radius.set(EPS_L*2,EPS_L*2,EPS_L*2);
		Sectors_xrc.box_options	(CDB::OPT_FULL_TEST);
		Sectors_xrc.box_query	(rmPortals,Device.vCameraPosition,box_radius);
		for (int K=0; K<Sectors_xrc.r_count(); K++)
		{
			CPortal*	pPortal		= (CPortal*) Portals[rmPortals->get_tris()[Sectors_xrc.r_begin()[K].id].dummy];
			pPortal->bDualRender	= TRUE;
		}
	}
	//
	if (L_DB)
		L_DB->Update();

	// Main process
	marker	++;
	if (pLastSector)
	{
		// Traverse sector/portal structure
		PortalTraverser.traverse	
			(
			pLastSector,
			ViewBase,
			Device.vCameraPosition,
			Device.mFullTransform,
			CPortalTraverser::VQ_HOM + CPortalTraverser::VQ_SSA + CPortalTraverser::VQ_FADE
			);

		// Determine visibility for static geometry hierrarhy
		if  (psDeviceFlags.test(rsDrawStatic))	{
			for (u32 s_it=0; s_it<PortalTraverser.r_sectors.size(); s_it++)
			{
				CSector*	sector		= (CSector*)PortalTraverser.r_sectors[s_it];
				dxRender_Visual*	root	= sector->root();
				for (u32 v_it=0; v_it<sector->r_frustums.size(); v_it++)
				{
					set_Frustum			(&(sector->r_frustums[v_it]));
					add_Geometry		(root);
				}
			}
		}

		// Traverse object database
		if  (psDeviceFlags.test(rsDrawDynamic))	{
			g_SpatialSpace->q_frustum
				(
				lstRenderables,
				ISpatial_DB::O_ORDERED,
				STYPE_RENDERABLE + STYPE_PARTICLE + STYPE_LIGHTSOURCE,
				ViewBase
				);

			// Exact sorting order (front-to-back)
			std::sort							(lstRenderables.begin(),lstRenderables.end(),pred_sp_sort);

			// Determine visibility for dynamic part of scene
			set_Object							(0);

			if (psGameFlags.test(rsActorShadow)) {
				g_hud->Render_First();
			}

			g_hud->Render_Last					( );	
			u32 uID_LTRACK						= 0xffffffff;
			if (phase==PHASE_NORMAL)			{
				uLastLTRACK	++;
				if (lstRenderables.size())		uID_LTRACK	= uLastLTRACK%lstRenderables.size();

				// update light-vis for current entity / actor
				CObject*	O					= g_pGameLevel->CurrentViewEntity();
				if (O)		{
					CROS_impl*	R					= (CROS_impl*) O->ROS();
					if (R)		R->update			(O);
				}
			}
			for (u32 o_it=0; o_it<lstRenderables.size(); o_it++)
			{
				ISpatial*	spatial		= lstRenderables[o_it];		spatial->spatial_updatesector	();
				CSector*	sector		= (CSector*)spatial->spatial.sector	;
				if	(0==sector)										
					continue;	// disassociated from S/P structure

				// Filter only not light spatial
				if	(PortalTraverser.i_marker != sector->r_marker && (spatial->spatial.type&STYPE_RENDERABLE || spatial->spatial.type&STYPE_PARTICLE) )	continue;	// inactive (untouched) sector

				if(spatial->spatial.type&STYPE_RENDERABLE || spatial->spatial.type&STYPE_PARTICLE)
				{
					for (u32 v_it=0; v_it<sector->r_frustums.size(); v_it++)
					{
						set_Frustum			(&(sector->r_frustums[v_it]));
						if (!View->testSphere_dirty(spatial->spatial.sphere.P,spatial->spatial.sphere.R)) continue;
						// renderable
						IRenderable*	renderable		= spatial->dcast_Renderable	();
						if (renderable)
						{
							// Occlusiond
							vis_data&		v_orig			= renderable->renderable.visual->getVisData();
							vis_data		v_copy			= v_orig;
							v_copy.box.xform				(renderable->renderable.xform);
							BOOL			bVisible		= HOM.visible(v_copy);
							v_orig.accept_frame				= v_copy.accept_frame;
							v_orig.marker					= v_copy.marker;
							v_orig.hom_frame				= v_copy.hom_frame;
							v_orig.hom_tested				= v_copy.hom_tested;
							if (!bVisible)					break;	// exit loop on frustums

							// rendering
							if (o_it==uID_LTRACK && renderable->renderable_ROS())	{
								// track lighting environment
								CROS_impl*		T = (CROS_impl*)renderable->renderable_ROS();
								T->update			(renderable);
							}
							set_Object						(renderable);
							renderable->renderable_Render	();
							set_Object						(0);	//? is it needed at all
						}
						else
						{
							// It may be an glow
							CGlow*		glow				= fast_dynamic_cast<CGlow*>(spatial);
							VERIFY							(glow);
							L_Glows->add					(glow);
						}
						break;	// exit loop on frustums
					}
				}

				if (spatial->spatial.type & STYPE_LIGHTSOURCE)
				{
					if ( ViewBase.testSphere_dirty(spatial->spatial.sphere.P,spatial->spatial.sphere.R) )
					{
						VERIFY								(spatial->spatial.type & STYPE_LIGHTSOURCE);
						// lightsource
						if(light*			L					= (light*)	spatial->dcast_Light	())
						{
							if (L->spatial.sector)				{
								vis_data&		vis		= L->get_homdata	( );
								if	(HOM.visible(vis))	L_DB->add_light		(L);
							}
						}
					}
				}
			}
		}

		// Calculate miscelaneous stuff
		L_Shadows->calculate								();
		L_Projector->calculate								();
	}
	else
	{
		set_Object											(0);
		/*
		g_pGameLevel->pHUD->Render_First					();	
		g_pGameLevel->pHUD->Render_Last						();	

		// Calculate miscelaneous stuff
		L_Shadows->calculate								();
		L_Projector->calculate								();
		*/
	}

	// End calc
	Device.Statistic->RenderCALC.End	();
}

void	CRender::rmNear		()
{
	IRender_Target* T	=	getTarget	();
	D3DVIEWPORT9 VP		=	{0,0,T->get_width(),T->get_height(),0,0.02f };
	CHK_DX				(RDevice->SetViewport(&VP));
}
void	CRender::rmFar		()
{
	IRender_Target* T	=	getTarget	();
	D3DVIEWPORT9 VP		=	{0,0,T->get_width(),T->get_height(),0.99999f,1.f };
	CHK_DX				(RDevice->SetViewport(&VP));
}
void	CRender::rmNormal	()
{
	IRender_Target* T	=	getTarget	();
	D3DVIEWPORT9 VP		= {0,0,T->get_width(),T->get_height(),0,1.f };
	CHK_DX				(RDevice->SetViewport(&VP));
}

extern u32 g_r;
void	CRender::Render		()
{
	RDevice->SetRenderTarget(0, RTarget);
	if( m_bFirstFrameAfterReset )
	{
		m_bFirstFrameAfterReset = false;
		return;
	}

	g_r											= 1;
	Device.Statistic->RenderDUMP.Begin();
	// Begin
	Target->Begin								();
	o.vis_intersect								= FALSE			;
	phase										= PHASE_NORMAL	;
	r_dsgraph_render_hud						();				// hud
	r_dsgraph_render_graph						(0);			// normal level
	if(Details)Details->Render					();				// grass / details
	r_dsgraph_render_lods						(true,false);	// lods - FB

	g_pGamePersistent->Environment().RenderSky	();				// sky / sun
	g_pGamePersistent->Environment().RenderClouds	();				// clouds

	r_pmask										(true,false);	// disable priority "1"
	o.vis_intersect								= TRUE			;
	HOM.Disable									();
	L_Dynamic->render							(0);				// addititional light sources
	if(Wallmarks){
		g_r										= 0;
		Wallmarks->Render						();				// wallmarks has priority as normal geometry
	}
	HOM.Enable									();
	o.vis_intersect								= FALSE			;
	phase										= PHASE_NORMAL	;
	r_pmask										(true,true);	// enable priority "0" and "1"
	if(L_Shadows)L_Shadows->render				();				// ... and shadows
	r_dsgraph_render_lods						(false,true);	// lods - FB
	r_dsgraph_render_graph						(1);			// normal level, secondary priority
	L_Dynamic->render							(1);			// addititional light sources, secondary priority
	phase = PHASE_NORMAL;
	PortalTraverser.fade_render					();				// faded-portals
	r_dsgraph_render_sorted						();				// strict-sorted geoms
	if(L_Glows)L_Glows->Render					();				// glows
	g_pGamePersistent->Environment().RenderFlares	();				// lens-flares
	g_pGamePersistent->Environment().RenderLast	();				// rain/thunder-bolts

#if DEBUG
	for (int _priority=0; _priority<2; ++_priority)
	{
		for ( u32 iPass = 0; iPass<SHADER_PASSES_MAX; ++iPass)
		{
			R_ASSERT( mapNormalPasses[_priority][iPass].size() == 0);
			R_ASSERT( mapMatrixPasses[_priority][iPass].size() == 0);
		}
	}

#endif
	// Postprocess, if necessary
	Target->End									();
	if (L_Projector) L_Projector->finalize		();

	// HUD
	Device.Statistic->RenderDUMP.End	();
}

void	CRender::ApplyBlur4		(FVF::TL4uv* pv, u32 w, u32 h, float k)
{
	float	_w					= float(w);
	float	_h					= float(h);
	float	kw					= (1.f/_w)*k;
	float	kh					= (1.f/_h)*k;
	Fvector2					p0,p1;
	p0.set						(.5f/_w, .5f/_h);
	p1.set						((_w+.5f)/_w, (_h+.5f)/_h );
	u32		_c					= 0xffffffff;

	// Fill vertex buffer
	pv->p.set(EPS,			float(_h+EPS),	EPS,1.f); pv->color=_c; pv->uv[0].set(p0.x-kw,p1.y-kh);pv->uv[1].set(p0.x+kw,p1.y+kh);pv->uv[2].set(p0.x+kw,p1.y-kh);pv->uv[3].set(p0.x-kw,p1.y+kh);pv++;
	pv->p.set(EPS,			EPS,			EPS,1.f); pv->color=_c; pv->uv[0].set(p0.x-kw,p0.y-kh);pv->uv[1].set(p0.x+kw,p0.y+kh);pv->uv[2].set(p0.x+kw,p0.y-kh);pv->uv[3].set(p0.x-kw,p0.y+kh);pv++;
	pv->p.set(float(_w+EPS),float(_h+EPS),	EPS,1.f); pv->color=_c; pv->uv[0].set(p1.x-kw,p1.y-kh);pv->uv[1].set(p1.x+kw,p1.y+kh);pv->uv[2].set(p1.x+kw,p1.y-kh);pv->uv[3].set(p1.x-kw,p1.y+kh);pv++;
	pv->p.set(float(_w+EPS),EPS,			EPS,1.f); pv->color=_c; pv->uv[0].set(p1.x-kw,p0.y-kh);pv->uv[1].set(p1.x+kw,p0.y+kh);pv->uv[2].set(p1.x+kw,p0.y-kh);pv->uv[3].set(p1.x-kw,p0.y+kh);pv++;
}

#include "../../xrEngine/GameFont.h"
void	CRender::Statistics	(CGameFont* _F)
{
	CGameFont&	F	= *_F;
	F.OutSet(250, 35);
	F.OutNext	(" **** Occ-Q(%03.1f) **** ",100.f*f32(stats.o_culled)/f32(stats.o_queries?stats.o_queries:1));
	F.OutNext	(" total  : %2d",	stats.o_queries	);	stats.o_queries = 0;
	F.OutNext	(" culled : %2d",	stats.o_culled	);	stats.o_culled	= 0;
	F.OutSkip	();
#ifdef DEBUG
	HOM.stats	();
#endif
}

xr_string CRender::getShaderParams() {
	xr_string params = "";
	if (!m_ShaderOptions.empty()) {
		params.append("(").append(m_ShaderOptions[0].Name);

		for (auto i = 1u; i < m_ShaderOptions.size(); ++i) {
			params.append(",").append(m_ShaderOptions[i].Name);
		}

		params.append(")");
	}
	return params;
}

void CRender::addShaderOption(const char* name, const char* value) {
	m_ShaderOptions.emplace_back(name, value);
}

//--------------------------------------------------------------------------------------------------------------
class includer : public ID3DInclude {
public:
	HRESULT __stdcall Open(D3D_INCLUDE_TYPE IncludeType, LPCSTR pFileName, LPCVOID pParentData, LPCVOID* ppData, UINT* pBytes) {
		string_path pname;
		xr_strconcat(pname, ::Render->getShaderPath(), pFileName);
		IReader* R = FS.r_open("$game_shaders$", pname);
		if (0 == R) {
			// possibly in shared directory or somewhere else - open directly
			R = FS.r_open("$game_shaders$", pFileName);
			if (0 == R) {
				return E_FAIL;
			}
		}

		// duplicate and zero-terminate
		u32 size = R->length();
		u8* data = xr_alloc<u8>(size + 1);
		CopyMemory(data, R->pointer(), size);
		data[size] = 0;
		FS.r_close(R);

		*ppData = data;
		*pBytes = size;
		return	D3D_OK;
	}

	HRESULT __stdcall Close(LPCVOID pData) {
		xr_free(pData);
		return D3D_OK;
	}
};

static HRESULT create_shader				(
		LPCSTR const	pTarget,
		DWORD const*	buffer,
		u32	const		buffer_size,
		LPCSTR const	file_name,
		void*&			result,
		bool const		disasm
	)
{
	HRESULT		_result = E_FAIL;
	if (pTarget[0] == 'p') {
		SPS* sps_result = (SPS*)result;
		_result			= RDevice->CreatePixelShader(buffer, &sps_result->ps);
		if ( !SUCCEEDED(_result) ) {
			Msg("! PS: %s", file_name);
			Msg			("! CreatePixelShader hr == 0x%08x", _result);
			return		E_FAIL;
		}

		LPCVOID			data		= nullptr;
		_result			= D3D9FindShaderComment	(buffer,MAKEFOURCC('C','T','A','B'),&data,nullptr);
		if (SUCCEEDED(_result) && data)
		{
			LPD3DXSHADER_CONSTANTTABLE	pConstants	= LPD3DXSHADER_CONSTANTTABLE(data);
			sps_result->constants.parse	(pConstants,0x1);
		} 
		else
		{
			Msg("! PS: %s", file_name);
			Msg			("! D3DXFindShaderComment hr == 0x%08x", _result);
		}
	}
	else {
		SVS* svs_result = (SVS*)result;
		_result			= RDevice->CreateVertexShader(buffer, &svs_result->vs);
		if ( !SUCCEEDED(_result) ) {
			Msg("! VS: %s", file_name);
			Msg			("! CreatePixelShader hr == 0x%08x", _result);
			return		E_FAIL;
		}

		LPCVOID			data		= nullptr;
		_result			= D3D9FindShaderComment	(buffer,MAKEFOURCC('C','T','A','B'),&data,nullptr);
		if (SUCCEEDED(_result) && data)
		{
			LPD3DXSHADER_CONSTANTTABLE	pConstants	= LPD3DXSHADER_CONSTANTTABLE(data);
			svs_result->constants.parse	(pConstants,0x2);
		} 
		else
		{
			Msg("! VS: %s", file_name);
			Msg			("! D3DXFindShaderComment hr == 0x%08x", _result);
		}
	}

	if (disasm) {
		ID3DBlob* disasm_ = 0;
		D3DDisassemble(buffer, buffer_size, FALSE, 0, &disasm_);
		string_path dname;
		xr_strconcat(dname, "disasm\\", file_name, ('v' == pTarget[0]) ? ".vs.hlsl" : ".ps.hlsl");
		IWriter* W = FS.w_open("$logs$", dname);
		W->w(disasm_->GetBufferPointer(), disasm_->GetBufferSize());
		FS.w_close(W);
		_RELEASE(disasm_);
	}

	return				_result;
}

HRESULT	CRender::shader_compile			(
		LPCSTR							name,
		DWORD const*                    pSrcData,
		UINT                            SrcDataLen,
		LPCSTR                          pFunctionName,
		LPCSTR                          pTarget,
		DWORD                           Flags,
		void*&							result
	)
{
	D3D_SHADER_MACRO defines[128];
	int def_it = 0;

	char	sh_name[MAX_PATH] = "";
	u32 len	= 0;

	// options
	if (o.forceskinw)		{
		defines[def_it].Name		=	"SKIN_COLOR";
		defines[def_it].Definition	=	"1";
		def_it						++;
	}
	sh_name[len]='0'+char(o.forceskinw); ++len;

	if (m_skinning<0)		{
		defines[def_it].Name		=	"SKIN_NONE";
		defines[def_it].Definition	=	"1";
		def_it						++;
		sh_name[len]='1'; ++len;
	}
	else {
		sh_name[len]='0'; ++len;
	}

	if (0==m_skinning)		{
		defines[def_it].Name		=	"SKIN_0";
		defines[def_it].Definition	=	"1";
		def_it						++;
	}
	sh_name[len]='0'+char(0==m_skinning); ++len;

	if (1==m_skinning)		{
		defines[def_it].Name		=	"SKIN_1";
		defines[def_it].Definition	=	"1";
		def_it						++;
	}
	sh_name[len]='0'+char(1==m_skinning); ++len;

	if (2==m_skinning)		{
		defines[def_it].Name		=	"SKIN_2";
		defines[def_it].Definition	=	"1";
		def_it						++;
	}
	sh_name[len]='0'+char(2==m_skinning); ++len;

	if (3==m_skinning)		{
		defines[def_it].Name		=	"SKIN_3";
		defines[def_it].Definition	=	"1";
		def_it						++;
	}
	sh_name[len]='0'+char(3==m_skinning); ++len;

	if (4==m_skinning)		{
		defines[def_it].Name		=	"SKIN_4";
		defines[def_it].Definition	=	"1";
		def_it						++;
	}
	sh_name[len]='0'+char(4==m_skinning); ++len;

	// finish
	defines[def_it].Name			=	0;
	defines[def_it].Definition		=	0;
	def_it							++;
	R_ASSERT						(def_it<128);

	HRESULT		_result = E_FAIL;

	char extension[3];
	strncpy_s(extension, pTarget, 2);

	string_path file_name;
	{
		string_path file;
		xr_strcpy(file, "shaders_cache\\");
		xr_strcat(file, _VER);
		xr_strcat(file, "\\r1\\");
		xr_strcat(file, name);
		xr_strcat(file, ".");
		xr_strcat(file, extension);
		xr_strcat(file, "\\");
		xr_strcat(file, sh_name);
		FS.update_path(file_name, "$app_data_root$", file);
	}

	u32 const RealCodeCRC = crc32(pSrcData, SrcDataLen);
	if (FS.exist(file_name) && ps_r__common_flags.test(RFLAG_USE_CACHE)) {
#ifdef DEBUG
		Msg("compilied shader library found %s", file_name);
#endif // DEBUG

		IReader* file = FS.r_open(file_name);
		if (file->length()>4)
		{
			u32 ShaderCRC = file->r_u32();
			u32 CodeSRC = file->r_u32();

			if (RealCodeCRC == CodeSRC) {
				u32 const real_crc = crc32(file->pointer(), file->elapsed());
				if (real_crc == ShaderCRC) {
					_result = create_shader(pTarget, (DWORD*)file->pointer(), file->elapsed(), file_name, result, o.disasm);
				}
			}
		}
		file->close();
	}

	if (FAILED(_result))
	{
		includer					Includer;
		LPD3DBLOB					pShaderBuf = nullptr;
		LPD3DBLOB					pErrorBuf = nullptr;

		_result = D3DCompile(pSrcData, SrcDataLen,
				"",//nullptr, //LPCSTR pFileName,	//	NVPerfHUD bug workaround.
				defines, &Includer, pFunctionName,
				pTarget,
				Flags, 0,
				&pShaderBuf,
				&pErrorBuf
			);

		if (SUCCEEDED(_result)) {
			if (ps_r__common_flags.test(RFLAG_USE_CACHE)) {
				IWriter* file = FS.w_open(file_name);
				u32 const crc = crc32(pShaderBuf->GetBufferPointer(), pShaderBuf->GetBufferSize());
				file->w_u32(crc);
				file->w_u32(RealCodeCRC);
				file->w(pShaderBuf->GetBufferPointer(), pShaderBuf->GetBufferSize());
				FS.w_close				(file);
			}
			_result					= create_shader(pTarget, (DWORD*)pShaderBuf->GetBufferPointer(), pShaderBuf->GetBufferSize(), file_name, result, o.disasm);
		}
		else {
			Msg("! %s", file_name);
			if ( pErrorBuf )
				Msg("! error: %s",(LPCSTR)pErrorBuf->GetBufferPointer());
			else
				Msg					("Can't compile shader hr=0x%08x", _result);
		}
	}

	return						_result;
}
