// HOM.cpp: implementation of the CHOM class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "HOM.h"

#include "../../xrEngine/GameFont.h"

#include "dxRenderDeviceRender.h"
 
float	psOSSR		= .001f;

void CHOM::MT_RENDER()
{
	PROF_EVENT("Render HOM");

	bool b_main_menu_is_active = (g_pGamePersistent->m_pMainMenu && g_pGamePersistent->m_pMainMenu->IsActive());
	if (MT_frame_rendered != Device.dwFrame && !b_main_menu_is_active)
	{
		CFrustum ViewBase;
		ViewBase.CreateFromMatrix(Device.mFullTransform, FRUSTUM_P_LRTB + FRUSTUM_P_FAR);
		Enable();
		Render(ViewBase);
	}
}

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CHOM::CHOM()
{
	bEnabled		= FALSE;
	m_pModel		= 0;
	m_pTris			= 0;
#ifdef DEBUG_DRAW
	Device.seqRender.Add(this,REG_PRIORITY_LOW-1000);
#endif
}

CHOM::~CHOM()
{
#ifdef DEBUG_DRAW
	Device.seqRender.Remove(this);
#endif
}

#pragma pack(push,4)
struct HOM_poly			
{
	Fvector	v1,v2,v3;
	u32		flags;
};
#pragma pack(pop)

IC float	Area		(Fvector& v0, Fvector& v1, Fvector& v2)
{
	float	e1 = v0.distance_to(v1);
	float	e2 = v0.distance_to(v2);
	float	e3 = v1.distance_to(v2);
	
	float	p  = (e1+e2+e3)/2.f;
	return	_sqrt( p*(p-e1)*(p-e2)*(p-e3) );
}

void CHOM::Load()
{
	// Find and open file
	string_path		fName;
	FS.update_path(fName, "$level$", "level.hom");
	if (!FS.exist(fName))
	{
		Msg(" WARNING: Occlusion map '%s' not found.", fName);
		return;
	}
	Msg("* Loading HOM: %s", fName);

	IReader* fs = FS.r_open(fName);
	IReader* S = fs->open_chunk(1);

	u32 crc = crc32(fs->pointer(), fs->length());

	// Load tris and merge them
	CDB::Collector CL;
	while (!S->eof())
	{
		HOM_poly P;
		S->r(&P, sizeof(P));

		CL.add_face_packed_D(P.v1, P.v2, P.v3, P.flags, 0.01f);
	}

	// Determine adjacency
	xr_vector<u32> adjacency;
	CL.calc_adjacency(adjacency);

	// Create RASTER-triangles
	m_pTris = xr_alloc<occTri>(CL.getTS());
	for (size_t it = 0; it < CL.getTS(); it++)
	{
		CDB::TRI& clT = CL.getT()[it];
		occTri& rT = m_pTris[it];

		Fvector& v0 = CL.getV()[clT.verts[0]];
		Fvector& v1 = CL.getV()[clT.verts[1]];
		Fvector& v2 = CL.getV()[clT.verts[2]];

		rT.adjacent[0] = (0xffffffff == adjacency[3 * it + 0]) ? ((occTri*)(-1)) : (m_pTris + adjacency[3 * it + 0]);
		rT.adjacent[1] = (0xffffffff == adjacency[3 * it + 1]) ? ((occTri*)(-1)) : (m_pTris + adjacency[3 * it + 1]);
		rT.adjacent[2] = (0xffffffff == adjacency[3 * it + 2]) ? ((occTri*)(-1)) : (m_pTris + adjacency[3 * it + 2]);
		rT.flags = clT.dummy;
		rT.area = Area(v0, v1, v2);

		if (rT.area < EPS_L) 
		{
			Msg("! Invalid HOM triangle (%f,%f,%f)-(%f,%f,%f)-(%f,%f,%f)", VPUSH(v0), VPUSH(v1), VPUSH(v2));
		}

		rT.plane.build(v0, v1, v2);
		rT.skip = 0;
		rT.center.add(v0, v1).add(v2).div(3.f);
	}

	// Make cache
	string_path LevelName;
	xr_strconcat(LevelName, "level_cache\\", FS.get_path("$level$")->m_Add, "HOM.cache");
	IReader* pReaderCache = CDB::GetModelCache(LevelName, crc);

	// Create AABB-tree
	m_pModel = new CDB::MODEL();

	if (pReaderCache != nullptr)
	{
		m_pModel->build(CL.getV(), CL.getVS(), CL.getT(), CL.getTS(), nullptr, nullptr, pReaderCache, true);
	}
	else
	{
		IWriter* pWriterCache = FS.w_open("$app_data_root$", LevelName);
		pWriterCache->w_u32(crc);
		m_pModel->build(CL.getV(), CL.getVS(), CL.getT(), CL.getTS(), nullptr, nullptr, pWriterCache, false);
	}

	bEnabled = TRUE;

	S->close();
	FS.r_close(fs);

	if (ps_r2_ls_flags.test(R2FLAG_EXP_MT_CALC))
	{
		// MT-details (@front)
		//Device.seqParallelRender.push_back(fastdelegate::FastDelegate0<>(Details, &CDetailManager::MT_CALC));

		// MT-HOM (@front)
		Device.seqParallelRender.push_back(xr_make_delegate(this, &CHOM::MT_RENDER));
	}
}

void CHOM::Unload()
{
	xr_delete(m_pModel);
	xr_free(m_pTris);
	bEnabled = FALSE;

	auto I = std::find(Device.seqParallelRender.begin(), Device.seqParallelRender.end(), xr_make_delegate(this, &CHOM::MT_RENDER));

	if (I != Device.seqParallelRender.end())
		Device.seqParallelRender.erase(I);
}

class	pred_fb	{
public:
	occTri*		m_pTris	;
	Fvector		camera	;
public:
	pred_fb		(occTri* _t) : m_pTris(_t)	{}
	pred_fb		(occTri* _t, Fvector& _c) : m_pTris(_t), camera(_c)	{}
	ICF bool	operator()		(const CDB::RESULT& _1, const CDB::RESULT& _2) const {
		occTri&	t0	= m_pTris	[_1.id];
		occTri&	t1	= m_pTris	[_2.id];
		return	camera.distance_to_sqr(t0.center) < camera.distance_to_sqr(t1.center);
	}
	ICF bool	operator()		(const CDB::RESULT& _1)	const {
		occTri&	T	= m_pTris	[_1.id];
		return	T.skip>Device.dwFrame;
	}
};

void CHOM::Render_DB			(CFrustum& base)
{
	//Update projection matrices on every frame to ensure valid HOM culling
	float			view_dim	= occ_dim_0;
	Fmatrix			m_viewport		= {
		view_dim/2.f,			0.0f,					0.0f,		0.0f,
		0.0f,					-view_dim/2.f,			0.0f,		0.0f,
		0.0f,					0.0f,					1.0f,		0.0f,
		view_dim/2.f + 0 + 0,	view_dim/2.f + 0 + 0,	0.0f,		1.0f
	};
	Fmatrix			m_viewport_01	= {
		1.f/2.f,			0.0f,				0.0f,		0.0f,
		0.0f,				-1.f/2.f,			0.0f,		0.0f,
		0.0f,				0.0f,				1.0f,		0.0f,
		1.f/2.f + 0 + 0,	1.f/2.f + 0 + 0,	0.0f,		1.0f
	};
	m_xform.mul					(m_viewport,	Device.mFullTransform);
	m_xform_01.mul				(m_viewport_01,	Device.mFullTransform);

	// Query DB
	xrc.frustum_options			(0);
	xrc.frustum_query			(m_pModel,base);
	if (0==xrc.r_count())		return;

	// Prepare
	CDB::RESULT*	it			= xrc.r_begin	();
	CDB::RESULT*	end			= xrc.r_end		();
	
	Fvector			COP			= Device.vCameraPosition;
	end				= std::remove_if	(it,end,pred_fb(m_pTris));
	std::sort		(it,end,pred_fb(m_pTris,COP));

	// Build frustum with near plane only
	CFrustum					clip;
	clip.CreateFromMatrix		(Device.mFullTransform,FRUSTUM_P_NEAR);
	sPoly						src,dst;
	u32		_frame				= Device.dwFrame	;
#ifdef DEBUG
	tris_in_frame				= xrc.r_count();
	tris_in_frame_visible		= 0;
#endif

	// Perfrom selection, sorting, culling
	for (; it!=end; it++)
	{
		// Control skipping
		occTri& T			= m_pTris	[it->id];
		u32	next			= _frame + ::Random.randI(3,10);

		// Test for good occluder - should be improved :)
		if (!(T.flags || (T.plane.classify(COP)>0)))	
		{ T.skip=next; continue; }

		// Access to triangle vertices
		CDB::TRI& t		= m_pModel->get_tris()	[it->id];
		Fvector*  v		= m_pModel->get_verts();
		src.clear		();	dst.clear	();
		src.push_back	(v[t.verts[0]]);
		src.push_back	(v[t.verts[1]]);
		src.push_back	(v[t.verts[2]]);
		sPoly* P =		clip.ClipPoly	(src,dst);
		if (0==P)		{ T.skip=next; continue; }

		// XForm and Rasterize
#ifdef DEBUG
		tris_in_frame_visible	++;
#endif
		u32		pixels			= 0;
		int		limit			= int(P->size())-1;
		for (int v_=1; v_<limit; v_++)	{
			m_xform.transform	(T.raster[0],(*P)[0]);
			m_xform.transform	(T.raster[1],(*P)[v_+0]);
			m_xform.transform	(T.raster[2],(*P)[v_+1]);
			pixels	+=			Raster.rasterize(&T);
		}
		if (0==pixels)	{ T.skip=next; continue; }
	}
}

void CHOM::Render(CFrustum& base)
{
	if (!bEnabled)
	{
		return;
	}

	Device.Statistic->RenderCALC_HOM.Begin();
	Raster.clear();
	Render_DB(base);
	Raster.propagade();
	MT_frame_rendered = Device.dwFrame;
	Device.Statistic->RenderCALC_HOM.End();
}

void CHOM::Disable()
{
	bEnabled = FALSE;
}

void CHOM::Enable()
{
	bEnabled = m_pModel != nullptr;
}

#ifdef DEBUG_DRAW
#include "dxDebugRender.h"

void CHOM::OnRender()
{
	Raster.on_dbg_render();

	if (psDeviceFlags.is(rsOcclusionDraw))
	{
		if (m_pModel)
		{
			xr_vector<u32> pairs;
			pairs.resize(m_pModel->get_tris_count() * 6);
			for (size_t i = 0; i < m_pModel->get_tris_count(); i++)
			{
				CDB::TRI* T = m_pModel->get_tris() + i;
				Fvector* verts = m_pModel->get_verts();
				pairs[(i * 6) + 0] = T->verts[0];
				pairs[(i * 6) + 1] = T->verts[1];
				pairs[(i * 6) + 2] = T->verts[1];
				pairs[(i * 6) + 3] = T->verts[2];
				pairs[(i * 6) + 4] = T->verts[2];
				pairs[(i * 6) + 5] = T->verts[0];
			}

			DebugRenderImpl.add_lines
			(
				m_pModel->get_verts(), m_pModel->get_verts_count(),
				pairs.data(), (u32)pairs.size() / 2, 0xFFFFFFFF
			);
		}
	}
}
#endif

#ifdef DEBUG
void CHOM::stats()
{
	if (m_pModel)
	{
		CGameFont& F = *Device.Statistic->Font();
		F.OutNext(" **** HOM-occ ****");
		F.OutNext("  visible:  %2d", tris_in_frame_visible);
		F.OutNext("  frustum:  %2d", tris_in_frame);
		F.OutNext("    total:  %2d", m_pModel->get_tris_count());
	}
}
#endif