#include "stdafx.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/Environment.h"
#include "FVF.h"

CPortalTraverser::CPortalTraverser	()
{
	i_marker			=	0xffffffff;
}

#ifdef DEBUG
xr_vector<IRender_Sector*>				dbg_sectors;
#endif

void CPortalTraverser::prepare_local_clips(u32 sector_count, u32 portal_count)
{
	if (local_frustums.size() != sector_count)
	{
		local_frustums.assign(sector_count, {});
		local_sector_marker.assign(sector_count, 0xffffffff);
	}
	if (local_portals.size() != portal_count)
		local_portals.assign(portal_count, {0xffffffff, 0, 0xffffffff, false});
}

void CPortalTraverser::traverse			(IRender_Sector* start, CFrustum& F, Fvector& vBase, Fmatrix& mXFORM, u32 options)
{
	PROF_EVENT("CPortalTraverser::traverse")
	if (options & VQ_FADE)			{
		f_portals.clear		();
		f_portals.reserve	(16);
	}

	VERIFY				(start);
	i_marker			++;
	i_options			= options;
	i_vBase				= vBase;
	i_mXFORM			= mXFORM;
	i_start				= (CSector*)start;
	NearDistance		= ViewportNear + EPS_L;
	r_sectors.clear		();
	i_start->traverse(F, *this);
}

bool CPortalTraverser::IsNearPortal(const CPortal& Portal) const
{
	if (std::abs(Portal.P.classify(i_vBase)) > NearDistance)
	{
		return false;
	}
	return i_vBase.distance_to_sqr(Portal.S.P) <= _sqr(Portal.S.R + NearDistance);
}

void CPortalTraverser::fade_portal	(CPortal* _p, float ssa)
{
	// The portal may be reached by several traversal paths
	u32& FadeMarker = local_portals[_p->index].FadeMarker;
	if (FadeMarker == i_marker)
		return;
	FadeMarker = i_marker;

	if(RImplementation.HOM.visible(_p->S) || psGameFlags.test(rsDrawPortals))
		f_portals.push_back				(std::make_pair(_p,ssa));
}
void CPortalTraverser::initialize	()
{
	f_shader.create					("portal");
	f_geom.create					(EGeomSource::Ring);
}
void CPortalTraverser::destroy		()
{
	f_geom.destroy					();
	f_shader.destroy				();
}

extern float r_ssaDISCARD			;
extern float r_ssaLOD_A, r_ssaLOD_B ;
void CPortalTraverser::fade_render	(GfxCmdList& Cmd)
{
	if (f_portals.empty())
		return;

	u32 _offset = 0;
	// re-sort, back to front
	if (!psGameFlags.test(rsDrawPortals))
	{
		std::sort(f_portals.begin(), f_portals.end(),
			[this](const std::pair<CPortal*, float>& _1, const std::pair<CPortal*, float>& _2)
			{
				float d1 = i_vBase.distance_to_sqr(_1.first->S.P);
				float d2 = i_vBase.distance_to_sqr(_2.first->S.P);
				return d2 > d1; // descending, back to front
			}
		);
	}

	// calc poly-count
	u32 poly_per_portal = 0;
	for (auto& pair : f_portals)
		poly_per_portal += pair.first->getPoly().size()-2;
	struct LF
	{
		FVF::L buff[3];
	};
	
	LF* _v = (LF*)Cmd.Vertex.Lock(poly_per_portal*3, EVertexFormat::L, _offset);
	float ssaRange = r_ssaLOD_A - r_ssaLOD_B;
	Fvector _ambient_f = g_pGamePersistent->Environment().CurrentEnv->ambient;
	u32 _ambient = psGameFlags.test(rsDrawPortals) ? u32(-1) : color_rgba_f(_ambient_f.x,_ambient_f.y,_ambient_f.z,0.f);
	for (std::pair<CPortal*, float>& fp : f_portals)
	{
		u32 _clr = u32(-1);
		if(psGameFlags.test(rsDrawPortals))
			_clr = color_rgba(0,255,100,255);
		else
			_clr = subst_alpha(_ambient,u32(clampr(iFloor((1-fp.second-r_ssaLOD_B/ssaRange)*255.5f),0,255)));

		// fill polys
		CPortal* _P = fp.first;
		u32 _polys = _P->getPoly().size()-2;
		for (u32 _pit = 0; _pit < _polys; _pit++)
		{
			*_v =
			{
				FVF::L{_P->getPoly()[0], _clr},
				FVF::L{_P->getPoly()[_pit + 1],_clr},
				FVF::L{_P->getPoly()[_pit + 2],_clr},
			};
			_v++;
		}
	}
	Cmd.Vertex.Unlock(poly_per_portal*3);

	// render
	Cmd.SetObjectTransform(Fidentity);
	Cmd.SetPass(f_shader);
	Cmd.SetGeometry(f_geom);

	Cmd.SetCullMode(ERHI_CULLMODE::NONE);
	Cmd.Draw(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, poly_per_portal * 3, _offset);
	Cmd.SetCullMode(ERHI_CULLMODE::BACK);

	// cleanup
	f_portals.clear();
}