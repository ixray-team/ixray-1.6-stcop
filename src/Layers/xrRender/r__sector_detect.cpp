#include "stdafx.h"
#include "FHierrarhyVisual.h" 

thread_local CDB::COLLIDER sectors_detect_xrc;

int CRender::translateSector(IRender_Sector* pSector)
{
	if (!pSector)
		return -1;

	for (u32 i=0; i<Sectors.size(); ++i)
	{
		if (Sectors[i]==pSector)
			return i;
	}

	FATAL			("Sector was not found!");
	NODEFAULT;

#ifdef DEBUG
	return			(-1);
#endif // #ifdef DEBUG
}

IRender_Sector* CRender::detectSector(const Fvector& P)
{
	IRender_Sector*	S = nullptr;
	Fvector dir; dir.set(0,-1,0);
	S = detectSector(P,dir);
	if (nullptr==S)
	{
		dir.set(0,1,0);
		S = detectSector(P,dir);
	}
	return S;
}

IRender_Sector* CRender::detectLastSector(const Fvector& P)
{
	if(SectorsCount()==1)
		return pOutdoorSector;

	auto detectSector = [&](const Fvector& P, Fvector& dir) -> IRender_Sector*
	{
		sectors_detect_xrc.ray_options(CDB::OPT_ONLYNEAREST);
		// Portals model
		if (rmPortals)	
		{
			sectors_detect_xrc.ray_query(rmPortals,P,dir,1000.f);
			if (sectors_detect_xrc.r_count())
			{
				auto& RP = sectors_detect_xrc.r_any();
				auto& pTri = RP.model->tris[RP.tris_id];
				CPortal* pPortal = (CPortal*) Portals[pTri.dummy];
				CSector* S = pPortal->getSectorFacing(P);
				FHierrarhyVisual* pV = (FHierrarhyVisual*)S->root();
				if(pV)
				{
					if(pV->vis.box.contains(P))
					{
						return S;
					}
				}
			}
		}

		// Geometry model
		sectors_detect_xrc.ray_query(g_pGameLevel->ObjectSpace.GetStaticModel(),P,dir,1000.f);
		if (sectors_detect_xrc.r_count())
		{
			auto& RP = sectors_detect_xrc.r_any();
			return getSector(RP.model->tris[RP.tris_id].sector);
		}

		return nullptr;
	};

	IRender_Sector*	S = nullptr;	
	Fvector dir; dir.set(0,-1,0);
	S = detectSector(P,dir);
	if (nullptr==S)		
	{
		dir.set(0,1,0);
		S = detectSector(P,dir);
	}
	return S;

}

IRender_Sector* CRender::detectSector(const Fvector& P, Fvector& dir)
{
	if(SectorsCount()==1)
		return pOutdoorSector;

	sectors_detect_xrc.ray_options(CDB::OPT_ONLYNEAREST);
	// Portals model
	CDB::RESULT Res1;
	Res1.model = nullptr;
	Res1.tris_id = size_t(-1);
	Res1.range = 500.0f;
	if (rmPortals)	
	{
		sectors_detect_xrc.ray_query(rmPortals,P,dir,Res1.range);
		if (sectors_detect_xrc.r_count())
		{
			Res1 = sectors_detect_xrc.r_any();
		}
	}

	// Geometry model
	CDB::RESULT Res2;
	Res2.model = nullptr;
	Res2.tris_id = size_t(-1);
	Res2.range = Res1.range;
	sectors_detect_xrc.ray_query(g_pGameLevel->ObjectSpace.GetStaticModel(),P,dir,Res2.range);
	if (sectors_detect_xrc.r_count())
	{
		Res2 = sectors_detect_xrc.r_any();
	}

	// Select ID
	CDB::RESULT* Res = nullptr;
	if (Res1.tris_id!=size_t(-1))
	{
		if (Res2.tris_id!=size_t(-1))
		{
			Res = (Res1.range<=Res2.range+EPS) ? &Res1 : &Res2;	// both was found
		}
		else
		{
			Res = &Res1; // only id1 found
		}
	} 
	else if (Res2.tris_id!=size_t(-1))
	{
		Res = &Res2; // only id2 found
	}
	else
	{
		return nullptr;
	}

	if (Res == &Res1)
	{
		// Take sector, facing to our point from portal
		CDB::TRI& pTri = rmPortals->get_tris()[Res->tris_id];
		CPortal* pPortal = (CPortal*)Portals[pTri.dummy];
		return pPortal->getSectorFacing(P);
	}
	else
	{
		// Take triangle at ID and use it's Sector
		return getSector(Res->Sector);
	}
}

void R_dsgraph_structure::detectSectors_sphere(CSector* sector, xr_vector<IRender_Sector*>& m_sectors, const Fsphere& sphere)
{
	m_sectors.clear();
	if(sector)
		m_sectors.push_back(sector);

	if (CDB::MODEL* portals_cform = RImplementation.rmPortals)
	{
		sectors_detect_xrc.box_options(CDB::OPT_FULL_TEST);
		float sphere_r = sphere.R;
		sectors_detect_xrc.box_query(portals_cform, sphere.P, { sphere_r, sphere_r, sphere_r });
		for (auto& elem : sectors_detect_xrc.r_vec())
		{
			CPortal* pPortal = (CPortal*)RImplementation.Portals[elem.model->get_tris()[elem.tris_id].dummy];

			if(!pPortal)
			{
				continue;
			}

			CSector *pFront = pPortal->Front();
			CSector *pBack = pPortal->Back();

			if(pFront)
			{
				m_sectors.push_back(pFront);
			}

			if(pBack)
			{
				m_sectors.push_back(pBack);
			}
			
		}
	}
}

void R_dsgraph_structure::detectSectors_frustum(CSector* sector, xr_vector<IRender_Sector*>& m_sectors, CFrustum* _frustum)
{
	m_sectors.clear();
	if(sector)
		m_sectors.push_back(sector);
	
	if (CDB::MODEL* portals_cform = RImplementation.rmPortals)
	{
		sectors_detect_xrc.frustum_options(CDB::OPT_FULL_TEST);
		sectors_detect_xrc.frustum_query(portals_cform, *_frustum);
		for (auto& elem : sectors_detect_xrc.r_vec())
		{
			CPortal* pPortal = (CPortal*)RImplementation.Portals[elem.model->get_tris()[elem.tris_id].dummy];

			if(!pPortal)
			{
				continue;
			}

			CSector *pFront = pPortal->Front();
			CSector *pBack = pPortal->Back();

			if(pFront)
			{
				m_sectors.push_back(pFront);
			}

			if(pBack)
			{
				m_sectors.push_back(pBack);
			}
			
		}
	}
}