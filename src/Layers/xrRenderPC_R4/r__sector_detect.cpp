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
				CDB::RESULT *RP = sectors_detect_xrc.r_begin();
				CDB::TRI& pTri = rmPortals->get_tris()[RP->id];
				CPortal* pPortal = (CPortal*) Portals[pTri.dummy];
				CSector* S = pPortal->getSectorFacing(P);
				FHierrarhyVisual* pV = (FHierrarhyVisual*)S->root();
				if(pV)
				{
					if(pV->vis.box.contains(P))
						return S;
				}
			}
		}

		// Geometry model
		sectors_detect_xrc.ray_query(g_pGameLevel->ObjectSpace.GetStaticModel(),P,dir,1000.f);
		if (sectors_detect_xrc.r_count())
		{
			CDB::RESULT *RP = sectors_detect_xrc.r_begin();
			return getSector(RP->sector);
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
	int id1 = -1;
	float range1 = 500.f;
	if (rmPortals)	
	{
		sectors_detect_xrc.ray_query(rmPortals,P,dir,range1);
		if (sectors_detect_xrc.r_count())
		{
			CDB::RESULT *RP1 = sectors_detect_xrc.r_begin();
			id1 = RP1->id; range1 = RP1->range; 
		}
	}

	// Geometry model
	int id2 = -1;
	float range2 = range1;
	sectors_detect_xrc.ray_query(g_pGameLevel->ObjectSpace.GetStaticModel(),P,dir,range2);
	if (sectors_detect_xrc.r_count())
	{
		CDB::RESULT *RP2 = sectors_detect_xrc.r_begin();
		id2 = RP2->id; range2 = RP2->range;
	}

	// Select ID
	int ID;
	if (id1>=0)
	{
		if (id2>=0) ID = (range1<=range2+EPS)?id1:id2;	// both was found
		else ID = id1;									// only id1 found
	} else if (id2>=0) ID = id2;						// only id2 found
	else return 0;

	if (ID==id1)
	{
		// Take sector, facing to our point from portal
		CDB::TRI& pTri = rmPortals->get_tris()[ID];
		CPortal* pPortal = (CPortal*)Portals[pTri.dummy];
		return pPortal->getSectorFacing(P);
	}
	else
	{
		// Take triangle at ID and use it's Sector
		CDB::TRI& pTri = g_pGameLevel->ObjectSpace.GetStaticTris()[ID];
		return getSector(pTri.sector);
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
		for (int K=0; K< sectors_detect_xrc.r_count(); K++)
		{
			CPortal* pPortal = (CPortal*)RImplementation.Portals[portals_cform->get_tris()[sectors_detect_xrc.r_begin()[K].id].dummy];

			if(!pPortal)
				continue;

			CSector *pFront = pPortal->Front();
			CSector *pBack = pPortal->Back();

			if(pFront)
				m_sectors.push_back(pFront);

			if(pBack)
				m_sectors.push_back(pBack);
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
		for (int K=0; K< sectors_detect_xrc.r_count(); K++)
		{
			CPortal* pPortal = (CPortal*)RImplementation.Portals[portals_cform->get_tris()[sectors_detect_xrc.r_begin()[K].id].dummy];

			if(!pPortal)
				continue;

			CSector *pFront = pPortal->Front();
			CSector *pBack = pPortal->Back();

			if(pFront)
				m_sectors.push_back(pFront);

			if(pBack)
				m_sectors.push_back(pBack);
		}
	}
}