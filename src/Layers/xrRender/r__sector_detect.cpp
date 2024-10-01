#include "stdafx.h"
#if (RENDER==R_R4)
#include "r4.h"
#endif
#if (RENDER==R_R2)
#include "r2.h"
#endif
#if (RENDER==R_R1)
#include "FStaticRender.h"
#endif

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
	IRender_Sector*	S	= nullptr;	
	Fvector			dir; 

	dir.set				(0,-1,0);
	S					= detectSector(P,dir);
	if (nullptr==S)		
	{
		dir.set				(0,1,0);
		S					= detectSector(P,dir);
	}
	return S;
}

IRender_Sector* CRender::detectSector(const Fvector& P, Fvector& dir)
{
	if(SectorsCount()==1)
		return pOutdoorSector;

	Sectors_xrc.ray_options		(CDB::OPT_ONLYNEAREST);
	// Portals model
	int		id1		= -1;
	float	range1	= 500.f;
	if (rmPortals)	
	{
		Sectors_xrc.ray_query	(rmPortals,P,dir,range1);
		if (Sectors_xrc.r_count()) {
			CDB::RESULT *RP1 = Sectors_xrc.r_begin();
			id1 = RP1->id; range1 = RP1->range; 
		}
	}

	// Geometry model
	int		id2		= -1;
	float	range2	= range1;
	Sectors_xrc.ray_query	(g_pGameLevel->ObjectSpace.GetStaticModel(),P,dir,range2);
	if (Sectors_xrc.r_count()) {
		CDB::RESULT *RP2 = Sectors_xrc.r_begin();
		id2 = RP2->id; range2 = RP2->range;
	}

	// Select ID
	int ID;
	if (id1>=0) {
		if (id2>=0) ID = (range1<=range2+EPS)?id1:id2;	// both was found
		else ID = id1;									// only id1 found
	} else if (id2>=0) ID = id2;						// only id2 found
	else return 0;

	if (ID==id1) {
		// Take sector, facing to our point from portal
		CDB::TRI*	pTri	= rmPortals->get_tris() + ID;
		CPortal*	pPortal	= (CPortal*) Portals[pTri->dummy];
		return pPortal->getSectorFacing(P);
	} else {
		// Take triangle at ID and use it's Sector
		CDB::TRI*	pTri	= g_pGameLevel->ObjectSpace.GetStaticTris()+ID;
		return getSector(pTri->sector);
	}
}

xr_vector<IRender_Sector*> CRender::detectSectors_sphere(CSector* sector, const Fvector& b_center, const Fvector& b_dim)
{
	xr_vector<IRender_Sector*> m_sectors;
	m_sectors.push_back(sector);
	if (rmPortals)
	{
		Sectors_xrc.box_options(CDB::OPT_FULL_TEST);
		Sectors_xrc.box_query(rmPortals,b_center,b_dim);
		for (int K=0; K<Sectors_xrc.r_count(); K++)
		{
			CPortal* pPortal = (CPortal*) Portals[rmPortals->get_tris()[Sectors_xrc.r_begin()[K].id].dummy];

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
	return m_sectors;
}

xr_vector<IRender_Sector*> CRender::detectSectors_frustum(CSector* sector, CFrustum* _frustum)
{
	xr_vector<IRender_Sector*> m_sectors;
	m_sectors.push_back(sector);
	if (rmPortals)
	{
		Sectors_xrc.frustum_options(CDB::OPT_FULL_TEST);
		Sectors_xrc.frustum_query(rmPortals,*_frustum);
		for (int K=0; K<Sectors_xrc.r_count(); K++)
		{
			CPortal* pPortal = (CPortal*) Portals[rmPortals->get_tris()[Sectors_xrc.r_begin()[K].id].dummy];

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
	return m_sectors;
}