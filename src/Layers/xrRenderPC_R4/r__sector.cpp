// Portal.cpp: implementation of the CPortal class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "r__sector.h"
#include "../../xrEngine/xrLevel.h"
#include "../../xrEngine/xr_object.h"
#include "FBasicVisual.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "dxRenderDeviceRender.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
CPortal::CPortal		()
{
#ifdef DEBUG
	Device.seqRender.Add(this,REG_PRIORITY_LOW-1000);
#endif
}

CPortal::~CPortal		()
{
#ifdef DEBUG
	if (DevicePtr)
		Device.seqRender.Remove(this);
#endif
}

#ifdef DEBUG_DRAW
#include "dxDebugRender.h"

void CPortal::OnRender	()
{

}
#endif
//
void CPortal::Setup(Fvector* V, int vcnt, CSector* face, CSector* back)
{
	// calc sphere
	Fbox BB;
	BB.invalidate();
	for (int v = 0; v < vcnt; v++)
		BB.modify(V[v]);
	BB.getsphere(S.P, S.R);

	// 
	poly.assign(V, vcnt);
	pFace = face;
	pBack = back;

	Fvector N, T;

	u32	_cnt = 0;
	for (int i = 2; i < vcnt; i++)
	{
		T.mknormal_non_normalized(poly[0], poly[i - 1], poly[i]);
		float m = T.magnitude();
		if (m > EPS_S)
		{
			N.add(T.div(m));
			_cnt++;
		}
	}

	R_ASSERT2(_cnt, "Invalid portal detected");
	N.div(float(_cnt));
	P.build(poly[0], N);
}

//
CSector::~CSector()
{
}

//
extern float r_ssaDISCARD			;
extern float r_ssaLOD_A, r_ssaLOD_B ;

void CSector::traverse(CFrustum& F, CPortalTraverser& PortalTraverser)
{
	// Register traversal process
	u32& SectorMarker = PortalTraverser.local_sector_marker[index];
	xr_vector<CFrustum>& Frustums = PortalTraverser.local_frustums[index];
	if (SectorMarker != PortalTraverser.i_marker)
	{
		SectorMarker = PortalTraverser.i_marker;
		PortalTraverser.r_sectors.push_back(this);
		Frustums.clear();
	}
	Frustums.push_back(F);

	// Search visible portals and go through them
	sPoly	S,D;
	for	(u32 I=0; I<m_portals.size(); I++)
	{
		CPortal* PORTAL = m_portals[I];

		// A portal may be crossed through several openings of one traversal: a sector reached by
		// two paths needs both frustums. It is never crossed twice on one path, and the visit
		// limit keeps traversal of dense portal graphs bounded.
		CPortalTraverser::PortalState& State = PortalTraverser.local_portals[PORTAL->index];
		u32& portal_marker = State.Marker;
		u32& Visits = State.Visits;
		bool& OnPath = State.OnPath;
		if (portal_marker != PortalTraverser.i_marker)
		{
			portal_marker	= PortalTraverser.i_marker;
			Visits			= 0;
			OnPath			= false;
		}
		if (OnPath || Visits >= CPortalTraverser::MaxPortalVisits) continue;

		// A frustum built from a portal the view point lies on is degenerate, so such portals
		// lead in both directions and keep the current frustum
		const bool NearPortal = PortalTraverser.IsNearPortal(*PORTAL);

		// Select sector (allow intersecting portals to be finely classified)
		CSector* pSector;
		if (NearPortal) {
			pSector = PORTAL->getSector						(this);
		} else {
			pSector = PORTAL->getSectorBack					(PortalTraverser.i_vBase);
		}
		if (pSector==this)								continue;
		if (pSector==nullptr)							continue;
		if (pSector==PortalTraverser.i_start)			continue;	// already has the whole view frustum

		// Early-out sphere
		if (!F.testSphere_dirty(PORTAL->S.P,PORTAL->S.R))	continue;

		if (NearPortal)
		{
			Visits++;
			OnPath				= true;
			pSector->traverse	(F,PortalTraverser);
			OnPath				= false;
			continue;
		}

		if(psGameFlags.test(rsDrawPortals))
			PortalTraverser.fade_portal			(PORTAL,1.f);
		else
		{
			// SSA	(if required)
			if (PortalTraverser.i_options&CPortalTraverser::VQ_SSA)
			{
				Fvector				dir2portal;
				dir2portal.sub		(PORTAL->S.P,	PortalTraverser.i_vBase);
				float R				=	PORTAL->S.R	;
				float distSQ		=	dir2portal.square_magnitude();
				float ssa			=	R*R/distSQ;
				dir2portal.div		(_sqrt(distSQ));
				ssa					*= std::abs(PORTAL->P.n.dotproduct(dir2portal));
				if (ssa<r_ssaDISCARD)	continue;

				if (PortalTraverser.i_options&CPortalTraverser::VQ_FADE)	{
					if (ssa<r_ssaLOD_A)	PortalTraverser.fade_portal			(PORTAL,ssa);
					if (ssa<r_ssaLOD_B)	continue							;
				}
			}
		}

		// Clip by frustum
		FixedVector<Fvector,8>&	POLY = PORTAL->getPoly();
		S.assign			(&*POLY.begin(),POLY.size()); D.clear();
		sPoly* P			= F.ClipPoly(S,D);
		if (0==P)			continue;

		// Cull by HOM (slower algo)
		if ((PortalTraverser.i_options&CPortalTraverser::VQ_HOM) && !RImplementation.HOM.visible(*P))
		{
			continue;
		}

		// Create _new_ frustum and recurse
		CFrustum				Clip;
		Clip.CreateFromPortal	(P, PORTAL->P.n, PortalTraverser.i_vBase,PortalTraverser.i_mXFORM);
		Visits++;
		OnPath					= true;
		pSector->traverse		(Clip,PortalTraverser);
		OnPath					= false;
	}
}

void CSector::load		(IReader& fs)
{
	// Assign portal polygons
	u32 size			= fs.find_chunk(fsP_Portals); R_ASSERT(0==(size&1));
	u32 count			= size/2;
	m_portals.reserve	(count);
	while (count) {
		u16 ID		= fs.r_u16();
		CPortal* P	= (CPortal*)RImplementation.getPortal	(ID);
		m_portals.push_back(P);
		count--;
	}

	if	(g_dedicated_server)	m_root	= 0;
	else {
		// Assign visual
		size	= fs.find_chunk(fsP_Root);	R_ASSERT(size==4);
		m_root	= (dxRender_Visual*)RImplementation.getVisual	(fs.r_u32());
	}
}
