// Portal.h: interface for the CPortal class.
//
//////////////////////////////////////////////////////////////////////
#if !defined(_PORTAL_H_)
#define _PORTAL_H_
#pragma once
#include "r__dsgraph_types.h"
class CPortal;
class CSector;
// Connector
class CPortal : public IRender_Portal
{
private:
	svector<Fvector,8>				poly;
	CSector							*pFace,*pBack;
public:
	Fplane							P;
	Fsphere							S;
	Fbox							BB;
	void							Setup								(Fvector* V, int vcnt, CSector* face, CSector* back);

	svector<Fvector,8>&				getPoly()							{ return poly;		}
	CSector*						Back()								{ return pBack;		}
	CSector*						Front()								{ return pFace;		}
	CSector*						getSector		(CSector* pFrom)	{ return pFrom==pFace?pBack:pFace; }
	CSector*						getSectorFacing	(const Fvector& V)	{ if (P.classify(V)>0) return pFace; else return pBack; }
	CSector*						getSectorBack	(const Fvector& V)	{ if (P.classify(V)>0) return pBack; else return pFace;	}
	float							distance		(const Fvector &V)	{ return _abs(P.classify(V)); }
};

class dxRender_Visual;
class CDSGraphManager;
// Main 'Sector' class
class CSector : public IRender_Sector
{
protected:
	dxRender_Visual*				m_root;			// whole geometry of that sector
	xr_vector<CPortal*>				m_portals;
public:
	// Main interface
	dxRender_Visual*				root			()				{ return m_root; }
	void							traverse		(CFrustum& F, CDSGraphManager& DM);
	void							load			(IReader& fs);

	CSector							()				{ m_root = NULL;	}
	virtual							~CSector		( );
};

#endif // !defined(AFX_PORTAL_H__1FC2D371_4A19_49EA_BD1E_2D0F8DEBBF15__INCLUDED_)