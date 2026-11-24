// Portal.h: interface for the CPortal class.
//
//////////////////////////////////////////////////////////////////////
#pragma once

#include "r__portal_traverser.h"

class CPortal;
class CSector;

// Connector
class CPortal :
	public IRender_Portal
#ifdef DEBUG_DRAW
	, public pureRender
#endif
{
private:
	FixedVector<Fvector,8>				poly;
	CSector							*pFace,*pBack;
public:
	Fplane							P;
	Fsphere							S;
	u32								index = 0;

	void							Setup								(Fvector* V, int vcnt, CSector* face, CSector* back);

	FixedVector<Fvector,8>&				getPoly()							{ return poly;		}
	CSector*						Back()								{ return pBack;		}
	CSector*						Front()								{ return pFace;		}
	CSector*						getSector		(CSector* pFrom)	{ return pFrom==pFace?pBack:pFace; }
	CSector*						getSectorFacing	(const Fvector& V)	{ if (P.classify(V)>0) return pFace; else return pBack; }
	CSector*						getSectorBack	(const Fvector& V)	{ if (P.classify(V)>0) return pBack; else return pFace;	}
	float							distance		(const Fvector &V)	{ return std::abs(P.classify(V)); }

									CPortal			();
	virtual ~CPortal		() override;

#ifdef DEBUG_DRAW
	virtual void					OnRender		() override;
#endif
};

class dxRender_Visual;


// Main 'Sector' class
class CSector :
	public IRender_Sector
{
protected:
	dxRender_Visual* m_root; // whole geometry of that sector
	xr_vector<CPortal*> m_portals;

public:
	u32 index = 0;

public:
	// Main interface
	dxRender_Visual* root() { return m_root; }
	const xr_vector<CPortal*>& portals() const { return m_portals; }
	void traverse(CFrustum& F, CPortalTraverser&);
	void load(IReader& fs);

	CSector() { m_root = NULL; }
	virtual ~CSector() override;
};
