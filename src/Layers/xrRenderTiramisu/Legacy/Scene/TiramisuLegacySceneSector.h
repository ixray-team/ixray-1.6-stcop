#pragma once

#include "TiramisuRenderTypes.h"

class CDS0_RenderVisual;
class	TiramisuLegacyScenePortal;
class	TiramisuLegacySceneSector;

struct	_scissor					: public Fbox2
{
	float	depth;
};

// Connector
class	TiramisuLegacyScenePortal						: public IRender_Portal
#ifdef DEBUG
	, public pureRender
#endif
{
private:
	svector<Fvector,8>				poly;
	TiramisuLegacySceneSector							*pFace,*pBack;
public:
	Fplane							P;
	Fsphere							S;
	u32								marker;
	BOOL							bDualRender;

	void							Setup								(Fvector* V, size_t vcnt, TiramisuLegacySceneSector* face, TiramisuLegacySceneSector* back);

	svector<Fvector,8>&				getPoly()							{ return poly;		}
	TiramisuLegacySceneSector*				Back()								{ return pBack;		}
	TiramisuLegacySceneSector*				Front()								{ return pFace;		}
	TiramisuLegacySceneSector*				getSector		(TiramisuLegacySceneSector* pFrom)	{ return pFrom==pFace?pBack:pFace; }
	TiramisuLegacySceneSector*				getSectorFacing	(const Fvector& V)	{ if (P.classify(V)>0) return pFace; else return pBack; }
	TiramisuLegacySceneSector*				getSectorBack	(const Fvector& V)	{ if (P.classify(V)>0) return pBack; else return pFace;	}
	float							distance		(const Fvector &V)	{ return abs(P.classify(V)); }

									TiramisuLegacyScenePortal			();
	virtual							~TiramisuLegacyScenePortal			();

#ifdef DEBUG
	virtual void					OnRender		();
#endif
};

// Main 'Sector' class
class	 TiramisuLegacySceneSector					: public IRender_Sector
{
protected:
	CDS0_RenderVisual*				m_root;			// whole geometry of that sector
	xr_vector<TiramisuLegacyScenePortal*>	m_portals;
public:
	xr_vector<CFrustum>				r_frustums;
	xr_vector<_scissor>				r_scissors;
	_scissor						r_scissor_merged;
	u32								r_marker;
	TiramisuLegacyScene*					LegacyOwner = nullptr;
public:
	// Main interface
	CDS0_RenderVisual*					root			()				{ return m_root; }
	void							traverse		(CFrustum& F,	_scissor& R);
	void							load			(IReader& fs);

	TiramisuLegacySceneSector							()				{ m_root = NULL;	}
	virtual							~TiramisuLegacySceneSector		( );
};

class	CPortalTraverser
{
public:
	enum
	{
		VQ_HOM		= (1<<0),
		VQ_SSA		= (1<<1),
		VQ_SCISSOR	= (1<<2),
		VQ_FADE		= (1<<3),				// requires SSA to work
	};
public:
	u32										i_marker;		// input
	u32										i_options;		// input:	culling options
	Fvector									i_vBase;		// input:	"view" point
	Fmatrix									i_mXFORM;		// input:	4x4 xform
	Fmatrix									i_mXFORM_01;	// 
	TiramisuLegacySceneSector*								i_start;		// input:	starting point
	xr_vector<TiramisuLegacySceneSector*>				r_sectors;		// result
	xr_vector<xr_pair<TiramisuLegacyScenePortal*, float> >	f_portals;	
public:
									CPortalTraverser	();
	void							initialize			();
	void							destroy				();
	void							traverse			(TiramisuLegacySceneSector* start, CFrustum& F, Fvector& vBase, Fmatrix& mXFORM, u32 options);
	void							fade_portal			(TiramisuLegacyScenePortal* _p, float ssa);
	void							fade_render			();
#ifdef DEBUG
	void							dbg_draw		();
#endif
};

extern	CPortalTraverser			GPortalTraverser	;
