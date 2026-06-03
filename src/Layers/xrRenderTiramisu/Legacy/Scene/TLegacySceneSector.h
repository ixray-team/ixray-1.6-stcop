#pragma once

class CDS0_RenderVisual;
class	TLegacyScenePortal;
class	TLegacySceneSector;

struct	_scissor					: public Fbox2
{
	float	depth;
};

// Connector
class	TLegacyScenePortal						: public IRender_Portal
#ifdef DEBUG
	, public pureRender
#endif
{
private:
	svector<Fvector,8>				poly;
	TLegacySceneSector							*pFace,*pBack;
public:
	Fplane							P;
	Fsphere							S;
	u32								marker;
	BOOL							bDualRender;

	void							Setup								(Fvector* V, size_t vcnt, TLegacySceneSector* face, TLegacySceneSector* back);

	svector<Fvector,8>&				getPoly()							{ return poly;		}
	TLegacySceneSector*				Back()								{ return pBack;		}
	TLegacySceneSector*				Front()								{ return pFace;		}
	TLegacySceneSector*				getSector		(TLegacySceneSector* pFrom)	{ return pFrom==pFace?pBack:pFace; }
	TLegacySceneSector*				getSectorFacing	(const Fvector& V)	{ if (P.classify(V)>0) return pFace; else return pBack; }
	TLegacySceneSector*				getSectorBack	(const Fvector& V)	{ if (P.classify(V)>0) return pBack; else return pFace;	}
	float							distance		(const Fvector &V)	{ return abs(P.classify(V)); }

									TLegacyScenePortal			();
	virtual							~TLegacyScenePortal			();

#ifdef DEBUG
	virtual void					OnRender		();
#endif
};

// Main 'Sector' class
class	 TLegacySceneSector					: public IRender_Sector
{
protected:
	CDS0_RenderVisual*				m_root;			// whole geometry of that sector
	xr_vector<TLegacyScenePortal*>	m_portals;
public:
	xr_vector<CFrustum>				r_frustums;
	xr_vector<_scissor>				r_scissors;
	_scissor						r_scissor_merged;
	u32								r_marker;
	TLegacyScene*					LegacyOwner = nullptr;
public:
	// Main interface
	CDS0_RenderVisual*					root			()				{ return m_root; }
	void							traverse		(CFrustum& F,	_scissor& R);
	void							load			(IReader& fs);

	TLegacySceneSector							()				{ m_root = NULL;	}
	virtual							~TLegacySceneSector		( );
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
	TLegacySceneSector*								i_start;		// input:	starting point
	xr_vector<TLegacySceneSector*>				r_sectors;		// result
	xr_vector<std::pair<TLegacyScenePortal*, float> >	f_portals;	
public:
									CPortalTraverser	();
	void							initialize			();
	void							destroy				();
	void							traverse			(TLegacySceneSector* start, CFrustum& F, Fvector& vBase, Fmatrix& mXFORM, u32 options);
	void							fade_portal			(TLegacyScenePortal* _p, float ssa);
	void							fade_render			();
#ifdef DEBUG
	void							dbg_draw		();
#endif
};

extern	CPortalTraverser			GPortalTraverser	;
