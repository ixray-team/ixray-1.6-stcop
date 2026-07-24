#include "TiramisuLegacySceneSector.h"
#include "TiramisuLegacyScene.h"
#include "../../xrEngine/xrLevel.h"

TiramisuLegacyScenePortal::TiramisuLegacyScenePortal		()
{
#ifdef DEBUG
	DevicePtr->seqRender.Add(this,REG_PRIORITY_LOW-1000);
#endif
}

TiramisuLegacyScenePortal::~TiramisuLegacyScenePortal		()
{
#ifdef DEBUG
	DevicePtr->seqRender.Remove(this);
#endif
}

#ifdef DEBUG
void TiramisuLegacyScenePortal::OnRender	()
{
/*	if (psDeviceFlags.is(rsOcclusionDraw)){
		VERIFY				(poly.size());
		// draw rect
		DEFINE_VECTOR		(FVF::L,LVec,LVecIt);
		static LVec	V;		V.resize(poly.size()+2);
		Fvector C			= {0,0,0};
		for (u32 k=0; k<poly.size(); k++){ C.add(poly[k]); V[k+1].set(poly[k],0x800000FF);}
		V.back().set		(poly[0],0x800000FF);
		C.div				((float)poly.size());
		V[0].set			(C,0x800000FF);

		RCache.set_xform_world(Fidentity);
		// draw solid
		RCache.set_Shader	(dxRenderDeviceRender::Instance().m_SelectionShader);
		RCache.dbg_Draw		(D3DPT_TRIANGLEFAN,&*V.begin(),V.size()-2);

		// draw wire
		if (bDebug){
			RImplementation.rmNear();
		}else{
			Device->SetNearer(TRUE);
		}
		RCache.set_Shader	(dxRenderDeviceRender::Instance().m_WireShader);
		RCache.dbg_Draw		(D3DPT_LINESTRIP,&*(V.begin()+1),V.size()-2);
		if (bDebug){
			RImplementation.rmNormal();
		}else{
			Device->SetNearer(FALSE);
		}
	}*/
}
#endif
//
void	TiramisuLegacyScenePortal::Setup	(Fvector* V, size_t vcnt, TiramisuLegacySceneSector* face, TiramisuLegacySceneSector* back)
{
	// calc sphere
	Fbox				BB;
	BB.invalidate		();
	for (int v=0; v<vcnt; v++)
		BB.modify		(V[v]);
	BB.getsphere		(S.P,S.R);

	// 
	poly.assign			(V,vcnt);
	pFace				= face; 
	pBack				= back;
	marker				= 0xffffffff; 

	Fvector				N,T;
	N.set				(0,0,0);

	//FPU::m64r();
	size_t	_cnt			= 0;
	for (int i=2; i<vcnt; i++) {
		T.mknormal_non_normalized		(poly[0],poly[i-1],poly[i]);
		float		m	= T.magnitude	();
		if (m>EPS_S)	{
			N.add		(T.div(m))	;
			_cnt		++			;
		}
	}
	R_ASSERT2	(_cnt, "Invalid portal detected");
	N.div		(float(_cnt));
	P.build		(poly[0],N);
	//FPU::m24r	();

	/*
	if (abs(1-P.n.magnitude())<EPS)
	Debug.fatal		(DEBUG_INFO,"Degenerated portal found at {%3.2f,%3.2f,%3.2f}.",VPUSH(poly[0]));
	*/
}

//
TiramisuLegacySceneSector::~TiramisuLegacySceneSector()
{
}


void TiramisuLegacySceneSector::traverse			(CFrustum &F, _scissor& R_scissor)
{
	// Register traversal process
	if (r_marker	!=	GPortalTraverser.i_marker)	{
		r_marker							=	GPortalTraverser.i_marker;
		GPortalTraverser.r_sectors.push_back	(this);
		r_frustums.clear					();
		r_scissors.clear					();
	}
	r_frustums.push_back		(F);
	r_scissors.push_back		(R_scissor);

	// Search visible portals and go through them
	sPoly	S,D;
	for	(u32 I=0; I<m_portals.size(); I++)
	{
		if (m_portals[I]->marker == GPortalTraverser.i_marker) continue;

		TiramisuLegacyScenePortal* PORTAL = m_portals[I];
		TiramisuLegacySceneSector* pSector;

		// Select sector (allow intersecting portals to be finely classified)
		if (PORTAL->bDualRender) {
			pSector = PORTAL->getSector						(this);
		} else {
			pSector = PORTAL->getSectorBack					(GPortalTraverser.i_vBase);
			if (pSector==this)								continue;
			if (pSector==GPortalTraverser.i_start)			continue;
		}

		// Early-out sphere
		if (!F.testSphere_dirty(PORTAL->S.P,PORTAL->S.R))	continue;

		// SSA	(if required)
		if (GPortalTraverser.i_options&CPortalTraverser::VQ_SSA)
		{
			Fvector				dir2portal;
			dir2portal.sub		(PORTAL->S.P,	GPortalTraverser.i_vBase);
			float R				=	PORTAL->S.R	;
			float distSQ		=	dir2portal.square_magnitude();
			float ssa			=	R*R/distSQ;
			dir2portal.div		(sqrt(distSQ));
			ssa					*=	abs(PORTAL->P.n.dotproduct(dir2portal));
			if (ssa < LegacyOwner->SsaDiscardThreshold)	continue;

			if (GPortalTraverser.i_options&CPortalTraverser::VQ_FADE)	{
				if (ssa < LegacyOwner->PortalFadeSsaStartThreshold)	GPortalTraverser.fade_portal			(PORTAL,ssa);
				if (ssa < LegacyOwner->PortalFadeSsaEndThreshold)	continue							;
			}
		}

		// Clip by frustum
		svector<Fvector,8>&	POLY = PORTAL->getPoly();
		S.assign			(&*POLY.begin(),POLY.size()); D.clear();
		sPoly* P			= F.ClipPoly(S,D);
		if (0==P)			continue;

		// Scissor and optimized HOM-testing
		_scissor			scissor	;
		if (GPortalTraverser.i_options&CPortalTraverser::VQ_SCISSOR && (!PORTAL->bDualRender))
		{
			// Build scissor rectangle in projection-space
			Fbox2	bb;	bb.invalidate(); float depth = flt_max;
			sPoly&	p	= *P;
			for		(u32 vit=0; vit<p.size(); vit++)	{
				Fvector4	t;	
				Fmatrix&	M	= GPortalTraverser.i_mXFORM_01;
				Fvector&	v	= p[vit];

				t.x = v.x*M._11 + v.y*M._21 + v.z*M._31 + M._41;
				t.y = v.x*M._12 + v.y*M._22 + v.z*M._32 + M._42;
				t.z = v.x*M._13 + v.y*M._23 + v.z*M._33 + M._43;
				t.w = v.x*M._14 + v.y*M._24 + v.z*M._34 + M._44;
				t.mul	(1.f/t.w);

				if (t.x < bb.min.x)	bb.min.x	= t.x; 
				if (t.x > bb.max.x) bb.max.x	= t.x;
				if (t.y < bb.min.y)	bb.min.y	= t.y; 
				if (t.y > bb.max.y) bb.max.y	= t.y;
				if (t.z < depth)	depth		= t.z;
			}
			// Msg	("bb(%s): (%f,%f)-(%f,%f), d=%f", PORTAL->bDualRender?"true":"false",bb.min.x, bb.min.y, bb.max.x, bb.max.y,depth);
			if (depth<EPS)	{
				scissor	= R_scissor;

				// Cull by HOM (slower algo)
				if  (
					(GPortalTraverser.i_options&CPortalTraverser::VQ_HOM) && 
					(/*!RImplementation.HOM.visible(*P)*/false)
					)	continue;
			} else {
				// perform intersection (this is just to be sure, it is probably clipped in 3D already)
				if (bb.min.x > R_scissor.min.x)	scissor.min.x = bb.min.x; else scissor.min.x = R_scissor.min.x;
				if (bb.min.y > R_scissor.min.y)	scissor.min.y = bb.min.y; else scissor.min.y = R_scissor.min.y;
				if (bb.max.x < R_scissor.max.x) scissor.max.x = bb.max.x; else scissor.max.x = R_scissor.max.x;
				if (bb.max.y < R_scissor.max.y) scissor.max.y = bb.max.y; else scissor.max.y = R_scissor.max.y;
				scissor.depth	= depth;

				// Msg	("scissor: (%f,%f)-(%f,%f)", scissor.min.x, scissor.min.y, scissor.max.x, scissor.max.y);
				// Check if box is non-empty
				if (scissor.min.x >= scissor.max.x)	continue;
				if (scissor.min.y >= scissor.max.y)	continue;

				// Cull by HOM (faster algo)
				if  (
					(GPortalTraverser.i_options&CPortalTraverser::VQ_HOM) && 
					(/*!RImplementation.HOM.visible(scissor,depth)*/false)
					)	continue;
			}
		} else {
			scissor	= R_scissor;

			// Cull by HOM (slower algo)
			if  (
				(GPortalTraverser.i_options&CPortalTraverser::VQ_HOM) && 
				(/**!RImplementation.HOM.visible(*P)*/false)
				)	continue;
		}

		// Create _new_ frustum and recurse
		CFrustum				Clip;
		Clip.CreateFromPortal	(P, PORTAL->P.n, GPortalTraverser.i_vBase,GPortalTraverser.i_mXFORM);
		PORTAL->marker			= GPortalTraverser.i_marker;
		PORTAL->bDualRender		= FALSE;
		pSector->traverse		(Clip,scissor);
	}
}

void TiramisuLegacySceneSector::load(IReader& fs)
{
	// Assign portal polygons
	size_t size = fs.find_chunk(fsP_Portals); R_ASSERT(0 == (size & 1));
	size_t count = size / 2;
	m_portals.reserve(count);
	while (count) 
	{
		u16 ID = fs.r_u16();
		TiramisuLegacyScenePortal* P = (TiramisuLegacyScenePortal*)LegacyOwner->GetPortal(ID);
		m_portals.push_back(P);
		count--;
	}
	// Assign visual
	size = fs.find_chunk(fsP_Root);	R_ASSERT(size == 4);
	m_root = LegacyOwner->GetVisual(fs.r_u32());

}

CPortalTraverser	GPortalTraverser;

CPortalTraverser::CPortalTraverser	()
{
	i_marker			=	0xffffffff;
}

#ifdef DEBUG
xr_vector<IRender_Sector*>				dbg_sectors;
#endif

void CPortalTraverser::traverse			(TiramisuLegacySceneSector* start, CFrustum& F, Fvector& vBase, Fmatrix& mXFORM, u32 options)
{
	Fmatrix			m_viewport_01	= {
		1.f/2.f,			0.0f,				0.0f,		0.0f,
		0.0f,				-1.f/2.f,			0.0f,		0.0f,
		0.0f,				0.0f,				1.0f,		0.0f,
		1.f/2.f + 0 + 0,	1.f/2.f + 0 + 0,	0.0f,		1.0f
	};

	if (options & VQ_FADE)			{
		f_portals.clear		();
		f_portals.reserve	(16);
	}

	VERIFY				(start);
	i_marker			++;
	i_options			= options;
	i_vBase				= vBase;
	i_mXFORM			= mXFORM;
	i_mXFORM_01.mul		(m_viewport_01,mXFORM);
	i_start				= (TiramisuLegacySceneSector*)start;
	r_sectors.clear		();
	_scissor			scissor;
	scissor.set			(0,0,1,1);
	scissor.depth		= 0;
	i_start->traverse	(F,scissor);

	if (options & VQ_SCISSOR)		{
		// dbg_sectors					= r_sectors;
		// merge scissor info
		for (u32 s=0; s<r_sectors.size(); s++)
		{
			TiramisuLegacySceneSector*	S		= (TiramisuLegacySceneSector*)r_sectors[s];
			S->r_scissor_merged.invalidate	();
			S->r_scissor_merged.depth		= flt_max;
			for (u32 it=0; it<S->r_scissors.size(); it++)
			{
				S->r_scissor_merged.merge(S->r_scissors[it]);
				if (S->r_scissors[it].depth < S->r_scissor_merged.depth)
					S->r_scissor_merged.depth = S->r_scissors[it].depth;
			}
		}
	}
}

void CPortalTraverser::fade_portal	(TiramisuLegacyScenePortal* _p, float ssa)
{
	f_portals.push_back(std::make_pair (_p, ssa));
}
void CPortalTraverser::initialize	()
{
	/*f_shader.create					("portal");
	f_geom.create					(FVF::F_L, RCache.Vertex.Buffer(), 0);*/
}
void CPortalTraverser::destroy		()
{
	/*f_geom.destroy					();
	f_shader.destroy				();*/
}
ICF		bool	psort_pred			(const xr_pair<TiramisuLegacyScenePortal*, float>& _1, const xr_pair<TiramisuLegacyScenePortal*, float>& _2)
{
	float		d1		= GPortalTraverser.i_vBase.distance_to_sqr(_1.first->S.P);
	float		d2		= GPortalTraverser.i_vBase.distance_to_sqr(_2.first->S.P);
	return		d2>d1;	// descending, back to front
}
void CPortalTraverser::fade_render	()
{
	if (f_portals.empty())			return;

	// re-sort, back to front
/*	std::sort						(f_portals.begin(),f_portals.end(),psort_pred);
	
	// calc poly-count
	size_t		_pcount					= 0;
	for		(size_t _it = 0; _it<f_portals.size(); _it++)	_pcount	+= f_portals[_it].first->getPoly().size()-2;

	// fill buffers
	size_t			_offset				= 0;
	FVF::L*		_v					= (FVF::L*)RCache.Vertex.Lock(_pcount*3,f_geom.stride(),_offset);
	float		ssaRange			= GRenderResourcesManager->LegacyScene->PortalFadeSsaStartThreshold - GRenderResourcesManager->LegacyScene->PortalFadeSsaEndThreshold;
	u32			_ambient = 0;
	if (gameVersionController->getGame() == gameVersionController->SOC)
	{
		auto& env = ENV_SOC;
		Fvector		_ambient_f = env.CurrentEnv.ambient;;
					_ambient =XrColor::color_rgba_f(_ambient_f.x, _ambient_f.y, _ambient_f.z, 0);
		
	}
	else
	{
		auto& env = ENV;
		Fvector		_ambient_f = env.CurrentEnv->ambient;;
					_ambient =XrColor::color_rgba_f(_ambient_f.x, _ambient_f.y, _ambient_f.z, 0);
		
	}
	for (size_t _it = 0; _it < f_portals.size(); _it++)
	{
		xr_pair<CPortal*, float>&	fp = f_portals[_it];
		CPortal*					_P = fp.first;
		float						_ssa = fp.second;
		float		ssaDiff = _ssa - GRenderResourcesManager->LegacyScene->PortalFadeSsaEndThreshold;
		float		ssaScale = ssaDiff / ssaRange;
		int			iA = iFloor((1 - ssaScale)*255.5f);	clamp(iA, 0, 255);
		u32							_clr = XrColor::subst_alpha(_ambient, u32(iA));

		// fill polys
		size_t			_polys = _P->getPoly().size() - 2;
		for (u32 _pit = 0; _pit < _polys; _pit++) {
			_v->set(_P->getPoly()[0], _clr);	_v++;
			_v->set(_P->getPoly()[_pit + 1], _clr);	_v++;
			_v->set(_P->getPoly()[_pit + 2], _clr);	_v++;
		}
	}
	RCache.Vertex.Unlock			(_pcount*3,f_geom.stride());

	// render
	RCache.set_xform_world			(Fidentity);
	RCache.set_Shader				(f_shader);
	RCache.set_Geometry				(f_geom);
	RCache.set_CullMode				(CULL_NONE);
	RCache.Render					(D3DPT_TRIANGLELIST,_offset,_pcount);
	RCache.set_CullMode				(CULL_CCW);

	// cleanup
	f_portals.clear					();*/
}

#ifdef DEBUG
void CPortalTraverser::dbg_draw		()
{
/*	RCache.OnFrameEnd		();
	RCache.set_xform_world	(Fidentity);
	RCache.set_xform_view	(Fidentity);
	RCache.set_xform_project(Fidentity);
	for (u32 s=0; s<dbg_sectors.size(); s++)	{
		CSector*	S		= (CSector*)dbg_sectors[s];
		FVF::L		verts	[5];
		Fbox2		bb		= S->r_scissor_merged;
		bb.min.x			= bb.min.x * 2 - 1;
		bb.max.x			= bb.max.x * 2 - 1;
		bb.min.y			= (1-bb.min.y) * 2 - 1;
		bb.max.y			= (1-bb.max.y) * 2 - 1;

		verts[0].set(bb.min.x,bb.min.y,EPS,0xffffffff);
		verts[1].set(bb.max.x,bb.min.y,EPS,0xffffffff);
		verts[2].set(bb.max.x,bb.max.y,EPS,0xffffffff);
		verts[3].set(bb.min.x,bb.max.y,EPS,0xffffffff);
		verts[4].set(bb.min.x,bb.min.y,EPS,0xffffffff);
		RCache.dbg_Draw		(D3DPT_LINESTRIP,verts,4);
	}*/
}
#endif

