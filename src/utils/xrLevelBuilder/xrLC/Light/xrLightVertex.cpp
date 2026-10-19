#include "stdafx.h"
#include "xrLightVertex.h"
#include "../Mesh/xrFace.h"
#include "../Build/xrLC_GlobalData.h"
#include "light_point.h"

#include "../../../../xrCore/Collision/xrCDB.h"
#include "LightingBackend.h"
#include "xrDeflector.h"

//-----------------------------------------------------------------------
typedef	xr_multimap<float,vecVertex>	mapVert;
typedef	mapVert::iterator				mapVertIt;
mapVert* g_trans;
xrCriticalSection g_trans_CS;

extern XRLC_LIGHT_API void		LightPoint		(CDB::COLLIDER* DB, CDB::MODEL* MDL, base_color_c &C, Fvector &P, Fvector &N, base_lighting& lights, u32 flags, Face* skip);
 
void	g_trans_register_internal		(Vertex* V)
{
	R_ASSERT	(V);

	const float eps		= EPS_L;
	const float eps2	= 2.f*eps;
	
	// Search
	const float key		= V->P.x;
	mapVertIt	it		= g_trans->lower_bound	(key);
	mapVertIt	it2		= it;

	// Decrement to the start and inc to end
	while (it!=g_trans->begin() && ((it->first+eps2)>key)) it--;
	while (it2!=g_trans->end() && ((it2->first-eps2)<key)) it2++;
	if (it2!=g_trans->end())	it2++;
	
	// Search
	for (; it!=it2; it++)
	{
		vecVertex&	VL		= it->second;
		Vertex* Front		= VL.front();
		R_ASSERT			(Front);
		if (Front->P.similar(V->P,eps))
		{
			VL.push_back		(V);
			return;
		}
	}

	// Register
	mapVertIt	ins			= g_trans->insert(std::make_pair(key,vecVertex()));
	ins->second.reserve		(32);
	ins->second.push_back	(V);
}

void	g_trans_register	(Vertex* V)
{
	g_trans_CS.Enter			();
	g_trans_register_internal	(V);
	g_trans_CS.Leave			();
}

//////////////////////////////////////////////////////////////////////////
bool GetTranslucency(const Vertex* V,float &v_trans )
{
	// Get transluency factor
			
	bool		bVertexLight= false;
	u32 		L_flags		= 0;
	for (u32 f=0; f<V->m_adjacents.size(); ++f)
	{
		Face*	F								=	V->m_adjacents[f];
		v_trans									+=	F->Shader().vert_translucency;
		if	(F->Shader().flags.bLIGHT_Vertex)	
			bVertexLight		= true;
	}
	v_trans				/=	float(V->m_adjacents.size());
	return bVertexLight;
}

void LightVertex()
{
	g_trans = new mapVert();

	// Start threads, wait, continue --- perform all the work
	UpdateCurrentPhase("Vertex");

	Status("Calculating...");

	u32 flags = LGetCurrentFlags() | LP_dont_hemi;
	GetLightingBackend().LightVertices(flags);
 
	// Process all groups
	Status("Transluenting...");
	for (mapVertIt it = g_trans->begin(); it != g_trans->end(); it++)
	{
		// Unique
		vecVertex& VL = it->second;
		std::sort(VL.begin(), VL.end());
		VL.erase(std::unique(VL.begin(), VL.end()), VL.end());

		// Calc summary color
		base_color_c C;
		for (u32 v = 0; v < VL.size(); v++)
		{
			base_color_c cc;
			VL[v]->C._get(cc);
			C.max(cc);
		}

		// Calculate final vertex color
		for (u32 v = 0; v < VL.size(); v++)
		{
			base_color_c vC;
			VL[v]->C._get(vC);

			base_color_c R;
			R.lerp(vC, C, vC._tmp_); // trans-level
			R.max(vC);
			VL[v]->C._set(R);
		}
	}

	xr_delete(g_trans);
	Status("Wating...");
}
