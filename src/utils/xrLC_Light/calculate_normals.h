#pragma once

#include "itterate_adjacents_static.h"
#include "xrFace.h"
#include "../../Editors/Public/itterate_adjacents.h"

class calculate_normals
{	
	
	//these typedefs to hide global typedefs!!!
	typedef xr_vector<TVertex*>								vecVertex;
	typedef typename vecVertex::iterator						vecVertexIt;
	typedef xr_vector<TFace*>								vecFace;
	typedef typename vecFace::iterator							vecFaceIt;

	typedef vecFace												vecAdj;
	typedef typename vecAdj::iterator							vecAdjIt;
private:	

typedef  itterate_adjacents< itterate_adjacents_params_static> itterate_adjacents_type;

public:
	
static void	calc_normals( vecVertex &vertices, vecFace &faces, bool IsMU )
{
 	u32		Vcount	= (u32)vertices.size();
	float	p_total = 0;
	float	p_cost  = 1.f/(Vcount);

	// Clear temporary flag
	// Status			("Processing...");
	float sm_cos	= std::cos(deg2rad(g_params().m_sm_angle));

	for (auto face : faces)
	{
		face->flags.bSplitted	= true;
		face->CalcNormal		();
	}
	 
	// remark:
	//	we use Face's bSplitted value to indicate that face is processed
	//  so bSplitted means bUsed
	u64 VCountAllocated = 0;

 	for (u32 I=0; I<Vcount; I++)
	{
		TVertex* pTestVertex = vertices[I];
 
		for (vecAdjIt AFit = pTestVertex->m_adjacents.begin(); AFit!=pTestVertex->m_adjacents.end(); ++AFit)
		{
			TFace*	F					= *AFit;
			F->flags.bSplitted			= false;
		}

		std::ranges::sort(pTestVertex->m_adjacents);

		while ( pTestVertex->m_adjacents.size() )	
		{
 			vecFace new_adj;
			itterate_adjacents_type::recurse_tri_params p( pTestVertex, new_adj, sm_cos );
			itterate_adjacents_type::RecurseTri( 0, p );
 
			TVertex*	pNewVertex			= pTestVertex->CreateCopy_NOADJ( vertices, IsMU );
			VCountAllocated++;

			if (IsMU)
			{
				vertices.push_back( pNewVertex);
			}

 			for (u32 a=0; a<new_adj.size(); ++a)
			{
				TFace* test		= new_adj[a];
				test->VReplace	( pTestVertex, pNewVertex );
			}
			new_adj.clear();
  			pNewVertex->normalFromAdj	();
		}
		Progress( p_total+=p_cost );
	}
	Progress		( 1.f );

	// Destroy unused vertices
 	isolate_vertices( false, vertices, IsMU);
	  
	// Recalculate normals
	for (auto Vertex : vertices)
	{
		Vertex->normalFromAdj();
	}

	// clMsg	("%d vertices was duplicated 'cause of SM groups",vertices.size()-Vcount);

	// Clear temporary flag
	for (auto face : faces)
	{
		face->flags.bSplitted = false;
	}
}
};