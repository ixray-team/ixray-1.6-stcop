#include "stdafx.h"

#include "xrFace.h"
#include "xrDeflector.h"
#include "xrLC_GlobalData.h"
#include "Lightmap.h"
#include "utils/xrLC/Build.h"

volatile u32					dwInvalidFaces;//= 0;
u32		InvalideFaces()
{
	return dwInvalidFaces;
}

const Shader_xrLC&	base_Face::Shader		()const
{
	const auto data = inlc_global_data();
	VERIFY(data);
	if (flags.bSharedMaterial)
	{
		auto& Arr = data->materials_shared();
		VERIFY( dwMaterial < Arr.size());
		return data->shaders().Get(Arr[dwMaterial].reserved);
	}
	auto& Arr = data->materials();
	VERIFY( dwMaterial < Arr.size());
	return data->shaders().Get(Arr[dwMaterial].reserved);
}

void base_Face::CacheOpacity()
{
	flags.bOpaque = true;
	VERIFY(inlc_global_data());

	b_BuildTexture& T = pBuild->GetTexture(dwMaterial, flags.bSharedMaterial);
	flags.bOpaque = !T.bHasAlpha;

	// pSurface was possible deleted
	if (!flags.bOpaque && !T.HasSurface())
	{
		flags.bOpaque = true;
		clMsg("Strange face detected... Has alpha without texture... [%s]", T.name);
	}
}

bool g_bUnregister = true;

void destroy_vertex( Vertex* &v, bool unregister )
{
	bool tmp_unregister = g_bUnregister;
	g_bUnregister = unregister;
	inlc_global_data()->destroy_vertex( v );
	g_bUnregister = tmp_unregister;
}

void destroy_face( Face* &v, bool unregister )
{
	bool tmp_unregister = g_bUnregister;
	g_bUnregister = unregister;
	inlc_global_data()->destroy_face( v );
	g_bUnregister = tmp_unregister;
}

template<>
Tvertex<DataVertex>::Tvertex()
{
 	R_ASSERT( inlc_global_data() );
	if( inlc_global_data()->vert_construct_register() )
	  	inlc_global_data()->g_vertices().push_back(this);
}

template<>
Tvertex<DataVertex>::~Tvertex()
{
 	if (g_bUnregister) 
	{
		vecVertexIt F = std::find(inlc_global_data()->g_vertices().begin(), inlc_global_data()->g_vertices().end(), this);
		if (F!=inlc_global_data()->g_vertices().end())
		{
			vecVertex& verts = inlc_global_data()->g_vertices();
			std::swap( *F, *( verts.end()-1 ) );
			verts.pop_back();
		}
		else clMsg("* ERROR: Unregistered VERTEX destroyed");
	}

	m_adjacents.clear();
	m_adjacents.shrink_to_fit();
}

template<>
Vertex*	Vertex::CreateCopy_NOADJ( vecVertex& vertises_storage ) const
{
	R_ASSERT( &vertises_storage == &inlc_global_data()->g_vertices() );

	Vertex* V	= inlc_global_data()->create_vertex();
	V->P.set	(P);
	V->N.set	(N);
	V->C		= C;
	return		V;
}
 
//////////////////////////////////////////////////////////////////////////////////////////////////////////////
template<>
Tface<DataVertex>::Tface()
{
 	pDeflector				= nullptr;
	flags.bSplitted			= false;
 	inlc_global_data()->g_faces().push_back		(this);

	sm_group				= u32(-1);
	lmap_layer				= nullptr;
}

template<>
Tface<DataVertex>::~Tface()
{
	if (g_bUnregister) 
	{
		vecFaceIt F = std::find(inlc_global_data()->g_faces().begin(), inlc_global_data()->g_faces().end(), this);
		if (F!=inlc_global_data()->g_faces().end())
		{
			vecFace& faces = inlc_global_data()->g_faces();
			std::swap( *F, *( faces.end()-1 ) );
			faces.pop_back();
 		}
		else clMsg("* ERROR: Unregistered FACE destroyed");
	}

	// Remove 'this' from adjacency info in vertices
	for (int i=0; i<3; ++i)
		v[i]->prep_remove(this);
 	lmap_layer				= nullptr;
}
 
template<>
void Face::	Failure		()
{
	dwInvalidFaces			++;

	inlc_global_data()->err_invalid().w_fvector3	(v[0]->P);
	inlc_global_data()->err_invalid().w_fvector3	(v[1]->P);
	inlc_global_data()->err_invalid().w_fvector3	(v[2]->P);
}

template<>
void Face::Verify()
{
	// 1st :: area
	float	_a	= CalcArea();
	if		(!_valid(_a) || (_a<EPS))		{ Failure(); return; }

	// 2nd :: TC0
	Fvector2*	tc			= getTC0();
	float	eps				= .5f / 4096.f;		// half pixel from 4096 texture (0.0001220703125)
	float	e0				= tc[0].distance_to(tc[1]);	
	float	e1				= tc[1].distance_to(tc[2]);
	float	e2				= tc[2].distance_to(tc[0]);
	float	p				= e0+e1+e2;
	if		(!_valid(_a) || (p<eps))		{ Failure(); return; }

	// 3rd :: possibility to calc normal
	CalcNormal				();
	if (!_valid(N))			{ Failure(); return; }
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
int affected = 0;
void start_unwarp_recursion()
{
	affected				= 1;
}

template<>
void Face::OA_Unwarp( CDeflector *D, xr_vector<type_face*>& faces)
{ 
	// range: no recursive method realisation
	xr_stack<Face*> st;

	Face* f = this;
	while (true)
	{
		for (int i = 0; i < 3; ++i)
		{
			for (auto it : f->v[i]->m_adjacents)
			{
				if (it->pDeflector) continue;
				
				if (!D->OA_Place(it)) continue;

				affected++;
				st.push(it);
				faces.push_back(it);
			}
		}

		if (!st.empty())
		{
			f = st.top();
			st.pop();
		}
		else break;
	}
}

bool DataFace::RenderEqualTo(Face *F)
{
	if (F->dwMaterial	!= dwMaterial		)	
		return false;
 	return true;
}

void DataFace::AddChannel(Fvector2 &p1, Fvector2 &p2, Fvector2 &p3) 
{
	_TCF	TC;
	TC.uv[0] = p1;	TC.uv[1] = p2;	TC.uv[2] = p3;
	tc.push_back(TC);
}

bool DataFace::hasImplicitLighting()
{
	if (!Shader().flags.bRendering)
	{
		return false;
	}
	VERIFY( inlc_global_data() );
	auto& T = pBuild->GetTexture(dwMaterial, flags.bSharedMaterial);
	return (T.THM.flags.test(STextureParams::flImplicitLighted));
}

void GetBarycentric(Face* F, Fvector& wP, Fvector& wN, Fvector& B)
{
	Vertex * V1 = F->v[0];
	Vertex* V2 = F->v[1];
	Vertex* V3 = F->v[2];
	wP.from_bary(V1->P, V2->P, V3->P, B);
	wN.from_bary(V1->N, V2->N, V3->N, B);
	wN.normalize();
};

void GetBarycentricNormalized(Face* F, Fvector& wP, Fvector& wN, Fvector& B)
{
	Vertex* V1 = F->v[0];
	Vertex* V2 = F->v[1];
	Vertex* V3 = F->v[2];
	wP.from_bary(V1->P, V2->P, V3->P, B);
	wN.from_bary(V1->N, V2->N, V3->N, B);   exact_normalize(wN);
	wN.add(F->N);							exact_normalize(wN);
};