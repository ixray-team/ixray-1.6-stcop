#include "stdafx.h"

#include "xrFace.h"
//#include "build.h"
#include "xrDeflector.h"
#include "xrLC_GlobalData.h"
#include "Lightmap.h"

volatile u32					dwInvalidFaces;//= 0;
u32		InvalideFaces()
{
	return dwInvalidFaces;
}

const Shader_xrLC&	base_Face::Shader		()const
{
	VERIFY( inlc_global_data() );
	return shader( dwMaterial, inlc_global_data()->shaders(), inlc_global_data()->materials() );
}
void			base_Face::CacheOpacity	()
{
	flags.bOpaque				= true;
	VERIFY ( inlc_global_data() );

	b_material& M		= inlc_global_data()->materials()		[dwMaterial];
	b_BuildTexture&	T	= inlc_global_data()->textures()		[M.surfidx];
	if (T.bHasAlpha)	
		flags.bOpaque = false;
	else			
		flags.bOpaque = true;

	if ( !flags.bOpaque && (!T.THM.HasSurface()) )	////	pSurface was possible deleted
	{
		flags.bOpaque	= true;
		clMsg			("Strange face detected... Has alpha without texture... [%s]", T.name);
	}
}
static bool do_not_add_to_vector_in_global_data = false;
 


//
//const int	edge2idx	[3][2]	= { {0,1},		{1,2},		{2,0}	};
//const int	edge2idx3	[3][3]	= { {0,1,2},	{1,2,0},	{2,0,1}	};
//const int	idx2edge	[3][3]  = {
//	{-1,  0,  2},
//	{ 0, -1,  1},
//	{ 2,  1, -1}
//};



//extern CBuild*	pBuild;

bool			g_bUnregister = true;

//template<>
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


Tvertex<DataVertex>::Tvertex()
{
	
	VERIFY( inlc_global_data() );
	if( inlc_global_data()->vert_construct_register() )
	{	
 		inlc_global_data()->g_vertices().push_back(this);
	}
	m_adjacents.reserve	(4);
}

template <>
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
}

Vertex*	Vertex::CreateCopy_NOADJ( vecVertex& vertises_storage ) const
{
	VERIFY( &vertises_storage == &inlc_global_data()->g_vertices() );
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
	
	pDeflector				= 0;
	flags.bSplitted			= false;
	VERIFY( inlc_global_data() );
	if( !do_not_add_to_vector_in_global_data )
	{
		//set_index( inlc_global_data()->g_faces().size() );
		inlc_global_data()->g_faces().push_back		(this);
	}
	sm_group				= u32(-1);
	lmap_layer				= NULL;
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
			//faces.erase(F);
		}
		else clMsg("* ERROR: Unregistered FACE destroyed");
	}
	// Remove 'this' from adjacency info in vertices
	for (int i=0; i<3; ++i)
		v[i]->prep_remove(this);

	lmap_layer				= NULL;
}
 
template<>
void Face::	Failure		()
{
	dwInvalidFaces			++;

	clMsg		("* ERROR: Invalid face. (A=%f,e0=%f,e1=%f,e2=%f)",
		CalcArea(),
		v[0]->P.distance_to(v[1]->P),
		v[0]->P.distance_to(v[2]->P),
		v[1]->P.distance_to(v[2]->P)
		);
	clMsg		("*        v0[%f,%f,%f], v1[%f,%f,%f], v2[%f,%f,%f]",
		VPUSH(v[0]->P),
		VPUSH(v[1]->P),
		VPUSH(v[2]->P)
		);
	inlc_global_data()->err_invalid().w_fvector3	(v[0]->P);
	inlc_global_data()->err_invalid().w_fvector3	(v[1]->P);
	inlc_global_data()->err_invalid().w_fvector3	(v[2]->P);
}

void	Face::Verify		()
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
				if (it->pDeflector)
					continue;
				
				if (!D->OA_Place(it))
					continue;

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


BOOL	DataFace::RenderEqualTo	(Face *F)
{
	if (F->dwMaterial	!= dwMaterial		)	return FALSE;
	//if (F->tc.size()	!= F->tc.size()		)	return FALSE;	// redundant???
	return TRUE;
}



void	DataFace::AddChannel	(Fvector2 &p1, Fvector2 &p2, Fvector2 &p3) 
{
	_TCF	TC;
	TC.uv[0] = p1;	TC.uv[1] = p2;	TC.uv[2] = p3;
	tc.push_back(TC);
}

BOOL	DataFace::hasImplicitLighting()
{
	if (0==this)								return FALSE;
	if (!Shader().flags.bRendering)				return FALSE;
	VERIFY( inlc_global_data() );
	b_material& M		= inlc_global_data()->materials()		[dwMaterial];
	b_BuildTexture&	T	= inlc_global_data()->textures()		[M.surfidx];
	return (T.THM.flags.test(STextureParams::flImplicitLighted));
}

///////////////////////////////////////////////////////////////
