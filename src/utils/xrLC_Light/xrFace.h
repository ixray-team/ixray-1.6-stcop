#pragma once
#include "base_basis.h"
#include "base_color.h"
#include "../Shader_xrLC.h"

#include "tcf.h"


class CLightmap;
class CDeflector;

struct TVertex;

struct XRLC_LIGHT_API TFace
{
	TVertex* v[3] = {};
	
	FixedVector<_TCF,2> tc; // TC
	Fvector N; // face normal
	base_basis basis_tangent[3];
	base_basis basis_binormal[3];

	CDeflector* pDeflector = nullptr; // does the face has LM-UV map?
	CLightmap* lmap_layer = nullptr;
	u32 sm_group;
	
	u16 dwMaterial; // index of material
	u16 dwMaterialGame; // unique-id of game material (must persist up to game-CForm saving)

	struct {
		u16 bSplitted : 1;
		u16 bProcessed : 1;
		u16 bOpaque : 1;	// For ray-tracing speedup
		u16 bLocked : 1;	// For tesselation
		u16 bWater : 1;
		u16 bSharedMaterial : 1;
	} flags;

	const Shader_xrLC& Shader()const;
	void CacheOpacity();
	Fvector2* getTC0();

	bool RenderEqualTo(TFace* F) const;

	void AddChannel(Fvector2 &p1, Fvector2 &p2, Fvector2 &p3); 
	bool hasImplicitLighting() const;
	
	void Verify();
	void Failure() const;
	void OA_Unwarp(CDeflector * d, xr_vector<TFace*>& affected) const;
	
	IC TVertex*	vertex(u8 index) const
	{
		R_ASSERT( index<3 );
		return v[index];
	}

	bool VContains(const TVertex* pV) const
	{
		return VIndex(pV)>=0;	
	}

	void VReplace(TVertex* what, TVertex* to);
	void VReplace_not_remove(TVertex* what, TVertex* to);

	IC int VIndex(const TVertex* pV) const
	{
		if (v[0]==pV) return 0;
		if (v[1]==pV) return 1;
		if (v[2]==pV) return 2;
		return -1;
	}

	void SetVertex(int idx, TVertex* V);

	IC void	SetVertices(TVertex *V1, TVertex *V2, TVertex *V3)
	{
		SetVertex(0,V1);
		SetVertex(1,V2);
		SetVertex(2,V3);
	}
	
	IC bool isDegenerated() const
	{
		return (v[0]==v[1] || v[0]==v[2] || v[1]==v[2]);
	}

	IC void	EdgeVerts(int e, TVertex** A, TVertex** B) const
	{
		*A = v[edge2idx[e][0]];
		*B = v[edge2idx[e][1]];
	}

	// Calculate Normal
	void CalcNormal();
	void CalcNormal2();

	// UV, Deflector
	float CalcArea() const;

	// xrSubdivide used
	void CalcCenter(Fvector &C);
};

struct XRLC_LIGHT_API TVertex
{
	/*			TYPES			*/
	typedef xr_vector<TFace*>			v_faces;
	typedef typename v_faces::iterator		v_faces_it;

	typedef xr_vector<TVertex*>			 v_vertices;
	typedef typename v_vertices::iterator	v_vertices_it;
	
	Fvector P;
	Fvector N;
	base_color C; // all_lighting info
	int handle; // used in mesh-processing/optimization/conversion
	
	IC bool similar(const TVertex& V, float eps) const
	{
		return P.similar(V.P, eps);
	}

	/*	FUNCTIONS MAIN */
	TVertex* CreateCopy_NOADJ(v_vertices& vertises_storage, bool IsMU) const;

	v_faces m_adjacents;
	IC	TVertex* CreateCopy(v_vertices& vertises_storage, bool IsMU) const
	{
		TVertex* V = CreateCopy_NOADJ(vertises_storage, IsMU);
		V->m_adjacents = m_adjacents;
		return V;
	}

	IC	void	prep_add(TFace* F)
	{	
		v_faces_it I = std::ranges::find(m_adjacents,F);
		if (I==m_adjacents.end())
		{
			m_adjacents.push_back(F);
		}
	}

	IC	void	prep_remove(TFace* F)
	{	
		v_faces_it I = std::ranges::find(m_adjacents,F);	
		if (I != m_adjacents.end())
		{
			m_adjacents.erase(I);
		}
	}

	IC void	normalFromAdj()
	{
		this->N.set( 0, 0, 0 );
		for ( v_faces_it ad = m_adjacents.begin(); ad!=m_adjacents.end(); ++ad )
		{
			this->N.add( (*ad)->N );
		}
		exact_normalize	(this->N );
	}

};

void isolate_vertices(bool bProgress, xr_vector<TVertex*> &vertices, bool IsMU);

#pragma pack(push,4)

struct DataFace;
class  CLightmap;

//typedef	Tvertex< base_Vertex> Vertex;

typedef std::pair<TVertex*, TVertex *>	PAIR_VV;
typedef xr_map<TVertex*,TVertex*>			map_v2v;	// vertex to vertex translation
typedef map_v2v::iterator				map_v2v_it;

//template <typename DataVertexType>
//struct Tface;
//typedef	Tface<base_Vertex> Face;

//template <typename DataVertexType>
//struct Tvertex;
//typedef	Tvertex<base_Vertex>	Vertex;

typedef xr_vector<TVertex*>			vecVertex;
typedef vecVertex::iterator			vecVertexIt;

typedef xr_vector<TFace*>			vecFace;
typedef vecFace::iterator			vecFaceIt;
typedef vecFace::const_iterator		vecFaceCit;

typedef xr_vector<vecFace*>			vec2Face;
typedef vec2Face::iterator			splitIt;


typedef vecFace						vecAdj;
typedef vecAdj::iterator			vecAdjIt;

/*struct XRLC_LIGHT_API DataVertex :	public base_Vertex
{
public:
	typedef DataFace DataFaceType;

	IC bool similar(Tvertex<DataVertex>& V, float eps);

	DataVertex() {};
	virtual ~DataVertex() {};
};*/

/*struct XRLC_LIGHT_API DataFace	: public base_Face
{
public:

 	Fvector					N;				// face normal
 	FixedVector<_TCF,2>			tc;				// TC

	CDeflector*				pDeflector;		// does the face has LM-UV map?
	CLightmap*				lmap_layer;
	u32						sm_group;
	virtual Fvector2*		getTC0			( ) { return tc[0].uv; }


	bool		RenderEqualTo		( Face *F );

	void		AddChannel			( Fvector2 &p1, Fvector2 &p2, Fvector2 &p3 ); 
	bool		hasImplicitLighting	();

	DataFace(){};
	virtual ~DataFace(){};
};*/

class Material;
class Edge;

// Typedefs
#include "xrUVpoint.h"
extern XRLC_LIGHT_API bool g_bUnregister;

#pragma pack(pop)

extern "C" XRLC_LIGHT_API void start_unwarp_recursion();
extern "C" XRLC_LIGHT_API void destroy_vertex(TVertex* &v, bool unregister);

void destroy_face(TFace*& v, bool unregister);
							
/*IC bool Vertex::similar(Vertex& V, float eps)
{
	return P.similar(V.P, eps);
}*/


void GetBarycentric(TFace* F, Fvector& wP, Fvector& wN, Fvector& B);
void GetBarycentricNormalized(TFace* F, Fvector& wP, Fvector& wN, Fvector& B);