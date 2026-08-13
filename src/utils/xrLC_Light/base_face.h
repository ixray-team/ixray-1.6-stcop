#pragma once

#include "base_basis.h"
#include "base_color.h"
#include "MeshStructure.h"
#include "tcf.h"

class CLightmap;
class CDeflector;
struct Shader_xrLC;

class base_Face;

class XRLC_LIGHT_API base_Vertex
{
public: 
	using DataVertexType = base_Vertex;
	using DataFaceType = base_Face;
	
	Fvector					P;
	Fvector					N;
	base_color				C;			// all_lighting info
	int						handle;		// used in mesh-processing/optimization/conversion
public:
	IC bool similar(Tvertex<base_Vertex>& V, float eps)
	{
		return P.similar(V.P, eps);
	}

	base_Vertex() = default;
	virtual ~base_Vertex(){}
};
 
class XRLC_LIGHT_API base_Face
{
public: 
	
	FixedVector<_TCF,2>			tc;				// TC
	Fvector					N;				// face normal
	base_basis				basis_tangent		[3];
	base_basis				basis_binormal		[3];

	CDeflector*				pDeflector;		// does the face has LM-UV map?
	CLightmap*				lmap_layer;
	u32						sm_group;
	
	u16						dwMaterial;			// index of material
	u16						dwMaterialGame;		// unique-id of game material (must persist up to game-CForm saving)

	struct					{
		u16 bSplitted : 1;
		u16 bProcessed : 1;
		u16 bOpaque : 1;	// For ray-tracing speedup
		u16 bLocked : 1;	// For tesselation
		u16 bWater : 1;
		u16 bSharedMaterial : 1;
	}						flags;


	virtual	const Shader_xrLC&	Shader			( )const;
	virtual void			CacheOpacity		( );
	virtual Fvector2*		getTC0			( );

	base_Face();
	virtual ~base_Face(){}; 

	bool RenderEqualTo(Tface<base_Vertex> *F);

	void AddChannel(Fvector2 &p1, Fvector2 &p2, Fvector2 &p3); 
	bool hasImplicitLighting() const;

 
};		

