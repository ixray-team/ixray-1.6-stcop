#include "stdafx.h"

#include "base_face.h"
#include "serialize.h"

base_Face::base_Face()
{
	basis_tangent[0].set	(0,0,0);
	basis_tangent[1].set	(0,0,0);
	basis_tangent[2].set	(0,0,0);
	basis_binormal[0].set	(0,0,0);
	basis_binormal[1].set	(0,0,0);
	basis_binormal[2].set	(0,0,0);
}
base_Face::~base_Face()		{};

base_Vertex::~base_Vertex() {};


/*

	base_basis				basis_tangent		[3];
	base_basis				basis_binormal		[3];
	u16						dwMaterial;			// index of material
	u16						dwMaterialGame;		// unique-id of game material (must persist up to game-CForm saving)

		struct					{
		u16					bSplitted			:		1;
		u16					bProcessed			:		1;
		u16					bOpaque				:		1;	// For ray-tracing speedup
		u16					bLocked				:		1;	// For tesselation
	}						flags;
*/
 

/*	Fvector					P;
	Fvector					N;
	base_color				C;			// all_lighting info
	int						handle;		// used in mesh-processing/optimization/conversion
*/ 