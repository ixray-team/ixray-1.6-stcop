#include "StdAfx.h"
#include "Build.h"
#include "../xrLC_Light/xrFace.h"
#include "../xrLC_Light/calculate_normals.h"
#include "../xrLC_Light/xrLC_GlobalData.h"



//void 
// Performs simple cross-smooth

void CBuild::CalcNormals()
{
	size_t VSizeStart = lc_global_data()->g_vertices().size() * sizeof(Vertex);
	size_t FSizeStart = lc_global_data()->g_faces().size() * sizeof(Face);
	 
	calculate_normals<Vertex>::calc_normals( lc_global_data()->g_vertices(), lc_global_data()->g_faces() );

	size_t VSize = lc_global_data()->g_vertices().size() * sizeof(Vertex);
	size_t FSize = lc_global_data()->g_faces().size() * sizeof(Face);
	AditionalData("Verts(%u \\ %umb) Tris(%u \\ %umb)",
		VSizeStart / 1024 / 1024,
		VSize / 1024 / 1024,
		FSizeStart / 1024 / 1024,
		FSize / 1024 / 1024
	);
	Status("PRE MU MODELS: Memory Verts(%u \\ %umb) Tris(%u \\ %umb)",
		VSizeStart / 1024 / 1024,
		VSize / 1024 / 1024,
		FSizeStart / 1024 / 1024,
		FSize / 1024 / 1024);
	// Models
	Status	("Models...");
	MU_ModelsCalculateNormals();
 
	Status("MU MODELS: Memory Verts(%u \\ %umb) Tris(%u \\ %umb)",
		VSizeStart / 1024 / 1024,
		VSize / 1024 / 1024,
		FSizeStart / 1024 / 1024,
		FSize / 1024 / 1024);

}

