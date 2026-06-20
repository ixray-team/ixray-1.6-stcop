#pragma once

#include "../xrLC_Light/embree_raytracing/EmbreeGeometryBuilder.h"

class IComputeData
{
public:
	Lights g_lights;
	xr_unique_ptr<CDB::MODEL> LevelPtr;

	Shader_xrLC_LIB* g_shaders_xrlc;
	xr_vector<b_material> g_materials;
	xr_vector<b_shader> g_shader_render;
	xr_vector<b_shader> g_shader_compile;
	xr_vector<b_BuildTexture> g_textures;
 
	xr_vector<FaceDataEmbree>	build_faces;

	void xrLoadData(const char* name, bool draft_mode, bool skipThm);
	void xrLoadGeometry(IReader* fs);
	
	void xrUnload();

	void xrCalculateOpacity()
	{
		for (auto& F : build_faces)
		{
			F.bOpaque = true;

			b_material& M = g_materials[F.dwMaterial];
			b_BuildTexture& T = g_textures[M.surfidx];
			F.bOpaque = !T.bHasAlpha;

			// pSurface was possible deleted
			if ( !F.bOpaque && T.pSurface.Empty() )
			{
				F.bOpaque = true;
				clMsg("Strange face detected... Has alpha without texture... [%s]", T.name);
			}
		}
	}
};