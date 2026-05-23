#pragma once

#include "base_lighting.h"
#include "global_slots_data.h"
#include "b_build_texture.h" 
#include "../../xrCore/Collision/xrCDB.h"

#include "embree_raytracing/EmbreeGeometryBuilder.h"
#include "xrMU_Model.h"
#include "xrMU_Model_Reference.h"

class Shader_xrLC_LIB;



//-----------------------------------------------------------------
struct global_claculation_data
{
	base_lighting						g_lights; /////////////////////lc
	Shader_xrLC_LIB*					g_shaders_xrlc;////////////////lc
	b_params							g_params;//////////////////////lc
	xr_vector<b_material>				g_materials;///////////////////lc
	xr_vector<b_BuildTexture>			g_textures;////////////////////lc
	CDB::MODEL							RCAST_Model;///////////////////lc

	Fbox								LevelBB;//-----------============
	global_slots_data					slots_data;//-------=============
	xr_vector<b_shader>					g_shader_compile;//-----==========
 
  	xr_vector<FaceDataEmbree>			building_embree_faces;

	// Mu Stuff
			global_claculation_data		(): g_shaders_xrlc( 0 ) {}
	void	xrLoad						(bool skipThm);
	void	xrLoadGeometry				(IReader* fs);
	void	xrCalculateOpacity();
};

extern global_claculation_data	gl_data;
