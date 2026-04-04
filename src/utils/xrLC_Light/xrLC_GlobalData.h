#pragma once

#include "../Shader_xrLC.h"
#include "../../xrCore/xrPool.h"
#include "xrFace.h"
#include "xrDeflectorDefs.h"
#include "b_build_texture.h"
#include "base_lighting.h"
#include "../xrForms/CompilersUI.h"

namespace CDB{
class MODEL;
class CollectorPacked;
};
class CLightmap;
class xrMU_Model;
class xrMU_Reference;
class base_Vertex;
class base_Face;

class XRLC_LIGHT_API xrLC_GlobalData
{
 	CMemoryWriter					_err_invalid;
	CMemoryWriter					_err_multiedge;
	CMemoryWriter					_err_tjunction;

	xr_vector<b_BuildTexture>		_textures;
	xr_hash_map<b_material_shared*, b_BuildTexture> _textures_shared;
	xr_vector<b_material>			_materials;
	xr_vector<b_material_shared>	_materials_shared;
	Shader_xrLC_LIB					_shaders;
	b_params						_g_params;
	base_lighting					_L_static;


	// Computing
	xr_vector<CLightmap*>			_g_lightmaps;
	xr_vector<CDeflector*>			_g_deflectors;

	// Faces
	vecVertex						_g_vertices;
	vecFace							_g_faces;
	xr_vector<xrMU_Model*>			_mu_models;
	xr_vector<xrMU_Reference*>		_mu_refs;

public:
	xr_vector<base_Face*>			FacesStorage;

	xrLC_GlobalData();
	~xrLC_GlobalData();
		IC CMemoryWriter& err_invalid() { return _err_invalid; }
		IC CMemoryWriter& err_multiedge() { return _err_multiedge; };
		IC CMemoryWriter& err_tjunction() { return _err_tjunction; };
		IC b_params& g_params() { return _g_params; }
		IC Shader_xrLC_LIB& shaders() { return _shaders; }


		IC xr_vector<b_BuildTexture>& textures() { return _textures; }
		IC xr_hash_map<b_material_shared*, b_BuildTexture>& textures_shared() { return _textures_shared; }
		IC xr_vector<CLightmap*>& lightmaps() { return _g_lightmaps; }
		IC xr_vector<b_material>& materials() { return _materials; }
		IC xr_vector<b_material_shared>& materials_shared() { return _materials_shared; }
		
 		vecVertex&						g_vertices()	{ return	_g_vertices; }
		vecFace&						g_faces()		{ return	_g_faces; }
		vecDefl& g_deflectors() { return	_g_deflectors; }

		Face*						create_face();
		void						destroy_face(Face*& f);

		Vertex*						create_vertex();
		void						destroy_vertex(Vertex*& f);

		bool										b_r_vertices();
		bool										vert_construct_register() { return !b_r_vertices(); }


		base_lighting&				L_static()		{ return _L_static; }
 		xr_vector<xrMU_Model*>&		mu_models()		{ return _mu_models; }
		xr_vector<xrMU_Reference*>& mu_refs()		{ return _mu_refs; }

 
 		void						initialize		()		;
   
// Clearing Data
		void						clear					();
 		void						mu_models_calc_materials();
};

extern "C" XRLC_LIGHT_API xrLC_GlobalData*	lc_global_data();
extern "C" XRLC_LIGHT_API void				create_global_data();
extern "C" XRLC_LIGHT_API void				destroy_global_data();
extern "C" XRLC_LIGHT_API u32				InvalideFaces();
		   XRLC_LIGHT_API void				ImplicitLighting();

extern xrLC_GlobalData* data;
IC xrLC_GlobalData* inlc_global_data() { return data; }
static const char* gl_data_net_file_name = "tmp_global_data";

XRLC_LIGHT_API base_Face* convert_nax(u32 dummy);
XRLC_LIGHT_API u32 convert_nax(base_Face* F);