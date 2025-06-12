#pragma once

#include "../Shader_xrLC.h"
#include "../../xrCore/xrPool.h"
//#include "xrface.h"
#include "xrFaceDefs.h"
#include "xrDeflectorDefs.h"
#include "b_build_texture.h"
#include "base_lighting.h"
#include "../../Include/Editors/communicate.h"
//#include "mu_model_face.h"
//#include "mu_model_face_defs.h"
//struct _face;
//struct _vertex;
namespace CDB{
class MODEL;
class CollectorPacked;
};
class CLightmap;
class xrMU_Model;
class xrMU_Reference;
class base_Vertex;
class base_Face;

struct	compilers_global_data
{
		xr_vector<b_BuildTexture>		_textures;
		xr_vector<b_material>			_materials;
		Shader_xrLC_LIB					_shaders;
		b_params						_g_params;
		base_lighting					_L_static;
		CDB::MODEL*						_RCAST_Model;
};

extern XRLC_LIGHT_API void		setLMSIZE(int size);

class	XRLC_LIGHT_API xrLC_GlobalData
{
	
		compilers_global_data			_cl_globs;

		CMemoryWriter					_err_invalid;
		CMemoryWriter					_err_multiedge;
		CMemoryWriter					_err_tjunction;
		xr_vector<CLightmap*>			_g_lightmaps;
		xr_vector<xrMU_Model*>			_mu_models;
		xr_vector<xrMU_Reference*>		_mu_refs;
		vecVertex						_g_vertices;
		vecFace							_g_faces;
		vecDefl							_g_deflectors;


		bool							_b_nosun;
		bool							_gl_linear;
		bool							_b_use_intel;
		bool							_b_skipWeld;
		bool							_b_use_lmaps_build_alt;

private:
	bool _skipInvalid;
	bool _skipTesselate;
	bool _lmapRGBA;
	bool _skipSubdivide;
	u32 JSampleMU;
	bool _OverrideSettings;

private:
		bool							b_vert_not_register;
public:

public:
									xrLC_GlobalData	();//:_RCAST_Model (0), _b_nosun(false),_gl_linear(false){}
									~xrLC_GlobalData();
		IC xr_vector<b_BuildTexture>& textures		()		{	return _cl_globs._textures; }
		IC xr_vector<CLightmap*>	& lightmaps		()		{	return _g_lightmaps; }
		IC xr_vector<b_material>	& materials		()		{	return _cl_globs._materials; }
		IC Shader_xrLC_LIB			& shaders		()		{	return _cl_globs._shaders; }
		IC CMemoryWriter			&err_invalid	()		{	return _err_invalid; }
		IC CMemoryWriter			&err_multiedge	()		{ return _err_multiedge;  };
		IC CMemoryWriter			&err_tjunction	()		{ return _err_tjunction;  };
		IC b_params					&g_params		()		{	return _cl_globs._g_params; }
			
		Face						*create_face	()		;
		void						destroy_face	(Face* &f );

		Vertex						*create_vertex	()		;
		void						destroy_vertex	(Vertex* &f );

		void						vertices_isolate_and_pool_reload();

		vecVertex					&g_vertices		()		{	return	_g_vertices; }
		vecFace						&g_faces		()		{	return	_g_faces; }
		vecDefl						&g_deflectors	()		{	return	_g_deflectors; }
		bool						b_r_vertices	()		;
		bool						vert_construct_register(){return !b_r_vertices() && !b_vert_not_register; }

 
		base_lighting				&L_static		()		{	return _cl_globs._L_static; }
		CDB::MODEL*					RCAST_Model		()		{	return _cl_globs._RCAST_Model; }
		xr_vector<xrMU_Model*>		&mu_models		()		{	return _mu_models; }
		xr_vector<xrMU_Reference*>	&mu_refs		()		{	return _mu_refs; }


		shared_str					level_name;

		void						SetLevelName(LPCSTR name) { level_name = name; }
		LPCSTR						GetLavelName() { return level_name.c_str(); }

		bool						b_nosun			()		{	return _b_nosun; }
		bool						gl_linear		()		{	return _gl_linear; }
IC		void						b_nosun_set		(bool v){	_b_nosun = v; }
		void						initialize		()		;
		void						destroy_rcmodel	()		;

		void						create_rcmodel	(CDB::CollectorPacked& CL);

		void						clear_build_textures_surface();
		
		void						clear_build_textures_surface( const xr_vector<u32> &exept );

		void						set_faces_indexses		();
		void						set_vertices_indexses	();
 
		void						gl_mesh_clear			()		;
 
public:
	  
		void						clear					();
		void						clear_mesh				();
		void						clear_mu_models			();	
		void						mu_models_calc_materials();
		 

public:
	bool GetSkipInvalid() { return _skipInvalid; }
	void SetSkipInvalid(bool skipInvalid) { _skipInvalid = skipInvalid; }

	bool GetSkipTesselate() { return _skipTesselate; }
	void SetSkipTesselate(bool skipTesselate) { _skipTesselate = skipTesselate; }

	bool GetLmapRGBA() { return _lmapRGBA; }
	void SetLmapRGBA(bool lmapRGBA) { _lmapRGBA = lmapRGBA; }

	bool GetSkipSubdivide() { return _skipSubdivide; }
	void SetSkipSubdivide(bool skipSubdivide) { _skipSubdivide = skipSubdivide; }

	bool GetIsIntelUse() {return _b_use_intel;}
	void SetIsIntelUse(bool use_intel) { _b_use_intel = use_intel; }
 
	bool GetSkipWeld() { return _b_skipWeld; }
	void SetSkipWeld(bool value) { _b_skipWeld = value; }

	
	void SetLmapsSize(u32 size) { setLMSIZE(size); }

	void SetJitterMU(u32 size) { JSampleMU = size; }
	u32 GetJitterMU() { return JSampleMU; }

	void SetOverrideSettings(bool value) 
	{ _OverrideSettings = value; }

	bool GetOverrideSettings() 
	{  return _OverrideSettings; }


};

extern "C" XRLC_LIGHT_API xrLC_GlobalData*	lc_global_data();
extern "C" XRLC_LIGHT_API void				create_global_data();
extern "C" XRLC_LIGHT_API void				destroy_global_data();
extern "C" XRLC_LIGHT_API u32				InvalideFaces();
		   XRLC_LIGHT_API void				ImplicitLighting();

extern xrLC_GlobalData* data;
IC xrLC_GlobalData* inlc_global_data() { return data; }
static LPCSTR gl_data_net_file_name = "tmp_global_data";

XRLC_LIGHT_API base_Face* convert_nax(u32 dummy);
XRLC_LIGHT_API u32 convert_nax(base_Face* F);