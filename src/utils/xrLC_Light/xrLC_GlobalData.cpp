#include "stdafx.h"

#include "xrLC_GlobalData.h"
#include "xrFace.h"
#include "xrDeflector.h"
#include "Lightmap.h"
#include "mu_model_face.h"
#include "xrMU_Model.h"
#include "xrMU_Model_Reference.h"
#include "../../xrCore/Collision/xrCDB.h"

bool g_using_smooth_groups = true;
bool g_smooth_groups_by_faces = false;

xrLC_GlobalData* data =0;

xrLC_GlobalData*	lc_global_data()
{
	return data;
}

void	create_global_data()
{
	VERIFY( !inlc_global_data() );
	data = new xrLC_GlobalData();
}
void	destroy_global_data()
{
	VERIFY( inlc_global_data() );
	if(data)
		data->clear();
	xr_delete(data);
}


xrLC_GlobalData::xrLC_GlobalData()
{
	
}
 
void xrLC_GlobalData::initialize()
{
}


xrSRWLock NaxGuard;

XRLC_LIGHT_API base_Face* convert_nax(u32 dummy)
{
	xrSRWLockGuard guard(NaxGuard, true);
 	return lc_global_data()->FacesStorage[dummy];
}

XRLC_LIGHT_API u32 convert_nax(base_Face* F)
{
	xrSRWLockGuard guard(NaxGuard);
 	lc_global_data()->FacesStorage.push_back(F);
	return lc_global_data()->FacesStorage.size() - 1;
}
 
void	xrLC_GlobalData::mu_models_calc_materials()
{
	for (u32 m=0; m<mu_models().size(); m++)
			mu_models()[m]->calc_materials();

}
  
bool	xrLC_GlobalData	::			b_r_vertices	()		
{
	return false;
}
 
xrLC_GlobalData::~xrLC_GlobalData()
{
 
}
 
template<typename T>
void vec_clear( xr_vector<T*> &v )
{
	typename xr_vector<T*>::iterator i = v.begin(), e = v.end();
	for(;i!=e;++i)
		xr_delete(*i);
	v.clear();
 	v.shrink_to_fit();
}
 
template<typename T>
void vec_free(xr_vector<T*>& v)
{
	typename xr_vector<T*>::iterator i = v.begin(), e = v.end();
	for (; i != e; ++i)
		xr_free(*i);

 	v.clear();
	v.shrink_to_fit();
}

#include "../xrLC/Build.h"
 
void mu_mesh_clear();

// create - destroy 
Face* xrLC_GlobalData::create_face()
{
	return new Face();
}

void xrLC_GlobalData::destroy_face(Face*& f)
{
	xr_delete(f);
}

Vertex* xrLC_GlobalData::create_vertex()
{
	return new Vertex();
}

void xrLC_GlobalData::destroy_vertex(Vertex*& v)
{
	return xr_delete(v);
}

void xrLC_GlobalData::clear() 
{
	FacesStorage.clear();
	FacesStorage.shrink_to_fit();

	// se7kills (Проверил это отгружается хорошо !)
	for (auto& surface : textures())
		surface.pSurface.Clear();
 	textures().clear();
	textures().shrink_to_fit();

 	_materials.clear();
	_shaders.Unload();
	clMsg("[xrLC_Remove] mem textures: %u mb", GetHeapMemory() / 1024 / 1024);

	// Пометка чтобы не трогало векторы (_g_faces, _g_vertex) в деструкторе !
	g_bUnregister = false;
 	
	for (auto F : _g_faces)
	{
		F->~Tface();
		xr_free(F);
	}
	_g_faces.clear();
	_g_faces.shrink_to_fit();
 
	for (auto V : _g_vertices)
	{
		V->~Tvertex();
		xr_free(V);
	}
 	_g_vertices.clear();
	_g_vertices.shrink_to_fit();
	
	clMsg("[xrLC_Remove] mem faces-vertex: %u mb", GetHeapMemory() / 1024 / 1024);
 
	// Не замечал утечек памяти !
	vec_clear(_mu_models); 
	vec_clear(_mu_refs);
	mu_mesh_clear();

	clMsg("[xrLC_Remove] mem mu-models: %u mb", GetHeapMemory() / 1024 / 1024);


	// Lighting stuff
	for (auto D : _g_deflectors)
		D->~CDeflector();
 	vec_free(_g_deflectors);

	vec_clear(_g_lightmaps);
	clMsg("[xrLC_Remove] mem defl-lmaps: %u mb", GetHeapMemory() / 1024 / 1024);
}
 
