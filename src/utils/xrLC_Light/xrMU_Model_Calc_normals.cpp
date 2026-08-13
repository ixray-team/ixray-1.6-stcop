#include "stdafx.h"
//#include "build.h"
#include "mu_model_face.h"
#include "calculate_normals.h"
#include "xrMU_Model.h"

#include "../../xrCore/xrPool.h"

poolSS<Vertex,8*1024>	&mu_vertices_pool();

void destroy_vertex_mu( Vertex* &v, bool unregister )
{
	mu_vertices_pool().destroy(v);
	v = nullptr;
}

void calc_normals( xrMU_Model &model )
{
	calculate_normals<Vertex, true>::calc_normals( model.m_vertices, model.m_faces );
}
