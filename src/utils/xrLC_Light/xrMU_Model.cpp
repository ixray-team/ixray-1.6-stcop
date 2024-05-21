#include "stdafx.h"
#include "xrmu_model.h"

#include "mu_model_face.h"
#include "vector_clear.h"
#include "../../xrcore/xrPool.h"

xrMU_Model::xrMU_Model() : m_lod_ID(u16(-1)) 
{
}

xrMU_Model::~xrMU_Model()
{
  	clear_mesh	();
}


poolSS<_vertex,8*1024>	&mu_vertices_pool();
poolSS<_face,8*1024>	&mu_faces_pool();

static struct destruct_vertex_not_uregister
{
	static void destruct (_vertex * &v)
	{
		mu_vertices_pool().destroy( v );
	}
} _destruct_vertex;

static struct destruct_face_not_uregister
{
	static void destruct (_face * &f)
	{
		mu_faces_pool().destroy( f );
	}
} _destruct_face;

void xrMU_Model::clear_mesh			()
{
	vec_clear( m_vertices, _destruct_vertex ); 
	vec_clear( m_faces, _destruct_face );
}
 

u32	xrMU_Model::find( const _vertex *v ) const
{
 	v_vertices::const_iterator i = std::find( m_vertices.begin(), m_vertices.end(), v );
	if( i== m_vertices.end() )
		return u32(-1);
	return u32(i - m_vertices.begin());
}

u32	xrMU_Model::find( const _face *f ) const
{
	 v_faces::const_iterator i = std::find( m_faces.begin(), m_faces.end(), f ) ;
	 if(i== m_faces.end())
		return u32(-1);
	 return u32(i - m_faces.begin());
} 



 