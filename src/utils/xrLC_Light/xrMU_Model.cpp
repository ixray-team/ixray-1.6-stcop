#include "stdafx.h"
#include "xrMU_Model.h"

#include "mu_model_face.h"
#include "vector_clear.h"
#include "../../xrCore/xrPool.h"

xrMU_Model::xrMU_Model() 
{
}

xrMU_Model::~xrMU_Model()
{
  	clear_mesh	();
}

poolSS<Vertex,8*1024>	&mu_vertices_pool();
poolSS<Face,8*1024>	&mu_faces_pool();

static struct destruct_vertex_not_uregister
{
	static void destruct (Vertex * &v)
	{
		mu_vertices_pool().destroy( v );
	}
} _destruct_vertex;

static struct destruct_face_not_uregister
{
	static void destruct (Face * &f)
	{
		mu_faces_pool().destroy( f );
	}
} _destruct_face;

#include "../xrLC/OGF_Face.h"
void xrMU_Model::clear_mesh			()
{
	vec_clear( m_vertices, _destruct_vertex ); 
	vec_clear( m_faces, _destruct_face );
	color.clear();
	color.shrink_to_fit();

 	m_subdivs.clear();
	m_subdivs.shrink_to_fit();
}
  
u32	xrMU_Model::find( const Vertex *v ) const
{
 	v_vertices::const_iterator i = std::find( m_vertices.begin(), m_vertices.end(), v );
	if( i== m_vertices.end() )
		return u32(-1);
	return u32(i - m_vertices.begin());
}

u32	xrMU_Model::find( const Face *f ) const
{
	 v_faces::const_iterator i = std::find( m_faces.begin(), m_faces.end(), f ) ;
	 if(i== m_faces.end())
		return u32(-1);
	 return u32(i - m_faces.begin());
} 



 