#include "stdafx.h"
#include "../xrLC/Build.h"

#include "xrLC_GlobalData.h"

#include "xrFace.h"
#include "vector_clear.h"


typedef poolSS<Vertex, 16 * 1024>	poolVertices;
typedef poolSS<Face, 16 * 1024>		poolFaces;
static poolVertices	_VertexPool;
static poolFaces	_FacePool;

xrCriticalSection csDelete;

Face* xrLC_GlobalData::create_face()
{
	return _FacePool.create();
}

void xrLC_GlobalData::destroy_face(Face*& f)
{
 	_FacePool.destroy(f);
}

Vertex* xrLC_GlobalData::create_vertex()
{
 	//return _VertexPool.create();
	return new Vertex();
}

void xrLC_GlobalData::destroy_vertex(Vertex*& f)
{
 	xr_delete(f);
  	//_VertexPool.destroy(f);
}

void xrLC_GlobalData::vertexes_allocated(size_t& mem, size_t& VertexCount)
{
//	mem			=	_VertexPool.used_memory;
//	VertexCount =   _VertexPool.used_objects;
}
  
void	xrLC_GlobalData::faces_allocated(size_t& mem, size_t& FacesCount)
{
//	mem			= _FacePool.used_memory;
//	FacesCount	= _FacePool.used_objects;
}

// Face* xrLC_GlobalData	::create_face	()		
// {
// 	return new Face();
// }
// 
// void xrLC_GlobalData	::destroy_face	(Face* &f)
// {
// 	xr_delete(f); 
// }
// 
// Vertex* xrLC_GlobalData	::create_vertex	()		
// {
// 	return new Vertex();
// }
// void xrLC_GlobalData	::destroy_vertex	(Vertex* &f)
// {
// 	xr_delete(f); 
// }

static struct destruct_vertex_not_uregister
{
	static void destruct (Vertex * &v)
	{
		::destroy_vertex( v, false );
	}
} _destruct_vertex_not_uregister;

static struct destruct_face_not_uregister
{
	static void destruct (Face * &f)
	{
		::destroy_face( f, false );
	}
} _destruct_face_not_uregister;

void xrLC_GlobalData	::gl_mesh_clear	()
{
 	_g_vertices.clear();
  	_g_faces.clear();

	_VertexPool.clear();
	_FacePool.clear();
}

/*

//////////////////////////////////////////////////////////////
void	Vertex::isolate_pool_clear_read		( INetReader	&r )
{
	DataVertex::read( r );
	r_pod_vector( r, m_adjacents );
	for(u32 i= 0; i< m_adjacents.size();++i )
	{
		Face &f = *m_adjacents[i];
		int v_i = -1;
		r_pod( r, v_i );
		R_ASSERT( v_i>=0 );
		R_ASSERT( v_i<3 );
		R_ASSERT( f.vertex( v_i ) == 0 );
		f.raw_set_vertex( v_i, this );
	}
}
void	Vertex::isolate_pool_clear_write	( IWriter	&w )const
{
	DataVertex::write( w );
	w_pod_vector( w, m_adjacents );
	for(u32 i= 0; i< m_adjacents.size();++i )
	{
		Face &f = *m_adjacents[i];
		int v_i = f.VIndex( this );
		R_ASSERT( v_i>=0 );
		R_ASSERT( v_i<3 );
		w_pod( w, v_i );
		f.raw_set_vertex( v_i, 0 );
	}
}
*/


void xrLC_GlobalData::vertices_isolate_and_pool_reload()
{
	/*
	// Se7kills Понял для чего
	// Сначала сохраняем все вертексы потом дестроем и загружаем по новой 
	
 	const u32 inital_verts_count = (u32)_g_vertices.size();
	u32 not_empty_verts = 0;

	string_path path_name;
	xr_strconcat(path_name, pBuild->path, "build.vertices");
	{
		IWriter* file = FS.w_open(path_name);
		R_ASSERT(file);
		for (u32 i = 0; i < inital_verts_count; ++i)
		{
			Vertex& v = *_g_vertices[i];
			if (v.m_adjacents.empty())
			{
				::destroy_vertex(_g_vertices[i], false);
				continue;
			}
			
 			v.isolate_pool_clear_write(*file);
			::destroy_vertex(_g_vertices[i], false);
			++not_empty_verts;
		}
		FS.w_close(file);
	}

	_g_vertices.clear();
	clMsg("mem usage before clear pool: %u", Memory.mem_usage());

	_VertexPool.clear();

	{
		b_vert_not_register = true;
		_g_vertices.resize(not_empty_verts, 0);

		Memory.mem_compact();
		clMsg("mem usage after clear pool: %u", Memory.mem_usage());

		INetReaderFile r_verts(path_name);
		for (u32 i = 0; i < not_empty_verts; ++i)
		{
			Vertex*& v = _g_vertices[i];
			v = _VertexPool.create();
			v->isolate_pool_clear_read(r_verts);
		}
		b_vert_not_register = false;
	}
	*/
}


void xrLC_GlobalData::clear_mesh()
{
	clMsg("mem usage before clear mesh: %u", Memory.mem_usage());
	gl_mesh_clear();
	Memory.mem_compact();
	clMsg("mem usage after clear mesh: %u", Memory.mem_usage());
}
