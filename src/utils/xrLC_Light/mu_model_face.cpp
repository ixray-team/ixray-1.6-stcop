#include "stdafx.h"
#include "mu_model_face.h"
#include "../../xrCore/xrPool.h"

// POOLS  Для чего ?  

static poolSS<_vertex, 8 * 1024> mu_vertices;
static poolSS<_face,   8 * 1024> mu_faces;

poolSS<_vertex,8*1024>	&mu_vertices_pool();
poolSS<_face,8*1024>	&mu_faces_pool();

Tface<data_vertex>::Tface()
{}

Tvertex<data_vertex>::Tvertex()
{}

_vertex* _vertex::CreateCopy_NOADJ(v_vertices& vertises_storage) const
{
	_vertex* V = mu_vertices_pool().create();
	vertises_storage.push_back(V);
	V->P.set(P);
	V->N.set(N);
	V->C = C;
	return V;
}

template<>
Tface<data_vertex>::~Tface()
{}
template<>
Tvertex<data_vertex>::~Tvertex()
{}

void _face::Failure()
{
}

poolSS<_vertex,8*1024> &mu_vertices_pool()
{
	return mu_vertices;
}
poolSS<_face,8*1024> &mu_faces_pool()
{
	return mu_faces;
}

void mu_mesh_clear()
{
	mu_vertices.clear();
	mu_faces.clear();
}