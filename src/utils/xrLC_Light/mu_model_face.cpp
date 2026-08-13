#include "stdafx.h"
#include "mu_model_face.h"
#include "xrCore/xrPool.h"

// POOLS  Для чего ?  

static poolSS<Vertex, 8 * 1024> mu_vertices;
static poolSS<Face,   8 * 1024> mu_faces;

poolSS<Vertex,8*1024> &mu_vertices_pool()
{
	return mu_vertices;
}
poolSS<Face,8*1024> &mu_faces_pool()
{
	return mu_faces;
}

void mu_mesh_clear()
{
	mu_vertices.clear();
	mu_faces.clear();
}