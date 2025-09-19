#pragma once

#include "base_face.h"
#include "MeshStructure.h"

struct XRLC_LIGHT_API data_face :	public base_Face
{
public:
	Fvector2 tc[3];
	Fvector N;
	u32 sm_group;

public:
	virtual Fvector2* getTC0() { return tc; };
	data_face() { sm_group = 0; };
	virtual ~data_face() {};
};

struct XRLC_LIGHT_API data_vertex :	public base_Vertex
{
	using DataFaceType = data_face;
public:
	data_vertex()			{ };
	virtual ~data_vertex()	{ };
};

using _vertex = Tvertex<data_vertex>;
using _face = Tface<data_vertex>;