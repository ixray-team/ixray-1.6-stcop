#ifndef _MU_MODEL_FACE_
#define _MU_MODEL_FACE_
 

#include "base_face.h"
#include "MeshStructure.h"
#include "mu_model_face_defs.h"

struct	XRLC_LIGHT_API data_face	: public base_Face
{
public:
	//_vertex*	v	[3];
	Fvector2	tc	[3];
	Fvector		N;
	u32			sm_group;
public:
	virtual Fvector2*	getTC0				( ) { return tc; };
	data_face()				{ sm_group = 0;};
	virtual ~data_face()	{ };
};


struct	XRLC_LIGHT_API data_vertex	: public base_Vertex
{
	typedef		data_face			DataFaceType;
public:
	data_vertex()			{ };
	virtual ~data_vertex()	{ };
};

#endif