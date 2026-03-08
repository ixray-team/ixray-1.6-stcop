#pragma once
#include "../xrRender/light.h"

struct	CLightR_Vertex
{
	Fvector			P;
	Fvector			N;
	float			u0,v0;
	float			u1,v1;
};

class	CLightR_Manager
{
	CDB::COLLIDER					xrc;
	xr_vector<light*>				selected_point;
	xr_vector<light*>				selected_spot;
public:
	CLightR_Manager					();
	virtual ~CLightR_Manager		();

	void			add				(light* L);
	void			render			(u32 _priority);
	void			render_point	(u32 _priority);
	void			render_spot		(u32 _priority);
};

class cl_light_PR		: public RHIShaderConstant::Setup {	virtual void setup(RHIShaderConstant* C);	};
class cl_light_C		: public RHIShaderConstant::Setup {	virtual void setup(RHIShaderConstant* C);	};
class cl_light_XFORM	: public RHIShaderConstant::Setup {	virtual void setup(RHIShaderConstant* C);	};