#pragma once
#include "r__dsgraph_types.h"
#include "r__sector.h"
namespace sun
{

struct ray
{
    ray() = default;
	ray( Fvector3 const& _P, Fvector3 const& _D ):	P(_P), D(_D) { }
    ~ray() = default;
	Fvector3 D;
	Fvector3 P;
};

struct cascade 
{
    cascade() : size(0.0f), bias(0.0f), reset_chain(false) {}
    cascade(const cascade& other){}

	Fmatrix			xform;
	xr_vector<ray>	rays;
	float			size;
	float			bias;
	bool			reset_chain;

	CFrustum			cull_frustum;
	Fvector3			cull_COP;
	Fmatrix				cull_xform;

	CDSGraphManager GMCascade = CDSGraphManager(u32(0), u32(STYPE_RENDERABLE + STYPE_RENDERABLESHADOW), { true,false,false,false,true,false,false });
};

} //namespace sun