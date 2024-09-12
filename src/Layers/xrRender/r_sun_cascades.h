#pragma once

namespace sun
{

struct ray
{
	ray( ) { }
	ray( Fvector3 const& _P, Fvector3 const& _D ):	P(_P), D(_D) { }

	Fvector3 D;
	Fvector3 P;
};

struct cascade 
{
	cascade () : reset_chain( false )	{}

	Fmatrix			xform;
	Fmatrix			view_proj;
	CFrustum frustum;
	Fvector3 cop;
	xr_vector<ray>	rays;
	float			size;
	float			bias;
	bool			reset_chain;
};

} //namespace sun