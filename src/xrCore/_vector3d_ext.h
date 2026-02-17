#pragma once
#undef min
#undef max
#include "_vector3d.h"

ICF Fvector   cr_fvector3 (float f)
{
	Fvector		res		=	{ f, f, f };
	return		res;
}

ICF Fvector   cr_fvector3 (float x, float y, float z)
{
	Fvector		res		=	{ x, y, z };
	return		res;
}

ICF Fvector   cr_fvector3_hp (float h, float p)
{
	Fvector		res;
	res.setHP	(h, p);
	return		res;
}

/*inline
Fvector   operator + (const Fvector& v1, const Fvector& v2)
{
	return		cr_fvector3 (v1.x+v2.x, v1.y+v2.y, v1.z+v2.z);
}

inline
Fvector   operator - (const Fvector& v1, const Fvector& v2)
{
	return		cr_fvector3 (v1.x-v2.x, v1.y-v2.y, v1.z-v2.z);
}*/

ICF Fvector   operator - (const Fvector& v)
{
	return		cr_fvector3 (-v.x, -v.y, -v.z);
}

/*inline
Fvector   operator * (const Fvector& v, float f)
{
	return		cr_fvector3 (v.x*f, v.y*f, v.z*f);
}*/

ICF Fvector   operator * (float f, const Fvector& v)
{
	return		cr_fvector3 (v.x*f, v.y*f, v.z*f);
}

/*inline
Fvector   operator / (const Fvector& v, float f)
{
	const float	repr_f	=	1.f / f;
	return		cr_fvector3 (v.x*repr_f, v.y*repr_f, v.z*repr_f);
}*/

ICF Fvector   _min (const Fvector& v1, const Fvector& v2)
{
	Fvector		r;
	r.min		(v1, v2);
	return		r;
}

ICF Fvector   _max (const Fvector& v1, const Fvector& v2)
{
	Fvector		r;
	r.max		(v1, v2);
	return		r;
}

ICF Fvector   _abs (const Fvector& v)
{
	Fvector		r;
	r.abs		(v);
	return		r;
}

ICF Fvector   normalize (const Fvector& v)
{
	Fvector		r(v);
	r.normalize	();
	return		r;
}

ICF float   magnitude (const Fvector& v)
{
	return		v.magnitude();
}

ICF float   sqaure_magnitude (const Fvector& v)
{
	return		v.square_magnitude();
}

ICF float	dotproduct (const Fvector& v1, const Fvector& v2)
{	
	return		v1.x*v2.x + v1.y*v2.y + v1.z*v2.z;
}

// CrossProduct
ICF Fvector   crossproduct (const Fvector& v1, const Fvector& v2)
{
	Fvector r;
	r.crossproduct(v1, v2);
	return r;
}

ICF Fvector   cr_vectorHP (float h, float p)
{
	float ch = std::cos(h), cp= std::cos(p), sh= std::sin(h), sp= std::sin(p);
	Fvector r;
	r.x = -cp*sh;
	r.y = sp;
	r.z = cp*ch;
	return r;
}

ICF float angle_between_vectors (Fvector const v1, Fvector const v2)
{
	float const mag1	=	v1.magnitude();
	float const mag2	=	v2.magnitude();
	float const epsilon	=	(float) 1e-6;
	if ( mag1 < epsilon || mag2 < epsilon )
	{
		return 0.f;
	}

	float angle_cos		=	dotproduct(v1, v2) / (mag1*mag2);
	if ( angle_cos < -1.f )
	{
		angle_cos		=	-1.f;
	}
	else if ( angle_cos > +1.f )
	{
		angle_cos		=	+1.f;
	}
	return					acosf(angle_cos);
}

ICF Fvector rotate_point(Fvector const& point, float const angle)
{
	float const cos_alpha = std::cos(angle);
	float const sin_alpha = std::sin(angle);

	return Fvector().set(point.x * cos_alpha - point.z * sin_alpha, 0, point.x * sin_alpha + point.z * cos_alpha);
}