#pragma once

#include "R_light.h"

#pragma pack(push,4)
class XRLC_LIGHT_API base_lighting
{
public:
	xr_vector<R_Light>		rgb;		// P,N	
	xr_vector<R_Light>		hemi;		// P,N	
	xr_vector<R_Light>		sun;		// P

	void					select		(xr_vector<R_Light>& dest, xr_vector<R_Light>& src, Fvector& P, float R);
	void					select		(base_lighting& from, Fvector& P, float R);
	void	clear() 
	{ 
		rgb.clear(); hemi.clear(); sun.clear();
		rgb.shrink_to_fit(); hemi.shrink_to_fit(); sun.shrink_to_fit();
	}

 
};
#pragma pack(pop)