#pragma once

#include "../xrRender/light.h"
#include "../xrRender/light_package.h"

class	CLight_DB
{
private:
	xr_vector<ref_light>	v_static;
	xr_vector<ref_light>	v_hemi;
public:
	ref_light				sun_original;
	ref_light				sun_adapted;
	light_Package			package;
public:
	void					add_light			(light*		L	);

	void					Load				(IReader*	fs	);
#if RENDER != R_R1
	void					LoadHemi			();
#endif
	void					Unload				();

	light*					Create				();
	void					Update				();

	CLight_DB				();
	~CLight_DB				();
};

#define LT_DIRECT		0
#define LT_POINT		1
#define LT_SECONDARY	2

struct R_Light
{
	u16				type;				// Type of light source		
	u16				level;				// GI level
	Fvector         diffuse;			// Diffuse color of light	
	Fvector         position;			// Position in world space	
	Fvector         direction;			// Direction in world space	
	float		    range;				// Cutoff range
	float			range2;				// ^2
	float			falloff;			// precalc to make light aqal to zero at light range
	float	        attenuation0;		// Constant attenuation		
	float	        attenuation1;		// Linear attenuation		
	float	        attenuation2;		// Quadratic attenuation	
	float			energy;				// For radiosity ONLY

	Fvector			tri[3];

	R_Light() {
		tri[0].set(0, 0, 0);
		tri[1].set(0, 0, EPS_S);
		tri[2].set(EPS_S, 0, 0);
	}
};