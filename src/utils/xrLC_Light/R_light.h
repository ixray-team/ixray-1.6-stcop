#pragma once

#define LT_DIRECT		0
#define LT_POINT		1
#define LT_SECONDARY	2

//On CUDA device we use different vector type, so there is a workaround
#ifndef VECTORWORKAROUND_DEFINE
#define VECTORWORKAROUND_DEFINE

#ifdef __CUDACC__
typedef HardwareVector VectorType;
#else
typedef Fvector VectorType;
#endif // end __CUDACC__

#endif //end VECTORWORKAROUND_DEFINE

struct R_Light
{
	u16				type;				// Type of light source		
	u16				level;				// GI level
	VectorType      diffuse;			// Diffuse color of light	
	VectorType      position;			// Position in world space	
	VectorType      direction;			// Direction in world space	
	float		    range;				// Cutoff range
	float			range2;				// ^2
	float			falloff;			// precalc to make light aqal to zero at light range
	float	        attenuation0;		// Constant attenuation		
	float	        attenuation1;		// Linear attenuation		
	float	        attenuation2;		// Quadratic attenuation	
	float			energy;				// For radiosity ONLY

	VectorType		tri[3];

#ifndef __CUDACC__
	R_Light() {
		tri[0].set(0, 0, 0);
		tri[1].set(0, 0, EPS_S);
		tri[2].set(EPS_S, 0, 0);
	}
#endif
};
