#pragma once

// Actually this must be < sqrt(MAXFLOAT) since we store this value squared.
#define P_MAXFLOAT	1.0e16f

#ifdef MAXINT
	#define P_MAXINT	MAXINT
#else
	#define P_MAXINT	0x7fffffff
#endif

#define drand48()		::Random.randF()

namespace PAPI{

	// A single particle
	struct Rotation
	{
		float		x;
		ICF void set(float _x)				{ x = _x;};
		ICF void set(const Rotation& rot_)  { x = rot_.x;};
		ICF	void inertion(const Rotation &p, float v) 
		{
			float inv = 1.f-v;
			x = v*x + inv*p.x;
		};
	};
	struct Particle
	{
		enum{
			ANIMATE_CCW	= (1<<0),
		};

		Fvector		pos;	
		Fvector		posB;   
		Fvector		posI;   
		Fvector		vel;     	
		Fvector		velI;
		Fvector		rot_vel;
		Fvector		rot_velS;
		Fvector		size;   
		Fvector		sizeI;  
		Rotation	rot;	
		Rotation	rotI;	
		u32			color;	
		float		age;	      
		u16			frame;	
		Flags16		flags;	
	};                  	

	typedef void (* OnBirthParticleCB)	(void* owner, u32 param, PAPI::Particle& P, u32 idx);
	typedef void (* OnDeadParticleCB)	(void* owner, u32 param, PAPI::Particle& P, u32 idx);
	//////////////////////////////////////////////////////////////////////
	// Type codes for domains
	enum PDomainEnum
	{
		PDPoint 	= 0,	// Single point
		PDLine 		= 1,	// Line segment
		PDTriangle 	= 2,	// Triangle
		PDPlane 	= 3,	// Arbitrarily-oriented plane
		PDBox 		= 4,	// Axis-aligned box
		PDSphere 	= 5,	// Sphere
		PDCylinder 	= 6,	// Cylinder
		PDCone 		= 7,	// Cone
		PDBlob 		= 8,	// Gaussian blob
		PDDisc 		= 9,	// Arbitrarily-oriented disc
		PDRectangle = 10,	// Rhombus-shaped planar region
		domain_enum_force_dword = u32(-1)
	};
};
