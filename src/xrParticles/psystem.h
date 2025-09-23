#pragma once

#ifdef XR_PARTICLES_EXPORTS
	#define PARTICLES_API __declspec(dllexport)
#else
	#define PARTICLES_API __declspec(dllimport)
#endif

// Actually this must be < sqrt(MAXFLOAT) since we store this value squared.
#define P_MAXFLOAT	1.0e16f

#ifdef MAXINT
	#define P_MAXINT	MAXINT
#else
	#define P_MAXINT	0x7fffffff
#endif

#define drand48()		::Random.randF()
//#define drand48() (((float) rand())/((float) RAND_MAX))

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
	//////////////////////////////////////////////////////////////////////
	// Type codes for all actions
	enum PActionEnum
	{
		PAAvoidID,			// Avoid entering the domain of space.
		PABounceID,			// Bounce particles off a domain of space.
		PACallActionListID_obsolette,	// 
		PACopyVertexBID,	// Set the secondary position from current position.
		PADampingID,		// Dampen particle velocities.
		PAExplosionID,		// An Explosion.
		PAFollowID,			// Accelerate toward the previous particle in the effect.
		PAGravitateID,		// Accelerate each particle toward each other particle.
		PAGravityID,		// Acceleration in the given direction.
		PAJetID,			// 
		PAKillOldID,		// 
		PAMatchVelocityID,	// 
		PAMoveID,			// 
		PAOrbitLineID,		// 
		PAOrbitPointID,		// 
		PARandomAccelID,	// 
		PARandomDisplaceID,	// 
		PARandomVelocityID,	// 
		PARestoreID,		// 
		PASinkID,			// 
		PASinkVelocityID,	// 
		PASourceID,			// 
		PASpeedLimitID,		// 
		PATargetColorID,	// 
		PATargetSizeID,		// 
		PATargetRotateID,	// 
		PATargetRotateDID,	// 
		PATargetVelocityID,	// 
		PATargetVelocityDID,// 
		PAVortexID,			// 
        PATurbulenceID,     //
        PAScatterID, 	    //
		// Binders
		PABindVelocityValueID,
		PABindRotationValueID,
		PABindSizeValueID,
		PABindColorValueID,
		PABindColorAlphaID,
		// Animators
		PAColorAnimatorID,
		PASizeAnimatorID,
		PAVelocityAnimatorID,
		PAVelocityRotationAnimatorID,
		action_enum_force_dword = u32(-1)
	};
    struct ParticleAction;

    class IParticleManager
	{
    public:
									IParticleManager()  = default;
		virtual						~IParticleManager() = default;

		// create&destroy
		virtual int					CreateEffect		(u32 max_particles)=0;
		virtual void				DestroyEffect		(int effect_id)=0;
		virtual int					CreateActionList	()=0;
		virtual void				DestroyActionList	(int alist_id)=0;

        // control
        virtual void				PlayEffect			(int effect_id, int alist_id)=0;
        virtual void				StopEffect			(int effect_id, int alist_id, BOOL deffered=TRUE)=0;

        // update&render
        virtual void				Update				(int effect_id, int alist_id, float dt)=0;
        virtual void				Transform			(int alist_id, const Fmatrix& m, const Fvector& velocity)=0;

        // effect
        virtual void				RemoveParticle		(int effect_id, u32 p_id)=0;
        virtual void				SetMaxParticles		(int effect_id, u32 max_particles)=0;
        virtual void				SetCallback			(int effect_id, OnBirthParticleCB b, OnDeadParticleCB d, void* owner, u32 param)=0;
    	virtual void				GetParticles		(int effect_id, Particle*& particles, u32& cnt)=0;
    	virtual u32					GetParticlesCount	(int effect_id)=0;
        
        // action
        virtual ParticleAction*		CreateAction		(PActionEnum type)=0;
        virtual u32					LoadActions			(int alist_id, IReader& R)=0;
        virtual void				SaveActions			(int alist_id, IWriter& W)=0;

    	virtual ParticleAction*		FindAction(int alist_id, PActionEnum type)=0;
    };

    PARTICLES_API IParticleManager* ParticleManager		();
};
