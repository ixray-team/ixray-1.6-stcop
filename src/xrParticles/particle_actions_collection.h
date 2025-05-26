//---------------------------------------------------------------------------
#ifndef particle_actions_collectionH
#define particle_actions_collectionH

#include "particle_actions.h"
#include "particle_core.h"
namespace PAPI{
#define _METHODS	virtual void 	Load		(IReader& F);\
                    virtual void 	Save		(IWriter& F);\
                    virtual void 	Execute		(ParticleEffect *pe, const float dt, float& m_max);\
					virtual void 	Transform	(const Fmatrix& m);\
					protected:\
					void* GetVariableImpl(u8 VarID) override;

	struct PARTICLES_API PAAvoid : public ParticleAction
	{
		enum class EVariable : u8
		{
			position,
			look_ahead,
			magnitude,
			epsilon
		};
		pDomain positionL;	// Avoid region (in local space)
		pDomain position;	// Avoid region
		float look_ahead;	// how many time units ahead to look
		float magnitude;	// what percent of the way to go each time
		float epsilon;		// add to r^2 for softening

        _METHODS;
	};

	struct PARTICLES_API PABounce : public ParticleAction
	{
		enum class EVariable : u8
		{
			position,
			oneMinusFriction,
			resilience,
			cutoffSqr
		};
		pDomain positionL;	// Bounce region (in local space)
		pDomain position;	// Bounce region
		float oneMinusFriction;	// Friction tangent to surface
		float resilience;	// Resilence perpendicular to surface
		float cutoffSqr;	// cutoff velocity; friction applies iff v > cutoff

        _METHODS;
	};

	struct PARTICLES_API PACopyVertexB : public ParticleAction
	{
		enum class EVariable : u8
		{
			copy_pos
		};
		BOOL copy_pos;		// True to copy pos to posB.

        _METHODS;
	};

	struct PARTICLES_API PADamping : public ParticleAction
	{
		enum class EVariable : u8
		{
			damping,
			vlowSqr,
			vhighSqr
		};
		pVector damping;	// Damping constant applied to velocity
		float vlowSqr;		// Low and high cutoff velocities
		float vhighSqr;

        _METHODS;
	};

	struct PARTICLES_API PAExplosion : public ParticleAction
	{
		enum class EVariable : u8
		{
			center,
			velocity,
			magnitude,
			stdev,
			age,
			epsilon
		};
		pVector centerL;	// The center of the explosion (in local space)
		pVector center;		// The center of the explosion
		float velocity;		// Of shock wave
		float magnitude;	// At unit radius
		float stdev;		// Sharpness or width of shock wave
		float age;			// How long it's been going on
		float epsilon;		// Softening parameter

        _METHODS;
	};

	struct PARTICLES_API PAFollow : public ParticleAction
	{
		enum class EVariable : u8
		{
			magnitude,
			epsilon,
			max_radius
		};
		float magnitude;	// The grav of each particle
		float epsilon;		// Softening parameter
		float max_radius;	// Only influence particles within max_radius

        _METHODS;
	};

	struct PARTICLES_API PAGravitate : public ParticleAction
	{
		enum class EVariable : u8
		{
			magnitude,
			epsilon,
			max_radius
		};
		float magnitude;	// The grav of each particle
		float epsilon;		// Softening parameter
		float max_radius;	// Only influence particles within max_radius

        _METHODS;
	};

	struct PARTICLES_API PAGravity : public ParticleAction
	{
		enum class EVariable : u8
		{
			direction
		};
		pVector directionL;	// Amount to increment velocity (in local space)
		pVector direction;	// Amount to increment velocity

        _METHODS;
	};

	struct PARTICLES_API PAJet : public ParticleAction
	{
		enum class EVariable : u8
		{
			center,
			acc,
			magnitude,
			epsilon,
			max_radius
		};
		pVector	centerL;	// Center of the fan (in local space)
		pDomain accL;		// Acceleration vector domain  (in local space)
		pVector	center;		// Center of the fan
		pDomain acc;		// Acceleration vector domain
		float magnitude;	// Scales acceleration
		float epsilon;		// Softening parameter
		float max_radius;	// Only influence particles within max_radius

        _METHODS;
	};

	struct PARTICLES_API PAKillOld : public ParticleAction
	{
		enum class EVariable : u8
		{
			age_limit,
			kill_less_than
		};
    	float age_limit;		// Exact age at which to kill particles.
		BOOL kill_less_than;	// True to kill particles less than limit.

        _METHODS;
	};

	struct PARTICLES_API PAMatchVelocity : public ParticleAction
	{
		enum class EVariable : u8
		{
			magnitude,
			epsilon,
			max_radius
		};
		float magnitude;	// The grav of each particle
		float epsilon;		// Softening parameter
		float max_radius;	// Only influence particles within max_radius

        _METHODS;
	};

	struct PARTICLES_API PAMove : public ParticleAction
	{
        _METHODS;
	};

	struct PARTICLES_API PAOrbitLine : public ParticleAction
	{
		enum class EVariable : u8
		{
			p,
			axis,
			magnitude,
			epsilon,
			max_radius
		};
		pVector pL, axisL;	// Endpoints of line to which particles are attracted (in local space)
		pVector p, axis;	// Endpoints of line to which particles are attracted
		float magnitude;	// Scales acceleration
		float epsilon;		// Softening parameter
		float max_radius;	// Only influence particles within max_radius

        _METHODS;
	};

	struct PARTICLES_API PAOrbitPoint : public ParticleAction
	{
		enum class EVariable : u8
		{
			center,
			magnitude,
			epsilon,
			max_radius
		};
		pVector centerL;	// Point to which particles are attracted (in local space)
		pVector center;		// Point to which particles are attracted
		float magnitude;	// Scales acceleration
		float epsilon;		// Softening parameter
		float max_radius;	// Only influence particles within max_radius

        _METHODS;
	};

	struct PARTICLES_API PARandomAccel : public ParticleAction
	{
		enum class EVariable : u8
		{
			gen_acc
		};
		pDomain gen_accL;	// The domain of random accelerations.(in local space)
		pDomain gen_acc;	// The domain of random accelerations.

        _METHODS;
	};

	struct PARTICLES_API PARandomDisplace : public ParticleAction
	{
		enum class EVariable : u8
		{
			gen_disp
		};
		pDomain gen_dispL;	// The domain of random displacements.(in local space)
		pDomain gen_disp;	// The domain of random displacements.

        _METHODS;
	};

	struct PARTICLES_API PARandomVelocity : public ParticleAction
	{
		enum class EVariable : u8
		{
			gen_vel
		};
		pDomain gen_velL;	// The domain of random velocities.(in local space)
		pDomain gen_vel;	// The domain of random velocities.

        _METHODS;
	};

	struct PARTICLES_API PARestore : public ParticleAction
	{
		enum class EVariable : u8
		{
			time_left
		};
		float time_left;	// Time remaining until they should be in position.

        _METHODS;
	};

	struct PARTICLES_API PAScatter : public ParticleAction
	{
		enum class EVariable : u8
		{
			center,
			magnitude,
			epsilon,
			max_radius
		};
		pVector	centerL;	// Center of the fan (in local space)
		pVector	center;		// Center of the fan
		float magnitude;	// Scales acceleration
		float epsilon;		// Softening parameter
		float max_radius;	// Only influence particles within max_radius

        _METHODS;
	};

	struct PARTICLES_API PASink : public ParticleAction
	{
		enum class EVariable : u8
		{
			kill_inside,
			position
		};
		BOOL kill_inside;	// True to dispose of particles *inside* domain
		pDomain positionL;	// Disposal region (in local space)
		pDomain position;	// Disposal region

        _METHODS;
	};

	struct PARTICLES_API PASinkVelocity : public ParticleAction
	{
		enum class EVariable : u8
		{
			kill_inside,
			velocity
		};
		BOOL kill_inside;	// True to dispose of particles with vel *inside* domain
		pDomain velocityL;	// Disposal region (in local space)
		pDomain velocity;	// Disposal region

        _METHODS;
	};

	struct PARTICLES_API PASpeedLimit : public ParticleAction
	{
		enum class EVariable : u8
		{
			min_speed,
			max_speed
		};
		float min_speed;		// Clamp speed to this minimum.
		float max_speed;		// Clamp speed to this maximum.

        _METHODS;
	};

	struct PARTICLES_API PASource : public ParticleAction
	{
		enum class EVariable : u8
		{
			position,
			velocity,
			rot,
			size,
			color,
			alpha,
			particle_rate,
			age,
			age_sigma,
			parent_vel,
			parent_motion
		};
		enum{
			flSingleSize		= (1ul<<29ul),// True to get positionB from position.
			flSilent			= (1ul<<30ul),
			flVertexB_tracks	= (1ul<<31ul),// True to get positionB from position.
			fl_FORCEDWORD		= u32(-1)
		};
		pDomain positionL;	// Choose a position in this domain. (local_space)
		pDomain velocityL;	// Choose a velocity in this domain. (local_space)
		pDomain position;	// Choose a position in this domain.
		pDomain velocity;	// Choose a velocity in this domain.
		pDomain rot;		// Choose a rotation in this domain.
		pDomain size;		// Choose a size in this domain.
		pDomain color;		// Choose a color in this domain.
		float alpha;		// Alpha of all generated particles
		float particle_rate;// Particles to generate per unit time
		float age;			// Initial age of the particles
		float age_sigma;	// St. dev. of initial age of the particles
		pVector parent_vel;	
		float parent_motion;

        _METHODS;
	};

	struct PARTICLES_API PATargetColor : public ParticleAction
	{
		enum class EVariable : u8
		{
			color,
			alpha,
			scale,
			timeFrom,
			timeTo
		};
		PATargetColor():timeFrom(0.0f),timeTo(1.0f){}
		pVector color;		// Color to shift towards
		float alpha;		// Alpha value to shift towards
		float scale;		// Amount to shift by (1 == all the way)
		float timeFrom;
		float timeTo;

        _METHODS;
	};

	struct PARTICLES_API PATargetSize : public ParticleAction
	{
		enum class EVariable : u8
		{
			size,
			scale
		};
		pVector size;		// Size to shift towards
		pVector scale;		// Amount to shift by per frame (1 == all the way)

        _METHODS;
	};

	struct PARTICLES_API PATargetRotate : public ParticleAction
	{
		enum class EVariable : u8
		{
			rot,
			scale
		};
		pVector rot;		// Rotation to shift towards
		float scale;		// Amount to shift by per frame (1 == all the way)

        _METHODS;
	};

	struct PARTICLES_API PATargetVelocity : public ParticleAction
	{
		enum class EVariable : u8
		{
			velocity,
			scale
		};
		pVector velocityL;	// Velocity to shift towards (in local space)
		pVector velocity;	// Velocity to shift towards
		float scale;		// Amount to shift by (1 == all the way)

        _METHODS;
	};

	struct PARTICLES_API PAVortex : public ParticleAction
	{
		enum class EVariable : u8
		{
			center,
			axis,
			magnitude,
			epsilon,
			max_radius
		};
		pVector centerL;	// Center of vortex (in local space)
		pVector axisL;		// Axis around which vortex is applied (in local space)
		pVector center;		// Center of vortex
		pVector axis;		// Axis around which vortex is applied
		float magnitude;	// Scale for rotation around axis
		float epsilon;		// Softening parameter
		float max_radius;	// Only influence particles within max_radius

        _METHODS;
	};

    struct PARTICLES_API PATurbulence : public ParticleAction
    {
    	enum class EVariable : u8
    	{
    		frequency,
			octaves,
    		magnitude,
    		epsilon,
    		offset,
    		age
		};
		float frequency;	// Frequency
		int	octaves;		// Octaves
		float magnitude;	// Scale for rotation around axis
		float epsilon;		// Softening parameter
        pVector offset;		// Offset
        float age;

        _METHODS;
    };

	struct PARTICLES_API PABindVelocityValue : public ParticleAction
	{
		enum class EVariable : u8
		{
			BindValue
		};
		pVector BindValue;

		_METHODS;
	};

	struct PARTICLES_API PABindRotationValue : public ParticleAction
	{
		enum class EVariable : u8
		{
			BindValue
		};
		pVector BindValue;

		_METHODS;
	};

	struct PARTICLES_API PABindSizeValue : public ParticleAction
	{
		enum class EVariable : u8
		{
			BindValue,
			Pivot
		};
		pVector BindValue;
		pVector Pivot;

		_METHODS;
	};

	struct PARTICLES_API PABindColorValue : public ParticleAction
	{
		enum class EVariable : u8
		{
			BindValue
		};
		pVector BindValue;

		_METHODS;
	};

	struct PARTICLES_API PABindColorAlpha : public ParticleAction
	{
		enum class EVariable : u8
		{
			BindValue
		};
		float BindValue;

		_METHODS;
	};
};

//---------------------------------------------------------------------------
#endif
