//---------------------------------------------------------------------------
#pragma once

#include "particle_core.h"
#include "../../xrEngine/ParticleAnimCurveInterface.h"
#include "../../Include/xrRender/ParticleCustom.h"

namespace PAPI{
	enum class ParticleActionVersion : u32
	{
		Original,
		Extended,
		SomeVasnyaBranch,
		MAX,
		Current = MAX - 1,
	};
	
	struct ParticleHolder;
	struct ParticleAction
	{
		enum
		{
			ALLOW_ROTATE	= (1<<1),
			EXTENSIONS		= (1<<2),
			MAX
		};
		Flags32			m_Flags;
		PActionEnum		type;	// Type field
		ParticleActionVersion Version = ParticleActionVersion::Original;
		ParticleAction	() : type(action_enum_force_dword) {m_Flags.zero();}

		virtual ~ParticleAction() = default;
		virtual void PreExecute(ParticleHolder *pe){}
		virtual void Execute(ParticleHolder *pe, const float dt, float& m_max) = 0;
		virtual void Transform	(const Fmatrix& m)				= 0;

		virtual void Load(IReader& F)=0;
		virtual void Save(IWriter& F)=0;

		template<typename T, typename TEnum>
		T* GetVariable(TEnum VarID)
		{
			return (T*)GetVariableImpl((u8)VarID);
		}

	protected:
		virtual void* GetVariableImpl(u8 VarID) = 0;
	};

	
#define _METHODS	virtual void 	Load		(IReader& F);\
                    virtual void 	Save		(IWriter& F);\
                    virtual void 	Execute		(ParticleHolder *pHolder, const float dt, float& m_max);\
                    virtual void 	Transform	(const Fmatrix& m);\
					protected:\
					void* GetVariableImpl(u8 VarID) override;

	struct PAAvoid : public ParticleAction
	{
		enum class EVariable : u8
		{
			position,
			look_ahead,
			magnitude,
			epsilon,
			align_rot_vel_to_vel
		};
		pDomain positionL;	// Avoid region (in local space)
		pDomain position;	// Avoid region
		float look_ahead;	// how many time units ahead to look
		float magnitude;	// what percent of the way to go each time
		float epsilon;		// add to r^2 for softening
		bool AlighRotVelocityToVelocity = true;

        _METHODS;
	};

	struct PABounce : public ParticleAction
	{
		enum class EVariable : u8
		{
			position,
			oneMinusFriction,
			resilience,
			cutoffSqr,
			align_rot_vel_to_vel
		};
		pDomain positionL;	// Bounce region (in local space)
		pDomain position;	// Bounce region
		float oneMinusFriction;	// Friction tangent to surface
		float resilience;	// Resilence perpendicular to surface
		float cutoffSqr;	// cutoff velocity; friction applies iff v > cutoff
		bool AlighRotVelocityToVelocity = true;

        _METHODS;
	};

	struct  PACopyVertexB : public ParticleAction
	{
		enum class EVariable : u8
		{
			copy_pos
		};
		BOOL copy_pos;		// True to copy pos to posB.

        _METHODS;
	};

	struct  PADamping : public ParticleAction
	{
		enum class EVariable : u8
		{
			damping,
			vlowSqr,
			vhighSqr,
			align_rot_vel_to_vel
		};
		Fvector damping;	// Damping constant applied to velocity
		float vlowSqr;		// Low and high cutoff velocities
		float vhighSqr;
		bool AlighRotVelocityToVelocity = true;

        _METHODS;
	};

	struct  PAExplosion : public ParticleAction
	{
		enum class EVariable : u8
		{
			center,
			velocity,
			magnitude,
			stdev,
			age,
			epsilon,
			align_rot_vel_to_vel
		};
		Fvector centerL;	// The center of the explosion (in local space)
		Fvector center;		// The center of the explosion
		float velocity;		// Of shock wave
		float magnitude;	// At unit radius
		float stdev;		// Sharpness or width of shock wave
		float age;			// How long it's been going on
		float epsilon;		// Softening parameter
		bool AlighRotVelocityToVelocity = true;

        _METHODS;
	};

	struct  PAFollow : public ParticleAction
	{
		enum class EVariable : u8
		{
			magnitude,
			epsilon,
			max_radius,
			align_rot_vel_to_vel
		};
		float magnitude;	// The grav of each particle
		float epsilon;		// Softening parameter
		float max_radius;	// Only influence particles within max_radius
		bool AlighRotVelocityToVelocity = true;

        _METHODS;
	};

	struct  PAGravitate : public ParticleAction
	{
		enum class EVariable : u8
		{
			magnitude,
			epsilon,
			max_radius,
			align_rot_vel_to_vel
		};
		float magnitude;	// The grav of each particle
		float epsilon;		// Softening parameter
		float max_radius;	// Only influence particles within max_radius
		bool AlighRotVelocityToVelocity = true;

        _METHODS;
	};

	struct  PAGravity : public ParticleAction
	{
		enum class EVariable : u8
		{
			direction
		};
		Fvector directionL;	// Amount to increment velocity (in local space)
		Fvector direction;	// Amount to increment velocity

        _METHODS;
	};

	struct  PAJet : public ParticleAction
	{
		enum class EVariable : u8
		{
			center,
			acc,
			magnitude,
			epsilon,
			max_radius,
			align_rot_vel_to_vel
		};
		Fvector	centerL;	// Center of the fan (in local space)
		pDomain accL;		// Acceleration vector domain  (in local space)
		Fvector	center;		// Center of the fan
		pDomain acc;		// Acceleration vector domain
		float magnitude;	// Scales acceleration
		float epsilon;		// Softening parameter
		float max_radius;	// Only influence particles within max_radius
		bool AlighRotVelocityToVelocity = true;

        _METHODS;
	};

	struct  PAKillOld : public ParticleAction
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

	struct  PAMatchVelocity : public ParticleAction
	{
		enum class EVariable : u8
		{
			magnitude,
			epsilon,
			max_radius,
			align_rot_vel_to_vel
		};
		float magnitude;	// The grav of each particle
		float epsilon;		// Softening parameter
		float max_radius;	// Only influence particles within max_radius
		bool AlighRotVelocityToVelocity = true;

        _METHODS;
	};

	struct  PAMove : public ParticleAction
	{
        _METHODS;
	};

	struct  PAOrbitLine : public ParticleAction
	{
		enum class EVariable : u8
		{
			p,
			axis,
			magnitude,
			epsilon,
			max_radius,
			align_rot_vel_to_vel
		};
		Fvector pL, axisL;	// Endpoints of line to which particles are attracted (in local space)
		Fvector p, axis;	// Endpoints of line to which particles are attracted
		float magnitude;	// Scales acceleration
		float epsilon;		// Softening parameter
		float max_radius;	// Only influence particles within max_radius
		bool AlighRotVelocityToVelocity = true;

        _METHODS;
	};

	struct  PAOrbitPoint : public ParticleAction
	{
		enum class EVariable : u8
		{
			center,
			magnitude,
			epsilon,
			max_radius,
			align_rot_vel_to_vel
		};
		Fvector centerL;	// Point to which particles are attracted (in local space)
		Fvector center;		// Point to which particles are attracted
		float magnitude;	// Scales acceleration
		float epsilon;		// Softening parameter
		float max_radius;	// Only influence particles within max_radius
		bool AlighRotVelocityToVelocity = true;

        _METHODS;
	};

	struct  PARandomAccel : public ParticleAction
	{
		enum class EVariable : u8
		{
			gen_acc,
			align_rot_vel_to_vel
		};
		pDomain gen_accL;	// The domain of random accelerations.(in local space)
		pDomain gen_acc;	// The domain of random accelerations.
		bool AlighRotVelocityToVelocity = true;

        _METHODS;
	};

	struct  PARandomDisplace : public ParticleAction
	{
		enum class EVariable : u8
		{
			gen_disp
		};
		pDomain gen_dispL;	// The domain of random displacements.(in local space)
		pDomain gen_disp;	// The domain of random displacements.

        _METHODS;
	};

	struct  PARandomVelocity : public ParticleAction
	{
		enum class EVariable : u8
		{
			gen_vel,
			align_rot_vel_to_vel
		};
		pDomain gen_velL;	// The domain of random velocities.(in local space)
		pDomain gen_vel;	// The domain of random velocities.
		bool AlighRotVelocityToVelocity = true;

        _METHODS;
	};

	struct  PARestore : public ParticleAction
	{
		enum class EVariable : u8
		{
			time_left,
			align_rot_vel_to_vel
		};
		float time_left;	// Time remaining until they should be in position.
		bool AlighRotVelocityToVelocity = true;

        _METHODS;
	};

	struct  PAScatter : public ParticleAction
	{
		enum class EVariable : u8
		{
			center,
			magnitude,
			epsilon,
			max_radius,
			align_rot_vel_to_vel
		};
		Fvector	centerL;	// Center of the fan (in local space)
		Fvector	center;		// Center of the fan
		float magnitude;	// Scales acceleration
		float epsilon;		// Softening parameter
		float max_radius;	// Only influence particles within max_radius
		bool AlighRotVelocityToVelocity = true;

        _METHODS;
	};

	struct  PASink : public ParticleAction
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

	struct  PASinkVelocity : public ParticleAction
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

	struct  PASpeedLimit : public ParticleAction
	{
		enum class EVariable : u8
		{
			min_speed,
			max_speed,
			align_rot_vel_to_vel
		};
		float min_speed;		// Clamp speed to this minimum.
		float max_speed;		// Clamp speed to this maximum.
		bool AlighRotVelocityToVelocity = true;

        _METHODS;
	};

	struct  PASource : public ParticleAction
	{
		enum class EVariable : u8
		{
			position,
			velocity,
			rot,
			size,
			color,
			random_alpha,
			alpha,
			alpha2,
			particle_rate,
			age,
			age_sigma,
			parent_vel,
			parent_motion,
			aligh_rot_vel_to_vel,
			rot_vel
		};
		enum{
			flPrevValue = MAX-1, // flag of prev end
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
		bool AlighRotVelocityToVelocity = true;
		pDomain rot_vel;
		pDomain size;		// Choose a size in this domain.
		pDomain color;		// Choose a color in this domain.
		bool random_alpha = false;	// If true, make ramdom number between alpha and alpha2, otherwise just take alpha value
		float alpha;		// Start alpha of all generated particles
		float alpha2;		// End alpha of all generated particles
		float particle_rate;// Particles to generate per unit time
		float age;			// Initial age of the particles
		float age_sigma;	// St. dev. of initial age of the particles
		Fvector parent_vel;	
		float parent_motion;

        _METHODS;
	};

	struct  PATargetColor : public ParticleAction
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
		Fvector color;		// Color to shift towards
		float alpha;		// Alpha value to shift towards
		float scale;		// Amount to shift by (1 == all the way)
		float timeFrom;
		float timeTo;

        _METHODS;
	};

	struct  PATargetSize : public ParticleAction
	{
		enum class EVariable : u8
		{
			size,
			scale
		};
		Fvector size;		// Size to shift towards
		Fvector scale;		// Amount to shift by per frame (1 == all the way)

        _METHODS;
	};

	struct  PATargetRotate : public ParticleAction
	{
		enum class EVariable : u8
		{
			rot,
			scale
		};
		Fvector rot;		// Rotation to shift towards
		float scale;		// Amount to shift by per frame (1 == all the way)

        _METHODS;
	};

	struct  PATargetVelocity : public ParticleAction
	{
		enum class EVariable : u8
		{
			velocity,
			scale,
			align_rot_vel_to_vel
		};
		Fvector velocityL;	// Velocity to shift towards (in local space)
		Fvector velocity;	// Velocity to shift towards
		float scale;		// Amount to shift by (1 == all the way)
		bool AlighRotVelocityToVelocity = true;

        _METHODS;
	};

	struct  PAVortex : public ParticleAction
	{
		enum class EVariable : u8
		{
			center,
			axis,
			magnitude,
			epsilon,
			max_radius
		};
		Fvector centerL;	// Center of vortex (in local space)
		Fvector axisL;		// Axis around which vortex is applied (in local space)
		Fvector center;		// Center of vortex
		Fvector axis;		// Axis around which vortex is applied
		float magnitude;	// Scale for rotation around axis
		float epsilon;		// Softening parameter
		float max_radius;	// Only influence particles within max_radius

        _METHODS;
	};

    struct  PATurbulence : public ParticleAction
    {
    	enum class EVariable : u8
    	{
    		frequency,
			octaves,
    		magnitude,
    		epsilon,
    		offset,
    		age,
    		align_rot_vel_to_vel
		};
		float frequency;	// Frequency
		int	octaves;		// Octaves
		float magnitude;	// Scale for rotation around axis
		float epsilon;		// Softening parameter
        Fvector offset;		// Offset
        float age;
    	bool AlighRotVelocityToVelocity = true;

        _METHODS;
    };

	struct PABindVelocityValue : public ParticleAction
	{
		enum class EVariable : u8
		{
			BindValue,
			align_rot_vel_to_vel
		};
		Fvector BindValue;
		bool AlighRotVelocityToVelocity = true;

		_METHODS;
	};

	struct PABindRotationValue : public ParticleAction
	{
		enum class EVariable : u8
		{
			BindValue
		};
		Fvector BindValue;

		_METHODS;
	};

	struct PABindSizeValue : public ParticleAction
	{
		enum class EVariable : u8
		{
			BindValue,
			Pivot
		};
		Fvector BindValue;
		Fvector Pivot;

		_METHODS;
	};

	struct PABindColorValue : public ParticleAction
	{
		enum class EVariable : u8
		{
			BindValue
		};
		Fvector BindValue;

		_METHODS;
	};

	struct PABindColorAlpha : public ParticleAction
	{
		enum class EVariable : u8
		{
			BindValue
		};
		float BindValue;

		_METHODS;
	};

	enum class PAAnimatorType : u8
	{
		Replace,
		Multiply,
	};

	struct PAColorAnimator : public ParticleAction
	{
		enum class EVariable : u8
		{
			Animator,
			Looped,
			Reverse
		};
		shared_str Animator;
		PS::IPAC* AnimPtr = nullptr;
		PAAnimatorType AnimatorType;
		bool Looped;
		bool Reverse;

		void PreExecute(ParticleHolder *pe) override;
		_METHODS;
	};

	struct PASizeAnimator : public ParticleAction
	{
		enum class EVariable : u8
		{
			Animator,
			Looped,
			Reverse
		};
		shared_str Animator;
		PS::IPAC* AnimPtr = nullptr;
		PAAnimatorType AnimatorType;
		bool Looped;
		bool Reverse;

		void PreExecute(ParticleHolder *pe) override;
		_METHODS;
	};

	struct PAVelocityAnimator : public ParticleAction
	{
		enum class EVariable : u8
		{
			Animator,
			Looped,
			Reverse
		};
		shared_str Animator;
		PS::IPAC* AnimPtr = nullptr;
		PAAnimatorType AnimatorType;
		bool Looped;
		bool Reverse;

		_METHODS;
	};

	struct PAVelocityRotationAnimator : public ParticleAction
	{
		enum class EVariable : u8
		{
			Animator,
			Looped,
			Reverse
		};
		shared_str Animator;
		PS::IPAC* AnimPtr = nullptr;
		PAAnimatorType AnimatorType;
		bool Looped;
		bool Reverse;

		_METHODS;
	};
};
