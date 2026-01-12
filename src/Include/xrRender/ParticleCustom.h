#pragma once

namespace PAPI
{
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
}

class IParticleCustom
{
public:
	virtual ~IParticleCustom() {;}

	virtual void 	OnDeviceCreate		()=0;
	virtual void 	OnDeviceDestroy		()=0;

	virtual void	UpdateParent		(const Fmatrix& m, const Fvector& velocity, BOOL bXFORM)=0;
	virtual void	OnFrame				(u32 dt)=0;

	virtual void	Play				()=0;
	virtual void	Stop				(BOOL bDefferedStop=TRUE)=0;
	virtual BOOL	IsPlaying			()=0;

	virtual u32		ParticlesCount		()=0;

	virtual float	GetTimeLimit		()=0;
	virtual BOOL	IsLooped			(){return GetTimeLimit()<0.f;}
	
	virtual const shared_str	Name		()=0;
	virtual void	SetHudMode			(BOOL b)=0;
	virtual BOOL	GetHudMode			()=0;
	virtual void	SetLiveUpdate		(BOOL b)=0;
	virtual BOOL	GetLiveUpdate		()=0;

	virtual PAPI::ParticleAction* FindPA(shared_str PEName, PAPI::PActionEnum Action) = 0;
};
