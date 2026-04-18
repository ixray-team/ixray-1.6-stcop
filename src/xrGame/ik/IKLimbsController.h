#pragma once
#include "IKLimb.h"
#include "pose_extrapolation.h"
#include "ik_object_shift.h"

class CGameObject;
class CBlend;

class TIKLimbsController
{
private:
	static constexpr u16 max_size = 4;

public:
	void BeginComponent(IECSOwner* O);
	void EndComponent();

public:
			void	PlayLegs					( CBlend *b );
			void	Update						( );
			float	Shift						( ){ return _object_shift.shift(); }
private:
			void	Calculate					( );
			void	LimbCalculate				(  SCalculateData &cd );
			void	ShiftObject					( const SCalculateData cd[max_size] );
			float	StaticObjectShift			( const SCalculateData cd[max_size] );
			float	LegLengthShiftLimit			( float current_shift, const SCalculateData cd[max_size] );
			bool	PredictObjectShift			( const SCalculateData cd[max_size] );
			void	ObjectShift					( float static_shift, const SCalculateData cd[max_size] );
			void	LimbUpdate					( CIKLimb &L );
			void	LimbSetup					( );

private:
	static void IKVisualCallback(IKinematics* K);

private:
	CBlend *m_legs_blend = nullptr;
	CGameObject *m_object = nullptr;
	xr_vector<CIKLimb> _bone_chains;
	object_shift _object_shift;
	extrapolation::points _pose_extrapolation;

#ifdef DEBUG
	const char* anim_name;
	const char* anim_set_name;
#endif

private:
	ECS_COMPONENT(TIKLimbsController)
#ifdef DEBUG
		ECS_STRING(anim_name, "Anim name")
		ECS_STRING(anim_set_name, "Anim set name")
#endif
	ECS_END
};
