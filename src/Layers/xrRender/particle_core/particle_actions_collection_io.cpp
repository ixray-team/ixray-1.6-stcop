#include "stdafx.h"
#include "particle_actions_collection.h"
#include "../../xrEngine/ParticleAnimCurveInterface.h"
using namespace PAPI;

void ParticleAction::Load	(IReader& F)
{
	m_Flags.assign	(F.r_u32());
    type			= (PActionEnum)F.r_u32();
	if (m_Flags.test(EXTENSIONS))
	{
		Version = F.r_enum<ParticleActionVersion>();
	}
}
void ParticleAction::Save	(IWriter& F)
{
	m_Flags.set(EXTENSIONS, true);
	F.w_u32			(m_Flags.get());
    F.w_u32			(type);
	F.w_enum(ParticleActionVersion::Current);
}

void PAAvoid::Load			(IReader& F)
{
	ParticleAction::Load	(F);
    F.r				(&position,sizeof(pDomain));
    look_ahead		= F.r_float();
    magnitude		= F.r_float();
    epsilon			= F.r_float();
    positionL		= position;
}
void PAAvoid::Save			(IWriter& F)
{
	ParticleAction::Save  	(F);
    F.w				(&position,sizeof(pDomain));
    F.w_float		(look_ahead);
    F.w_float		(magnitude);
    F.w_float		(epsilon);
}

void PABounce::Load			(IReader& F)
{
	ParticleAction::Load   	(F);
    F.r				(&position,sizeof(pDomain));
    oneMinusFriction= F.r_float();
    resilience		= F.r_float();
    cutoffSqr		= F.r_float();
    positionL		= position;
}
void PABounce::Save			(IWriter& F)
{
	ParticleAction::Save  	(F);
    F.w				(&position,sizeof(pDomain));
    F.w_float		(oneMinusFriction);
    F.w_float		(resilience);
    F.w_float		(cutoffSqr);
}

void PACopyVertexB::Load   	(IReader& F)
{
	ParticleAction::Load   	(F);
    copy_pos		= F.r_u32();
}
void PACopyVertexB::Save   	(IWriter& F)
{
	ParticleAction::Save   	(F);
    F.w_u32			(copy_pos);
}

void PADamping::Load		(IReader& F)
{
	ParticleAction::Load  	(F);
    F.r_fvector3	(damping);
    vlowSqr			= F.r_float	();
    vhighSqr		= F.r_float	();
}
void PADamping::Save		(IWriter& F)
{
	ParticleAction::Save  	(F);
    F.w_fvector3	(damping);
	F.w_float		(vlowSqr);
    F.w_float		(vhighSqr);
}

void PAExplosion::Load		(IReader& F)
{
	ParticleAction::Load  	(F);
	F.r_fvector3	(center);
	velocity		= F.r_float();
	magnitude		= F.r_float();
	stdev			= F.r_float();
	age				= F.r_float();
	epsilon			= F.r_float();
	centerL			= center;
}
void PAExplosion::Save		(IWriter& F)
{
	ParticleAction::Save   	(F);
	F.w_fvector3	(center);
	F.w_float		(velocity);
	F.w_float		(magnitude);
	F.w_float		(stdev);
	F.w_float		(age);
	F.w_float		(epsilon);
}

void PAFollow::Load			(IReader& F)
{
	ParticleAction::Load  	(F);
	magnitude		= F.r_float();
	epsilon			= F.r_float();
	max_radius		= F.r_float();
}
void PAFollow::Save			(IWriter& F)
{
	ParticleAction::Save  	(F);
	F.w_float		(magnitude);
	F.w_float		(epsilon);
	F.w_float		(max_radius);
}

void PAGravitate::Load		(IReader& F)
{
	ParticleAction::Load   	(F);
	magnitude		= F.r_float();
	epsilon			= F.r_float();
	max_radius		= F.r_float();
}
void PAGravitate::Save		(IWriter& F)
{
	ParticleAction::Save   	(F);
	F.w_float		(magnitude);
	F.w_float		(epsilon);
	F.w_float		(max_radius);
}

void PAGravity::Load		(IReader& F)
{
	ParticleAction::Load	(F);
	F.r_fvector3	(direction);
    directionL		= direction;
}
void PAGravity::Save		(IWriter& F)
{
	ParticleAction::Save	(F);
	F.w_fvector3	(direction);
}

void PAJet::Load			(IReader& F)
{
	ParticleAction::Load	(F);
	F.r_fvector3	(center);
	F.r				(&acc,sizeof(pDomain));
	magnitude		= F.r_float();
	epsilon			= F.r_float();
	max_radius		= F.r_float();
	centerL			= center;
	accL			= acc;
}
void PAJet::Save			(IWriter& F)
{
	ParticleAction::Save   	(F);
	F.w_fvector3	(center);
	F.w				(&acc,sizeof(pDomain));
	F.w_float		(magnitude);
	F.w_float		(epsilon);
	F.w_float		(max_radius);
}

void PAKillOld::Load		(IReader& F)
{
	ParticleAction::Load	(F);
    age_limit		= F.r_float	();
    kill_less_than	= F.r_u32	();
}
void PAKillOld::Save		(IWriter& F)
{
	ParticleAction::Save	(F);
	F.w_float		(age_limit);
    F.w_u32			(kill_less_than);
}

void PAMatchVelocity::Load 	(IReader& F)
{
	ParticleAction::Load	(F);
	magnitude		= F.r_float();
	epsilon			= F.r_float();
	max_radius		= F.r_float();
}
void PAMatchVelocity::Save 	(IWriter& F)
{
	ParticleAction::Save   	(F);
	F.w_float		(magnitude);
	F.w_float		(epsilon);
	F.w_float		(max_radius);
}

void PAMove::Load			(IReader& F)
{
	ParticleAction::Load   	(F);
}
void PAMove::Save			(IWriter& F)
{
	ParticleAction::Save   	(F);
}

void PAOrbitLine::Load		(IReader& F)
{
	ParticleAction::Load   	(F);
	F.r_fvector3	(p);
	F.r_fvector3	(axis);
	magnitude		= F.r_float();
	epsilon			= F.r_float();
	max_radius		= F.r_float();
	pL				= p;
	axisL			= axis;
}
void PAOrbitLine::Save		(IWriter& F)
{
	ParticleAction::Save	(F);
	F.w_fvector3	(p);
	F.w_fvector3	(axis);
	F.w_float		(magnitude);
	F.w_float		(epsilon);
	F.w_float		(max_radius);
}

void PAOrbitPoint::Load		(IReader& F)
{
	ParticleAction::Load	(F);
	F.r_fvector3	(center);
	magnitude		= F.r_float();
	epsilon			= F.r_float();
	max_radius		= F.r_float();
	centerL			= center;
}
void PAOrbitPoint::Save		(IWriter& F)
{
	ParticleAction::Save  	(F);
	F.w_fvector3	(center);
	F.w_float		(magnitude);
	F.w_float		(epsilon);
	F.w_float		(max_radius);
}

void PARandomAccel::Load	(IReader& F)
{
	ParticleAction::Load	(F);
    F.r				(&gen_acc,sizeof(pDomain));
    gen_accL		= gen_acc;
}
void PARandomAccel::Save	(IWriter& F)
{
	ParticleAction::Save   	(F);
    F.w				(&gen_acc,sizeof(pDomain));
}

void PARandomDisplace::Load	(IReader& F)
{
	ParticleAction::Load  	(F);
    F.r				(&gen_disp,sizeof(pDomain));
    gen_dispL		= gen_disp;
}
void PARandomDisplace::Save	(IWriter& F)
{
	ParticleAction::Save  	(F);
    F.w				(&gen_disp,sizeof(pDomain));
}

void PARandomVelocity::Load	(IReader& F)
{
	ParticleAction::Load   	(F);
    F.r				(&gen_vel,sizeof(pDomain));
    gen_velL		= gen_vel;
}
void PARandomVelocity::Save	(IWriter& F)
{
	ParticleAction::Save 	(F);
    F.w				(&gen_vel,sizeof(pDomain));
}

void PARestore::Load		(IReader& F)
{
	ParticleAction::Load  	(F);
    time_left		= F.r_float();
}
void PARestore::Save		(IWriter& F)
{
	ParticleAction::Save  	(F);
    F.w_float		(time_left);
}

void PAScatter::Load		(IReader& F)
{
	ParticleAction::Load	(F);
	F.r_fvector3	(center);
	magnitude		= F.r_float();
	epsilon			= F.r_float();
	max_radius		= F.r_float();
	centerL			= center;
}
void PAScatter::Save		(IWriter& F)
{
	ParticleAction::Save   	(F);
	F.w_fvector3	(center);
	F.w_float		(magnitude);
	F.w_float		(epsilon);
	F.w_float		(max_radius);
}

void PASink::Load			(IReader& F)
{
	ParticleAction::Load 	(F);
	kill_inside		= F.r_u32();
    F.r				(&position,sizeof(pDomain));
    positionL		= position;
}
void PASink::Save			(IWriter& F)
{
	ParticleAction::Save	(F);
	F.w_u32			(kill_inside);
    F.w				(&position,sizeof(pDomain));
}

void PASinkVelocity::Load  	(IReader& F)
{
	ParticleAction::Load   	(F);
	kill_inside		= F.r_u32();
    F.r				(&velocity,sizeof(pDomain));
    velocityL		= velocity;
}
void PASinkVelocity::Save  	(IWriter& F)
{
	ParticleAction::Save   	(F);
	F.w_u32			(kill_inside);
    F.w				(&velocity,sizeof(pDomain));
}

void PASpeedLimit::Load		(IReader& F)
{
	ParticleAction::Load   	(F);
	min_speed		= F.r_float();
	max_speed		= F.r_float();
}
void PASpeedLimit::Save		(IWriter& F)
{
	ParticleAction::Save 	(F);
    F.w_float		(min_speed);
    F.w_float		(max_speed);
}

void PASource::Load			(IReader& F)
{
	ParticleAction::Load 	(F);
    F.r(&position,sizeof(pDomain));
    F.r(&velocity,sizeof(pDomain));
    F.r(&rot,sizeof(pDomain));
	if (Version >= ParticleActionVersion::Extended)
	{
		AlighRotVelocityToVelocity = F.r_u8();
		F.r(&rot_vel, sizeof(pDomain));
	}
    F.r(&size,sizeof(pDomain));
	F.r(&color,sizeof(pDomain));
	if (Version >= ParticleActionVersion::SomeVasnyaBranch)
	{
		random_alpha = F.r_u8();
	}
	alpha = F.r_float();
	if (Version >= ParticleActionVersion::SomeVasnyaBranch)
	{
		alpha2 = F.r_float();
	}
	particle_rate = F.r_float();
	age = F.r_float();
	age_sigma = F.r_float();
    F.r_fvector3(parent_vel);
    parent_motion = F.r_float();
    positionL = position;
    velocityL = velocity;
}
void PASource::Save			(IWriter& F)
{
	ParticleAction::Save  	(F);
    F.w(&position,sizeof(pDomain));
    F.w(&velocity,sizeof(pDomain));
    F.w(&rot,sizeof(pDomain));
	F.w_u8(AlighRotVelocityToVelocity);
	F.w(&rot_vel,sizeof(pDomain));
    F.w(&size,sizeof(pDomain));
    F.w(&color,sizeof(pDomain));
	F.w_u8(random_alpha);
	F.w_float(alpha);
	F.w_float(alpha2);
	F.w_float(particle_rate);
	F.w_float(age);
	F.w_float(age_sigma);
    F.w_fvector3(parent_vel);
    F.w_float(parent_motion);
}

void PATargetColor::Load	(IReader& F)
{
	ParticleAction::Load   	(F);
	F.r_fvector3	(color);
    alpha			= F.r_float();
	scale			= F.r_float();
	if (!EngineExternal()[EEngineExternalRender::UseLegacyParticleLoader])
	{
		timeFrom		= F.r_float();
		timeTo			= F.r_float();
	}
}

void PATargetColor::Save	(IWriter& F)
{
	ParticleAction::Save   	(F);
	F.w_fvector3	(color);
	F.w_float		(alpha);
	F.w_float		(scale);
	if (!EngineExternal()[EEngineExternalRender::UseLegacyParticleLoader])
	{
		F.w_float		(timeFrom);
		F.w_float		(timeTo);
	}
}

void PATargetSize::Load		(IReader& F)
{
	ParticleAction::Load   	(F);
	F.r_fvector3	(size);
	F.r_fvector3	(scale);
}
void PATargetSize::Save		(IWriter& F)
{
	ParticleAction::Save	(F);
	F.w_fvector3	(size);
	F.w_fvector3	(scale);
}

void PATargetRotate::Load	(IReader& F)
{
	ParticleAction::Load   	(F);
	F.r_fvector3	(rot);
	scale			= F.r_float();
}
void PATargetRotate::Save	(IWriter& F)
{
	ParticleAction::Save  	(F);
	F.w_fvector3	(rot);
	F.w_float		(scale);
}

void PATargetVelocity::Load	(IReader& F)
{
	ParticleAction::Load  	(F);
	F.r_fvector3	(velocity);
	scale			= F.r_float();
	velocityL		= velocity;
}
void PATargetVelocity::Save	(IWriter& F)
{
	ParticleAction::Save		(F);
	F.w_fvector3	(velocity);
	F.w_float		(scale);
}

void PAVortex::Load			(IReader& F)
{
	ParticleAction::Load  	(F);
	F.r_fvector3	(center);
	F.r_fvector3	(axis);
	magnitude		= F.r_float();
	epsilon			= F.r_float();
	max_radius		= F.r_float();
	centerL			= center;
	axisL			= axis;
}
void PAVortex::Save			(IWriter& F)
{
	ParticleAction::Save 	(F);
	F.w_fvector3	(center);
	F.w_fvector3	(axis);
	F.w_float		(magnitude);
	F.w_float		(epsilon);
	F.w_float		(max_radius);
}

void PATurbulence::Load		(IReader& F)
{
	ParticleAction::Load  	(F);
	frequency		= F.r_float();
	octaves			= F.r_s32();
	magnitude		= F.r_float();
	epsilon			= F.r_float();
    F.r_fvector3	(offset);
}
void PATurbulence::Save		(IWriter& F)
{
	ParticleAction::Save 	(F);
	F.w_float		(frequency);
	F.w_s32			(octaves);
	F.w_float		(magnitude);
	F.w_float		(epsilon);
    F.w_fvector3	(offset);
}

// Binders
void PABindVelocityValue::Load(IReader& F) {
	ParticleAction::Load(F);
	F.r_fvector3(BindValue);
}
void PABindVelocityValue::Save(IWriter& F) {
	ParticleAction::Save(F);
	F.w_fvector3(BindValue);
}
void PABindRotationValue::Load(IReader& F) {
	ParticleAction::Load(F);
	F.r_fvector3(BindValue);
}
void PABindRotationValue::Save(IWriter& F) {
	ParticleAction::Save(F);
	F.w_fvector3(BindValue);
}
void PABindSizeValue::Load(IReader& F) {
	ParticleAction::Load(F);
	F.r_fvector3(BindValue);
	F.r_fvector3(Pivot);
}
void PABindSizeValue::Save(IWriter& F) {
	ParticleAction::Save(F);
	F.w_fvector3(BindValue);
	F.w_fvector3(Pivot);
}
void PABindColorValue::Load(IReader& F) {
	ParticleAction::Load(F);
	F.r_fvector3(BindValue);
}
void PABindColorValue::Save(IWriter& F) {
	ParticleAction::Save(F);
	F.w_fvector3(BindValue);
}
void PABindColorAlpha::Load(IReader& F) {
	ParticleAction::Load(F);
	BindValue = F.r_float();
}
void PABindColorAlpha::Save(IWriter& F) {
	ParticleAction::Save(F);
	F.w_float(BindValue);
}

// Animators
void PAColorAnimator::Load(IReader& F) {
	ParticleAction::Load(F);
	F.r_stringZ(Animator);
	Looped = F.r_u8();
	Reverse = F.r_u8();
	auto Library = PS::CPACLibraryWrapper::GetInstance().GetPACLibrary();
	VERIFY(Library);
	AnimPtr = Library->FindIPAC(Animator.c_str());
}
void PAColorAnimator::Save(IWriter& F) {
	ParticleAction::Save(F);
	F.w_stringZ(Animator);
	F.w_u8(Looped);
	F.w_u8(Reverse);
}
void PASizeAnimator::Load(IReader& F) {
	ParticleAction::Load(F);
	F.r_stringZ(Animator);
	Looped = F.r_u8();
	Reverse = F.r_u8();
	auto Library = PS::CPACLibraryWrapper::GetInstance().GetPACLibrary();
	VERIFY(Library);
	AnimPtr = Library->FindIPAC(Animator.c_str());
}
void PASizeAnimator::Save(IWriter& F) {
	ParticleAction::Save(F);
	F.w_stringZ(Animator);
	F.w_u8(Looped);
	F.w_u8(Reverse);
}
void PAVelocityAnimator::Load(IReader& F) {
	ParticleAction::Load(F);
	F.r_stringZ(Animator);
	Looped = F.r_u8();
	Reverse = F.r_u8();
	auto Library = PS::CPACLibraryWrapper::GetInstance().GetPACLibrary();
	VERIFY(Library);
	AnimPtr = Library->FindIPAC(Animator.c_str());
}
void PAVelocityAnimator::Save(IWriter& F) {
	ParticleAction::Save(F);
	F.w_stringZ(Animator);
	F.w_u8(Looped);
	F.w_u8(Reverse);
}
void PAVelocityRotationAnimator::Load(IReader& F) {
	ParticleAction::Load(F);
	F.r_stringZ(Animator);
	Looped = F.r_u8();
	Reverse = F.r_u8();
	auto Library = PS::CPACLibraryWrapper::GetInstance().GetPACLibrary();
	VERIFY(Library);
	AnimPtr = Library->FindIPAC(Animator.c_str());
}
void PAVelocityRotationAnimator::Save(IWriter& F) {
	ParticleAction::Save(F);
	F.w_stringZ(Animator);
	F.w_u8(Looped);
	F.w_u8(Reverse);
}


