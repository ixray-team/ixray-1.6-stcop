#include "stdafx.h"
#include "bloodsucker.h"
#include "../../../level.h"
#include "../../../Actor.h"
#include "../../../ActorEffector.h"
#include "../../../inventory.h"
#include "../../../HudItem.h"
#include "../../../../xrEngine/CustomHUD.h"

#include "bloodsucker_alien_pp.h"
#include "bloodsucker_alien.h"
#include "bloodsucker_alien_effector.h"

CustomBloodsuckerAlienEffector::CustomBloodsuckerAlienEffector(ECamEffectorType type, CustomBloodsucker* object) :
	inherited(type, flt_max)
{
	m_time_total = 0.f;

	dangle_target.set(angle_normalize(Random.randFs(SBloodsuckerAlienCameraEffectorProperies::DeltaAngleX)),
		angle_normalize(Random.randFs(SBloodsuckerAlienCameraEffectorProperies::DeltaAngleY)),
		angle_normalize(Random.randFs(SBloodsuckerAlienCameraEffectorProperies::DeltaAngleZ)));

	dangle_current.set(0.f, 0.f, 0.f);

	this->object = object;

	m_prev_eye_matrix.c = get_head_position(this->object);
	m_prev_eye_matrix.k = this->object->Direction();
	Fvector::generate_orthonormal_basis(m_prev_eye_matrix.k, m_prev_eye_matrix.j, m_prev_eye_matrix.i);
	m_inertion = 1.f;
	m_current_fov = SBloodsuckerAlienCameraEffectorProperies::MinFov;
}

CustomBloodsuckerAlienEffector::~CustomBloodsuckerAlienEffector()
{

}

BOOL CustomBloodsuckerAlienEffector::ProcessCam(SCamEffectorInfo& info)
{
	// Инициализация
	Fmatrix	Mdef;
	Mdef.identity();
	Mdef.j.set(info.n);
	Mdef.k.set(info.d);
	Mdef.i.crossproduct(info.n, info.d);
	Mdef.c.set(info.p);

	// set angle 
	if (angle_lerp(dangle_current.x, dangle_target.x, SBloodsuckerAlienCameraEffectorProperies::AngleSpeed, Device.fTimeDelta)) {
		dangle_target.x = angle_normalize(Random.randFs(SBloodsuckerAlienCameraEffectorProperies::DeltaAngleX));
	}

	if (angle_lerp(dangle_current.y, dangle_target.y, SBloodsuckerAlienCameraEffectorProperies::AngleSpeed, Device.fTimeDelta)) {
		dangle_target.y = angle_normalize(Random.randFs(SBloodsuckerAlienCameraEffectorProperies::DeltaAngleY));
	}

	if (angle_lerp(dangle_current.z, dangle_target.z, SBloodsuckerAlienCameraEffectorProperies::AngleSpeed, Device.fTimeDelta)) {
		dangle_target.z = angle_normalize(Random.randFs(SBloodsuckerAlienCameraEffectorProperies::DeltaAngleZ));
	}

	// update inertion
	Fmatrix cur_matrix;
	cur_matrix.k = object->Direction();
	cur_matrix.c = get_head_position(object);

	float	rel_dist = m_prev_eye_matrix.c.distance_to(cur_matrix.c) / SBloodsuckerAlienCameraEffectorProperies::MaxCameraDist;
	clamp(rel_dist, 0.f, 1.f);

	def_lerp(m_inertion, 1 - rel_dist, rel_dist, Device.fTimeDelta);

	// set pos and dir with inertion
	m_prev_eye_matrix.c.inertion(cur_matrix.c, m_inertion);
	m_prev_eye_matrix.k.inertion(cur_matrix.k, m_inertion);
	Fvector::generate_orthonormal_basis_normalized(m_prev_eye_matrix.k, m_prev_eye_matrix.j, m_prev_eye_matrix.i);

	// apply position and direction
	Mdef = m_prev_eye_matrix;

	//set fov
	float	rel_speed = object->m_fCurSpeed / 15.f;
	clamp(rel_speed, 0.f, 1.f);

	float	m_target_fov = SBloodsuckerAlienCameraEffectorProperies::MinFov + 
		(SBloodsuckerAlienCameraEffectorProperies::MaxFov - SBloodsuckerAlienCameraEffectorProperies::MinFov) * rel_speed;

	def_lerp(m_current_fov, m_target_fov, SBloodsuckerAlienCameraEffectorProperies::FovSpeed, Device.fTimeDelta);

	info.fFov = m_current_fov;
	//////////////////////////////////////////////////////////////////////////

	// Установить углы смещения
	Fmatrix		R;
	R.setHPB(dangle_current.x, dangle_current.y, dangle_current.z);

	Fmatrix		mR;
	mR.mul(Mdef, R);

	info.d.set(mR.k);
	info.n.set(mR.j);
	info.p.set(mR.c);

	return TRUE;
}