#include "stdafx.h"
#include "bloodsucker_vampire_camera_effector.h"

CustomBloodsuckerVampireCameraEffector::CustomBloodsuckerVampireCameraEffector(float time, const Fvector& src, const Fvector& tgt) :
	inherited(eCEVampire, time)
{
	fLifeTime = time;
	m_time_total = time;

	m_dist = src.distance_to(tgt);

	if (m_dist < SBloodsuckerVampireCameraEffectorProperies::BestDistance) {
		m_direction.sub(src, tgt);
		m_dist = SBloodsuckerVampireCameraEffectorProperies::BestDistance - m_dist;
	}
	else {
		m_direction.sub(tgt, src);
		m_dist = m_dist - SBloodsuckerVampireCameraEffectorProperies::BestDistance;
	}

	m_direction.normalize();

	dangle_target.set(Random.randFs(SBloodsuckerVampireCameraEffectorProperies::DeltaAngleX), 
		Random.randFs(SBloodsuckerVampireCameraEffectorProperies::DeltaAngleY), Random.randFs(SBloodsuckerVampireCameraEffectorProperies::DeltaAngleZ));
	dangle_current.set(0.f, 0.f, 0.f);
}

CustomBloodsuckerVampireCameraEffector::~CustomBloodsuckerVampireCameraEffector()
{

}

BOOL CustomBloodsuckerVampireCameraEffector::ProcessCam(SCamEffectorInfo& info)
{
	fLifeTime -= Device.fTimeDelta;
	if (fLifeTime < 0)
		return FALSE;

	// процент оставшегося времени
	float time_left_perc = fLifeTime / m_time_total;

	// Инициализация
	Fmatrix	Mdef;
	Mdef.identity();
	Mdef.j.set(info.n);
	Mdef.k.set(info.d);
	Mdef.i.crossproduct(info.n, info.d);
	Mdef.c.set(info.p);


	//////////////////////////////////////////////////////////////////////////
	// using formula: y = k - 2*k*abs(x-1/2)   k - max distance
	//float	cur_dist = m_dist * (1 - 2*_abs((1-time_left_perc) - 0.5f));
	float time_passed = 1 - time_left_perc;
	float cur_dist = m_dist * (_sqrt(0.5f * 0.5f - (time_passed - 0.5f) * (time_passed - 0.5f)));

	Mdef.c.mad(m_direction, cur_dist);

	// check the time to return
	if (time_left_perc < 0.2f) {

		dangle_target.x = 0.f;
		dangle_target.y = 0.f;
		dangle_target.z = 0.f;

		angle_lerp(dangle_current.x, dangle_target.x, _abs(dangle_current.x / fLifeTime + 0.001f), Device.fTimeDelta);
		angle_lerp(dangle_current.y, dangle_target.y, _abs(dangle_current.y / fLifeTime + 0.001f), Device.fTimeDelta);
		angle_lerp(dangle_current.z, dangle_target.z, _abs(dangle_current.z / fLifeTime + 0.001f), Device.fTimeDelta);

	}
	else {

		if (angle_lerp(dangle_current.x, dangle_target.x, SBloodsuckerVampireCameraEffectorProperies::AngleSpeed, Device.fTimeDelta)) {
			dangle_target.x = Random.randFs(SBloodsuckerVampireCameraEffectorProperies::DeltaAngleX);
		}

		if (angle_lerp(dangle_current.y, dangle_target.y, SBloodsuckerVampireCameraEffectorProperies::AngleSpeed, Device.fTimeDelta)) {
			dangle_target.y = Random.randFs(SBloodsuckerVampireCameraEffectorProperies::DeltaAngleY);
		}

		if (angle_lerp(dangle_current.z, dangle_target.z, SBloodsuckerVampireCameraEffectorProperies::AngleSpeed, Device.fTimeDelta)) {
			dangle_target.z = Random.randFs(SBloodsuckerVampireCameraEffectorProperies::DeltaAngleZ);
		}
	}

	//////////////////////////////////////////////////////////////////////////

	// Установить углы смещения
	Fmatrix			R;
	R.setHPB(dangle_current.x, dangle_current.y, dangle_current.z);

	Fmatrix			mR;
	mR.mul(Mdef, R);

	info.d.set(mR.k);
	info.n.set(mR.j);
	info.p.set(mR.c);

	return TRUE;
}
