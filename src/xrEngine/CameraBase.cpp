#include "stdafx.h"
#include "IGame_Level.h"
#include "CameraBase.h"

IC float AClamp(Fvector2& l, float v){
	return (2*v-l[0]-l[1])/(l[1]-l[0]);
}

CCameraBase::CCameraBase(CObject* p, u32 flags)
{
	m_Flags.assign	(flags);
	vPosition.set	(0,0,0);
	vDirection.set	(0,0,1);
	vNormal.set		(0,1,0);
	yaw				= 0.f;
	pitch			= 0.f;
	roll			= 0.f;
	prev_yaw		= 0.f;
	prev_pitch		= 0.f;
	prev_roll		= 0.f;
	lim_yaw.set		(0.f,0.f);
	lim_pitch.set	(0.f,0.f);
	lim_roll.set	(0.f,0.f);
	bClampYaw		= false;
	bClampPitch		= false;
	bClampRoll		= false;
	SetParent		(p);
	f_fov			= 90;
	f_aspect		= 1.f;
	tag				= 0;
	lookat_active	= false;
}

CCameraBase::~CCameraBase()
{
}

void CCameraBase::Load(const char* section)
{
	rot_speed			= pSettings->r_fvector3	(section,"rot_speed");

	lim_yaw				= pSettings->r_fvector2	(section,"lim_yaw");
	lim_pitch			= pSettings->r_fvector2	(section,"lim_pitch");

	bClampPitch			= (0!=lim_pitch[0])||(0!=lim_pitch[1]);
	bClampYaw			= (0!=lim_yaw[0])||(0!=lim_yaw[1]);

	if (bClampPitch)	pitch = (lim_pitch[0]+lim_pitch[1])*0.5f;
	if (bClampYaw)		yaw	  = (lim_yaw[0]+lim_yaw[1])*0.5f;
}

void CCameraBase::UpdateLookat()
{
	if (!lookat_active)
	{
		return;
	}

	Fvector _dest_dir;
	_dest_dir.sub(lookat_point, vPosition);

	Fmatrix _m;
	_m.identity();
	_m.k.normalize_safe(_dest_dir);
	Fvector::generate_orthonormal_basis(_m.k, _m.j, _m.i);

	Fvector xyz;
	_m.getXYZi(xyz);

	if (fsimilar(yaw, xyz.y, EPS) && fsimilar(pitch, xyz.x, EPS))
	{
		lookat_active = false;
	}

	yaw = angle_inertion_var(yaw, xyz.y, turn_speed_min, turn_speed_max, PI, Device.fTimeDelta);
	pitch = angle_inertion_var(pitch, xyz.x, turn_speed_min, turn_speed_max, PI, Device.fTimeDelta);
}

void CCameraBase::LookAtPoint(Fvector p, float turnSpeedMin, float turnSpeedMax)
{
	lookat_point = p;
	turn_speed_min = turnSpeedMin;
	turn_speed_max = turnSpeedMax;
	lookat_active = true;
}

float CCameraBase::CheckLimYaw()
{
	if (bClampYaw)
	{
		return AClamp(lim_yaw, yaw);
	}
	return 0;
}

float CCameraBase::CheckLimPitch()
{
	if (bClampYaw)
	{
		return AClamp(lim_pitch, pitch);
	}
	return 0;
}

float CCameraBase::CheckLimRoll()
{
	if (bClampYaw)
	{
		return AClamp(lim_roll, roll);
	}
	return 0;
}