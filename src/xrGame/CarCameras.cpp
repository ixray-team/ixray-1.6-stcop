#include "stdafx.h"
#pragma hdrstop
#ifdef DEBUG
#include "ode_include.h"
#include "../xrEngine/StatGraph.h"
#include "PHDebug.h"
#include "phworld.h"
#endif
#include "alife_space.h"
#include "hit.h"
#include "PHDestroyable.h"
#include "../Include/xrRender/Kinematics.h"
#include "car.h"
#include "actor.h"
#include "actoreffector.h"
#include "cameralook.h"
#include "camerafirsteye.h"
#include "level.h"
#include "../xrEngine/cameramanager.h"
#include "../xrEngine/camerabase.h"
#include "../xrEngine/GameMtlLib.h"
bool CCar::HUDView() const		
{
	return active_camera->tag==EctFirstCam;
}
#include "phworld.h"
float rs_car_fov_effects = 1.f;
float rs_car_cam_inert = 1.f;
float inertion_factor	= 0.95f;
float rs_car_cam_angle = 6.f;

namespace
{
	constexpr float CarCameraGroundClearance = 0.3f;
	constexpr float CarCameraWaterClearance = 0.3f;
	constexpr float CarCameraWaterProbeHeight = 20.f;

	float AngleInertionVarLimited(float Src, float Tgt, float MinSpeed, float MaxSpeed, float Clmp, float SpeedLimit, float Dt)
	{
		Tgt = angle_normalize_signed(Tgt);
		Src = angle_normalize_signed(Src);

		float Speed = std::abs((MaxSpeed - MinSpeed) * angle_difference(Tgt, Src) / Clmp) + MinSpeed;
		Speed = std::min(Speed, SpeedLimit);

		angle_lerp(Src, Tgt, Speed, Dt);
		Src = angle_normalize_signed(Src);

		float DH = angle_difference_signed(Src, Tgt);
		float DCH = clampr(DH, -Clmp, Clmp);
		Src -= DH - DCH;
		return Src;
	}

	bool GetCameraGroundY(const Fvector& Position, const Fvector& Reference, float& GroundY)
	{
		const Fbox& Bounds = Level().ObjectSpace.GetBoundingVolume();
		const float ProbeHeight = 0.5f;
		const Fvector Down = Fvector().set(0.f, -1.f, 0.f);

		Fvector From = Position;
		From.y = Reference.y + ProbeHeight;

		collide::rq_result RQ;
		float Range = From.y - Bounds.min.y;
		if (Range > EPS && Level().ObjectSpace.RayPick(From, Down, Range, collide::rqtStatic, RQ, nullptr))
		{
			GroundY = From.y - RQ.range;
			return true;
		}

		// A steep slope under the camera may put the local probe below the
		// surface. Retry from above the whole level to recover the top surface.
		From.y = Bounds.max.y;
		Range = From.y - Bounds.min.y;
		if (Range > EPS && Level().ObjectSpace.RayPick(From, Down, Range, collide::rqtStatic, RQ, nullptr))
		{
			GroundY = From.y - RQ.range;
			return true;
		}

		return false;
	}

	void ClampCameraAboveGround(Fvector& Position, const Fvector& Reference)
	{
		// The car center rests above the ground. Points at or above it can
		// only be underground on an uphill slope, which the camera trace
		// already handles.
		if (Position.y >= Reference.y)
			return;

		float GroundY = 0.f;
		if (!GetCameraGroundY(Position, Reference, GroundY))
			return;

		const float MinY = GroundY + CarCameraGroundClearance;
		if (Position.y < MinY)
			Position.y = MinY;
	}

	bool GetCameraWaterY(const Fvector& Position, const Fvector& Reference, CObject* Ignore, float& WaterY)
	{
		Fvector From = Position;
		From.y = std::max(Position.y, Reference.y) + CarCameraWaterProbeHeight;

		// Only the water surface above the camera matters. Stop the probe a
		// little below the camera so water below a bridge or a shoreline does
		// not lift the camera through the geometry above it.
		const float Bottom = Position.y - CarCameraWaterClearance - 0.5f;
		const float Range = From.y - Bottom;
		if (Range <= EPS)
			return false;

		collide::rq_results Results;
		const collide::ray_defs Ray(From, Fvector().set(0.f, -1.f, 0.f), Range, 0, collide::rqtStatic);
		Level().ObjectSpace.RayQuery(Results, Ray, nullptr, nullptr, nullptr, Ignore);

		bool Found = false;
		float BestRange = Range;
		for (auto It = Results.r_begin(); It != Results.r_end(); ++It)
		{
			if (It->O || It->element < 0)
				continue;

			const CDB::TRI& Tri = Level().ObjectSpace.GetStaticTris()[It->element];
			SGameMtl* Material = GMLib.GetMaterialByIdx(Tri.material);
			if (!Material || !Material->Flags.test(SGameMtl::flLiquid))
				continue;

			if (It->range < BestRange)
			{
				BestRange = It->range;
				Found = true;
			}
		}

		if (!Found)
			return false;

		WaterY = From.y - BestRange;
		return true;
	}

	void ClampCameraAboveWater(Fvector& Position, const Fvector& Reference, CObject* Ignore)
	{
		float WaterY = 0.f;
		if (!GetCameraWaterY(Position, Reference, Ignore, WaterY))
			return;

		const float MinY = WaterY + CarCameraWaterClearance;
		if (Position.y < MinY)
			Position.y = MinY;
	}
}

void	CCar::cam_Update			(float dt, float fov)
{
	float ifps = Device.fTimeDelta > 0.f ? (1.f / Device.fTimeDelta) : 60.f;

	//clamp(ifps, 0.f, 300.0f);
	//ifps = pow((300.f - ifps), 4.f) / 8100000000.f;
	//ifps = 0.5f + 0.5f * (1.f - ifps);
	ifps *= 0.008f;
	ifps = (powf(ifps, 1.f / 8.f) - (1.f / ifps) + 10.4f) * (0.089f);
	clamp(ifps, 0.1f, 1.f);

	inertion_factor = ifps;
	//Msg("inert %f", inertion_factor);

	VERIFY(!physics_world()->Processing());
	Fvector							P,Da;
	Da.set							(0,0,0);

	if(active_camera->tag == EctFirstCam)
	{
		if(OwnerActor())
		{
			OwnerActor()->Orientation().yaw	= -active_camera->yaw;
			OwnerActor()->Orientation().pitch = -active_camera->pitch;
		}
	}
	else
	{
		if(OwnerActor())
		{
			OwnerActor()->Orientation().yaw	= 0;
			OwnerActor()->Orientation().pitch = 0;
		}
	}

	XFORM().transform_tiny(P, m_camera_position[active_camera->tag]);

	const bool ThirdPersonCamera = active_camera->tag != EctFirstCam && active_camera->tag != EctFiveCam;
	if (ThirdPersonCamera)
	{
		ClampCameraAboveGround(P, XFORM().c);
		ClampCameraAboveWater(P, XFORM().c, this);
	}

	float car_fov = fov;
	if (rs_car_fov_effects)// && GetVelocity().dotproduct(XFORM().k) > 0.f)
	{
		//float fov_spd = GetSpeedKmH() * 0.075f;
		//clamp(fov_spd, 0.f, 1.f);
		//fov_spd = sinf(0.5f * PI * powf(fov_spd, 0.5f));

		//float accel = (m_last_avelocity) * 50.f;
		//clamp(accel, 0.f, 8.f);
		//accel = powf(accel, 0.8f);
		//Msg("accel %f", accel);
		float car_fov_accel = 1.f;
		if (e_state_drive == drive && !IsTransmissionSwitching())
		{
			if (!m_nitro_engage)
				car_fov_accel = 1.25f;
			else
				car_fov_accel = 2.5f;
		}
		//Msg("fov accel %f", car_fov_accel);
		float speed = GetSpeedKmH() - 15.f;
		speed /= 400.f;
		clamp(speed, 0.f, 1.f);
		float car_fov_spd = 1.f + rs_car_fov_effects * 0.04f * sinf(0.5f * PI * powf(speed, 0.8f));
		//Msg("fov spd %f", car_fov_spd);
		car_fov_accel = (1.f + rs_car_fov_effects * 0.0025f * m_acceleration * car_fov_accel);
		//Msg("fov accel2 %f", car_fov_accel);
		car_fov = car_fov_accel * car_fov_spd;
		//Msg("car fov %f", car_fov);
		m_camera_acceleration = m_camera_acceleration + (car_fov - m_camera_acceleration) * 7.f * Device.fTimeDelta;
		//Msg("car fov inert %f", m_camera_acceleration);
		car_fov = m_camera_acceleration * fov;
	}
	if (rs_car_fov_effects && active_camera->tag != EctFirstCam)
		car_fov *= 0.93f;

	if (m_pPhysicsShell && Device.dwTimeGlobal - m_camera_last_mmove_time > 3000)
	{
		// Automatic recentering is useful while driving, but it must not
		// fight the player's camera when the car is standing still.
		if (GetSpeedKmH() > 5.f)
		{
			Fvector lin_vel;
			m_pPhysicsShell->get_ElementByStoreOrder(0)->get_LinearVel(lin_vel);
			//lin_vel.y = 0;
			Fmatrix _m;
			_m.identity();
			_m.k.normalize_safe(lin_vel);
			Fvector::generate_orthonormal_basis(_m.k, _m.j, _m.i);

			Fvector xyz;
			_m.getXYZi(xyz);

			const float camera_angle = deg2rad(90.f * powf(rs_car_cam_angle / 90.f, 1.f / m_camera_angle_factor));
			//Msg("%f", rad2deg(camera_angle));

			camera[EctThridCam]->yaw = angle_inertion_var(camera[EctThridCam]->yaw, xyz.y,
												0.001f,
												60.f,
												20.f,
												Device.fTimeDelta);

			camera[EctThridCam]->pitch = angle_inertion_var(camera[EctThridCam]->pitch, xyz.x + camera_angle,
												0.001f,
												30.f,
												20.f,
												Device.fTimeDelta);

			Fvector xyz2;
			XFORM().getXYZi(xyz2);

			// The look-at camera should face the direction of travel. When the
			// car reverses, turn it around by 180 degrees.
			const bool bReversing = GetVelocity().dotproduct(XFORM().k) < 0.f;
			const float target_yaw = bReversing ? angle_normalize_signed(xyz2.y + PI) : xyz2.y;

			// Keep the original centering speed, but cap the yaw rate. A full
			// 180-degree reversal should turn at roughly the same rate as a
			// normal 90-degree correction instead of snapping around.
			constexpr float YawSpeedMin = 0.01f;
			constexpr float YawSpeedMax = 20.f;
			constexpr float YawSpeedLimit = (YawSpeedMax - YawSpeedMin) * (PI / 2.f) / 5.f + YawSpeedMin;

			camera[EctSecondCam]->yaw = AngleInertionVarLimited(camera[EctSecondCam]->yaw, target_yaw,
												YawSpeedMin,
												YawSpeedMax,
												5.f,
												YawSpeedLimit,
												Device.fTimeDelta);

			camera[EctSecondCam]->pitch = angle_inertion_var(camera[EctSecondCam]->pitch, (xyz2.x * 0.95f + camera_angle),
												0.01f,
												10.f,
												5.f,
												Device.fTimeDelta);
		}

		if (m_roll_lock_second)
		{
			Fvector xyz2;
			XFORM().getXYZi(xyz2);

			camera[EctSecondCam]->roll = angle_inertion_var(camera[EctSecondCam]->roll, xyz2.z,
												0.01f,
												20.f,
												5.f,
												Device.fTimeDelta);
		}
	}

	active_camera->f_fov = car_fov;
	active_camera->Update(P, Da);

	if (ThirdPersonCamera)
	{
		ClampCameraAboveGround(active_camera->vPosition, XFORM().c);
		ClampCameraAboveWater(active_camera->vPosition, XFORM().c, this);
	}

	if((active_camera->tag == EctFirstCam) && rs_car_cam_inert && Device.dwTimeGlobal - m_dwAttachTime > 50)
	{
		float time = float(Device.dwTimeGlobal - m_dwAttachTime) / 1000.f;
		clamp(time, 0.f, 1.f);
		m_last_avelocity = 0.02f * GetAVelocityM() * 4.f + (1.f - 0.02f) * m_last_avelocity;
		clamp(m_last_avelocity, 0.f, 1.f);
		//Msg("avel %f", m_last_avelocity);
		//Msg("getavel %f", GetAVelocityM());
		float inert_k = fabsf(m_camera_inertion_k);
		float dir_inert = inert_k * inertion_factor * m_last_avelocity * time * rs_car_cam_inert;
		//if (m_camera_inertion_k < 0.f)
		//	dir_inert += (1.f - inert_k * inertion_factor) * psCamInert;
		//nev_dir.inertion(active_camera->vDirection, m_camera_inertion_k*inertion_factor);
		CarCameraDirection.x = dir_inert * CarCameraDirection.x + (1.f - dir_inert) * active_camera->vDirection.x;
		CarCameraDirection.y = dir_inert * CarCameraDirection.y + (1.f - dir_inert) * active_camera->vDirection.y;
		CarCameraDirection.z = dir_inert * CarCameraDirection.z + (1.f - dir_inert) * active_camera->vDirection.z;
		if (m_camera_inertion_k < 0.f)
		{
			m_camera_inert_x = 5.f * Device.fTimeDelta * (active_camera->vDirection.x - CarCameraDirection.x) + (1.f - 5.f * Device.fTimeDelta) * m_camera_inert_x;
			m_camera_inert_z = 5.f * Device.fTimeDelta * (active_camera->vDirection.z - CarCameraDirection.z) + (1.f - 5.f * Device.fTimeDelta) * m_camera_inert_z;
			//Msg("x %f", m_camera_inert_x);
			//Msg("x %f", m_camera_inert_z);
			Fvector nev_dir = Device.vCameraDirection;
			nev_dir.x = active_camera->vDirection.x + m_camera_inert_x;
			nev_dir.y = active_camera->vDirection.y - (active_camera->vDirection.y - CarCameraDirection.y);
			nev_dir.z = active_camera->vDirection.z + m_camera_inert_z;
			active_camera->vDirection.set(nev_dir);
		}
		else
			active_camera->vDirection.set(CarCameraDirection);
		
		float up_inert = fabsf(m_camera_inertion_n) * inertion_factor * (0.5f + 0.5f * m_last_avelocity) * time * rs_car_cam_inert;
		Fvector nev_up = Device.vCameraTop;
		nev_up.inertion(active_camera->vNormal, up_inert);
		active_camera->vNormal.set(nev_up);
	}
	else
	{
		CarCameraDirection.x = active_camera->vDirection.x;
		CarCameraDirection.y = active_camera->vDirection.y;
		CarCameraDirection.z = active_camera->vDirection.z;
	}
	//if (active_camera->tag == ectFirst)
	//{
		if (OwnerActor())
		{
			if (Level().Cameras().GetCamEffector(cefDemo))
			{
				Level().Cameras().UpdateFromCamera(active_camera);
			}
			else
			{
				OwnerActor()->Cameras().UpdateFromCamera(active_camera);
				OwnerActor()->Cameras().ApplyDevice(Device.fViewportNear);
			}
		}
	//}
	//else
	//{
	//	Level().Cameras().UpdateFromCamera(active_camera);
	//	Level().Cameras().ApplyDevice(Device.fViewportNear);
	//}
}


void	CCar::OnCameraChange(int type)
{
	if (Owner())
	{
		if(type == EctFiveCam)
		{
			camera[EctFiveCam]->Set(camera[EctFourthCam]->Position(), camera[EctFourthCam]->Direction(), camera[EctFourthCam]->Up());
			camera[EctFiveCam]->f_fov = camera[EctFourthCam]->f_fov;
			camera[EctFiveCam]->yaw = camera[EctFourthCam]->yaw;
			camera[EctFiveCam]->pitch = camera[EctFourthCam]->pitch;
			camera[EctFiveCam]->roll = camera[EctFourthCam]->roll;
		}

		m_dwAttachTime = Device.dwTimeGlobal;
		// Visibility must follow the target camera, not the previous one: entering the
		// car while the chase camera is already active has to show the actor model.
		Owner()->setVisible(type != EctFirstCam);

		// First-person view muffles the outside world and the car itself, except
		// for the gearbox sounds, which opt out of the factor with sm_NoCarFactor.
		// The factor is recalculated again while driving as doors open and close.
		UpdateInteriorSoundFactor(type == EctFirstCam);

		//IKinematics*	pKinematics = smart_cast<IKinematics*>(Owner()->Visual());
		//u16 		head_bone = pKinematics->LL_BoneID("bip01_head");
		/*
		if (type==ectFirst)
			pKinematics->LL_HideBoneVisible(head_bone,FALSE);
		else if (active_camera->tag==ectFirst)
			pKinematics->LL_HideBoneVisible(head_bone,TRUE);
		*/
		//Level().Cameras().UpdateFromCamera(active_camera);
		//Level().Cameras().ApplyDevice(Device.fViewportNear);
	}

	if (active_camera && active_camera->tag == EctFiveCam && type == EctFourthCam)
	{
		active_camera = camera[type];
		return;
	}

	if (!active_camera || active_camera->tag != type)
	{
		active_camera = camera[type];

		if (EctFirstCam == type)
			return;
		if (EctFiveCam == type)
			return;
		if (EctFirstCam != type && EctFiveCam != type)
		{
			Fvector xyz;
			XFORM().getXYZi(xyz);
			active_camera->yaw = xyz.y;
		}
	}
}

