#include "StdAfx.h"
#pragma hdrstop
#ifdef DEBUG
#include "ode_include.h"
#include "../xrEngine/StatGraph.h"
#include "PHDebug.h"
#include "PHWorld.h"
#endif
#include "alife_space.h"
#include "Hit.h"
#include "PHDestroyable.h"
#include "../Include/xrRender/Kinematics.h"
#include "Car.h"
#include "Actor.h"
#include "../xrEngine/bone.h"
#include "ActorEffector.h"
#include "cameralook.h"
#include "CameraFirstEye.h"
#include "Level.h"
#include "../xrEngine/CameraManager.h"
#include "../xrEngine/CameraBase.h"
#include "../xrEngine/GameMtlLib.h"

namespace
{
	// The first-person driver model is shown only after the vehicle idle has
	// replaced the standing pose. Hiding the head and arms earlier makes the
	// skeleton stretch from the hidden bones for a frame.
	constexpr u32 DriverVisibilityDelayMs = 300;

	Fvector GetDriverBoneHidePosition(IKinematics* Kinematics, u16 Bone, bool UseParentPosition)
	{
		const CBoneData& Data = Kinematics->LL_GetData(Bone);
		const u16 Parent = Data.GetParentID();

		// The head collapses at the neck. For the arms use the current shoulder
		// joint: the visible parent transform plus the bone's bind offset. The
		// hidden upper arm's own transform is not updated by the animation.
		if (Parent != BI_NONE)
		{
			const CBoneInstance& ParentInstance = Kinematics->LL_GetBoneInstance(Parent);
			if (UseParentPosition)
			{
				if (ParentInstance.mTransform.c.square_magnitude() > EPS_L * EPS_L)
					return ParentInstance.mTransform.c;
			}
			else
			{
				Fmatrix Shoulder;
				Shoulder.mul_43(ParentInstance.mTransform, Data.bind_transform);
				if (Shoulder.c.square_magnitude() > EPS_L * EPS_L)
					return Shoulder.c;
			}
		}

		const Fvector& Position = Kinematics->LL_GetBoneInstance(Bone).mTransform.c;
		if (Position.square_magnitude() > EPS_L * EPS_L)
			return Position;

		// A freshly rebuilt or not yet calculated skeleton can keep the bone
		// at the model origin. Fall back to its bind position.
		Fmatrix BindModel;
		BindModel.invert(Data.m2b_transform);
		return BindModel.c;
	}

	void SetDriverBoneHidePositions(IKinematics* Kinematics, u16 Bone, const Fvector& Position)
	{
		if (Bone == BI_NONE || Bone >= Kinematics->LL_BoneCount())
			return;

		CBoneData& Data = Kinematics->LL_GetData(Bone);
		CBoneInstance& Instance = Kinematics->LL_GetBoneInstance(Bone);

		// Collapse the whole hidden subtree at the neck/shoulder. Its own
		// transforms can be stale (identity or a previous hidden pose), which
		// would stretch the vertices toward the actor's root/seat.
		Instance.mTransform.c = Position;
		if (Instance.mHiddenTransformValid)
			Instance.mTransformHidden.c = Position;
		Instance.mRenderTransform.mul_43(Instance.mTransform, Data.m2b_transform);

		for (CBoneData* Child : Data.children)
			SetDriverBoneHidePositions(Kinematics, Child->GetSelfID(), Position);
	}

	bool IsLocalDriver(CActor* Driver)
	{
		// Only the local view entity may hide its first-person body. In
		// multiplayer, viewers and NPC-driven cars keep the driver visible.
		return Driver != nullptr && (Driver == g_actor || Driver->IsMyCamera());
	}

	void SetDriverBonesVisible(CActor* Driver, bool Visible)
	{
		if (Driver == nullptr || Driver->Visual() == nullptr)
			return;

		IKinematics* Kinematics = smart_cast<IKinematics*>(Driver->Visual());
		if (Kinematics == nullptr)
			return;

		// Keep the neck and clavicles: they cover the torso openings. Only the
		// head and the arms from the shoulder joints are hidden.
		static const char* BoneNames[] = {"bip01_head", "bip01_l_upperarm", "bip01_r_upperarm"};

		// Evaluate the visible parent bones (neck/clavicles) before using
		// their positions for the hidden subtree.
		Kinematics->CalculateBones_Invalidate();
		Kinematics->CalculateBones(true);

		for (const char* Name : BoneNames)
		{
			const u16 Bone = Kinematics->LL_BoneID(Name);
			if (Bone == BI_NONE)
				continue;

			if (!Visible)
			{
				const bool UseParentPosition = xr_strcmp(Name, "bip01_head") == 0;
				const Fvector Position = GetDriverBoneHidePosition(Kinematics, Bone, UseParentPosition);
				SetDriverBoneHidePositions(Kinematics, Bone, Position);
			}

			Kinematics->LL_SetBoneVisible(Bone, Visible, true);
		}

		// Apply the visibility mask and the hidden-bone matrices in this
		// frame. Otherwise CalculateBones() may defer submesh culling, and the
		// hole appears to grow over the next few frames.
		Kinematics->CalculateBones_Invalidate();
		Kinematics->CalculateBones(true);
	}
}

void CCar::ApplyDriverModelVisibility(bool FirstPerson)
{
	if (Owner() == nullptr)
		return;

	if (FirstPerson && !IsLocalDriver(OwnerActor()))
		FirstPerson = false;

	// Re-selecting the first-person camera must not restart the transition.
	if (!m_driver_visibility_pending && FirstPerson && m_driver_visibility_first_person_applied)
		return;

	// A visible first-person driver must wait for the vehicle idle before the
	// head/arms are hidden or the pose is frozen. Keep the whole model hidden
	// during the transition: showing it with the standing pose and the
	// head/arms already hidden causes the stretch on entry.
	const bool DelayVisibility = FirstPerson && !m_hide_driver_first_person &&
		(m_hide_driver_head_arms_first_person || !m_driver_first_person_animations);
	m_driver_visibility_pending = DelayVisibility;
	if (DelayVisibility)
	{
		m_driver_visibility_first_person_applied = false;
		m_driver_visibility_time = Device.dwTimeGlobal;
		Owner()->setVisible(false);
		return;
	}

	ApplyDriverModelVisibilityNow(FirstPerson);
}

void CCar::ApplyDriverModelVisibilityNow(bool FirstPerson)
{
	if (Owner() == nullptr)
		return;

	CActor* Driver = OwnerActor();
	if (FirstPerson && !IsLocalDriver(Driver))
		FirstPerson = false;

	m_driver_visibility_first_person_applied = FirstPerson;

	const bool HideWholeModel = FirstPerson && m_hide_driver_first_person;
	Owner()->setVisible(!HideWholeModel);

	if (Driver == nullptr)
		return;

	// The vehicle can keep the body visible and hide only the parts that the
	// first-person HUD hands replace.
	const bool HideBones = FirstPerson && !m_hide_driver_first_person && m_hide_driver_head_arms_first_person;
	SetDriverBonesVisible(Driver, !HideBones);

	// Keep the first-person body visible but freeze its animation tracks.
	const bool FreezeAnimations = FirstPerson && !m_hide_driver_first_person && !m_driver_first_person_animations;
	Driver->SetVehicleAnimationsEnabled(!FreezeAnimations);
}

void CCar::UpdateDriverBoneHidePositions()
{
	if (!HasFirstCam() || m_hide_driver_first_person || !m_hide_driver_head_arms_first_person)
		return;

	if (m_driver_visibility_pending)
		return;

	CActor* Driver = OwnerActor();
	if (!IsLocalDriver(Driver) || Driver->Visual() == nullptr)
		return;

	IKinematics* Kinematics = smart_cast<IKinematics*>(Driver->Visual());
	if (Kinematics == nullptr)
		return;

	// Keep the collapsed head/arms points on the current neck/shoulder while
	// the idle is still settling. Otherwise the visible body moves during the
	// freeze delay and the fixed points appear pulled inside.
	struct SBoneTarget
	{
		u16 Bone = BI_NONE;
		Fvector Position = {};
	};

	static const char* BoneNames[] = {"bip01_head", "bip01_l_upperarm", "bip01_r_upperarm"};
	SBoneTarget Targets[3];
	u8 HiddenCount = 0;

	for (u8 Index = 0; Index < 3; ++Index)
	{
		const u16 Bone = Kinematics->LL_BoneID(BoneNames[Index]);
		if (Bone == BI_NONE || Kinematics->LL_GetBoneVisible(Bone))
			continue;

		const bool UseParentPosition = xr_strcmp(BoneNames[Index], "bip01_head") == 0;
		Targets[HiddenCount].Bone = Bone;
		Targets[HiddenCount].Position = GetDriverBoneHidePosition(Kinematics, Bone, UseParentPosition);
		++HiddenCount;
	}

	if (HiddenCount == 0)
		return;

	// While the animation is frozen the targets no longer move. Skip the
	// matrix updates unless a mask or another system moved the hidden points.
	if (Driver->m_vehicle_anim_pause.Paused)
	{
		bool Unchanged = true;
		for (u8 Index = 0; Index < HiddenCount; ++Index)
		{
			const Fvector& Current = Kinematics->LL_GetBoneInstance(Targets[Index].Bone).mTransform.c;
			if ((Current - Targets[Index].Position).square_magnitude() > EPS_L * EPS_L)
			{
				Unchanged = false;
				break;
			}
		}
		if (Unchanged)
			return;
	}

	for (u8 Index = 0; Index < HiddenCount; ++Index)
		SetDriverBoneHidePositions(Kinematics, Targets[Index].Bone, Targets[Index].Position);
}

void CCar::UpdateDriverModelVisibility()
{
	if (!m_driver_visibility_pending)
		return;

	// An invisible model is not rendered, so its animation tracks would not
	// advance and the idle would not replace the entry pose. Drive the
	// skeleton manually until the delay elapses.
	if (CActor* Driver = OwnerActor())
	{
		if (IKinematics* Kinematics = PKinematics(Driver->Visual()))
			Kinematics->CalculateBones();
	}

	if (Device.dwTimeGlobal - m_driver_visibility_time < DriverVisibilityDelayMs)
		return;

	m_driver_visibility_pending = false;
	ApplyDriverModelVisibilityNow(HasFirstCam());
}

bool CCar::HUDView() const		
{
	return active_camera->tag==EctFirstCam;
}
#include "PHWorld.h"
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
		for (auto It = Results.results.begin(); It != Results.results.end(); ++It)
		{
			if (!It->IsStatic() || It->element < 0)
				continue;

			const CDB::TRI& Tri = It->GetStatic()->tris[It->element];
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


void CCar::OnCameraChange(int type)
{
	if (Owner())
	{
		if (type == EctFiveCam)
		{
			camera[EctFiveCam]->Set(camera[EctFourthCam]->Position(), camera[EctFourthCam]->Direction(), camera[EctFourthCam]->Up());
			camera[EctFiveCam]->f_fov = camera[EctFourthCam]->f_fov;
			camera[EctFiveCam]->yaw = camera[EctFourthCam]->yaw;
			camera[EctFiveCam]->pitch = camera[EctFourthCam]->pitch;
			camera[EctFiveCam]->roll = camera[EctFourthCam]->roll;
		}

		m_dwAttachTime = Device.dwTimeGlobal;

		ApplyDriverModelVisibility(type == EctFirstCam);
		UpdateInteriorSoundFactor(type == EctFirstCam);

		// IKinematics*	pKinematics = smart_cast<IKinematics*>(Owner()->Visual());
		// u16 		head_bone = pKinematics->LL_BoneID("bip01_head");
		/*
		if (type==ectFirst)
			pKinematics->LL_HideBoneVisible(head_bone,FALSE);
		else if (active_camera->tag==ectFirst)
			pKinematics->LL_HideBoneVisible(head_bone,TRUE);
		*/
		// Level().Cameras().UpdateFromCamera(active_camera);
		// Level().Cameras().ApplyDevice(Device.fViewportNear);
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
		{
			return;
		}
		if (EctFiveCam == type)
		{
			return;
		}
		if (EctFirstCam != type && EctFiveCam != type)
		{
			Fvector xyz;
			XFORM().getXYZi(xyz);
			active_camera->yaw = xyz.y;
		}
	}
}