#include "StdAfx.h"


#include "Actor.h"
#include "../xrEngine/CameraBase.h"

#include "ActorEffector.h"
#include "holder_custom.h"
#ifdef DEBUG
#include "PHDebug.h"
#endif
#include "alife_space.h"
#include "Hit.h"
#include "PHDestroyable.h"
#include "Car.h"
#include "CustomDevice.h"
#include "Torch.h"
#include "../Include/xrRender/Kinematics.h"
//#include "PHShellSplitter.h"

#include "actor_anim_defs.h"
#include "game_object_space.h"
#include "CharacterPhysicsSupport.h"
#include "Inventory.h"

#include "pch_script.h"
#include "game_object_space.h"

extern bool need_restore_detector;
#include "script_game_object.h"
#include "../xrScripts/script_callback_ex.h"

namespace
{
	bool HasOmfExtension(const char* Path)
	{
		const size_t Length = xr_strlen(Path);
		return Length >= 4 && _stricmp(Path + Length - 4, ".omf") == 0;
	}

	void StripOmfExtension(char* Path)
	{
		const size_t Length = xr_strlen(Path);
		if (Length >= 4 && _stricmp(Path + Length - 4, ".omf") == 0)
			Path[Length - 4] = 0;
	}
}

void CActor::LoadVehicleAnimationSet(LPCSTR Path)
{
	if (Path == nullptr || Path[0] == 0)
		return;

	IKinematicsAnimated* V = Visual() != nullptr ? Visual()->dcast_PKinematicsAnimated() : nullptr;
	if (V == nullptr)
		return;

	string_path FullPath = {};
	const size_t PathLength = xr_strlen(Path);
	if (PathLength + 4 >= sizeof(FullPath))
	{
		Msg("! [CarAnim] path is too long: '%s'", Path);
		return;
	}

	xr_strcpy(FullPath, Path);
	if (!HasOmfExtension(FullPath))
		xr_strcat(FullPath, ".omf");

	string_path OmfPath = {};
	xr_strcpy(OmfPath, FullPath);
	StripOmfExtension(OmfPath);

	if (!FS.exist(_game_meshes_, FullPath) && !FS.exist("$level$", FullPath))
	{
		Msg("! [CarAnim] '%s' not found, using standard driver animations", FullPath);
		return;
	}

	for (u16 Slot = 0; Slot < V->LL_MotionsSlotCount(); ++Slot)
	{
		const char* SlotId = V->LL_MotionsSlot(Slot).id().c_str();
		if (SlotId == nullptr || xr_strlen(SlotId) >= sizeof(string_path))
			continue;

		string_path SlotPath = {};
		xr_strcpy(SlotPath, SlotId);
		StripOmfExtension(SlotPath);
		if (_stricmp(SlotPath, OmfPath) == 0)
			return;
	}

	const u16 OldCount = V->LL_MotionsSlotCount();
	V->append_motion_from_path(cNameSect().c_str(), OmfPath);
	if (V->LL_MotionsSlotCount() == OldCount)
	{
		Msg("! [CarAnim] failed to load '%s', using standard driver animations", FullPath);
		return;
	}

	if (m_vehicle_anims != nullptr)
		m_vehicle_anims->Create(V);

	Msg("* [CarAnim] loaded '%s'", FullPath);
}

void CActor::attach_Vehicle(CHolderCustom* vehicle)
{
	if (vehicle == nullptr)
	{
		return;
	}

	if (m_holder)
	{
		return;
	}

	CCar* car = vehicle->cast_car();
	if (car == nullptr)
	{
		return;
	}

	m_holder = vehicle;

	IRenderVisual* pVis = Visual();

	IKinematicsAnimated* V = pVis->dcast_PKinematicsAnimated();
	R_ASSERT(V);

	IKinematics* pK = PKinematics(pVis);

	if (!m_holder->attach_Actor(this))
	{
		m_holder = nullptr;
		return;
	}

	// Add the optional car-specific OMF before any vehicle animation is played.
	LoadVehicleAnimationSet(car->DriverCarAnimation());

	// temp play animation
	u16 anim_type = car->DriverAnimationType();
	SVehicleAnimCollection& anims = m_vehicle_anims->GetCollection(anim_type, car->UseOldDriverAnimations());
	anims.PlayIdles(V, false);
	m_vehicle_steer_angle = 0.f;

	ResetCallbacks();
	u16 head_bone = pK->LL_BoneID("bip01_head");
	pK->LL_GetBoneInstance(u16(head_bone)).set_callback(bctPhysics, VehicleHeadCallback, this);

	character_physics_support()->movement()->DestroyCharacter();
	mstate_wishful = 0;
	m_holderID = car->ID();

	SetWeaponHideState(INV_STATE_BLOCK_ALL, true);

	if (CCustomDevice* device = GetDevice(true))
	{
		if (!device->IsHidden() && !device->IsHiding())
		{
			need_restore_detector = true;
			device->HideDetector(true, true);
		}
	}

	if (PIItem torch_item = inventory().ItemFromSlot(TORCH_SLOT))
	{
		if (CTorch* torch = torch_item->cast_torch())
		{
			torch->Switch(false);
		}
	}

	CStepManager::on_animation_start(MotionID(), 0);

	car->ApplyDriverModelVisibility(car->HasFirstCam());

	this->callback(GameObject::eAttachVehicle)(car->lua_game_object());
}

void CActor::ReapplyVehicleVisual()
{
	if (m_holder == nullptr)
	{
		return;
	}

	CCar* car = m_holder->cast_car();
	if (car == nullptr)
	{
		return;
	}

	IKinematicsAnimated* V = Visual() != nullptr ? Visual()->dcast_PKinematicsAnimated() : nullptr;
	IKinematics* pK = Visual() != nullptr ? PKinematics(Visual()) : nullptr;
	if (V == nullptr || pK == nullptr)
	{
		return;
	}

	// OnChangeVisual() rebuilds the animation collections and reinstalls the
	// default bone callbacks. While the actor is sitting in a car the normal
	// head callback and standing idles must not come back: restore the vehicle
	// callback and the steering idles, as attach_Vehicle() would do.
	ResetCallbacks();

	const u16 HeadBone = pK->LL_BoneID("bip01_head");
	if (HeadBone != BI_NONE)
	{
		pK->LL_GetBoneInstance(HeadBone).set_callback(bctPhysics, VehicleHeadCallback, this);
	}

	LoadVehicleAnimationSet(car->DriverCarAnimation());

	SVehicleAnimCollection& Anims = m_vehicle_anims->GetCollection(car->DriverAnimationType(), car->UseOldDriverAnimations());
	switch (car->e_state_steer)
	{
	case CCar::right:
		if (Anims.steer_right.valid())
			V->PlayCycle(Anims.steer_right, false);
		break;
	case CCar::left:
		if (Anims.steer_left.valid())
			V->PlayCycle(Anims.steer_left, false);
		break;
	default:
		Anims.PlayIdles(V, false);
		break;
	}

	m_current_head.invalidate();
	m_current_legs.invalidate();
	m_current_torso.invalidate();
	m_current_legs_blend = nullptr;
	m_current_torso_blend = nullptr;
	m_current_jump_blend = nullptr;

	m_holderID = car->ID();

	// OnChangeVisual() builds a fresh skeleton, so the first-person body
	// parts have to be hidden again. No entry transition is needed here: the
	// actor is already sitting in the car.
	car->ApplyDriverModelVisibilityNow(car->HasFirstCam());
}

void CActor::SetVehicleAnimationsEnabled(bool Enabled)
{
	IKinematicsAnimated* K = Visual() != nullptr ? Visual()->dcast_PKinematicsAnimated() : nullptr;
	if (K == nullptr)
	{
		return;
	}

	if (!Enabled)
	{
		// Keep the current pose: the callback skips LL_UpdateTracks while
		// paused, so the skeleton stays exactly where it was.
		if (K->GetUpdateTracksCalback() != &m_vehicle_anim_pause)
		{
			m_vehicle_anim_pause.Previous = K->GetUpdateTracksCalback();
			K->SetUpdateTracksCalback(&m_vehicle_anim_pause);
		}

		// Switching from a steering animation must freeze in the idle pose,
		// not in the turn pose. Start the idle and let it blend in for a short
		// time before pausing. Steering changes are ignored while pending.
		const float CurrentSteer = m_vehicle_steer_angle;
		m_vehicle_anim_pause.Paused = false;
		m_vehicle_anim_pause.PendingTime = 0.f;
		m_vehicle_anim_pause.PendingDelay = 0.f;
		CCar* Car = m_holder != nullptr ? m_holder->cast_car() : nullptr;
		// Restart the idle only when the driver is actually steering. On entry
		// the idle is already playing; restarting it would blend from a random
		// phase and move the body around the hidden head and arms.
		if (Car != nullptr && (CurrentSteer != 0.f || Car->e_state_steer != CCar::idle))
		{
			steer_Vehicle(0.f);
			m_vehicle_steer_angle = CurrentSteer;
		}
		m_vehicle_anim_pause.PendingDelay = 0.25f;
		return;
	}

	const bool WasPaused = m_vehicle_anim_pause.Paused || m_vehicle_anim_pause.PendingDelay > 0.f;
	m_vehicle_anim_pause.Paused = false;
	m_vehicle_anim_pause.PendingDelay = 0.f;
	m_vehicle_anim_pause.PendingTime = 0.f;
	if (K->GetUpdateTracksCalback() == &m_vehicle_anim_pause)
	{
		K->SetUpdateTracksCalback(m_vehicle_anim_pause.Previous);
	}
	m_vehicle_anim_pause.Previous = nullptr;

	// Pick the animation up from the current steering state instead of the
	// pose that was frozen several seconds ago. Do not restart an animation
	// that was already running.
	if (WasPaused && m_holder != nullptr && m_holder->cast_car() != nullptr)
	{
		steer_Vehicle(m_vehicle_steer_angle);
	}
}

void CActor::detach_Vehicle()
{
	if (m_holder == nullptr)
	{
		return;
	}

	CCar* car = m_holder->cast_car();
	if (car == nullptr)
	{
		return;
	}

	car->PPhysicsShell()->SplitterHolderDeactivate();

	m_holder->detach_Actor();

	const Fvector exit_pos = m_holder->ExitPosition();
	const Fvector exit_vel = m_holder->ExitVelocity();

	// The character is destroyed while the actor is attached, so create it only
	// at the exit point. Activating it before detaching created a body inside the
	// car and pushed the vehicle on the first exit after loading a save.
	Position().set(exit_pos);
	character_physics_support()->movement()->SetPosition(exit_pos);
	character_physics_support()->movement()->CreateCharacter();
	character_physics_support()->movement()->SetPosition(exit_pos);
	character_physics_support()->movement()->SetVelocity(exit_vel);
	character_physics_support()->movement()->ActivateBoxDynamic(0);

	car->PPhysicsShell()->SplitterHolderActivate();

	r_model_yaw = -m_holder->Camera()->yaw;
	r_torso.yaw = r_model_yaw;
	r_model_yaw_dest = r_model_yaw;
	m_holder = nullptr;
	SetVehicleAnimationsEnabled(true);
	SetCallbacks();
	cam_Set(eacFirstEye);

	IKinematicsAnimated* V = Visual()->dcast_PKinematicsAnimated();
	R_ASSERT(V);

	V->PlayCycle(m_anims->m_normal.legs_idle);
	V->PlayCycle(m_anims->m_normal.m_torso_idle);
	m_holderID = ALife::INVALID_OBJECT_ID;

	SetWeaponHideState(INV_STATE_BLOCK_ALL, false);
	this->callback(GameObject::eDetachVehicle)(car->lua_game_object());
}

bool CActor::use_Vehicle(CHolderCustom* object)
{
	CHolderCustom* vehicle = object;
	Fvector center;
	Center(center);
	if (m_holder)
	{
		if (!vehicle && m_holder->Use(Device.vCameraPosition, Device.vCameraDirection, center))
		{
			detach_Vehicle();
		}
		else
		{
			if (m_holder == vehicle)
			{
				if (m_holder && m_holder->Use(Device.vCameraPosition, Device.vCameraDirection, center))
				{
					detach_Vehicle();
				}
			}
		}
		return true;
	}
	else
	{
		if (vehicle != nullptr)
		{
			if (vehicle->Use(Device.vCameraPosition, Device.vCameraDirection, center))
			{
				if (pCamBobbing)
				{
					Cameras().RemoveCamEffector(eCEBobbing);
					pCamBobbing = nullptr;
				}

				attach_Vehicle(vehicle);
			}
			else if (CCar* car = vehicle->cast_car())
			{
				this->callback(GameObject::eUseVehicle)(car->lua_game_object());
			}

			return true;
		}
		return false;
	}
}

void CActor::on_requested_spawn(CObject* object)
{
	// The client spawn manager drops the callback after invoking it, so the
	// actor must not try to remove it again during level unload.
	m_holder_id = ALife::INVALID_OBJECT_ID;

	CCar* car = object != nullptr ? object->cast_car() : nullptr;
	if (car == nullptr)
	{
		return;
	}

	attach_Vehicle(car);
	if (m_holder != car)
	{
		return;
	}

	// While attached, the torso yaw is relative to the car: the car transform
	// already provides the world heading, and VehicleHeadCallback applies the
	// torso as a bone-space offset. Writing the car world yaw here rotated the
	// head sideways after loading a save. The saved camera direction is restored
	// by the car camera update itself.
	Fvector xyz;
	car->XFORM().getXYZi(xyz);
	r_model_yaw = xyz.y;
	r_model_yaw_dest = r_model_yaw;
	r_model_yaw_delta = 0.f;
	r_torso.yaw = 0.f;
}
