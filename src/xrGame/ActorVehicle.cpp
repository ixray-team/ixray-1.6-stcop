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

	// temp play animation
	u16 anim_type = car->DriverAnimationType();
	SVehicleAnimCollection& anims = m_vehicle_anims->GetCollection(anim_type, car->UseOldDriverAnimations());
	anims.PlayIdles(V, false);

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
