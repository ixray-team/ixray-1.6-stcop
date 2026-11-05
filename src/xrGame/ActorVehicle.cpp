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
	SVehicleAnimCollection& anims = m_vehicle_anims->m_vehicles_type_collections[anim_type];
	V->PlayCycle(anims.idles[0], false);
	V->PlayCycle(anims.idles[1], false);

	ResetCallbacks();
	u16 head_bone = pK->LL_BoneID("bip01_head");
	pK->LL_GetBoneInstance(u16(head_bone)).set_callback(bctPhysics, VehicleHeadCallback, this);

	character_physics_support()->movement()->DestroyCharacter();
	mstate_wishful = 0;
	m_holderID = car->ID();

	SetWeaponHideState(INV_STATE_BLOCK_ALL, true);

	if (CCustomDevice* device = GetDevice())
	{
		if (!device->IsHidden() && !device->IsHiding())
		{
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

	if (!character_physics_support()->movement()->ActivateBoxDynamic(0))
	{
		car->PPhysicsShell()->SplitterHolderActivate();
		return;
	}

	car->PPhysicsShell()->SplitterHolderActivate();
	m_holder->detach_Actor();

	const Fvector exit_pos = m_holder->ExitPosition();
	const Fvector exit_vel = m_holder->ExitVelocity();

	// Recreate the character at the exit point after detaching from the car.
	// Without the second activation the character can stay inside the car shell
	// and the first exit after loading a save causes a collision push/damage.
	Position().set(exit_pos);
	character_physics_support()->movement()->CreateCharacter();
	character_physics_support()->movement()->SetPosition(exit_pos);
	character_physics_support()->movement()->SetVelocity(exit_vel);
	character_physics_support()->movement()->ActivateBoxDynamic(0);

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

	// Restore the actor torso from the car heading. Without this, a save made
	// while looking south can load with the actor/camera facing the opposite way.
	Fvector xyz;
	car->XFORM().getXYZi(xyz);
	r_torso.yaw = xyz.y;
}