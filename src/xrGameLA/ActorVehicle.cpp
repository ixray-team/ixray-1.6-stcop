#include "stdafx.h"
#pragma hdrstop

#include "actor.h"
#include "../CameraBase.h"
#include "ActorEffector.h"
#include "holder_custom.h"
#ifdef DEBUG
#include "PHDebug.h"
#endif
#include "alife_space.h"
#include "hit.h"
#include "PHDestroyable.h"
#include "Car.h"
#include "../../Include/xrRender/KinematicsAnimated.h"
#include "../../Include/xrRender/Kinematics.h"
#include "PHShellSplitter.h"
#include "actor_anim_defs.h"
#include "game_object_space.h"
#include "characterphysicssupport.h"
#include "inventory.h"

void CActor::attach_Vehicle(CHolderCustom* vehicle)
{
	if(!vehicle) return;

	if(m_holder) return;
	PickupModeOff		();

	// *** movement state - reload
	mstate_wishful			= 0;
	mstate_real				= 0;
	mstate_old				= 0;
	m_bJumpKeyPressed		= FALSE;

	m_holder=vehicle;

	cam_Set				(eacLookAt);

	IKinematics*		 V		= smart_cast<IKinematics*>(Visual()); R_ASSERT(V);
	IKinematicsAnimated* VA		= smart_cast<IKinematicsAnimated*>(Visual()); R_ASSERT(VA);
	
	if(!m_holder->attach_Actor(this)){
		m_holder=nullptr;
		return;
	}
	// temp play animation
	CCar*	car						= smart_cast<CCar*>(m_holder);
	u16 anim_type					= car->DriverAnimationType();
	SVehicleAnimCollection& anims	= m_vehicle_anims->m_vehicles_type_collections[anim_type];
	VA->PlayCycle					(anims.idles[0],FALSE);

	ResetCallbacks					();
	u16 head_bone					= V->LL_BoneID("bip01_head");
	V->LL_GetBoneInstance			(u16(head_bone)).set_callback		(bctPhysics, VehicleHeadCallback,this);

	character_physics_support		()->movement()->DestroyCharacter();
	mstate_wishful					= 0;
	m_holderID=car->ID				();

	SetWeaponHideState				(INV_STATE_BLOCK_ALL, true);

	CTorch *flashlight = GetCurrentTorch();
	if (flashlight)
		flashlight->Switch(FALSE);


	CStepManager::on_animation_start(MotionID(), 0);
}

void CActor::detach_Vehicle()
{
	if(!m_holder) return;
	CCar* car=smart_cast<CCar*>(m_holder);
	if(!car)return;

	IKinematics*	pKinematics	= smart_cast<IKinematics*>(Visual()); R_ASSERT(pKinematics);
	u16	head_bone		= pKinematics->LL_BoneID("bip01_head");
	//pKinematics->LL_HideBoneVisible(head_bone,TRUE);

	CPHShellSplitterHolder*sh= car->PPhysicsShell()->SplitterHolder();
	if(sh)sh->Deactivate();
	if(!character_physics_support()->movement()->ActivateBoxDynamic(0))
	{
		if(sh)sh->Activate();
		return;
	}
	if(sh)sh->Activate();
	m_holder->detach_Actor();

	character_physics_support()->movement()->SetPosition(m_holder->ExitPosition());
	character_physics_support()->movement()->SetVelocity(m_holder->ExitVelocity());

	r_model_yaw=-m_holder->Camera()->yaw;
	r_torso.yaw=r_model_yaw;
	r_model_yaw_dest=r_model_yaw;
	m_holder=nullptr;
	SetCallbacks		();
	IKinematicsAnimated* V= smart_cast<IKinematicsAnimated*>(Visual()); R_ASSERT(V);
	V->PlayCycle		(m_anims->m_normal.legs_idle);
	V->PlayCycle		(m_anims->m_normal.m_torso_idle);
	m_holderID=u16(-1);

	SetWeaponHideState(INV_STATE_BLOCK_ALL, false);

	cam_Set(eacFirstEye);
}

bool CActor::use_Vehicle(CHolderCustom* object)
{
	
//	CHolderCustom* vehicle=smart_cast<CHolderCustom*>(object);
	CHolderCustom* vehicle=object;
	Fvector center;
	Center(center);
	if(m_holder){
		if(!vehicle&& m_holder->Use(Device.vCameraPosition, Device.vCameraDirection,center)) detach_Vehicle();
		else{ 
			if(m_holder==vehicle)
				if(m_holder->Use(Device.vCameraPosition, Device.vCameraDirection,center))detach_Vehicle();
		}
		return true;
	}else{
		if(vehicle)
		{
			if( vehicle->Use(Device.vCameraPosition, Device.vCameraDirection,center))
			{
				if (pCamBobbing)
				{
					Cameras().RemoveCamEffector(eCEBobbing);
					pCamBobbing = nullptr;
				}

				attach_Vehicle(vehicle);
			}
			return true;
		}
		return false;
	}
}

void CActor::on_requested_spawn(CObject *object)
{
	CCar * car= smart_cast<CCar*>(object);
	if (!car) return;

	CPHShellSplitterHolder*sh= car->PPhysicsShell()->SplitterHolder();
	if(sh)sh->Deactivate();
	if(!character_physics_support()->movement()->ActivateBoxDynamic(0))
	{
		if(sh)sh->Activate();
		return;
	}
	if(sh)sh->Activate();

	character_physics_support()->movement()->SetPosition(car->ExitPosition());
	character_physics_support()->movement()->SetVelocity(car->ExitVelocity());
	
	car->DoEnter();
	attach_Vehicle(car);

	//SkyLoader: straightening of actor torso:
	Fvector			xyz;
	car->XFORM().getXYZi(xyz);
	r_torso.yaw		= xyz.y;
}


CCar* CActor::GetAttachedCar()
{
	return m_holder ? smart_cast<CCar*>(m_holder) : nullptr;
}