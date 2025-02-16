#include "stdafx.h"
#pragma hdrstop

#include "actor.h"
#include "WeaponMounted.h"
#include "mounted_turret.h"
#include "../CameraBase.h"
#include "ActorEffector.h"
#include "CharacterPhysicsSupport.h"
bool CActor::use_MountedWeapon(CHolderCustom* object)
{
	cam_Set(eacFirstEye);

//	CHolderCustom* wpn	=smart_cast<CHolderCustom*>(object);
	CHolderCustom* wpn	=object;
	if(m_holder){
		if(!wpn||(m_holder==wpn)){
			m_holder->detach_Actor();
			character_physics_support()->movement()->CreateCharacter();
			character_physics_support()->movement()->SetPosition(m_holder->ExitPosition());
			character_physics_support()->movement()->SetVelocity(m_holder->ExitVelocity());
			SetWeaponHideState(INV_STATE_BLOCK_ALL, false);
			m_holder=nullptr;
		}
		return true;
	}else{
		if(wpn){
			Fvector center;	Center(center);
			if(wpn->Use(Device.vCameraPosition, Device.vCameraDirection,center)){
				if(wpn->attach_Actor(this)){
					// destroy actor character
					character_physics_support()->movement()->DestroyCharacter();
					SetWeaponHideState(INV_STATE_BLOCK_ALL, true);
					PickupModeOff();
					m_holder=wpn;
					if (pCamBobbing){
						Cameras().RemoveCamEffector(eCEBobbing);
						pCamBobbing = nullptr;
					}
					return true;
				}
			}
		}
	}
	return false;
}