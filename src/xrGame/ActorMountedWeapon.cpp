#include "StdAfx.h"
#pragma hdrstop

#include "Actor.h"
#include "../xrEngine/CameraBase.h"
#include "ActorEffector.h"
#include "CharacterPhysicsSupport.h"
#include "holder_custom.h"
#include "../xrScripts/script_callback_ex.h"
#include "script_game_object.h"
#include "Car.h"

bool CActor::use_HolderEx(CHolderCustom* object, bool bForce)
{
	if (m_holder != nullptr)
	{
		CCar* car = m_holder->cast_car();
		if (car != nullptr)
		{
			detach_Vehicle();
			return true;
		}

		if (!m_holder->ExitLocked())
		{
			if (object == nullptr || (m_holder == object))
			{
				m_holder->detach_Actor();

				if (CGameObject* go = m_holder->cast_game_object())
				{
					this->callback(GameObject::eDetachVehicle)(go->lua_game_object());
				}

				character_physics_support()->movement()->CreateCharacter();
				m_holder = nullptr;
			}
		}
		return true;
	} 
	else if (object != nullptr)
	{
		if (CCar* car = object->cast_car())
		{
			attach_Vehicle(object);
			return true;
		}

		if (!object->EnterLocked())
		{
			Fvector center;	Center(center);
			if (object->Use(Device.vCameraPosition, Device.vCameraDirection, center))
			{
				if (object->attach_Actor(this))
				{
					// destroy actor character
					character_physics_support()->movement()->DestroyCharacter();

					m_holder = object;
					if (pCamBobbing != nullptr)
					{
						Cameras().RemoveCamEffector(eCEBobbing);
						pCamBobbing = nullptr;
					}

					if (CGameObject* go = m_holder->cast_game_object())
					{
						this->callback(GameObject::eAttachVehicle)(go->lua_game_object());
					}
					return true;
				}
			}
		}
	}
	return false;
}