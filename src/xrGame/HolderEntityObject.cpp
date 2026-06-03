#include "stdafx.h"
#include "HolderEntityObject.h"
#include "../Include/xrRender/Kinematics.h"
#include "../xrPhysics/PhysicsShell.h"
#include "object_broker.h"
#include "Actor.h"
#include "ActorEffector.h"
#include "CameraFirstEye.h"
#include "../xrEngine/xr_level_controller.h"
#include "game_object_space.h"
#include "Level.h"

void CHolderEntityObject::BoneCallbackX(CBoneInstance *B)
{

}

void CHolderEntityObject::BoneCallbackY(CBoneInstance *B)
{

}

CHolderEntityObject::CHolderEntityObject()
{
	m_camera_position = Fvector().set(0.0f, 0.0f, 0.0f);
	camera	= new CCameraFirstEye(this, CCameraBase::flRelativeLink|CCameraBase::flPositionRigid|CCameraBase::flDirectionRigid); 
	camera->Load("holder_entity_object_cam");
}

CHolderEntityObject::~CHolderEntityObject()
{
	xr_delete(camera);
}

void CHolderEntityObject::SetBoneCallbacks()
{
	m_pPhysicsShell->EnabledCallbacks(false);
}

void CHolderEntityObject::ResetBoneCallbacks()
{
	m_pPhysicsShell->EnabledCallbacks(true);
}

void CHolderEntityObject::Load(const char* section)
{
	inheritedPH::Load(section);
	m_bAllowWeapon = !!pSettings->r_bool(section,"allow_weapon");
	m_bExitLocked = !!pSettings->r_bool(section, "lock_exit");
	m_bEnterLocked = !!pSettings->r_bool(section, "lock_enter");

	m_exit_position = pSettings->read_if_exists<Fvector3>(section,"exit_pos",Fvector().set(0.0f, 0.0f, 0.0f));
	m_camera_position = pSettings->read_if_exists<Fvector3>(section,"camera_pos",Fvector().set(0.0f, 0.0f, 0.0f));
	m_camera_angle = pSettings->read_if_exists<Fvector3>(section,"camera_angle",Fvector().set(0.0f, 0.0f, 0.0f));
	m_sUseAction = pSettings->read_if_exists<str_c>(section,"use_action_hint",nullptr);
}

bool CHolderEntityObject::net_Spawn(CSE_Abstract* DC)
{
	if(!inheritedPH::net_Spawn	(DC)) return false;

	IKinematics* K = smart_cast<IKinematics*>(Visual());
	U16Vec fixed_bones;
	fixed_bones.push_back(K->LL_GetBoneRoot());
	PPhysicsShell()	= P_build_Shell(this,false,fixed_bones);

	processing_activate();
	setVisible(true);
	setEnabled(true);
	return true;
}

void CHolderEntityObject::net_Destroy()
{
	inheritedPH::net_Destroy	();
	processing_deactivate		();
}

void CHolderEntityObject::net_Export(NET_Packet& P)	// export to server
{
	inheritedPH::net_Export			(P);
}

void CHolderEntityObject::net_Import(NET_Packet& P)	// import from server
{
	inheritedPH::net_Import			(P);
}

void CHolderEntityObject::attach_actor_script(bool bForce)
{
	Actor()->use_HolderEx(smart_cast<CHolderCustom*>(this), bForce);
}

void CHolderEntityObject::detach_actor_script(bool bForce)
{
	Actor()->use_HolderEx(NULL, bForce);
}

void CHolderEntityObject::UpdateCL()
{
	inheritedPH::UpdateCL();

	if(OwnerActor() && OwnerActor()->IsMyCamera()) 
	{
		cam_Update(Device.fTimeDelta, g_fov);
		OwnerActor()->Cameras().UpdateFromCamera(Camera());
		OwnerActor()->Cameras().ApplyDevice(RDEVICE.fViewportNear);
	}
}

void CHolderEntityObject::Hit(SHit* pHDS)
{
	if(NULL==Owner())
		inheritedPH::Hit(pHDS);
}

void CHolderEntityObject::cam_Update(float dt, float fov)
{
	Fvector P;

	XFORM().transform_tiny(P, m_camera_position);

	if(OwnerActor()){
		// rotate head
		OwnerActor()->Orientation().yaw	= -Camera()->yaw;
		OwnerActor()->Orientation().pitch = -Camera()->pitch;
	}
	
	Camera()->f_fov = fov;
	Camera()->Update(P, m_camera_angle);
	Level().Cameras().UpdateFromCamera(Camera());
}

void CHolderEntityObject::renderable_Render	()
{
	inheritedPH::renderable_Render	();
}

void CHolderEntityObject::Action(u16 id, u32 flags)
{
	inheritedHolder::Action(id,flags);
	/*
	switch (id){
		case kWPN_FIRE:{
			if(flags==CMD_START)	FireStart	();
			else					FireEnd		();
		}break;
	}
	*/
}

void CHolderEntityObject::SetParam(int id, Fvector2 val)
{
	inheritedHolder::SetParam(id, val);
}

bool CHolderEntityObject::attach_Actor(CGameObject* actor)
{
	inheritedHolder::attach_Actor(actor);
	SetBoneCallbacks();
	return true;
}

void CHolderEntityObject::detach_Actor()
{
	inheritedHolder::detach_Actor();
	ResetBoneCallbacks();
}

void CHolderEntityObject::OnMouseMove(int dx, int dy)
{
	if (Remote())	
	{
		return;
	}

	CCameraBase* C = camera;
	float scale = (C->f_fov / g_base_fov) * psMouseSens * psMouseSensScale / 50.f;

	if (dx)
	{
		float d = float(dx) * scale;
		C->Move((d < 0) ? kLEFT : kRIGHT, std::abs(d));
	}

	if (dy) 
	{
		float d = (psMouseInvert ? -1 : 1) * float(dy) * scale * 3.f / 4.f;
		C->Move((d > 0) ? kUP : kDOWN, std::abs(d));
	}
}

void CHolderEntityObject::OnGamepadAxisMove(int id, Fvector2 value)
{
	if (Remote())
	{
		return;
	}

	// left stick
	if (id == 0)
	{

	}
	// right stick
	else if (id == 1)
	{
		CCameraBase* C = camera;
		float scale = (C->f_fov / g_fov) * psGamepadSens * Device.fTimeDelta * psMouseSensScale;
		if (value.x)
		{
			float d = value.x * scale * 8;
			C->Move((d < 0) ? kLEFT : kRIGHT, std::abs(d));
		}

		if (value.y)
		{
			float d = (psGamepadInvert ? -1 : 1) * value.y * scale * 3.f / 4.f;
			d *= 8;
			C->Move((d > 0) ? kUP : kDOWN, std::abs(d));
		}
	}
	// triggers
	else if (id == 2)
	{

	}
}

void CHolderEntityObject::OnGyroscopeMove(Fvector3 value)
{
	if (Remote())
	{
		return;
	}

	CCameraBase* C = camera;
	float scale = (C->f_fov / g_fov) * Device.fTimeDelta * psMouseSensScale;
	if (value.x)
	{
		float d = (psGyroscopeInvertY ? -1 : 1) * value.x * scale;
		d *= 8;
		C->Move((d < 0) ? kUP : kDOWN, std::abs(d));
	}

	if (value.y)
	{
		float d = (psGyroscopeInvertY ? -1 : 1) * value.y * scale * 8;
		C->Move((d > 0) ? kLEFT : kRIGHT, std::abs(d));
	}
}

void CHolderEntityObject::OnKeyboardPress(int dik)
{
	if (Remote())							return;

	switch (dik)	
	{
	case kWPN_FIRE:					
		break;
	};
}

void CHolderEntityObject::OnGamepadKeyPress(int id)
{
	if (Remote())							return;

	switch (get_binded_action(id))
	{
	case kWPN_FIRE:
		break;
	};
}

void CHolderEntityObject::OnKeyboardRelease	(int dik)
{
	if (Remote())							return;
	switch (get_binded_action(dik))	
	{
	case kWPN_FIRE:
		break;
	};
}

void CHolderEntityObject::OnGamepadKeyRelease(int id)
{
	if (Remote())							
		return;
}

void CHolderEntityObject::OnKeyboardHold		(int dik)
{}