
#include "StdAfx.h"

#include "PHMovementControl.h"

//#include "ExtendedGeom.h"
//#include "MathUtils.h"
//#include "Physics.h"
//#include "Level.h"
//#include "../xrEngine/gamemtllib.h"
//#include "IPhysicsShellHolder.h"
//#include "../xrPhysics/IPHWorld.h"
//extern	class CPHWorld	*ph_world;
#include "../xrPhysics/IPHCharacter.h"
#include "../xrPhysics/IPhysicsShellHolder.h"
#include "../xrPhysics/ODE/PHActorCharacter.h"
#include "../xrPhysics/ODE/PHObject.h"
#include "../xrCore/Collision/Frustum.h"
#include "../xrCore/Collision/ISpatial.h"
#include "Level.h"

collide::rq_result GetPickResult(Fvector pos, Fvector dir, float range, CObject* ignore);

bool CPHMovementControl:: ActivateBoxDynamic(DWORD id,int num_it/*=8*/,int num_steps/*5*/,float resolve_depth/*=0.01f*/)
{
	bool  character_exist=CharacterExist();
	if(character_exist&&trying_times[id]!=u32(-1))
	{
		
		Fvector character_body_pos;
		m_character->get_body_position( character_body_pos );
		Fvector dif;dif.sub(trying_poses[id],character_body_pos);
		if(Device.dwTimeGlobal-trying_times[id]<500&&dif.magnitude()<0.05f)
																	return false;
	}
	if(!m_character||m_character->PhysicsRefObject()->ObjectPPhysicsShell())
		return false;
	DWORD old_id=BoxID();

	bool  character_disabled=character_exist && !m_character->IsEnabled();

	if( character_exist && id==old_id )
		return true;

	if(!character_exist)
	{
		CreateCharacter();
	}

	// The actor can only stand up if there is enough headroom. Without this
	// check, standing inside a van (RAF/PAZ) pushes the roof and can flip the
	// car. This is the Lost Alpha ceiling check: a frustum around the target
	// box finds nearby physics objects, then an upward ray tests the ceiling.
	if (id == 0 || id == 1)
	{
		Fvector pos, box_c, aabb;
		boxes[id].get_CD(box_c, aabb);
		GetCharacterPosition(pos);
		box_c.add(pos);

		Fmatrix mProject, mFull, mView;
		mView.build_camera_dir(box_c, Fvector().set(0, 1, 0), Fvector().set(0, 0, 1));
		mProject.build_projection(boxes[id].getradius(), 1.f, 0.1f, boxes[id].getradius());
		mFull.mul(mProject, mView);

		bool go_next_test = false;
		CFrustum frustum;
		frustum.CreateFromMatrix(mFull, FRUSTUM_P_LRTB | FRUSTUM_P_FAR);

		qResultVec result;
		g_SpatialSpacePhysic->q_frustum(result, 0, ESPATIAL_TYPE::PHYSIC, frustum);
		for (const ISpatialShared& spatial : result)
		{
			CPHObject* pCPHS = spatial ? spatial->dcast_CPHObject() : nullptr;
			if (!pCPHS)
				continue;

			CPHActorCharacter* actor_character = smart_cast<CPHActorCharacter*>(pCPHS);
			if (actor_character)
				continue;

			go_next_test = true;
		}

		if (go_next_test)
		{
			collide::rq_result RQ = GetPickResult(box_c, Fvector().set(0, 1, 0), boxes[id].getradius(), nullptr);
			if (RQ.range < boxes[id].getradius())
			{
				return false;
			}
		}
	}

	Fvector vel;
	Fvector pos;

	GetCharacterVelocity(vel);
 
	GetCharacterPosition(pos);

	bool ret =	::ActivateBoxDynamic( this, character_exist,id, num_it, num_steps, resolve_depth );


	if(!ret)
	{	
		if(!character_exist)
			DestroyCharacter();
		else if(character_disabled)
			m_character->Disable();
		ActivateBox(old_id);
		SetVelocity(vel);
		if(m_character)
			m_character->fix_body_rotation();

		SetPosition(pos);
		
		//Msg("can not activate!");
	}
	else
	{
		ActivateBox(id);
		//Msg("activate!");
	}

//	SetOjectContactCallback(saved_callback);
//	saved_callback=0;
	SetVelocity(vel);


	if(!ret&&character_exist)
	{
		trying_times[id]=Device.dwTimeGlobal;

		m_character->GetBodyPosition(trying_poses[id]);
	}
	else
	{
		trying_times[id]=u32(-1);
	}
	return ret;
}
