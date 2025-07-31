////////////////////////////////////////////////////////////////////////////
//	Module 		: actor_memory.cpp
//	Created 	: 15.09.2005
//  Modified 	: 15.09.2005
//	Author		: Dmitriy Iassenev
//	Description : actor memory
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "actor_memory.h"
#include "Actor.h"
#include "../xrEngine/CameraBase.h"
#include "GamePersistent.h"

CActorMemory::CActorMemory(CActor* actor) : inherited(actor, 100), m_actor(actor)
{
	VERIFY(m_actor);
}

BOOL CActorMemory::feel_vision_isRelevant(CObject* O)
{
	CEntityAlive* entity_alive = O != nullptr ? O->cast_entity_alive() : nullptr;
	return !!(entity_alive != nullptr);
}

void CActorMemory::camera(Fvector& position, Fvector& direction, Fvector& normal, float& field_of_view, float& aspect_ratio, float& near_plane, float& far_plane)
{
	CCameraBase& camera = *m_actor->cam_Active();
	camera.Get(position, direction, normal);
	field_of_view = deg2rad(camera.f_fov);
	aspect_ratio = camera.f_aspect;
	near_plane = .1f;
	far_plane = g_pGamePersistent->Environment().CurrentEnv->far_plane;
}
