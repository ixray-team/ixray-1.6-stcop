#include "StdAfx.h"
#include "holder_custom.h"
#include "Actor.h"

bool CHolderCustom::attach_Actor(CGameObject* actor)
{
	m_owner = actor;
	m_ownerActor = actor->cast_actor();

	return true;
}

void CHolderCustom::detach_Actor()
{
	m_owner = nullptr;
	m_ownerActor = nullptr;
}
