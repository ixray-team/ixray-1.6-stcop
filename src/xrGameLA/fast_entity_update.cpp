#include "stdafx.h"
#include "pch_script.h"
#include "fast_entity_update.h"
#include "GameObject.h"


CFastEntityUpdater::SEntityCall::SEntityCall(CGameObject *game_object, const luabind::functor<void> &functor, const luabind::object &object) :
	m_object(game_object)
{
	m_callback.set(functor, object);
}

CFastEntityUpdater::SEntityCall::~SEntityCall()
{
	m_callback.clear();
}

bool CFastEntityUpdater::SEntityCallPred::operator ()(CFastEntityUpdater::SEntityCall *call)
{
	return call->GetGameObject()->ID() == m_id;		
}

CFastEntityUpdater::CFastEntityUpdater()
{
}

CFastEntityUpdater::~CFastEntityUpdater()
{
	Clear();
}

void CFastEntityUpdater::AddCall(CGameObject *game_object, const luabind::functor<void> &functor, const luabind::object &object)
{
	m_calls.push_back(new SEntityCall(game_object, functor, object));
}

void CFastEntityUpdater::AddCall(SEntityCall *call)
{
	m_calls.push_back(call);
}

void CFastEntityUpdater::RemoveCall(u16 id)
{
//	m_calls.erase(std::remove_if(m_calls.begin(), m_calls.end(), SEntityCallPred(id)), m_calls.end());
	ENTITIES_CALLS_IT it = std::find_if(m_calls.begin(), m_calls.end(), SEntityCallPred(id));
	if (it == m_calls.end())
		return;
	xr_delete(*it);
	m_calls.erase(it);
}
// Theta(N)
void CFastEntityUpdater::Update()
{
	for (ENTITIES_CALLS_IT it = m_calls.begin(), last = m_calls.end(); it != last; ++it)
	{
		(*it)->GetCallback()((*it)->GetGameObject());
	}
}

void CFastEntityUpdater::Clear()
{
	while (m_calls.size())	
	{
		xr_delete(*(m_calls.end() - 1));
		m_calls.erase(m_calls.end() - 1);
	}
}

