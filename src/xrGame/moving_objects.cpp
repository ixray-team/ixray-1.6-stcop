////////////////////////////////////////////////////////////////////////////
//	Module 		: moving_objects.cpp
//	Created 	: 27.03.2007
//  Modified 	: 27.03.2007
//	Author		: Dmitriy Iassenev
//	Description : moving objects
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "moving_objects.h"
#include "ai_space.h"
#include "level_graph.h"
#include "moving_object.h"
#include <algorithm>

moving_objects::moving_objects				() :
	m_tree					(0)
{
	m_nearest_moving.reserve(128);
	m_collision_emitters.reserve(128);
	m_visited_emitters.reserve(128);
	m_collisions.reserve(128);
	m_previous_collisions.reserve(128);
	m_spatial_objects.reserve(128);
}

moving_objects::~moving_objects				()
{
	xr_delete				(m_tree);
}

void moving_objects::on_level_unload()
{
	if (m_tree != nullptr)
	{
		for (moving_object* object : m_registered_objects)
			m_tree->remove(object);
	}
	m_registered_objects.clear();
#ifdef DEBUG
	m_objects.clear();
#endif
	xr_delete(m_tree);
}

void moving_objects::on_level_load()
{
	const NEAREST_MOVING registered = m_registered_objects;
	on_level_unload();
	m_tree = new TREE(ai().level_graph().header().box(), ai().level_graph().header().cell_size() * .5f, 16 * 1024, 16 * 1024);
	for (moving_object* object : registered)
		register_object(object);
}

void moving_objects::register_object(moving_object* moving_object)
{
	if (m_tree == nullptr)
		on_level_load();

	VERIFY2(m_objects.find(moving_object) == m_objects.end(),
		make_string<const char*>("moving object %s is registers twice", *moving_object->id()));
#ifdef DEBUG
	m_objects.insert(moving_object);
#endif
	VERIFY(std::find(m_registered_objects.begin(), m_registered_objects.end(), moving_object) == m_registered_objects.end());
	m_registered_objects.push_back(moving_object);
	VERIFY(m_tree);
	m_tree->insert(moving_object);
}

void moving_objects::unregister_object(moving_object* moving_object)
{
	const auto registered = std::find(m_registered_objects.begin(), m_registered_objects.end(), moving_object);
	if (registered == m_registered_objects.end())
	{
		Msg("! moving object %s is not registered; skipping unregister", *moving_object->id());
		return;
	}
#ifdef DEBUG
	m_objects.erase(m_objects.find(moving_object));
#endif
	if (m_tree != nullptr)
		m_tree->remove(moving_object);
	m_registered_objects.erase(registered);
}

void moving_objects::on_object_move(moving_object* moving_object)
{
	const auto registered = std::find(m_registered_objects.begin(), m_registered_objects.end(), moving_object);
	if (registered == m_registered_objects.end())
	{
		register_object(moving_object);
		return;
	}
	VERIFY(m_tree);
	m_tree->remove(moving_object);
	moving_object->update_position();
	m_tree->insert(moving_object);
}

void moving_objects::clear					()
{
	m_previous_collisions.resize(0);
}