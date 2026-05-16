////////////////////////////////////////////////////////////////////////////
//	Module 		: stalker_movement_params.h
//	Created 	: 23.12.2005
//  Modified 	: 23.12.2005
//	Author		: Dmitriy Iassenev
//	Description : Stalker movement parameters class
////////////////////////////////////////////////////////////////////////////
#pragma once
#include "movement_manager_space.h"
#include "ai_monster_space.h"
#include "detail_path_manager_space.h"

namespace smart_cover
{
	class cover;
	class loophole;
};

class CGameObject;
class stalker_movement_manager_smart_cover;

class stalker_movement_params
{
public:
	typedef MonsterSpace::EBodyState					EBodyState;
	typedef MonsterSpace::EMovementType					EMovementType;
	typedef MonsterSpace::EMentalState					EMentalState;
	typedef MovementManager::EPathType					EPathType;
	typedef DetailPathManager::EDetailPathType			EDetailPathType;
	typedef smart_cover::cover							cover_type;
	typedef smart_cover::loophole						loophole_type;

private:
								stalker_movement_params	(stalker_movement_params const &params);

public:
								stalker_movement_params	();
		stalker_movement_params&operator=				(stalker_movement_params const &params);
	IC		void				construct				(stalker_movement_manager_smart_cover* manager);

public:
		bool					equal_to_target			(stalker_movement_params const &target) const;

public:
	IC	void					desired_position		(Fvector const* position);
	IC	Fvector const*			desired_position		() const;

public:
	IC	void					desired_direction		(Fvector const* direction);
	IC	Fvector const*			desired_direction		() const;

public:
		void					cover_id				(shared_str const& cover_id);
	IC	shared_str const&		cover_id				() const;
	IC	cover_type const*		cover					() const;

public:
		void					cover_loophole_id		(shared_str const& loophole_id);
		const char*					cover_loophole_id		() const;
		loophole_type const*	cover_loophole			() const;

public:
	IC	void					cover_fire_object		(CGameObject const* object);
	IC	CGameObject const*		cover_fire_object		() const;

public:
	IC	void					cover_fire_position		(Fvector const* position);
	IC	Fvector const*			cover_fire_position		() const;

private:
		void					actualize_loophole		() const;

private:
	u32							m_vertex_id;

public:
	EBodyState					m_body_state;
	EMovementType				m_movement_type;
	EMentalState				m_mental_state;
	EPathType					m_path_type;
	EDetailPathType				m_detail_path_type;

private:
	Fvector const*				m_desired_direction;
	Fvector const*				m_desired_position;
	Fvector const*				m_cover_fire_position;
	Fvector						m_desired_position_impl;
	Fvector						m_desired_direction_impl;
	Fvector						m_cover_fire_position_impl;

	shared_str					m_cover_id;
	cover_type const*			m_cover;

	shared_str					m_cover_loophole_id;
	loophole_type const*		m_cover_loophole;

	CGameObject const*			m_cover_fire_object;
	mutable u32					m_last_selection_time;
	mutable bool				m_selected_loophole_actual;

	stalker_movement_manager_smart_cover*	m_manager;
	mutable loophole_type const*			m_cover_selected_loophole;
}; 

#include "stalker_movement_params_inline.h"