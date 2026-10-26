////////////////////////////////////////////////////////////////////////////
//	Module 		: smart_cover_object.h
//	Created 	: 28.08.2007
//  Modified 	: 28.08.2007
//	Author		: Dmitriy Iassenev
//	Description : smart cover object class
////////////////////////////////////////////////////////////////////////////
#pragma once

#include "GameObject.h"
#include "../xrScripts/script_export_space.h"

namespace smart_cover {
	class cover;

class object : 
	public CGameObject
{
private:
	typedef CGameObject	inherited;

private:
	cover const			*m_cover;
	float				m_enter_min_enemy_distance;
	float				m_exit_min_enemy_distance;

public:
	object() = default;
	object(const object & other) = delete;
	object& operator=(const object & other) = delete;

	virtual void		Load							(const char* section) override;
	virtual bool		feel_touch_on_contact			(CObject *) override { return false; }
	virtual bool		use								(CGameObject* who_use) override { return false; }
	virtual bool		net_Spawn						(CSE_Abstract* DC) override;
	virtual void		UpdateCL						() override;
	virtual void		shedule_Update					(u32 dt) override;
	virtual bool		IsVisibleForZones				() override { return false; }
	virtual bool		UsedAI_Locations				() override { return true; }
	virtual bool		can_validate_position_on_spawn	() override { return false; }
	virtual bool		use_parent_ai_locations			() const override { return false; }
	virtual bool		is_ai_obstacle					() const override { return false; }
	virtual bool		register_schedule				() const override { return false; }
	virtual void		Center							(Fvector &result) const override;
	virtual float		Radius							() const override;
#ifdef DEBUG
	virtual void		OnRender						() override;
#endif // DEBUG
			bool		inside							(Fvector const &position) const;

	IC		float const&enter_min_enemy_distance		() const;
	IC		float const&exit_min_enemy_distance			() const;
	IC		cover const&cover							() const;

public:
	DECLARE_SCRIPT_REGISTER_FUNCTION
};

} // namespace smart_cover

typedef smart_cover::object	smart_cover__object;

#include "smart_cover_object_inline.h"