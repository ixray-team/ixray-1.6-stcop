#pragma once
#include "../../ai_entity_definitions.h"
#include "../BaseMonster/base_monster.h"
#include "../telekinesis.h"
#include "../energy_holder.h"
#include "../../../../xrScripts/script_export_space.h"

class CPolterTele : public CPolterSpecialAbility {
protected:
	using inherited = CPolterSpecialAbility;

	xr_vector<CObject*>	m_nearest;

	// external params
	float				m_pmt_radius;
	float				m_pmt_object_min_mass;
	float				m_pmt_object_max_mass;
	u32					m_pmt_object_count;
	u32					m_pmt_time_to_hold;
	u32					m_pmt_time_to_wait;
	u32					m_pmt_time_to_wait_in_objects;
	u32					m_pmt_raise_time_to_wait_in_objects;
	float				m_pmt_distance;
	float				m_pmt_object_height;
	u32					m_pmt_time_object_keep;
	float				m_pmt_raise_speed;
	float				m_pmt_fly_velocity;

	float				m_pmt_object_collision_damage;

	ref_sound			m_sound_tele_hold;
	ref_sound			m_sound_tele_throw;

	enum ETeleState {
		eStartRaiseObjects,
		eRaisingObjects,
		eFireObjects,
		eWait
	} m_state;

	u32					m_time;
	u32					m_time_next;

public:	
					CPolterTele						(CPoltergeistBase *object);
	virtual			~CPolterTele					() override;

	virtual void	load							(LPCSTR section) override;
	virtual void	update_schedule					() override;
	virtual void	update_frame					() override;

private:
			void	tele_find_objects				(xr_vector<CObject*> &objects, const Fvector &pos);
			bool	tele_raise_objects				();
			void	tele_fire_objects				();

			bool	trace_object					(CObject *obj, const Fvector &target);
};