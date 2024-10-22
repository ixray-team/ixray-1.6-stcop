#pragma once
#include "../../ai_entity_definitions.h"
#include "../BaseMonster/base_monster.h"
#include "../telekinesis.h"
#include "../energy_holder.h"
#include "../../../../xrScripts/script_export_space.h"

class CPhysicsShellHolder;
class CStateManagerPoltergeist;
class CPoltergeisMovementManager;
class CPolterSpecialAbility;
class CPolterTele;

class CPolterFlame : public CPolterSpecialAbility {
protected:
	using inherited = CPolterSpecialAbility;

	ref_sound				m_sound;
	LPCSTR					m_particles_prepare;
	LPCSTR					m_particles_fire;
	LPCSTR					m_particles_stop;
	u32						m_time_fire_delay;
	u32						m_time_fire_play;

	float					m_length;
	float					m_hit_value;
	u32						m_hit_delay;

	u32						m_count;
	u32						m_delay;	// between 2 flames

	u32						m_time_flame_started;

	float					m_min_flame_dist;
	float					m_max_flame_dist;
	float					m_min_flame_height;
	float					m_max_flame_height;

	float					m_pmt_aura_radius;

	
	// Scanner
	float					m_scan_radius;
	u32						m_scan_delay_min;
	u32						m_scan_delay_max;
	
	SPPInfo					m_scan_effector_info;
	float					m_scan_effector_time;
	float					m_scan_effector_time_attack;
	float					m_scan_effector_time_release;
	ref_sound				m_scan_sound;

	bool					m_state_scanning;
	u32						m_scan_next_time;


	enum EFlameState {
		ePrepare,
		eFire,
		eStop
	};


public:
	struct SFlameElement {
		const CObject		*target_object;
		Fvector				position;
		Fvector				target_dir;
		u32					time_started;
		ref_sound			sound;
		CParticlesObject	*particles_object;
		EFlameState			state;
		u32					time_last_hit;
	};


private:
	using FLAME_ELEMS_VEC = xr_vector<SFlameElement*>;
	using FLAME_ELEMS_IT = FLAME_ELEMS_VEC::iterator;

	FLAME_ELEMS_VEC			m_flames;

public:	
					CPolterFlame				(CPoltergeistBase *object);
	virtual			~CPolterFlame				() override;

	virtual void	load						(LPCSTR section) override;
	virtual void	update_schedule				() override ;
	virtual void	on_destroy					() override;
	virtual void	on_die						() override;

private:
			void	select_state				(SFlameElement *elem, EFlameState state);
			bool	get_valid_flame_position	(const CObject *target_object, Fvector &res_pos);
			void	create_flame				(const CObject *target_object);
};
