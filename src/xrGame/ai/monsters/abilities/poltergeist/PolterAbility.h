#pragma once
#include <Hit.h>

class IPolterInterface;
class CParticlesObject;

class CPolterSpecialAbility {

	CParticlesObject* m_particles_object;
	CParticlesObject* m_particles_object_electro;

	str_c				m_particles_hidden;
	str_c				m_particles_damage;
	str_c				m_particles_death;
	str_c				m_particles_idle;

	ref_sound			m_sound_base;
	u32					m_last_hit_frame;

protected:
	IPolterInterface* m_object;

public:
	CPolterSpecialAbility(IPolterInterface* polter);
	virtual			~CPolterSpecialAbility();

	virtual void	load(str_c section);
	virtual void	update_schedule();
	virtual void	update_frame();
	virtual void	on_hide();
	virtual void	on_show();
	virtual void	on_destroy() {}
	virtual void	on_die();
	virtual void	on_hit(SHit* pHDS);
};
