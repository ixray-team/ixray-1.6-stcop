#pragma once

class CPolterSpecialAbility 
{
protected:
	CParticlesObject* m_particles_object;
	CParticlesObject* m_particles_object_electro;

	LPCSTR				m_particles_hidden;
	LPCSTR				m_particles_damage;
	LPCSTR				m_particles_death;
	LPCSTR				m_particles_idle;

	ref_sound			m_sound_base;
	u32					m_last_hit_frame;

	CPoltergeistBase* m_object;

public:
	CPolterSpecialAbility(CPoltergeistBase* object);
	virtual			~CPolterSpecialAbility();

	virtual void	load(LPCSTR section);
	virtual void	update_schedule();
	virtual void	update_frame();
	virtual void	on_hide();
	virtual void	on_show();
	virtual void	on_destroy() {}
	virtual void	on_die();
	virtual void	on_hit(SHit* pHDS);
};