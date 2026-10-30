#pragma once
#include "PHSimpleCharacter.h"
class CPHAICharacter : 
	public CPHSimpleCharacter
{
	using inherited = CPHSimpleCharacter;

	bool	m_forced_physics_control;
public:
	CPHAICharacter();
	virtual	CPHAICharacter* CastAICharacter() override { return this; }
	virtual	void		ValidateWalkOn() override;
	virtual	bool		TryPosition(Fvector pos, bool exact_state) override;
	virtual	void		Jump(const Fvector& jump_velocity) override;
	virtual	void		SetMaximumVelocity(dReal vel) override { m_max_velocity = vel; }
	virtual	void		InitContact(dContact* c, bool& do_collide, u16 material_idx_1, u16 material_idx_2) override;
	virtual	void		SetForcedPhysicsControl(bool v) override { m_forced_physics_control = v; }
	virtual	bool		ForcedPhysicsControl() override { return m_forced_physics_control; }
	virtual	void		Create(dVector3 sizes) override;
private:
	virtual	void		UpdateStaticDamage(dContact* c, SGameMtl* tri_material, bool bo1) override {}
#ifdef DEBUG_DRAW
	virtual	void		OnRender() override;
#endif
};