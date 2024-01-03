#pragma once
#include "PHSimpleCharacter.h"
class CPHAICharacter : 
	public CPHSimpleCharacter
{
	using inherited = CPHSimpleCharacter;

	bool	m_forced_physics_control;
public:
	CPHAICharacter();
	virtual CPHAICharacter* CastAICharacter() { return this; }
	virtual		void		ValidateWalkOn();
	virtual		bool		TryPosition(Fvector pos, bool exact_state);
	virtual		void		Jump(const Fvector& jump_velocity);
	virtual		void		SetMaximumVelocity(dReal vel) { m_max_velocity = vel; }
	virtual		void		InitContact(dContact* c, bool& do_collide, u16 material_idx_1, u16 material_idx_2);
	virtual		void		SetForcedPhysicsControl(bool v) { m_forced_physics_control = v; }
	virtual		bool		ForcedPhysicsControl() { return m_forced_physics_control; }
	virtual		void		Create(dVector3 sizes);
private:
	virtual		void		UpdateStaticDamage(dContact* c, SGameMtl* tri_material, bool bo1) {}
#ifdef DEBUG_DRAW
	virtual		void		OnRender() override;
#endif
};