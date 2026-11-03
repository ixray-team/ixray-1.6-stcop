#pragma once

#include "PhysicObject.h"
#include "PHDestroyable.h"
#include "PHCollisionDamageReceiver.h"

class CDestroyablePhysicsObject :
	public CPhysicObject,
	public CPHDestroyable,
	public CPHCollisionDamageReceiver,
	public CHitImmunity
{
	typedef CPhysicObject inherited;
	float m_fHealth;
	ref_sound m_destroy_sound;
	shared_str m_destroy_particles;
	bool child_part;

public:
	CDestroyablePhysicsObject();
	virtual ~CDestroyablePhysicsObject() override;
	virtual CPhysicsShellHolder* PPhysicsShellHolder() override;
	virtual bool net_Spawn(CSE_Abstract* DC) override;
	virtual void net_Destroy() override;
	virtual void Hit(SHit* pHDS) override;
	virtual void InitServerObject(CSE_Abstract* D) override;
	virtual ICollisionDamageReceiver* PHCollisionDamageReceiver() override { return (this); }
	virtual DLL_Pure* _construct() override;
	virtual CPhysicsShellHolder* cast_physics_shell_holder() override { return this; }
	virtual CPHDestroyable* ph_destroyable() override { return this; }
	virtual void shedule_Update(u32 dt) override;
	virtual bool CanRemoveObject() override;
	virtual void OnChangeVisual() override;
	virtual bool HasChildPart() { return !!(child_part || m_fHealth <= 0.f); };
protected:
	void Destroy();

private:
	xr_set<shared_str>					hit_object_name;
};