#pragma once
#include "Missile.h"
#include "../xrPhysics/DamageSource.h"

class CBolt final : public CMissile,
	public IDamageSource
{
	using inherited = CMissile;
	u16	m_thrower_id = u16(-1);
public:
	CBolt() = default;
	virtual ~CBolt() = default;

	virtual void OnH_A_Chield		();
	
	virtual	void SetInitiator		(u16 id);
	virtual	u16	 Initiator			();

	virtual void Throw				();
	virtual bool Action				(u16 cmd, u32 flags);
	virtual bool Useful				() const;
    virtual void activate_physic_shell	();

	virtual bool UsedAI_Locations	() {return false;}
	virtual IDamageSource*	cast_IDamageSource			()	{return this;}

	virtual CBolt* cast_bolt() { return this; }
};
