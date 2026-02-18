#pragma once
#include "Missile.h"
#include "../xrPhysics/DamageSource.h"

class CBolt final : public CMissile,
	public IDamageSource
{
	using inherited = CMissile;
	ALife::_OBJECT_ID m_thrower_id = ALife::INVALID_OBJECT_ID;
public:
	CBolt() = default;
	virtual ~CBolt() = default;

	virtual void OnH_A_Chield		();

	void SetInitiator		(ALife::_OBJECT_ID id) override;
	ALife::_OBJECT_ID	 Initiator			() override;

	virtual void Throw				();
	virtual bool Action				(u16 cmd, u32 flags);
	virtual bool Useful				() const;
    virtual void activate_physic_shell	();

	virtual bool UsedAI_Locations	() {return false;}
	virtual IDamageSource*	cast_IDamageSource			()	{return this;}

	virtual CBolt* cast_bolt() { return this; }
};
