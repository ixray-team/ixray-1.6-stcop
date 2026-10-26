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
	virtual ~CBolt() override = default;

	virtual void OnH_A_Chield		() override;

	void SetInitiator		(ALife::_OBJECT_ID id) override;
	ALife::_OBJECT_ID	 Initiator			() override;

	virtual void Throw				() override;
	virtual bool Action				(u16 cmd, u32 flags) override;
	virtual bool Useful				() const override;
	virtual void activate_physic_shell	() override;

	virtual bool UsedAI_Locations	() override {return false;}
	virtual IDamageSource*	cast_IDamageSource			() override {return this;}

	void FastThrow();

	virtual CBolt* cast_bolt() override { return this; }
	virtual CMissile* cast_missile() override { return this; }
};
