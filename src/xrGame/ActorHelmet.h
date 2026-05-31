#pragma once
#include "ArmorBase.h"

struct SBoneProtections;

class CHelmet :
	public CArmorBase
{
	using inherited = CArmorBase;

public:
	virtual void		Load					(const char* section) override;

	virtual void		OnMoveToSlot			(const SInvItemPlace& previous_place) override;
	virtual void		OnMoveToRuck			(const SInvItemPlace& previous_place) override;

	virtual bool can_be_attached() const override;

	virtual CHelmet*	cast_helmet				() override { return this; }

	float m_fShowNearestEnemiesDistance = 0.0f;

protected:
	virtual bool install_upgrade_impl			(const char* section, bool test) override;
};
