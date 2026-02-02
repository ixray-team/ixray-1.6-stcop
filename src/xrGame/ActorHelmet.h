#pragma once
#include "ArmorBase.h"

struct SBoneProtections;

class CHelmet :
	public CArmorBase
{
	using inherited = CArmorBase;

public:
	virtual void		Load					(LPCSTR section) override;

	virtual void		OnMoveToSlot			(const SInvItemPlace& previous_place) override;
	virtual void		OnMoveToRuck			(const SInvItemPlace& previous_place) override;

	virtual CHelmet*	cast_helmet				() override { return this; }

	float m_fShowNearestEnemiesDistance = 0.0f;

protected:
	virtual bool install_upgrade_impl			(LPCSTR section, bool test) override;
};
