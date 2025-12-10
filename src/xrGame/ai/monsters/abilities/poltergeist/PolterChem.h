#pragma once

#include "PolterAbility.h"

class CPolterChem final : public CPolterSpecialAbility {

	typedef CPolterSpecialAbility inherited;

public:

	CPolterChem(IPolterInterface* polter);
	~CPolterChem() override;

	void	load(LPCSTR section) override;
	void	update_schedule() override;
	void	update_frame() override;
	void	on_hide() override;
	void	on_show() override;
	void	on_destroy() override;
	void	on_die() override;
	void	on_hit(SHit* pHDS) override;
};