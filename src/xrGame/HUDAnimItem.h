#pragma once

#include "hud_item_object.h"

class CHUDAnimItem :
	public CHudItemObject
{
	shared_str CurrentMotion;
	u16 OldSlot = 0;
	bool DetectorActive = false;

public:
	CHUDAnimItem() = default;
	~CHUDAnimItem() = default;

	virtual void Load(LPCSTR section) override;

protected:
	virtual void UpdateXForm() override {}
	virtual bool SendDeactivateItem() override;
	virtual bool ActivateItem() override;
	virtual void DeactivateItem() override;

	virtual void UpdateCL() override;
	virtual bool need_renderable() override;

	virtual void PlayAnimIdle() override {};
	virtual void PlayAnimIdleMoving() override {};
	virtual void PlayAnimIdleSprint() override {};

public:
	static void PlayHudAnim(const char* Section, const char* Anim);
};