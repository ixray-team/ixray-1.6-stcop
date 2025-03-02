#pragma once

#include "hud_item_object.h"

class CHUDAnimItem :
	public CHudItemObject
{
	shared_str CurrentMotion;
	u16 OldSlot = 0;
	bool DetectorActive = false;
	TAnimationEffector callback = nullptr;
	u32 mark = 0;

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
	static void LoadSound(const xr_string Section, const xr_string snd, bool exclusive = false);
	static void PlayHudAnim(const xr_string Section, const xr_string Anim, const xr_string snd = "", TAnimationEffector fun = nullptr);
};