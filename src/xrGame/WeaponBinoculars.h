#pragma once

#include "WeaponMagazined.h"
#include "../xrScripts/script_export_space.h"

class CUIFrameWindow;
class CUIStatic;

class CWeaponBinoculars final : public CWeaponMagazined
{
	using inherited = CWeaponMagazined;

	bool m_bVision = false;

public:
	CWeaponBinoculars() = default;
	virtual ~CWeaponBinoculars() override = default;

	virtual void	Load				(const char* section) override;
	virtual void	LoadSounds			(const char* section) override;

	virtual void	OnZoomIn			() override;
	virtual void	OnZoomOut			() override;
	virtual void	ZoomInc				() override;
	virtual void	ZoomDec				() override;
	virtual void	net_Destroy			() override;
	virtual bool	net_Spawn			(CSE_Abstract* DC) override;
	virtual bool			can_kill() const override { return false; }
	virtual void	save				(NET_Packet &output_packet) override;
	virtual void	load				(IReader &input_packet) override;
	virtual void Serialize(ISaveObject& Object) override;

	virtual bool	Action				(u16 cmd, u32 flags) override;
	virtual void	UpdateCL			() override;
	virtual void	render_item_ui		() override;
	virtual bool	render_item_ui_query() override;
	virtual bool	use_crosshair()	const override { return false; }
	virtual bool	GetBriefInfo		(II_BriefInfo& info) override;
	virtual void	net_Relcase			(CObject *object) override;

	virtual bool WpnCanShoot() const override { return false; }
	virtual bool UseScopeTexture() override { return true; }
	virtual CWeaponBinoculars* cast_weapon_binoculars() override { return this; }

protected:

	DECLARE_SCRIPT_REGISTER_FUNCTION
};