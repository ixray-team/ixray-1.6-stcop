#pragma once

#include "WeaponCustomPistol.h"
#include "../xrScripts/script_export_space.h"

class CUIFrameWindow;
class CUIStatic;

class CWeaponBinoculars final : public CWeaponCustomPistol
{
	using inherited = CWeaponCustomPistol;

	bool m_bVision = false;

public:
	CWeaponBinoculars() = default;
	virtual	~CWeaponBinoculars() = default;

	virtual void	Load				(const char* section) override;
	virtual void	LoadSounds			(const char* section) override;

	virtual void	OnZoomIn			();
	virtual void	OnZoomOut			();
	virtual	void	ZoomInc				();
	virtual	void	ZoomDec				();
	virtual void	net_Destroy			();
	virtual bool	net_Spawn			(CSE_Abstract* DC);
	bool			can_kill() const { return false; }
	virtual void	save				(NET_Packet &output_packet);
	virtual void	load				(IReader &input_packet);

	virtual bool	Action				(u16 cmd, u32 flags);
	virtual void	UpdateCL			();
	virtual void	render_item_ui		();
	virtual bool	render_item_ui_query();
	virtual bool	use_crosshair()	const override { return false; }
	virtual bool	GetBriefInfo		(II_BriefInfo& info);
	virtual void	net_Relcase			(CObject *object);

	virtual bool WpnCanShoot() const { return false; }
	virtual bool UseScopeTexture() { return true; }
	virtual CWeaponBinoculars* cast_weapon_binoculars() { return this; }

protected:

	DECLARE_SCRIPT_REGISTER_FUNCTION
};