#pragma once
#include "WeaponMagazined.h"
#include "RocketLauncher.h"


class CWeaponFakeGrenade;


class CWeaponMagazinedWGrenade : public CWeaponMagazined,
								 public CRocketLauncher
{
	using inherited = CWeaponMagazined;
public:
	CWeaponMagazinedWGrenade() = default;
	virtual ~CWeaponMagazinedWGrenade() override = default;

	virtual void Load(const char* section) override;
	void ReloadConfig() override;
	virtual void	LoadSounds			(const char* section) override;

	virtual bool	net_Spawn			(CSE_Abstract* DC) override;
	virtual void	net_Destroy			() override;
	virtual void	net_Export			(NET_Packet& P) override;
	virtual void	net_Import			(NET_Packet& P) override;

	virtual void	OnH_B_Independent	(bool just_before_destroy) override;

	virtual void	save				(NET_Packet &output_packet) override;
	virtual void	load				(IReader &input_packet) override;
	virtual void Serialize(ISaveObject& Object) override;


	virtual bool	Attach					(PIItem pIItem) override;
	virtual bool	Detach					(PIItem pIItem) override;
	virtual bool	CanAttach				(PIItem pIItem) override;
	virtual bool	CanDetach				(PIItem pIItem) override;
	virtual void	InitAddons				() override;
	virtual bool	UseScopeTexture			() override;
	virtual float	CurrentZoomFactor		() override;
	virtual void	FireStart				() override;
	virtual void	FireEnd					() override;
			void	LaunchGrenade_Correct	(Fvector3* v);

	virtual void	OnStateSwitch	(u8 S) override;

	virtual void	switch2_Hiding() override;
	virtual void	switch2_Showing() override;
	virtual void	switch2_Reload() override;
			void	switch2_SwitchMode();
	virtual void	state_Fire		(float dt) override;
	virtual void	OnEvent			(NET_Packet& P, u16 type) override;
	virtual void	ReloadMagazine	() override;
	virtual void	UnloadMagazine	(bool spawn_ammo = true) override;

	virtual bool	Action			(u16 cmd, u32 flags) override;

	virtual void	UpdateSounds	() override;

	//переключение в режим подствольника
	virtual bool	SwitchMode		() override;
	void			PerformSwitchGL	();
	virtual void			OnAnimationEnd	(u8 state) override;
	virtual bool	GetBriefInfo			(II_BriefInfo& info) override;

	virtual bool	IsNecessaryItem	    (const shared_str& item_sect) override;
	virtual float Weight() const override;

	//виртуальные функции для проигрывания анимации HUD
	virtual void	PlayAnimModeSwitch();
	virtual shared_str SetCurrentReloadAnimation() override;
	virtual shared_str SetCurrentStateAnimation(const shared_str& first_name) override;
	virtual shared_str SetCurrentShootAnimation() override;

	virtual CWeaponMagazinedWGrenade* cast_weapon_magazined_w_grenade() override { return this; }
	virtual CRocketLauncher* cast_rocket_launcher() override { return this; }

	virtual bool	IsGrenadeMode() const override { return m_bGrenadeMode; }
private:
	virtual void	net_Spawn_install_upgrades	( Upgrades_type saved_upgrades ) override;
	virtual bool	install_upgrade_impl		( const char* section, bool test ) override;
	virtual bool	install_upgrade_ammo_class	( const char* section, bool test ) override;
	
			int		GetAmmoCount2				( u8 ammo2_type ) const;

protected:
	virtual void ForceUpdateHUD() override;

public:
	//дополнительные параметры патронов 
	//для подствольника
	xr_vector<shared_str>	m_ammoTypes2 = {};

	int						iMagazineSize2 = 0;
	xr_vector<CCartridge>	m_magazine2 = {};

	bool					m_bGrenadeMode = false;

	CCartridge				m_DefaultCartridge2;

	virtual u8				GetTargetAmmoType(bool for_grenade_mode = false) const override;
	virtual u8				GetAmmoType(bool for_grenade_mode = false) const override;
	virtual const xr_vector<shared_str>& getAmmoTypes(bool for_grenade_mode = false) const override;
	virtual int	GetCurrentElapsed(bool for_grenade_mode = false) const override;

	virtual void UpdateGrenadeVisibility(bool visibility);

	//название косточки для гранаты подствольника в HUD
	shared_str grenade_bone_name;
};
