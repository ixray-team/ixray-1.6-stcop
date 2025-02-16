#pragma once
#include "weaponmagazined.h"
#include "rocketlauncher.h"


class CWeaponFakeGrenade;


class CWeaponMagazinedWGrenade : public CWeaponMagazined,
								 public CRocketLauncher
{
	typedef CWeaponMagazined inherited;
public:
					CWeaponMagazinedWGrenade	(LPCSTR name="AK74",ESoundTypes eSoundType=SOUND_TYPE_WEAPON_SUBMACHINEGUN);
	virtual			~CWeaponMagazinedWGrenade	();

	virtual void	Load				(LPCSTR section);
	
	// Happens when object gets online. Import saved data from server here. Do oject initialization based on data imported from server
	virtual BOOL	net_Spawn			(CSE_Abstract* DC); // Note, this particular class does not call inherided weapon net Spawn, instead it calls huditem object net Spawn directly
	virtual void	net_Destroy			();
	// constant export of various weapon data to server. Here should be stuff, needed to be saved before object switches offline. Than it should be imported in net_spawn, when object gets online again
	virtual void	net_Export			(NET_Packet& P); // Note, this particular class does not call inherided weapon net Export, instead it calls huditem object net Export directly
	// not happens in Single Player
	virtual void	net_Import			(NET_Packet& P);
	
	virtual void	OnH_B_Independent	(bool just_before_destroy);

	// save stuff that is needed while weapon is in alife radius
	virtual void	save				(NET_Packet &output_packet);
	// load stuff needed in alife radius
	virtual void	load				(IReader &input_packet);


	virtual bool	Attach(PIItem pIItem, bool b_send_event);
	virtual bool	Detach(const char* item_section_name, bool b_spawn_item);
	virtual bool	CanAttach(PIItem pIItem);
	virtual bool	CanDetach(const char* item_section_name);
	virtual void	InitAddons();
	virtual bool	UseScopeTexture();
	virtual	float	CurrentZoomFactor	();

	virtual u8		GetCurrentHudOffsetIdx();
	virtual void	FireEnd					();
			void	LaunchGrenade			();
	
	virtual void	OnStateSwitch	(u32 S);
	
	virtual void	switch2_Reload	();
	virtual void	state_Fire		(float dt);
	virtual void	OnShot			();
	virtual void	OnEvent			(NET_Packet& P, u16 type);
	virtual void	ReloadMagazine	();

	virtual bool	Action			(u16 cmd, u32 flags);

	virtual void	UpdateSounds	();

	//переключение в режим подствольника
	virtual bool	SwitchMode		();
	void			PerformSwitchGL	();
	void			OnAnimationEnd	(u32 state);
	virtual void	OnMagazineEmpty	();

	virtual bool	IsNecessaryItem	    (const shared_str& item_sect);

	//виртуальные функции для проигрывания анимации HUD
	virtual void	PlayAnimShow		();
	virtual void	PlayAnimHide		();
	virtual void	PlayAnimReload		();
	virtual void	PlayAnimIdle		();
	virtual void	PlayAnimShoot		();
	virtual void	PlayAnimModeSwitch	();
	virtual void	PlayAnimBore		();

	//Fake grenade visability
	void			UpdateGrenadeVisibility(bool visibility);

private:
	virtual bool	install_upgrade_impl		( LPCSTR section, bool test );
	virtual	bool	install_upgrade_ammo_class	( LPCSTR section, bool test );

public:
	//дополнительные параметры патронов 
	//для подствольника
//-	CWeaponAmmo*			m_pAmmo2;
	xr_vector<shared_str>	ammoList2_; // переменная для временного хранения типов патронов неактивного магазина. Так же при инициализации хранит типы патронов для подствольника
	u8						inactiveAmmoIndex_; // переменная для временного хранения индекса типа патрона неактивного магазина.

	int						inactiveMagMaxSize_; // переменная для временного хранения макс размера неактивного магазина
	xr_vector<CCartridge>	inactiveMagazine_; // переменная для временного хранения содержимого неактивного магазина
	bool					grenadeMode_;

	CCartridge				m_DefaultCartridge2;
};