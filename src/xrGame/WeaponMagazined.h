#pragma once

#include "Weapon.h"
#include "../xrSound/ai_sounds.h"

class ENGINE_API CMotionDef;

//размер очереди считается бесконечность
//заканчиваем стрельбу, только, если кончились патроны
#define WEAPON_ININITE_QUEUE -1

class CWeaponBinoculars;
class CWeaponMagazinedWGrenade;
class CWeaponBM16;
class CWeaponRPG7;
class CWeaponRG6;

class CWeaponMagazined : public CWeapon
{
	using inherited = CWeapon;
protected:
	//звук текущего выстрела
	xr_string		m_sSndShotCurrent = "sndShot";
	ESoundTypes		m_eSoundShow = ESoundTypes(SOUND_TYPE_ITEM_TAKING);
	ESoundTypes		m_eSoundHide = ESoundTypes(SOUND_TYPE_ITEM_HIDING);
	ESoundTypes		m_eSoundShot = ESoundTypes(SOUND_TYPE_WEAPON_SHOOTING);
	ESoundTypes		m_eSoundEmptyClick = ESoundTypes(SOUND_TYPE_WEAPON_EMPTY_CLICKING);
	ESoundTypes		m_eSoundReload = ESoundTypes(SOUND_TYPE_WEAPON_RECHARGING);
	ESoundTypes		m_eSoundAim = ESoundTypes(SOUND_TYPE_WEAPON);
	ESoundTypes		m_eSoundAimOut = ESoundTypes(SOUND_TYPE_WEAPON);

	bool			m_sounds_enabled = true;
	// General
	//кадр момента пересчета UpdateSounds
	u32				dwUpdateSounds_Frame;
protected:

	virtual void	switch2_Idle	();
	virtual void	switch2_Fire	();
	virtual void	switch2_Empty	();
	virtual void	switch2_Device	();
	virtual void	switch2_Reload	();
	virtual void	switch2_Hiding	();
	virtual void	switch2_Hidden	();
	virtual void	switch2_Showing	();
	virtual void	switch2_FireMode();
	virtual void	switch2_LightMis();
	virtual void	switch2_Kick	();
	virtual void	switch2_MagCheck();
	virtual void	switch2_FiremodeCheck();
	virtual void	switch2_ChamberLoad();
	virtual void	switch2_ChamberUnload();
	virtual void	switch2_ChamberCheck();
	virtual void	switch2_Pump();
	virtual void	switch2_Bore() override;
	void			switch2_Safemode();
	
	virtual void	OnShot			();
			void	OnShotJammed	();
			void	SelectShotSound ();
	
	virtual void	OnEmptyClick	();

	virtual void	OnAnimationEnd	(u32 state);
	virtual void	OnStateSwitch	(u32 S);

	virtual void	UpdateSounds	();

	bool			TryReload		();
	bool			TryReloadChamber		();

protected:
	virtual void	ReloadMagazine();
	bool			HaveCartridgeInInventory(u8 cnt);
	virtual u8		AddCartridge(u8 cnt);
			void	ApplySilencerKoeffs();
			void	ResetSilencerKoeffs();

	virtual void	state_Fire		(float dt);
	virtual void	state_FireChamber(float dt);
public:
	CWeaponMagazined() = default;
	virtual ~CWeaponMagazined() = default;

	virtual void	Load(const char* section);
	virtual void	LoadSounds(const char* section);
			void	LoadSilencerKoeffs();

	virtual CWeaponBinoculars* cast_weapon_binoculars() { return nullptr; }
	virtual CWeaponMagazined* cast_weapon_magazined() { return this; }
	virtual CWeaponMagazinedWGrenade* cast_weapon_magazined_w_grenade() { return nullptr; }
	virtual CWeaponBM16* cast_weapon_bm16() { return nullptr; }
	virtual CWeaponRPG7* cast_weapon_rpg7() { return nullptr; }
	virtual CWeaponRG6* cast_weapon_rg6() { return nullptr; }

	virtual void	SetDefaults		();
	virtual void	FireStart		();
	virtual void	FireEnd			();
	virtual void	Reload			();
	

	virtual	void	UpdateCL		();
	virtual void	net_Destroy		();
	virtual void	net_Export		(NET_Packet& P);
	virtual void	net_Import		(NET_Packet& P);
	virtual void	OnEvent			(NET_Packet& P, u16 type);
	virtual void	OnH_A_Chield		();

	virtual bool	Attach			(PIItem pIItem, bool b_send_event);
	virtual bool	Detach			(const char* item_section_name, bool b_spawn_item);
			bool	DetachScope		(const char* item_section_name, bool b_spawn_item);
	virtual bool	CanAttach		(PIItem pIItem);
	virtual bool	CanDetach		(const char* item_section_name);

	virtual void	InitAddons		();
	virtual void	HudSelector		();

	virtual bool	Action			(u16 cmd, u32 flags);
	bool			IsAmmoAvailable	();
	virtual void	UnloadMagazine	(bool spawn_ammo = true);

	virtual bool	GetBriefInfo	(II_BriefInfo& info);

	virtual void	UpdateBonePartAnimations() override;
	void UpdateFiremodeAnimations();
	void UpdateIdleAnimations();

	bool			bMisfireReload = false;

public:
	virtual bool	SwitchMode				();
	virtual bool	SingleShotMode			()			{ return m_iQueueSize == 1; }
	virtual void	SetQueueSize			(s8 size)	{ m_iQueueSize = size; }
	IC		s8		GetQueueSize			() const	{ return m_iQueueSize; }
	virtual bool	StopedAfterQueueFired	()			{return m_bStopedAfterQueueFired; }
	virtual void	StopedAfterQueueFired	(bool value){ m_bStopedAfterQueueFired = value; }
	virtual float	GetFireDispersion		(float cartridge_k, bool for_crosshair = false);

protected:
	//максимальный размер очереди, которой можно стрельнуть
	s8				m_iQueueSize = WEAPON_ININITE_QUEUE;
	//количество реально выстреляных патронов
	int				m_iShotNum = 0;
	//после какого патрона, при непрерывной стрельбе, начинается отдача (сделано из-за Абакана)
	int				m_iBaseDispersionedBulletsCount;
	//скорость вылета патронов, на которые не влияет отдача (сделано из-за Абакана)
	float			m_fBaseDispersionedBulletsSpeed;
	float			m_fBaseDispersionedBulletsTimeDelta;
	float			m_fSingleShootsTimeDelta;
	//скорость вылета остальных патронов
	float			m_fOldBulletSpeed = 0.0f;
	Fvector			m_vStartPos, m_vStartDir;
	//флаг того, что мы остановились после того как выстреляли
	//ровно столько патронов, сколько было задано в m_iQueueSize
	bool			m_bStopedAfterQueueFired;
	//флаг того, что хотя бы один выстрел мы должны сделать
	//(даже если очень быстро нажали на курок и вызвалось FireEnd)
	bool			m_bFireSingleShot = false;
	//режимы стрельбы
	xr_vector<s8>	m_aFireModes;
	u8				m_iCurFireMode;
	s8				m_iPrevFireMode;

	//переменная блокирует использование
	//только разных типов патронов
	bool m_bLockType = false;

public:
	virtual void	OnZoomIn			();
	virtual void	OnZoomOut			();
			void	ChangeFireMode		(u16 cmd);
			void	SwitchGaussScreen();
			bool	HasFireModes		() { return m_aFireModes.size() > 1; };
	virtual	int		GetCurrentFireMode	() { return m_aFireModes[m_iCurFireMode]; };	

	virtual void	save				(NET_Packet &output_packet);
	virtual void	load				(IReader &input_packet);

	virtual void OnMotionMark(u32 state, const motion_marks&);
	virtual bool WpnCanShoot() const { return true; }

protected:
	virtual bool	install_upgrade_impl( const char* section, bool test );

protected:
	virtual bool	AllowFireWhileWorking() {return false;}

	//виртуальные функции для проигрывания анимации HUD
	virtual void	PlayAnimShow		();
	virtual void	PlayAnimHide		();
	virtual void	PlayAnimReload		();
	virtual void	PlayAnimIdle		();
	virtual void	PlayAnimShoot		();
	virtual void	PlayReloadSound		();
	virtual void	PlayAnimAim			();
	virtual void    PlaySoundAim		(bool in = true);
	virtual shared_str SetCurrentReloadAnimation();
	virtual shared_str SetCurrentShootAnimation();
	virtual shared_str SetCurrentStateAnimation(const shared_str& first_name);
	virtual shared_str SetCurrentAimAnimation();
	shared_str SetCurrentPumpAnimation();

	virtual	int		ShotsFired			() { return m_iShotNum; }
	virtual float	GetWeaponDeterioration() final override;

	virtual void	FireBullet			(const Fvector& pos, 
        								 const Fvector& dir, 
										 float fire_disp,
										 const CCartridge& cartridge,
										 u16 parent_ids,
										 u16 weapon_id,
										 bool send_hit) final override;

};
