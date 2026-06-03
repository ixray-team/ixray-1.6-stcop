#pragma once

#include "../xrSound/ai_sounds.h"
#include "Weapon.h"
#include "FlameCanister.h"
#include "FlamethrowerTraceCollision.h"

namespace FlamethrowerTrace
{
	class CManager;
}

class CFlamethrower : public CWeapon
{
private:
	typedef CWeapon inherited;
protected:
	//���� �������� ��������
	shared_str		m_sSndShotCurrent;

	ESoundTypes		m_eSoundShow;
	ESoundTypes		m_eSoundHide;
	ESoundTypes		m_eSoundShot;
	ESoundTypes		m_eSoundEmptyClick;
	ESoundTypes		m_eSoundReload;
	ESoundTypes		m_eSoundClose;
	ESoundTypes		m_eSoundReflect;
	bool			m_sounds_enabled;
	// General
	//���� ������� ��������� UpdateSounds
	u32				dwUpdateSounds_Frame;

	//virtual void    CheckMagazine();

	bool            m_bNeedBulletInGun;

	bool            m_bCustomShotSounds;
protected:
	void	OnMagazineEmpty() override;

	virtual void	switch2_Idle();
	virtual void	switch2_Fire();
	virtual void	switch2_Empty();
	virtual void	switch2_Reload();
	virtual void	switch2_Hiding();
	virtual void	switch2_Hidden();
	virtual void	switch2_Showing();

	void	OnShot() override;
	virtual void	StopShooting() override;

	virtual void	OnEmptyClick();

	void	OnAnimationEnd(u8 state) override;
	void	OnStateSwitch(u8 S) override;

	virtual void	UpdateSounds();

	bool			TryReload();

protected:
	virtual void	ReloadMagazine();

	virtual void	state_FireCharge(float dt);
	virtual void	state_Fire(float dt);
	virtual void	state_Idle(float dt);
	virtual void	state_MagEmpty(float dt);
	virtual void	state_Misfire(float dt);

public:
	CFlamethrower(ESoundTypes eSoundType = SOUND_TYPE_WEAPON_SUBMACHINEGUN);
	~CFlamethrower() override;

	void	Load(str_c section) override;
	virtual CFlamethrower* cast_flamethrower() override { return this; }

	bool    UseScopeTexture() override;
	void	SetDefaults() override;
	void	FireStart() override;
	void	FireEnd() override;
	void	Reload() override;

	virtual bool IsZoomEnabled() const override { return false; }

	void	UpdateCL() override;
	void	net_Destroy() override;
	void	net_Export(NET_Packet& P) override;
	void	net_Import(NET_Packet& P) override;

	void	OnH_A_Chield() override;

	bool	Attach(PIItem pIItem, bool b_send_event) override;
	bool	Detach(const char* item_section_name, bool b_spawn_item) override;
	bool	DetachScope(const char* item_section_name, bool b_spawn_item);
	bool	CanAttach(PIItem pIItem) override;
	bool	CanDetach(const char* item_section_name) override;

	void	InitAddons() override;

	bool	Action(u16 cmd, u32 flags) override;
	bool			IsAmmoAvailable();
	virtual void	UnloadMagazine(bool spawn_ammo = true);
	virtual int     CheckAmmoBeforeReload(u8& v_ammoType);
	void	OnMotionMark(u8 state, const motion_marks& M) override;

	bool	GetBriefInfo(II_BriefInfo& info) override;

	virtual bool			IsMisfire() const override;

protected:
	float			m_fOldBulletSpeed;
	Fvector			m_vStartPos, m_vStartDir;
	bool			m_bFireSingleShot;

	bool m_bLockType;
	bool m_bAutoreloadEnabled;
	bool m_opened;
	bool m_bUseFiremodeChangeAnim;

	CFlameCanister* AmmoCanister = nullptr;

	// overheating
	bool m_is_overheated = false;
	float m_overheating_state = 0.0f;
	float m_overheating_decrease_speed = 0.0f;
	float m_overheating_increase_speed_min = 0.0f;
	float m_overheating_increase_speed_max = 0.0f;
	float m_overheating_reset_level_max = 0.0f;

	//shooting
	float m_charge_speed = 0.0f;
	float m_current_charge = 0.0f;

	// fuel params
	float m_fuel_reduce_speed_charge = 0.0f;
	float m_fuel_reduce_speed_shoot = 0.0f;

	float m_current_fuel_level = 0.0f;
	shared_str m_fuel_section_name;
	float m_dps = 0.0f;
	float m_burn_time = 0.0f;

	bool m_keep_charge = false;
	bool m_is_particle_active = false;
	xr_shared_ptr<CParticlesObject> m_pFlameTraceParticles;
	shared_str m_FlameTraceParticlesName;

public:
	void	OnZoomIn() override;
	void	OnZoomOut() override;

	void	save(NET_Packet& output_packet) override;
	void	load(IReader& input_packet) override;

	virtual void Serialize(ISaveObject& Object) override;

	void	SpawnFuelCanister(float Condition, str_c ammoSect = nullptr, ALife::_OBJECT_ID ParentID = ALife::INVALID_OBJECT_ID);

protected:
	bool	install_upgrade_impl(str_c section, bool test) override;

protected:
	virtual bool	AllowFireWhileWorking() { return false; }

	//����������� ������� ��� ������������ �������� HUD
	virtual void	PlayAnimShow();
	virtual void	PlayAnimHide();
	virtual void	PlayAnimReload();
	void	PlayAnimIdle() override;
	virtual void	PlayAnimShoot();
	virtual void	PlayReloadSound();
	//virtual void	PlayAnimAim();
	void	PlayAnimBore() override;
	virtual shared_str SetCurrentReloadAnimation();
	virtual shared_str SetCurrentShootAnimation();
	virtual shared_str SetCurrentStateAnimation(const shared_str& first_name);
	virtual shared_str SetCurrentAimAnimation();

protected:

	virtual void    SetAnimFlag(u32 flag, str_c anim_name);

	// ����� ������� ��������, ����� �� ������ �������, ��� ����� ������� ����� �����
	enum {
		ANM_SHOW_EMPTY = (1 << 0),
		ANM_HIDE_EMPTY = (1 << 1),
		ANM_AIM_EMPTY = (1 << 2),
		ANM_BORE_EMPTY = (1 << 3),
		ANM_SHOT_EMPTY = (1 << 4),
		ANM_SPRINT_EMPTY = (1 << 5),
		ANM_MOVING_EMPTY = (1 << 6),
		ANM_RELOAD_EMPTY = (1 << 7),
		ANM_RELOAD_EMPTY_GL = (1 << 8),
		ANM_SHOT_AIM = (1 << 9),
		ANM_SHOT_AIM_GL = (1 << 10),
		ANM_MISFIRE = (1 << 11),
		ANM_MISFIRE_GL = (1 << 12),
		ANM_IDLE_EMPTY = (1 << 13),
	};

	Flags32 psWpnAnimsFlag;

	bool WeaponSoundExist(str_c section, str_c sound_name, bool log = false) const;

	float	GetWeaponDeterioration() override;


	void	FireBullet(const Fvector& pos,
		const Fvector& dir,
		float fire_disp,
		const CCartridge& cartridge,
		ALife::_OBJECT_ID parent_id,
		ALife::_OBJECT_ID weapon_id,
		bool send_hit) override;

	FlamethrowerTrace::CManager TraceManager;

};
