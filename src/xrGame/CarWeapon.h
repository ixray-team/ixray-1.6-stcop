#pragma once
#include "ShootingObject.h"
#include "HudSound.h"
#include "car.h"
#include "weaponAmmo.h"

XREFLECT(LuaEnum, Self="CCar", Name="wpn_action")
enum ECarWeaponAction
{
	eWpnDesiredDir = 1,
	eWpnDesiredPos,
	eWpnActivate,
	eWpnFire,
	eWpnAutoFire,
	eWpnToDefaultDir,
};

class CCarWeapon :public CShootingObject
{
protected:
	typedef CShootingObject ISO;

	virtual	void			UpdateFire			();
	virtual	void			OnShot				();
	virtual bool			IsHudModeNow		(){return false;};
	virtual const Fvector&	get_CurrentFirePoint(){return m_fire_xform.c;};
	virtual const Fmatrix&	get_ParticlesXFORM	(){return m_fire_xform;};
	bool					HasMGun				(){return m_AvlblMgun;};
	bool					HasRockets			(){return m_AvlblRockets;};
	u16						GetInitiatorID		();
	bool					inited;
	CCar*					m_pcar;
	shared_str				m_sRocketSection;
	bool					m_AvlblMgun;
	bool					m_AvlblRockets;
	float					m_fRocketLaunchSpeed;
	bool					m_fire_dir_by_cam;
	bool					m_unlimited_ammo;
public:
							CCarWeapon			(CCar* pcar, LPCSTR definition, int idx = 0);
				virtual		~CCarWeapon			();
				void		LoadParams			(LPCSTR section);
				void		UpdateCL			();
				void		UpdateFireDir		(const Fvector &dir);
				bool		IsDirByCam(){return m_fire_dir_by_cam;};
			void			startRocket			();
			void			StartFire			(){if(HasMGun())ISO::FireStart();};
			void			EndFire				(){if(HasMGun()){ISO::FireEnd();}};
			void			LightStop			(){if(HasMGun() && ISO::m_bLightShotEnabled)ISO::StopLight();};
	const Fvector&			FirePoint			(){return m_fire_xform.c;};
	const Fvector&			FireDir				(){return m_fire_xform.k;};
			void			Render_internal		(){if(HasMGun())ISO::RenderLight();};
	float					GetDispersion		(){if(HasMGun())return ISO::fireDispersionBase * m_Ammo->param_s.kDisp; return 0.0f;};
	bool					HasValid			(){return inited;};
	bool					HasFire				(){return ISO::bWorking;};
	void					SwitchAmmoType		();
	bool					unlimited_ammo		();

	void					AddAmmo(CWeaponAmmo *ammo);
	void					RefreshAmmoFromCar();
	void					DecreaseAmmo();
	void					DropEmptyAmmoBoxes();
	int						GetAmmoCount(int ammotype = -1);

	void					OnNextFireMode();
	void					OnPrevFireMode();

	bool					m_allow_fire;
	bool					m_bAutoFire;
	int						m_curr_idx;

	int						m_ammoTypesSize;
	int						m_ammoTypeCurrent;
	xr_vector<shared_str>	m_ammoTypes;
	CCartridge*				m_Ammo;

	xr_vector<int>			m_aFireModes;
	int						m_iCurFireMode;
	int						m_iShotsCount;
	int						GetFireMode(){if(m_aFireModes.size())return m_aFireModes[m_iCurFireMode]; return 0; };

	float					m_sndReloadTTotal;
	float					m_sndReloadSTime;

	bool					m_aftershot_delay;
private:
	u16						m_fire_bone;
	Fvector					m_fire_pos_rel, m_fire_dir_rel;

	Fmatrix					m_fire_xform;
	LPCSTR					m_weaponsect;

	HUD_SOUND_ITEM			m_sndShot;
	HUD_SOUND_ITEM			m_sndEmptyClick;
	HUD_SOUND_ITEM			m_sndReload;
	xr_vector<CWeaponAmmo*> m_ammo_objects[10];
};

class CCarTurret
{
protected:
	void					SetBoneCallbacks	();
	void					ResetBoneCallbacks	();
	bool					HasMGun(){return m_AvlblMgun;};
	bool					HasRockets(){return m_AvlblRockets;};
	bool					inited;
	CCar*					m_pcar;
	bool					m_bActive;
	bool					m_bMouseMoving_x, m_bMouseMoving_y;
	float					m_fGunVelMult_x, m_fGunVelMult_y;
	float					m_fGunForcMult_x, m_fGunForcMult_y;
	bool					m_bRelative_link_x, m_bRelative_link_y;
	float					m_weapon_h;
	shared_str				m_sRocketSection;
	bool					m_AvlblMgun;
	bool					m_AvlblRockets;
	float					m_fRocketLaunchSpeed;
	bool					m_combine_input;
	CCameraBase*			camera;
	u16						m_guns_size;
	xr_vector<CCarWeapon*>	m_turret_guns;
	CCarWeapon*				m_cur_gun;
	int						m_icur_gun_idx;
public:
	enum
	{
		eWpnDesiredDir =1,
		eWpnDesiredPos,
		eWpnActivate,
		eWpnFire,
		eWpnAutoFire,
		eWpnToDefaultDir,
	};	
							CCarTurret			(CCar* pcar, LPCSTR definition);
				virtual		~CCarTurret			();
	static void __stdcall	BoneCallbackX		(CBoneInstance *B);
	static void	__stdcall	BoneCallbackY		(CBoneInstance *B);
			void			UpdateCL			();
			void			Render_internal		();
			void			cam_Update			(float fov);
			void			PhDataUpdate		();
			void			UpdateBarrelDir		();
			void			OnMouseMove			(int x, int y);
			void			Action				(int id, u32 flags);
			void			SetParam			(int id, Fvector2 val);
			void			SetParam			(int id, Fvector val);
			bool			AllowFire			(){return m_cur_gun->m_allow_fire;};
			float			FireDirDiff			();
			IC bool			IsActive			() {return m_bActive;}
			float			_height				() const {return m_weapon_h;};
			const Fvector&	ViewCameraPos		(){return m_cam_xform.c;};
			const Fvector&	ViewCameraDir		(){return m_cam_xform.k;};
			const Fvector&	ViewCameraNorm		(){return m_cam_xform.i;};
			// UNUSED: ViewCameraMatx
			// const Fmatrix&	ViewCameraMatx		(){return m_cam_xform;};
			CCameraBase*	Camera				(){return camera;};
			bool			CameraEnabled		(){return !!(m_cam_bone != BI_NONE);};
			bool			CombinedInput		(){return m_combine_input;};
			bool            HasPhysical			(){return !!(JointGun && JointGun->bActive && JointTurret && JointTurret->bActive); }
			CCarWeapon*		GetCurrGun			(){if(GetGunsSize())return m_cur_gun; return NULL;};
			float			GetDispersion		(){if(GetGunsSize())return m_cur_gun->GetDispersion(); return 0.0f;};
			int				GetGunsSize			(){return m_turret_guns.size();};
			void			SetGun				(int val);
			// UNUSED: SetPrevGun / SetNextGun
			// void			SetPrevGun			();
			// void			SetNextGun			();
			void			SetGunCircle		();
			void			GetBriefInfo		(xr_string& str_name, xr_string& icon_sect_name, xr_string& str_count);
			void			AppendAmmoToGuns(CWeaponAmmo *ammo);
			void			DropEmptyAmmoBoxes();
private:
	CPhysicsJoint*			JointGun;
	CPhysicsJoint*			JointTurret;
	u32						mouse_moving_time;

	u16						m_rotate_x_bone, m_rotate_y_bone, m_cam_bone;
	float					m_tgt_x_rot, m_tgt_y_rot, m_cur_x_rot, m_cur_y_rot, m_bind_x_rot, m_bind_y_rot;
	Fvector					m_cam_pos_rel, m_cam_dir_rel;
	Fmatrix					m_saved_x_xform, m_saved_y_xform;
	Fmatrix					m_i_bind_x_xform, m_i_bind_y_xform, m_cam_xform;
	Fvector2				m_lim_x_rot, m_lim_y_rot;
	float					m_min_gun_speed, m_max_gun_speed;
	Fvector					m_destEnemyDir;
};
