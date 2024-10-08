#pragma once

#include "../xrPhysics/PhysicsShell.h"
#include "weaponammo.h"
#include "PHShellCreator.h"

#include "ShootingObject.h"
#include "hud_item_object.h"
#include "Actor_Flags.h"
#include "../Include/xrRender/KinematicsAnimated.h"
#include "firedeps.h"
#include "game_cl_single.h"
#include "first_bullet_controller.h"

#include "CameraRecoil.h"
#include "Actor.h"
#include "script_game_object.h"

#include "Scope.h"

class CEntity;
class ENGINE_API CMotionDef;
class CSE_ALifeItemWeapon;
class CSE_ALifeItemWeaponAmmo;
class CWeaponMagazined;
class CParticlesObject;
class CUIWindow;
class CBinocularsVision;
class CNightVisionEffector;

class CWeapon : public CHudItemObject,
				public CShootingObject
{
private:
	typedef CHudItemObject inherited;

public:
							CWeapon				();
	virtual					~CWeapon			();

	// Generic
	virtual void			Load				(LPCSTR section);

	virtual bool			WeaponSoundExist	(const char* section, const char* sound_name);

	virtual BOOL			net_Spawn			(CSE_Abstract* DC);
	virtual void			net_Destroy			();
	virtual void			net_Export			(NET_Packet& P);
	virtual void			net_Import			(NET_Packet& P);
	
	virtual CWeapon			*cast_weapon			()					{return this;}
	virtual CWeaponMagazined*cast_weapon_magazined	()					{return 0;}


	//serialization
	virtual void			save				(NET_Packet &output_packet);
	virtual void			load				(IReader &input_packet);
	virtual BOOL			net_SaveRelevant	()								{return inherited::net_SaveRelevant();}

	virtual void			UpdateCL			();
	virtual void			shedule_Update		(u32 dt);
	virtual bool			register_schedule() const {return false;}
	virtual void			renderable_Render	();
	virtual void			render_hud_mode		();
	virtual bool			need_renderable		();

	virtual void			render_item_ui		();
	virtual bool			render_item_ui_query();

	virtual void			OnH_B_Chield		();
	virtual void			OnH_A_Chield		();
	virtual void			OnH_B_Independent	(bool just_before_destroy);
	virtual void			OnH_A_Independent	();
	virtual void			OnEvent				(NET_Packet& P, u16 type);// {inherited::OnEvent(P,type);}

	virtual	void			Hit					(SHit* pHDS);

	virtual void			reinit				();
	virtual void			reload				(LPCSTR section);
	virtual void			create_physic_shell	();
	virtual void			activate_physic_shell();
	virtual void			setup_physic_shell	();

	virtual void			SwitchState			(u32 S);

	virtual void			OnActiveItem		();
	virtual void			OnHiddenItem		();
	virtual bool			SendDeactivateItem	();
	virtual void			SendHiddenItem		();	//same as OnHiddenItem but for client... (sends message to a server)...

		const bool			ScopeFit			(CScope*) const;

public:
	virtual bool			can_kill			() const;
	virtual CInventoryItem	*can_kill			(CInventory *inventory) const;
	virtual const CInventoryItem *can_kill		(const xr_vector<const CGameObject*> &items) const;
	virtual bool			ready_to_kill		() const;
	virtual bool			NeedToDestroyObject	() const; 
	virtual ALife::_TIME_ID	TimePassedAfterIndependant() const;
	virtual float GetHudFov();

	const CameraRecoil& getCameraRecoil(void) const;
	const CameraRecoil& getCameraZoomRecoil(void) const;
protected:
	//время удаления оружия
	ALife::_TIME_ID			m_dwWeaponRemoveTime;
	ALife::_TIME_ID			m_dwWeaponIndependencyTime;
	float m_HudFovZoom;
	virtual bool			IsHudModeNow		();
public:
	void					signal_HideComplete	();
	virtual bool			Action(u16 cmd, u32 flags);

	enum EWeaponStates {
		eFire		= eLastBaseState+1,
		eFire2,
		eReload,
		eMisfire,
		eSwitch,
		eSwitchMode,
		eEmptyClick,
		eUnjam,
		eCheckMisfire,
		eShowingDet,
		eShowingEndDet,
		eHideDet,
		eKick,
		eLightMis,
	};
	enum EWeaponSubStates{
		eSubstateReloadBegin		=0,
		eSubstateReloadInProcess,
		eSubstateReloadEnd,
	};
	enum { undefined_ammo_type = u8(-1) };

	// Does weapon need's update?
	BOOL					IsUpdating			();


	const bool				IsMisfire			() const;
	bool					CheckForMisfire		();


	BOOL					AutoSpawnAmmo		() const		{ return m_bAutoSpawnAmmo; };
	bool					IsTriStateReload	() const		{ return m_bTriStateReload;}
	EWeaponSubStates		GetReloadState		() const		{ return (EWeaponSubStates)m_sub_state;}
	u8						m_sub_state;
protected:
	bool					m_bTriStateReload;
	// a misfire happens, you'll need to rearm weapon
	bool					bMisfire;				

	BOOL					m_bAutoSpawnAmmo;
public:
	const bool IsGrenadeLauncherAttached() const;
	const bool IsScopeAttached() const;
	const bool IsSilencerAttached() const;

	const bool GrenadeLauncherAttachable() const;
	const bool ScopeAttachable() const;
	const bool SilencerAttachable() const;
			
	const ALife::EWeaponAddonStatus	get_GrenadeLauncherStatus	() const { return m_eGrenadeLauncherStatus; }
	const ALife::EWeaponAddonStatus	get_ScopeStatus				() const { return m_eScopeStatus; }
	const ALife::EWeaponAddonStatus	get_SilencerStatus			() const { return m_eSilencerStatus; }

	const bool UseScopeTexture() const {return !IsGrenadeMode();}

	//обновление видимости для косточек аддонов
	virtual void UpdateAddonsVisibility();
	virtual void UpdateHUDAddonsVisibility();
	//инициализация свойств присоединенных аддонов
	virtual void InitAddons();

	//для отображения иконок апгрейдов в интерфейсе

	const int GetScopeX() const
	{ 
		return pSettings->r_s32(m_scopes[m_cur_scope], "scope_x") * (1 + isHQIcons);
	}

	const int GetScopeY() const
	{
		return pSettings->r_s32(m_scopes[m_cur_scope], "scope_y") * (1 + isHQIcons);
	}

	const int	GetSilencerX() {return m_iSilencerX;}
	const int	GetSilencerY() {return m_iSilencerY;}
	void SetSilencerX(int value);
	void SetSilencerY(int value);
	const int	GetGrenadeLauncherX() {return m_iGrenadeLauncherX;}
	const int	GetGrenadeLauncherY() {return m_iGrenadeLauncherY;}

	const shared_str& GetGrenadeLauncherName	() const {return m_sGrenadeLauncherName;}
	const shared_str GetScopeName				() const {return pSettings->r_string(m_scopes[m_cur_scope], "scope_name");}
	const shared_str& GetSilencerName			() const {return m_sSilencerName;}

	IC void	ForceUpdateAmmo						()		{ m_BriefInfo_CalcFrame = 0; }

	u8		GetAddonsState						()		const		{return m_flagsAddOnState;};
	void	SetAddonsState						(u8 st)	{m_flagsAddOnState=st;}//dont use!!! for buy menu only!!!

	bool	NeedBlockSprint						() const; 

	struct lens_zoom_params
	{
		float delta = 0.0f;
		float target_position = 0.0f;
		float speed = 0.0f;
		float factor_min = 0.0f;
		float factor_max = 0.0f;
		float gyro_period = 0.0f;
		float real_position = 0.0f;
		u32   last_gyro_snd_time = 0;
	};

	lens_zoom_params _lens_zoom_params;

	struct stepped_params
	{
		float max_value = 1.f;
		float min_value = 0.f;
		float cur_value = 0.5f;
		int cur_step = 1;
		int steps = 2;
		float jitter = 0.1f;
	};

	stepped_params _lens_night_brightness;

	struct conditional_breaking_params
	{
		float start_condition = 0.0f;     // при каком состоянии начнутся проблемы
		float end_condition = 0.0f;       // при каком состоянии отрубится вообще
		float start_probability = 0.0f;   // вероятность проблем в стартовом состоянии
	};

	conditional_breaking_params CollimatorBreakingParams;
	conditional_breaking_params TorchBreakingParams;

	struct light_misfire_params
	{
		float startcond = 1.0f;
		float endcond = 0.0f;
		float startprob = 1.0f;
		float endprob = 0.0f;
	};

	light_misfire_params light_misfire;

	float lock_time;
	float _last_recharge_time;
	float m_fRechargeTime;
	float m_fCollimatorLevelsProblem;
	float m_fMisfireAfterProblemsLevel;

	xr_string curr_anim;

	int ammo_cnt_to_reload;
	int _last_shot_ammotype;
	u32 _last_update_time;
	s32 _lens_night_brightness_saved_step;
	u32 _last_shot_time;

	bool bReloadKeyPressed;
	bool bAmmotypeKeyPressed;
	bool bStopReloadSignal;
	bool bUnjamKeyPressed;
	bool bNextModeKeyPressed;
	bool bPrevModeKeyPressed;
	bool m_bUseSilHud;
	bool m_bUseScopeHud;
	bool m_bUseGLHud;
	bool m_bUseChangeFireModeAnim;
	bool m_bRestGL_and_Sil;
	bool m_bJamNotShot;
	bool m_bAmmoInChamber;
	bool bIsNeedCallDet;
	bool IsReloaded;
	bool IsAimStarted;
	bool m_bMixAfterIdle;
	bool m_bMixAfterReload;
	bool m_bMixAfterQueue;
	bool _wanim_force_assign;
	bool is_firstlast_ammo_swapped;
	bool m_bAimScopeAnims;
	bool m_bAddCartridgeOpen;
	bool m_bEmptyPreloadMode;
	bool bPreloadAnimAdapter;
	bool bUpdateHUDBonesVisibility;
	bool _is_just_after_reload;
	bool bBlockQK;
	bool bBlockQKSil;
	bool bBlockQKScp;
	bool bBlockQKGL;
	bool bBlockQKGLM;
	bool m_bHideColimSightInAlter;
	bool m_bPreviousShotType;
	bool m_bNoJamFirstShot;
	bool m_bActorCanShoot;
	bool m_bUseLightMis;
	bool m_bDisableLightMisDet;
	bool bIsTorchEnabled;

	shared_str hud_silencer;
	shared_str hud_scope;
	shared_str hud_gl;

	RStringVec m_bDefHideBones, m_bDefShowBones, m_bHideBonesOverride, m_bDefHideBonesGLAttached,
	m_bHideBonesGLAttached, m_bHideBonesSilAttached, m_bHideBonesScopeAttached,
	m_bHideBonesUpgrade, m_bScopeShowBones, m_bScopeHideBones, m_bShowBonesUpgToHide, m_bShowBonesUpgToShow, m_sCollimatorSightsBones;

	virtual void DoReload() {}
	void SetMisfireStatus(bool status) { bMisfire = status; }

	void HideOneUpgradeLevel(const char* section);
	void ModUpdate();
	void ProcessScope();
	void LoadUpgradeBonesToHide(const char* section, const char* line);
	void SelectCurrentOffset(Fvector& pos, Fvector& rot);
	void MakeWeaponKick(Fvector3& pos, Fvector3& dir);
	void ReassignWorldAnims();
	void switch2_Suicide();
	void switch2_SuicideStop();
	void ReloadNightBrightnessParams();
	void LoadNightBrightnessParamsFromSection(const char* sect);
	void ChangeNightBrightness(int steps);
	void SetNightBrightness(int steps, bool use_sound);
	void UpdateZoomCrosshairUI();
	void SetLensParams(lens_zoom_params& params);
	void UpdateLensFactor(u32 timedelta);
	void SetLastRechargeTime(float time) { _last_recharge_time = time; }
	void UpdateCollimatorSight();
	void UpdateTorch();
	void LaunchGrenade_Correct(Fvector3& v);
	void LaunchGrenade_controller_Correct(Fvector3& v);
	virtual void OnShotJammed() {}

	bool IsChangeAmmoType() const { return (m_set_next_ammoType_on_reload != undefined_ammo_type || m_ammoType == m_set_next_ammoType_on_reload); }
	bool OnActWhileReload_CanActNow() const;
	bool Action_PrepareEarlyShotInReload();
	bool CanAimNow() const;
	bool CanLeaveAimNow() const;
	bool IsCollimatorInstalled() const;
	bool IsHudModelForceUnhide() const;
	bool IsUIForceUnhiding() const;
	bool IsUIForceHiding() const;
	bool FindBoolValueInUpgradesDef(const char* key, bool def, bool scan_after_nodefault = false) const;
	bool IsActionProcessing() const;
	bool IsLensedScopeInstalled() const;
	bool IsJamProhibited();
	bool OnWeaponJam();
	bool CheckForMisfire_validate_NoMisfire();
	void SwitchTorch(bool status, bool forced = false);

	const virtual bool IsGrenadeMode() const { return false; }
	virtual bool TryReload() { return false; }

	shared_str GetCurrentScopeSection() const { return m_scopes[m_cur_scope]; }
	shared_str GetScopeSection(int idx) const { return m_scopes[idx]; }
	shared_str FindStrValueInUpgradesDef(const char* key, const char* def) const;

	void MakeLockByConfigParam(xr_string key, bool lock_shooting = false, TAnimationEffector fun = nullptr);
	virtual u32	PlayHUDMotion(xr_string M, BOOL bMixIn, u32 state, bool lock_shooting = false, bool need_suffix = true, TAnimationEffector fun = nullptr);
	xr_string AddSuffixName(xr_string M, xr_string suffix, xr_string test_suffix = "");
	virtual xr_string GetActualCurrentAnim() const;
	xr_string GetFiremodeSuffix() const { return ""; }

	u8 GetAmmoTypeIndex(bool second = false) const;
	u8 GetAmmoTypeToReload() const;
	u8 GetOrdinalAmmoType();
	u8 GetGlAmmotype() const;
	u8 GetCartridgeType(CCartridge* c) const;
	CCartridge* GetCartridgeFromMagVector(u32 index);
	CCartridge* GetGrenadeCartridgeFromGLVector(u32 index) const;
	u32 GetAmmoInGLCount() const;
	u32 GetAmmoInMagCount() const;
	int GetMagCapacity() const;

	void ProcessAmmo(bool forced = false);
	void ProcessAmmoAdv(bool forced = false);
	void ProcessAmmoGL(bool forced = false);

	int FindIntValueInUpgradesDef(const char* key, int def) const;

	float ModifyFloatUpgradedValue(const char* key, float def) const;
	float GetLensFOV(float default_value) const;
	float GetNightPPEFactor() const;
	 
protected:
	//состояние подключенных аддонов
	u8 m_flagsAddOnState;

	//возможность подключения различных аддонов
	ALife::EWeaponAddonStatus	m_eScopeStatus;
	ALife::EWeaponAddonStatus	m_eSilencerStatus;
	ALife::EWeaponAddonStatus	m_eGrenadeLauncherStatus;

	//названия секций подключаемых аддонов
	shared_str		m_sScopeName;
	shared_str		m_sSilencerName;
	shared_str		m_sGrenadeLauncherName;

	//смещение иконов апгрейдов в инвентаре
	int	m_iScopeX, m_iScopeY;
	int	m_iSilencerX, m_iSilencerY;
	int	m_iGrenadeLauncherX, m_iGrenadeLauncherY;

protected:

	struct SZoomParams
	{
		bool			m_bZoomEnabled;			//разрешение режима приближения
		bool			m_bHideCrosshairInZoom;
//		bool			m_bZoomDofEnabled;

		bool			m_bIsZoomModeNow;		//когда режим приближения включен
		float			m_fCurrentZoomFactor;	//текущий фактор приближения
		float			m_fZoomRotateTime;		//время приближения
	
		float			m_fIronSightZoomFactor;	//коэффициент увеличения прицеливания
		float			m_fScopeZoomFactor;		//коэффициент увеличения прицела

		float			m_fZoomRotationFactor;
		
//		Fvector			m_ZoomDof;
		Fvector4		m_ReloadDof;
		BOOL			m_bUseDynamicZoom;
		shared_str		m_sUseZoomPostprocess;
		shared_str		m_sUseBinocularVision;
		CBinocularsVision*		m_pVision;
		CNightVisionEffector*	m_pNight_vision;

	} m_zoom_params;
	
		float			m_fRTZoomFactor; //run-time zoom factor
		CUIWindow*		m_UIScope;

	InertionData	m_base_inertion;
	InertionData	m_zoom_inertion;
public:

	IC bool					IsZoomEnabled		()	const		{return m_zoom_params.m_bZoomEnabled;}
	virtual	void			ZoomInc				();
	virtual	void			ZoomDec				();
	virtual void			OnZoomIn			();
	virtual void			OnZoomOut			();
	IC		bool			IsZoomed			()	const		{return m_zoom_params.m_bIsZoomModeNow;};
	float					GetAimFactor		() const {return m_zoom_params.m_fZoomRotationFactor;}
	CUIWindow*				ZoomTexture			() const;	

	bool ZoomHideCrosshair() const {
		CActor* pA = smart_cast<CActor*>(H_Parent());
		if (pA && pA->active_cam() == eacLookAt && !ZoomTexture())
			return false;
		return m_zoom_params.m_bHideCrosshairInZoom || ZoomTexture();
	}

	IC float				GetZoomFactor		() const		{return m_zoom_params.m_fCurrentZoomFactor;}
	IC void					SetZoomFactor		(float f) 		{m_zoom_params.m_fCurrentZoomFactor = f;}

	virtual	float			CurrentZoomFactor	() const;
	//показывает, что оружие находится в соостоянии поворота для приближенного прицеливания
			bool			IsRotatingToZoom	() const		{	return (m_zoom_params.m_fZoomRotationFactor<1.f);}

	virtual float				Weight			() const;		
	virtual	u32					Cost			() const;
public:
    virtual EHandDependence		HandDependence		()	const		{	return eHandDependence;}
			bool				IsSingleHanded		()	const		{	return m_bIsSingleHanded; }

public:
	int m_strap_bone0_id;
	int m_strap_bone1_id;
	bool m_strapped_mode_rifle;
	IC		LPCSTR			strap_bone0			() const {return m_strap_bone0;}
	IC		LPCSTR			strap_bone1			() const {return m_strap_bone1;}
	IC		void			strapped_mode		(bool value) {m_strapped_mode = value;}
	IC		bool			strapped_mode		() const {return m_strapped_mode;}
	bool m_can_be_strapped_rifle;

protected:
	LPCSTR					m_strap_bone0;
	LPCSTR					m_strap_bone1;
	Fmatrix					m_StrapOffset;
	Fmatrix m_StrapOffset_alt;
	bool					m_strapped_mode;
	bool					m_can_be_strapped;

	Fmatrix					m_Offset;
	// 0-используется без участия рук, 1-одна рука, 2-две руки
	EHandDependence			eHandDependence;
	bool					m_bIsSingleHanded;

public:
	//загружаемые параметры
	Fvector					vLoadedFirePoint;
	Fvector					vLoadedFirePoint2;

private:
	firedeps				m_current_firedeps;

protected:
	virtual void			UpdateFireDependencies_internal	();
	virtual void UpdatePosition(const Fmatrix& transform);
	virtual void UpdatePosition_alt(const Fmatrix& transform);
	virtual void			UpdateXForm				();

	virtual u8 GetCurrentHudOffsetIdx() const;

	virtual void			UpdateHudAdditonal		(Fmatrix&);
	IC		void			UpdateFireDependencies	()			{ if (dwFP_Frame==Device.dwFrame) return; UpdateFireDependencies_internal(); };

	virtual void			LoadFireParams		(LPCSTR section);
public:	
	IC		const Fvector&	get_LastFP				()			{ UpdateFireDependencies(); return m_current_firedeps.vLastFP;	}
	IC		const Fvector&	get_LastFP2				()			{ UpdateFireDependencies(); return m_current_firedeps.vLastFP2;	}
	IC		const Fvector&	get_LastFD				()			{ UpdateFireDependencies(); return m_current_firedeps.vLastFD;	}
	IC		const Fvector&	get_LastSP				()			{ UpdateFireDependencies(); return m_current_firedeps.vLastSP;	}

	virtual const Fvector&	get_CurrentFirePoint	()			{ return get_LastFP();				}
	virtual const Fvector&	get_CurrentFirePoint2	()			{ return get_LastFP2();				}
	virtual const Fmatrix&	get_ParticlesXFORM		()			{ UpdateFireDependencies(); return m_current_firedeps.m_FireParticlesXForm;	}
	virtual void			ForceUpdateFireParticles();
	virtual void			debug_draw_firedeps		();

protected:
	virtual void			SetDefaults				();
	
	virtual bool			MovingAnimAllowedNow	();
	virtual void			OnStateSwitch			(u32 S);
	virtual void			OnAnimationEnd			(u32 state);

	//трассирование полета пули
	virtual	void			FireTrace			(const Fvector& P, const Fvector& D);
	virtual float			GetWeaponDeterioration	();

	virtual void			FireStart			() {CShootingObject::FireStart();}
	virtual void			FireEnd				();

			void			StopShooting		();
    

	// обработка визуализации выстрела
	virtual void			OnShot				(){};
	virtual void			AddShotEffector		();
	virtual void			RemoveShotEffector	();
	virtual	void			ClearShotEffector	();
	virtual	void			StopShotEffector	();

public:
	float					GetBaseDispersion	(float cartridge_k);
	float					GetFireDispersion	(bool with_cartridge, bool for_crosshair = false);
	float getFireDispersionConditionFactor(void) const;
	void setFireDispersionConditionFactor(float value);




	virtual float			GetFireDispersion	(float cartridge_k, bool for_crosshair = false);
	virtual	int				ShotsFired			() const { return 0; }
	virtual	int				GetCurrentFireMode	() const { return 1; }

	//параметы оружия в зависимоти от его состояния исправности
	float					GetConditionDispersionFactor	() const;
	float					GetConditionMisfireProbability	() const;
	virtual	float			GetConditionToShow				() const;

public:
	CameraRecoil			cam_recoil;			// simple mode (walk, run)
	CameraRecoil			zoom_cam_recoil;	// using zoom =(ironsight or scope)

protected:
	//фактор увеличения дисперсии при максимальной изношености 
	//(на сколько процентов увеличится дисперсия)
	float					fireDispersionConditionFactor;
	//вероятность осечки при максимальной изношености

// modified by Peacemaker [17.10.08]
//	float					misfireProbability;
//	float					misfireConditionK;
	float misfireStartCondition;			//изношенность, при которой появляется шанс осечки
	float misfireEndCondition;				//изношеность при которой шанс осечки становится константным
	float misfireStartProbability;			//шанс осечки при изношености больше чем misfireStartCondition
	float misfireEndProbability;			//шанс осечки при изношености больше чем misfireEndCondition
	float conditionDecreasePerQueueShot;	//увеличение изношености при выстреле очередью
	float conditionDecreasePerShot;			//увеличение изношености при одиночном выстреле

public:
	float GetMisfireStartCondition	() const {return misfireStartCondition;};
	float GetMisfireEndCondition	() const {return misfireEndCondition;};

protected:
	struct SPDM
	{
		float					m_fPDM_disp_base			;
		float					m_fPDM_disp_vel_factor		;
		float					m_fPDM_disp_accel_factor	;
		float					m_fPDM_disp_crouch			;
		float					m_fPDM_disp_crouch_no_acc	;
	};
	SPDM					m_pdm;
	
	float					m_crosshair_inertion;
	first_bullet_controller	m_first_bullet_controller;
protected:
	//для отдачи оружия
	Fvector					m_vRecoilDeltaAngle;

	//для сталкеров, чтоб они знали эффективные границы использования 
	//оружия
	float					m_fMinRadius;
	float					m_fMaxRadius;

protected:	
	//для второго ствола
			void			StartFlameParticles2();
			void			StopFlameParticles2	();
			void			UpdateFlameParticles2();
protected:
	shared_str				m_sFlameParticles2;
	//объект партиклов для стрельбы из 2-го ствола
	CParticlesObject*		m_pFlameParticles2;

public:
	int						GetAmmoCount_forType(shared_str const& ammo_type) const;

protected:
	int						GetAmmoCount		(u8 ammo_type) const;

public:
	IC int					GetAmmoElapsed		()	const		{	return /*int(m_magazine.size())*/iAmmoElapsed;}
	IC int					GetAmmoMagSize		()	const		{	return iMagazineSize;						}
	void SetAmmoMagSize(int size);
	int						GetSuitableAmmoTotal(bool use_item_to_spawn = false) const;

	void					SetAmmoElapsed		(int ammo_count);

	virtual void			OnMagazineEmpty		();
			void			SpawnAmmo			(u32 boxCurr = 0xffffffff, 
													LPCSTR ammoSect = NULL, 
													u32 ParentID = 0xffffffff);
	bool					SwitchAmmoType		(u32 flags);

	virtual	float			Get_PDM_Base		()	const	{ return m_pdm.m_fPDM_disp_base			; };
	void Set_PDM_Base(float value);
	virtual	float			Get_PDM_Vel_F		()	const	{ return m_pdm.m_fPDM_disp_vel_factor		; };
	void Set_PDM_Vel_F(float value);
	virtual	float			Get_PDM_Accel_F		()	const	{ return m_pdm.m_fPDM_disp_accel_factor	; };
	void Set_PDM_Accel_F(float value);
	virtual	float			Get_PDM_Crouch		()	const	{ return m_pdm.m_fPDM_disp_crouch			; };
	void Set_PDM_Crouch(float value);
	virtual	float			Get_PDM_Crouch_NA	()	const	{ return m_pdm.m_fPDM_disp_crouch_no_acc	; };
	void Set_PDM_Crouch_NA(float value);
	virtual	float			GetCrosshairInertion()	const	{ return m_crosshair_inertion; };
	void setCrosshairInertion(float value);
			float			GetFirstBulletDisp	()	const	{ return m_first_bullet_controller.get_fire_dispertion(); };
protected:
	int iAmmoElapsed;
	int iMagazineSize;

	//для подсчета в GetSuitableAmmoTotal
	mutable int				m_iAmmoCurrentTotal;
	mutable u32				m_BriefInfo_CalcFrame;	//кадр на котором просчитали кол-во патронов
	bool					m_bAmmoWasSpawned;

	virtual bool			IsNecessaryItem	    (const shared_str& item_sect);

public:
	const xr_vector<shared_str>& getAmmoTypes(void) const { return m_ammoTypes; }
	xr_vector<shared_str>	m_ammoTypes;

	using SCOPES_VECTOR = xr_vector<shared_str>;
	using SCOPES_VECTOR_IT = SCOPES_VECTOR::iterator;

	SCOPES_VECTOR			m_scopes;
	u8						m_cur_scope;

	CWeaponAmmo*			m_pCurrentAmmo;
	u8						m_ammoType;
	bool					m_bHasTracers;
	u8						m_u8TracerColorID;
	u8						m_set_next_ammoType_on_reload;
	// Multitype ammo support
	xr_vector<CCartridge>	m_magazine;
	CCartridge				m_DefaultCartridge;
	float					m_fCurrentCartirdgeDisp;

		bool				unlimited_ammo				();
	IC	bool				can_be_strapped				() const {return m_can_be_strapped;};

	float GetMagazineWeight(const decltype(m_magazine)& mag) const;

protected:
	u32						m_ef_main_weapon_type;
	u32						m_ef_weapon_type;

public:
	virtual u32				ef_main_weapon_type	() const;
	virtual u32				ef_weapon_type		() const;

protected:
	// This is because when scope is attached we can't ask scope for these params
	// therefore we should hold them by ourself :-((
	float					m_addon_holder_range_modifier;
	float					m_addon_holder_fov_modifier;

public:
	virtual	void			modify_holder_params		(float &range, float &fov) const;
	virtual bool			use_crosshair				()	const {return true;}
			bool			show_crosshair				();
			bool			show_indicators				();
	  const bool			ParentIsActor				() const override;
	
private:
	virtual	bool			install_upgrade_ammo_class	( LPCSTR section, bool test );
			bool			install_upgrade_disp		( LPCSTR section, bool test );
			bool			install_upgrade_hit			( LPCSTR section, bool test );
			bool			install_upgrade_addon		( LPCSTR section, bool test );
			bool			install_upgrade_hud_sect	(LPCSTR section, bool test);
			bool			install_upgrade_hud_sect_silencer (LPCSTR section, bool test);
			bool			install_upgrade_hud_sect_scope(LPCSTR section, bool test);
			bool			install_upgrade_hud_sect_gl(LPCSTR section, bool test);
			bool			install_upgrade_flame_particles(LPCSTR section, bool test);
			bool			install_upgrade_smoke_particles(LPCSTR section, bool test);
			bool			install_upgrade_quick_kick(LPCSTR section, bool test);
			bool			install_upgrade_bones(LPCSTR section, bool test);
protected:
	virtual bool			install_upgrade_impl		( LPCSTR section, bool test );

private:
	float					m_hit_probability[egdCount];

public:
	const float				&hit_probability			() const;

private:
	Fvector					m_overriden_activation_speed;
	bool					m_activation_speed_is_overriden;
	virtual bool			ActivationSpeedOverriden	(Fvector& dest, bool clear_override);

	bool					m_bRememberActorNVisnStatus;
public:
	virtual void			SetActivationSpeedOverride	(Fvector const& speed);
			bool			GetRememberActorNVisnStatus	() {return m_bRememberActorNVisnStatus;};
	virtual void			EnableActorNVisnAfterZoom	();
	
	virtual void				DumpActiveParams			(shared_str const & section_name, CInifile & dst_ini) const;
	virtual shared_str const	GetAnticheatSectionName		() const { return cNameSect(); };
};
