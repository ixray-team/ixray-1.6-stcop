#pragma once

#include "../xrPhysics/PhysicsShell.h"
#include "WeaponAmmo.h"
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

#include "../xrScripts/script_export_space.h"

class CEntity;
class ENGINE_API CMotionDef;
class CSE_ALifeItemWeapon;
class CSE_ALifeItemWeaponAmmo;
class CWeaponMagazined;
class CWeaponMagazinedWGrenade;
class CWeaponBinoculars;
class CWeaponKnife;
class CWeaponBM16;
class CWeaponRPG7;
class CWeaponRG6;
class CWeaponPistol;
class CWeaponCustomPistol;
class CParticlesObject;
class CUIStatic;
struct TAmmoBones;

class CWeapon : public CHudItemObject,
				public CShootingObject
{
	using inherited = CHudItemObject;

protected:
	friend struct TAmmoBones;

public:
							CWeapon				();
	virtual					~CWeapon			();

	// Generic
	virtual void			Load				(const char* section);

	virtual bool			net_Spawn			(CSE_Abstract* DC);
	virtual void			net_Destroy			();
	virtual void			net_Export			(NET_Packet& P);
	virtual void			net_Import			(NET_Packet& P);
	virtual void			net_Relcase			(CObject* object);

	virtual bool			AlwaysTheCrow       ();
	virtual CWeapon			*cast_weapon			()					{return this;}
	virtual CWeaponBinoculars* cast_weapon_binoculars() { return nullptr; }
	virtual CWeaponKnife* cast_weapon_knife() { return nullptr; }
	virtual CWeaponMagazined* cast_weapon_magazined() { return nullptr; }
	virtual CWeaponMagazinedWGrenade* cast_weapon_magazined_w_grenade() { return nullptr; }
	virtual CWeaponBM16* cast_weapon_bm16() { return nullptr; }
	virtual CWeaponRPG7* cast_weapon_rpg7() { return nullptr; }
	virtual CWeaponRG6* cast_weapon_rg6() { return nullptr; }
	virtual CWeaponShotgun* cast_weapon_shotgun() override { return nullptr; }


	//serialization
	virtual void			save				(NET_Packet &output_packet);
	virtual void			load				(IReader &input_packet);
	virtual bool			net_SaveRelevant	()								{return inherited::net_SaveRelevant();}

	virtual void			UpdateCL			();
	virtual void			shedule_Update		(u32 dt);
	virtual bool			register_schedule() const {return false;};
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

	virtual void			OnMoveToRuck		(const SInvItemPlace& prev) override;

	virtual	void			Hit					(SHit* pHDS);

	virtual void			reinit				();
	virtual void			reload				(const char* section);
	virtual void			create_physic_shell	();
	virtual void			activate_physic_shell();
	virtual void			setup_physic_shell	();

	virtual void			SwitchState			(u8 S);

	virtual void			OnActiveItem		();
	virtual void			OnHiddenItem		();
	virtual bool			SendDeactivateItem	(bool Force);
	virtual void			SendHiddenItem		();	//same as OnHiddenItem but for client... (sends message to a server)...

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
	bool IsUIForceHiding();
	bool IsCollimatorInstalled() const;
	bool IsHudModelForceUnhide() const;
	bool IsUIForceUnhiding() const;
	bool ScopeFit(CScope*) const;

	virtual void on_a_hud_attach() override;
	
	SBlendParams m_sAimBlendParams[3], m_sGLAimBlendParams[3], m_sSafemodeBlendParams[3], m_sFakeShootBlendParams;

	virtual void OnBlendEnd(u8 state) override;
	virtual void OnBlendStart(u8 state) override;

protected:
	//время удаления оружия
	ALife::_TIME_ID			m_dwWeaponRemoveTime;
	ALife::_TIME_ID			m_dwWeaponIndependencyTime;
	float m_fHudFovZoomFactor;
	float m_fHudFovGLZoomFactor;
	float m_HudFovZoom;
	virtual bool			IsHudModeNow		();
	bool m_Allow3DScope = false;
public:
	void					signal_HideComplete	();
	virtual bool			Action(u16 cmd, u32 flags);

	enum EWeaponStates : u8
	{
		eFire = eLastBaseState + 1,
		eFire2,
		eReload,
		eMisfire,
		eSwitch,
		eSwitchMode,
		eEmptyClick,
		eDevice,
		eLightMis,
		eKick,
		eMagCheck,
		eFiremodeCheck,
		eLoadChamber,
		eUnloadChamber,
		eChamberCheck,
		ePump,
		eSafemodeSwitch,
	};

	enum EWeaponSubStates : u8
	{
		eSubstateReloadBegin = 0,
		eSubstateReloadInProcess,
		eSubstateReloadEnd,
	};

	enum { undefined_ammo_type = u8(-1) };

	IC bool					IsValid				()	const		{	return iAmmoElapsed;						}
	// Does weapon need's update?
	bool					IsUpdating			();


	bool					IsMisfire			() const;
	bool					CheckForMisfire		();


	bool					AutoSpawnAmmo		() const		{ return m_bAutoSpawnAmmo; };
	bool					IsTriStateReload	() const		{ return m_bTriStateReload;}
	EWeaponSubStates		GetReloadState		() const		{ return (EWeaponSubStates)m_sub_state;}
	u8						m_sub_state = eSubstateReloadBegin;
protected:
	bool					m_bTriStateReload = false;
	// a misfire happens, you'll need to rearm weapon
	bool					bMisfire = false;

	bool					m_bAutoSpawnAmmo;
public:
			bool IsGrenadeLauncherAttached	() const;
			bool IsScopeAttached			() const;
			bool IsSilencerAttached			() const;

	virtual bool GrenadeLauncherAttachable();
	virtual bool ScopeAttachable();
	virtual bool SilencerAttachable();
			
	ALife::EWeaponAddonStatus	get_GrenadeLauncherStatus	() const { return m_eGrenadeLauncherStatus; }
	ALife::EWeaponAddonStatus	get_ScopeStatus				() const { return m_eScopeStatus; }
	ALife::EWeaponAddonStatus	get_SilencerStatus			() const { return m_eSilencerStatus; }

	virtual bool NeedMovementBlend() const override;
	bool AllowSafemode() const;

	virtual bool UseScopeTexture();

	public:
		struct SRecoilPoint {
			float x; 
			float y; 
		};

		struct SRecoilPattern
		{
			shared_str name;
			xr_vector<SRecoilPoint> bullet_patterns;
			u32 current_bullet;

			SRecoilPattern() : current_bullet(0) {}
		};

		// Данные паттернов
		SRecoilPattern m_hipfire_pattern;
		SRecoilPattern* m_current_pattern = nullptr;

		void LoadRecoilPatterns(const char* section);
		void ApplyPattern();
		void StopPattern();
		// для доступа к паттерну отдачи
		bool GetCurrentRecoilPattern(float& out_x, float& out_y);

		float GetAddonRecoil() const;
		float m_fGrenadeAttachedRecoil = 1.0f;

		struct scope_recoil_params
		{
			float           m_fScopeAttachedRecoil = 1.0f;
			float           m_fScopeAttachedRecoilReduction = 1.0f;
		} m_scope_recoil;

protected:
	// Вспомогательные методы
	void LoadBulletPattern(const char* section, const char* line, SRecoilPattern& pattern);
	void StartRecoilPattern();
	SRecoilPattern* GetPatternByName(const shared_str& name);

	public:

	//обновление видимости для косточек аддонов
	void UpdateAddonsVisibility();
	void UpdateHUDAddonsVisibility();
	void ProcessScope();
	void UpdateScopePosition();
	virtual void UpdateBonePartAnimations() {}
	//инициализация свойств присоединенных аддонов
	virtual void InitAddons();

	//для отоброажения иконок апгрейдов в интерфейсе

	int	GetScopeX();
	int	GetScopeY();

	int	GetSilencerX() {return m_iSilencerX;}
	int	GetSilencerY() {return m_iSilencerY;}
	void SetSilencerX(int value);
	void SetSilencerY(int value);
	int	GetGrenadeLauncherX() {return m_iGrenadeLauncherX;}
	int	GetGrenadeLauncherY() {return m_iGrenadeLauncherY;}

	const shared_str& GetGrenadeLauncherName	() const{return m_sGrenadeLauncherName;}
	const shared_str GetScopeName() const;
	void UpdateAltScope();
	shared_str GetNameWithAttachmentScope();
	bool bReloadSectionScope(const char* section);
	bool bLoadAltScopesParams(const char* section);
	void LoadOriginalScopesParams(const char* section);
	void LoadCurrentScopeParams(const char* section);
	const shared_str& GetSilencerName			() const{return m_sSilencerName;}

	IC void	ForceUpdateAmmo						()		{ m_BriefInfo_CalcFrame = 0; }

	u8		GetAddonsState						()		const		{return m_flagsAddOnState;};
	void	SetAddonsState						(u8 st)	{m_flagsAddOnState=st;}//dont use!!! for buy menu only!!!

	bool	NeedBlockSprint						() const;

	virtual void OnMotionMark(u8 state, const motion_marks&);

	bool IsJamProhibited();
	bool OnWeaponJam();
	bool CheckForMisfire_validate_NoMisfire();

	bool IsActionProcessing() const;
	bool CanAimNow();
	bool CanLeaveAimNow();

	struct conditional_breaking_params
	{
		float start_condition = 0.0f;     // при каком состоянии начнутся проблемы
		float end_condition = 0.0f;       // при каком состоянии отрубится вообще
		float start_probability = 0.0f;   // вероятность проблем в стартовом состоянии
	};

	conditional_breaking_params CollimatorBreakingParams;

	struct light_misfire_params
	{
		float startcond = 1.0f;
		float endcond = 0.0f;
		float startprob = 1.0f;
		float endprob = 0.0f;
	} light_misfire;

	struct lens_zoom_params
	{
		float delta = 0.0f;
		float target_position = 0.0f;
		float speed = 0.0f;
		float factor_min = 0.0f;
		float factor_max = 0.0f;
		float gyro_period = 0.0f;
		float real_position = 0.0f;
		float lens_factor_levels_count = 5.0f;
		u32   last_gyro_snd_time = 0;
		bool  need_lens_frame = false;
		bool  force_zoom_sound = false;
	} m_lens_zoom_params;

	struct stepped_params
	{
		float max_value = 1.0f;
		float min_value = 0.0f;
		float cur_value = 0.5f;
		int cur_step = 1;
		int steps = 2;
		float jitter = 0.1f;
		float min_factor = 0.0f;
		s32 lens_night_brightness_saved_step = -1;
	} m_lens_night_brightness;


	struct fast_kick_params
	{
		shared_str material = "objects\\knife";
		int cnt = 1;
		float hp = 0.0f;
		float imp = 0.0f;
		ALife::EHitType htype = ALife::EHitType::eHitTypeWound;
		float hdist = 0.0f;
		float disp_hor = 0.0f;
		float disp_ver = 0.0f;
		float ap = EPS_L;
		float wallmark_size = 0.05f;
		bool bBlockQK = false;
		bool bBlockQKSil = false;
		bool bBlockQKScp = false;
		bool bBlockQKGL = false;
		bool bBlockQKGLM = false;
	} m_fast_kick_params;

	bool IsLensedScopeInstalled() const { return m_lens_zoom_params.need_lens_frame; }
	float GetLensFOV() const;
	void LoadNightBrightnessParamsFromSection(shared_str sect);
	void ChangeNightBrightness(int steps);
	void SetNightBrightness(int steps, bool use_sound);
	void UpdateZoomCrosshairUI();
	void SetLensParams(lens_zoom_params& params);
	void UpdateLensFactor(u32 timedelta);
	void MakeWeaponKick(Fvector& pos, Fvector& dir);
	float GetNightPPEFactor();


	float m_fCollimatorLevelsProblem = 0.0f;
	float m_fMisfireAfterProblemsLevel = 10.0f;
	float m_fRechargeTime = 0.0f;
	float m_fLastRechargeTime = 0.0f;
	float m_fSafeModeRotationFactor = 0.0f;
	float m_fSafeModeRotateTime = 0.25f;


	bool bUpdateHUDBonesVisibility = false;
	u32 _last_update_time;
	u32 m_iLastShotTime = 0;

	xr_vector<std::pair<u8, u32>> m_mags_capacity{};

	bool bStopReloadSignal = false;
	bool m_bUseSilHud = false;
	bool m_bUseScopeHud = false;
	bool m_bUseGLHud = false;
	bool m_bIsAimStarted = false;
	bool m_bRestGlSil = false;
	bool m_bTacticalTorchStatus = false;
	bool m_bTacticalLaserStatus = false;
	bool m_bJustAfterReload = false;
	bool m_bIsPreloaded = false;
	bool m_bAddCartridgeInOpen = false;
	bool m_bBlockReload = false;
	bool m_bJamNotShot = true;
	bool m_bUseLightMis = false;
	bool m_bDisableLightMisDet = false;
	bool m_bNoJamFirstShot = false;
	bool m_bActorCanShoot = true;
	bool m_bIsAimAnimationPlaying = false;
	bool m_bBlockFiremodeinGLM = false;
	bool m_bNeedPumpReloadEnd = false;
	bool m_bGaussScheme = false;
	bool m_bGaussScreen = false;
	bool m_bUseRevolverScheme = false;
	bool m_bUseMosinScheme = false;
	bool m_AlterZoomAllowed = false;
	bool m_bAllowSafemode = false;
	bool m_bAimActions = false;
	bool NeedMisfireAmmo = false;

	bool m_bHaveShell = false;
	bool m_bNeedPumpState = false;
	bool m_bIsPumpEnabled = false;

	s32	m_iAutoAimTime = 0;
	bool m_bAutoAimOnlyAlive = false;
	bool m_bAutoAimIgnoreDead = false;
	bool m_bAutoAimShotAfterKeyReleased = false;
	bool m_bAutoAimNeedReleaseShot = false;
	bool m_bAutoAimNeedAutoShot = false;
	bool m_bAutoAimShooted = false;
	s32 m_iAutoAimValidTime = 0;
	bool m_bAutoAimAutoShot = false;

	bool	IsAutoAimHaveTarget();
	s32		GetAutoAimPeriod() const;
	void	SetAutoAimStartTime(int cnt) { m_iAutoAimValidTime = cnt; }
	IC s32	GetAutoAimStartTime() const { return m_iAutoAimValidTime; }

	shared_str hud_silencer;
	shared_str hud_scope;
	shared_str hud_gl;

	shared_str m_safemode_cams[2] = {};
	shared_str m_aim_cams[2] = {};

	RStringVec m_shot_cams[2] = {};

protected:
	bool m_bBlockUpdateAmmoBonesShooting = false;
	bool m_bUseLastAmmoType = false;
	bool m_bUseChamberInUpdateBones = false;

protected:
	//состояние подключенных аддонов
	u8 m_flagsAddOnState = 0;

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

		bool			m_bIsZoomModeNow = false;		//когда режим приближения включен
		bool			m_bIsAltZoomModeNow = false;		//когда режим приближения включен
		float			m_fCurrentZoomFactor = g_fov;	//текущий фактор приближения
		float			m_fZoomRotateTime;		//время приближения
	
		float			m_fIronSightZoomFactor;	//коэффициент увеличения прицеливания
		float			m_fScopeZoomFactor;		//коэффициент увеличения прицела

		float			m_fZoomRotationFactor = 0.0f;
		float			m_fZoomRotationFactor2 = 0.0f;
		
//		Fvector			m_ZoomDof;
		Fvector4		m_ReloadDof;
		bool			m_bUseDynamicZoom;
		shared_str		m_sUseZoomPostprocess;
		shared_str		m_sUseBinocularVision;

	} m_zoom_params;
	
		float			m_fRTZoomFactor; //run-time zoom factor
		CUIStatic*		m_UIScope = nullptr;

	InertionData	m_base_inertion;
	InertionData	m_zoom_inertion;
	bool m_bIAmWeaponRPG7 = false;
	shared_str GetCurrentScopeSection() const { return m_scopes[m_cur_scope]; }
	shared_str GetScopeSection(int idx) const { return m_scopes[idx]; }



protected:

	u8 m_LastShotAmmoType = 0;

	RStringVec m_bDefHideBones {}, m_bDefShowBones {}, m_bHideBonesOverride {}, m_bDefHideBonesGLAttached {},
		m_bHideBonesGLAttached {}, m_bHideBonesSilAttached {}, m_bHideBonesScopeAttached {},
		m_bHideBonesUpgrade {}, m_bScopeShowBones{}, m_bScopeHideBones{}, m_bShowBonesUpgToHide{}, m_bShowBonesUpgToShow{},
		m_sCollimatorSightsBones{};

	bool m_bDisableFireModeAim = false;
	bool m_bIsReloaded = false;

	void HideOneUpgradeLevel(const char* section);
	void LoadUpgradeBonesToHide(const char* section, const char* line);
	u32 FakeReload();

public:
	virtual bool IsGrenadeMode() const { return false; }
	virtual void ForceUpdateHUD();

	bool SwitchZoom(u32 flags);

	IC bool					IsZoomEnabled		()	const		{return m_zoom_params.m_bZoomEnabled;}
	virtual	void			ZoomInc				();
	virtual	void			ZoomDec				();
	virtual void			OnZoomIn			();
	virtual void			OnZoomOut			();
	void					OnSafemodeOut		();
	IC		bool			IsZoomed			()	const		{return m_zoom_params.m_bIsZoomModeNow;}
	IC		bool			IsAltZoomed			()	const		{return m_zoom_params.m_bIsAltZoomModeNow;}
	CUIStatic*				ZoomTexture			();	

	IC bool ZoomHideCrosshair() {
		CActor* pA = H_Parent() ? H_Parent()->cast_actor() : NULL;
		if (pA && pA->active_cam() == eacLookAt && !ZoomTexture())
			return false;
		return m_zoom_params.m_bHideCrosshairInZoom || ZoomTexture();
	}

	IC float				GetZoomFactor		() const		{return m_zoom_params.m_fCurrentZoomFactor;}

	IC void					SetZoomFactor		(float f) 		{m_zoom_params.m_fCurrentZoomFactor = f;}

	virtual	float			CurrentZoomFactor	();
	//показывает, что оружие находится в соостоянии поворота для приближенного прицеливания
			bool			IsRotatingToZoom	() const		{	return (m_zoom_params.m_fZoomRotationFactor<1.f);}

	virtual EHudOffsetType GetCurrentHudOffsetIdx() const final override;

	virtual float				Weight			() const;		
	virtual	u32					Cost			() const;
public:
    virtual EHandDependence		HandDependence		()	const		{	return eHandDependence;}
			bool				IsSingleHanded		()	const		{	return m_bIsSingleHanded; }
			void				SetMisfireStatus	(bool b)		{ bMisfire = b; }
			THudLightLaser*		GetLightLaser		();

protected:
	u16 m_strap_bone0_id = BI_NONE;
	u16 m_strap_bone1_id = BI_NONE;

	bool m_strapped_mode_rifle = false;
	bool m_strapped_mode = false;

	bool m_can_be_strapped = false;
	bool m_can_be_strapped_rifle = false;

	const char* m_strap_bone0 = nullptr;
	const char* m_strap_bone1 = nullptr;

	// 0-используется без участия рук, 1-одна рука, 2-две руки
	EHandDependence eHandDependence = EHandDependence::hdNone;
	bool m_bIsSingleHanded = false;

public:
	struct SStrapParams
	{
		Fmatrix StrapMatrix = Fidentity;
		Fvector StrapPosition = zero_vel;
		Fvector StrapRotation = zero_vel;

		const Fmatrix& GetXFORM()
		{
			StrapMatrix.setHPB(VPUSH(Fvector(StrapRotation).mul(PI / 180.0f)));
			StrapMatrix.translate_over(StrapPosition);

			return StrapMatrix;
		}

	} m_StrapOffset, m_StrapOffsetAlt, m_ActiveOffset;

	IC const char* strap_bone0() const { return m_strap_bone0; }
	IC const char* strap_bone1() const { return m_strap_bone1; }
	IC void strapped_mode(bool value) { m_strapped_mode = value; }
	IC bool strapped_mode() const { return m_strapped_mode; }
	IC bool strapped_mode_rifle() const { return m_strapped_mode_rifle; }

	//загружаемые параметры
	Fvector					vLoadedFirePoint;
	Fvector					vLoadedFirePoint2;

private:
	firedeps				m_current_firedeps;

protected:
			void UpdateFireDependencies_internal	();
	virtual void UpdatePosition(const Fmatrix& transform);
	virtual void UpdatePosition_alt(const Fmatrix& transform);
	virtual void			UpdateXForm				();

	virtual void			UpdateHudAdditonal		(Fmatrix&);
	IC		void			UpdateFireDependencies	()			{ if (dwFP_Frame==Device.dwFrame) return; UpdateFireDependencies_internal(); };

	virtual void			LoadFireParams		(const char* section);
public:	
	IC		const Fvector&	get_LastFP				()			{ UpdateFireDependencies(); return m_current_firedeps.vLastFP;	}
	IC		const Fvector&	get_LastFP2				()			{ UpdateFireDependencies(); return m_current_firedeps.vLastFP2;	}
	IC		const Fvector&	get_LastFD				()			{ UpdateFireDependencies(); return m_current_firedeps.vLastFD;	}
	IC		const Fvector&	get_LastSP				()			{ UpdateFireDependencies(); return m_current_firedeps.vLastSP;	}

	virtual const Fvector&	get_CurrentFirePoint	()			{ return get_LastFP(); }
	virtual const Fvector&	get_CurrentFirePoint2	()			{ return get_LastFP2(); }
	virtual const Fvector&	get_CurrentShellPoint	()			{ return get_LastSP(); };
	virtual const Fmatrix&	get_ParticlesXFORM		()			{ UpdateFireDependencies(); return m_current_firedeps.m_FireParticlesXForm;	}
	virtual void			debug_draw_firedeps		();

protected:
	virtual void			SetDefaults				();
	
	virtual bool			MovingAnimAllowedNow	();
	virtual void			OnStateSwitch			(u8 S);

	//трассирование полета пули
	virtual	void			FireTrace			(const Fvector& P, const Fvector& D);
	virtual	void			FireTraceChamber			(const Fvector& P, const Fvector& D);
	virtual float			GetWeaponDeterioration	();

	virtual void			FireStart			() {CShootingObject::FireStart();}
	virtual void			FireEnd				();

	virtual void			Reload				();
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
	virtual	int				ShotsFired			() { return 0; }
	virtual	int				GetCurrentFireMode	() { return 1; }

	//параметы оружия в зависимоти от его состояния исправности
	float					GetConditionDispersionFactor	() const;
	float					GetConditionMisfireProbability	() const;
	virtual	float			GetConditionToShow				() const;

	IC virtual void SetNextState(u8 v) final override;

public:
	CameraRecoil			cam_recoil;			// simple mode (walk, run)
	CameraRecoil			zoom_cam_recoil;	// using zoom =(ironsight or scope)

protected:
	bool					useLegacyMisfire = false;
	//фактор увеличения дисперсии при максимальной изношености 
	//(на сколько процентов увеличится дисперсия)
	float					fireDispersionConditionFactor;
	//вероятность осечки при максимальной изношености

	// CS System
	float					misfireProbability;
	float					misfireConditionK;
	// CoP system
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
	
	float					m_crosshair_inertion = 0.0f;
	first_bullet_controller	m_first_bullet_controller;
protected:
	//для отдачи оружия
	Fvector					m_vRecoilDeltaAngle;

public:
	int						GetAmmoCount_forType(shared_str const& ammo_type) const;

protected:
	int						GetAmmoCount		(u8 ammo_type) const;

public:
	IC int					GetAmmoElapsed		()	const		{ return iAmmoElapsed; }
	virtual int				GetCurrentElapsed	(bool for_grenade_mode = false)	const { return iAmmoElapsed; }
	int						GetAmmoChamberElapsed()	const		{ return iAmmoChamberElapsed; }
	IC int					GetAmmoMagSize		()	const		{ return iMagazineSize; }
	bool					IsChamber			()  const		{ return m_bAmmoInChamber; }
	bool					IsChangeAmmoType	()	const		{ return (m_set_next_ammoType_on_reload != undefined_ammo_type || m_ammoType == m_set_next_ammoType_on_reload); }

	virtual u8				GetTargetAmmoType(bool for_grenade_mode = false) const { return m_set_next_ammoType_on_reload != undefined_ammo_type ? m_set_next_ammoType_on_reload : GetAmmoType(for_grenade_mode); }
	virtual u8				GetAmmoType(bool for_grenade_mode = false) const { return m_ammoType; }
	u8						GetSetNextAmmoType() const { return m_set_next_ammoType_on_reload; }

	void SetAmmoMagSize(int size);
	int						GetSuitableAmmoTotal(bool use_item_to_spawn = false) const;

	void					SetAmmoElapsed		(int ammo_count);
	void					SetChamberAmmoElapsed(int ammo_count);

	virtual void			OnMagazineEmpty		();
			void			SpawnAmmo			(u32 boxCurr = 0xffffffff, 
													const char* ammoSect = NULL, 
													u32 ParentID = 0xffffffff);
			void			ReturnAmmoToInventory(xr_map<shared_str, u16>& ammo,
													xr_map<u16, u16>* ammos_to_sync = nullptr);
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

	virtual void			UnloadChamber(bool spawn_ammo = true);
	virtual void			LoadChamber();

	virtual void OnChangeVisual() final override;

protected:
	int						iAmmoElapsed = 0;		// ammo in magazine, currently
	int						iMagazineSize = 0;		// size (in bullets) of magazine

	int						iAmmoChamberElapsed = 0;
	int						iChamberSize = 1;

	bool					m_bAmmoInChamber;

	//для подсчета в GetSuitableAmmoTotal
	mutable int				m_iAmmoCurrentTotal = 0;
	mutable u32				m_BriefInfo_CalcFrame = 0;	//кадр на котором просчитали кол-во патронов
	bool					m_bAmmoWasSpawned;

	virtual bool			IsNecessaryItem	    (const shared_str& item_sect);

	virtual void			GiveAmmoFromMagToChamber();
	virtual void			DeleteAmmoInChamber();
	virtual int				GetMagCapacity();

public:
	virtual const xr_vector<shared_str>& getAmmoTypes(bool for_grenade_mode = false) const { return m_ammoTypes; }
	xr_vector<shared_str>	m_ammoTypes;
/*
	struct SScopes
	{
		shared_str			m_sScopeName;
		int					m_iScopeX;
		int					m_iScopeY;
	};

	using SCOPES_VECTOR = xr_vector<SScopes*>;
	using SCOPES_VECTOR_IT = SCOPES_VECTOR::iterator;
	
	SCOPES_VECTOR			m_scopes;

	u8						cur_scope;
*/
	using SCOPES_VECTOR = xr_vector<shared_str>;
	using SCOPES_VECTOR_IT = SCOPES_VECTOR::iterator;

	SCOPES_VECTOR			m_scopes = {};
	u8						m_cur_scope = 0;

	CWeaponAmmo*			m_pCurrentAmmo = nullptr;
	u8						m_ammoType = 0;
	u8						m_ChamberAmmoType = 0;
//-	shared_str				m_ammoName; <== deleted
	bool					m_bHasTracers;
	u8						m_u8TracerColorID;
	u8						m_set_next_ammoType_on_reload = undefined_ammo_type;
	// Multitype ammo support
	xr_vector<CCartridge>	m_magazine;
	xr_vector<CCartridge>	m_chamber;
	CCartridge				m_DefaultCartridge;
	CCartridge				m_DefaultCartridgeInChamber;
	float					m_fCurrentCartirdgeDisp = 1.0f;

		bool				unlimited_ammo				();
		bool				infinite_fire();
	IC	bool				can_be_strapped				() const {return m_can_be_strapped;};

	float GetMagazineWeight(const decltype(m_magazine)& mag) const;

protected:
	u32						m_ef_main_weapon_type = u32(-1);
	u32						m_ef_weapon_type = u32(-1);

protected:
	float					m_bullet_point_offset_hud;
	float					m_bullet_point_offset_world;

public:
	virtual u32				ef_main_weapon_type	() const;
	virtual u32				ef_weapon_type		() const;
	
	virtual void			set_ef_main_weapon_type(u32 type){ m_ef_main_weapon_type = type; };
	virtual void			set_ef_weapon_type(u32 type){ m_ef_weapon_type = type; };
	virtual void			SetAmmoType(u8 type) { m_ammoType = type; };
	u8						GetAmmoType() { return m_ammoType; };

protected:
	// This is because when scope is attached we can't ask scope for these params
	// therefore we should hold them by ourself :-((
	float					m_addon_holder_range_modifier;
	float					m_addon_holder_fov_modifier;

public:
	virtual	void			modify_holder_params		(float &range, float &fov) const;
	virtual bool			use_crosshair				() const;
			bool			show_crosshair				();
			bool			show_indicators				();
	virtual bool			ParentMayHaveAimBullet		();
	virtual bool			ParentIsActor				();
	
private:
	virtual	bool			install_upgrade_ammo_class	( const char* section, bool test );
			bool			install_upgrade_disp		( const char* section, bool test );
			bool			install_upgrade_hit			( const char* section, bool test );
			bool			install_upgrade_addon		( const char* section, bool test );
			
			bool			install_upgrade_hud_sect(const char* section, bool test);
			bool			install_upgrade_hud_sect_silencer(const char* section, bool test);
			bool			install_upgrade_hud_sect_scope(const char* section, bool test);
			bool			install_upgrade_hud_sect_gl(const char* section, bool test);

			bool			install_upgrade_bones		( const char* section, bool test );
			bool			install_upgrade_ammo_bones	( const char* section, bool test );

			bool			install_upgrade_torch_laser	( const char* section, bool test );
			bool			install_upgrade_scope_zoom( const char* section, bool test );

			bool			install_upgrade_fast_knife ( const char* section, bool test );
protected:
	virtual bool			install_upgrade_impl		( const char* section, bool test );

private:
	float					m_hit_probability[egdCount];

public:
	const float				&hit_probability			() const;

private:
	//bool					m_bRememberActorNVisnStatus; //оно тут в омп висело но я не знаю где используется. оставил что бы не запутаться

public:
	
	virtual void				DumpActiveParams			(shared_str const & section_name, CInifile & dst_ini) const;
	virtual shared_str const	GetAnticheatSectionName		() const { return cNameSect(); };

public:
	bool bUseAltScope{};
	bool bScopeIsHasTexture{};

	float GetAimFactor() const { return m_zoom_params.m_fZoomRotationFactor; }
	float GetAltAimFactor() const { return m_zoom_params.m_fZoomRotationFactor2; }
	bool GetScopeBack();
	void UpdateCollimatorSight();

	//UIWpnParams stuff
	float GetRPM() const;
	float GetDamage() const;
	float GetDamageMP() const;
	float GetHandling() const;
	float GetAccuracy() const;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
