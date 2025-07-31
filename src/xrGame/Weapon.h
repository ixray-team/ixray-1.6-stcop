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
class CParticlesObject;
class CUIWindow;
class CBinocularsVision;
class CWeaponNightVision;

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

	virtual BOOL			net_Spawn			(CSE_Abstract* DC);
	virtual void			net_Destroy			();
	virtual void			net_Export			(NET_Packet& P);
	virtual void			net_Import			(NET_Packet& P);
	
	virtual CWeapon			*cast_weapon			()					{return this;}
	virtual CWeaponBinoculars* cast_weapon_binoculars() { return nullptr; }
	virtual CWeaponKnife* cast_weapon_knife() { return nullptr; }
	virtual CWeaponMagazined* cast_weapon_magazined() { return nullptr; }
	virtual CWeaponMagazinedWGrenade* cast_weapon_magazined_w_grenade() { return nullptr; }
	virtual CWeaponBM16* cast_weapon_bm16() { return nullptr; }
	virtual CWeaponRPG7* cast_weapon_rpg7() { return nullptr; }
	virtual CWeaponRG6* cast_weapon_rg6() { return nullptr; }


	//serialization
	virtual void			save				(NET_Packet &output_packet);
	virtual void			load				(IReader &input_packet);
	virtual BOOL			net_SaveRelevant	()								{return inherited::net_SaveRelevant();}

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
		eDevice,
	};
	enum EWeaponSubStates{
		eSubstateReloadBegin		=0,
		eSubstateReloadInProcess,
		eSubstateReloadEnd,
	};
	enum { undefined_ammo_type = u8(-1) };

	IC BOOL					IsValid				()	const		{	return iAmmoElapsed;						}
	// Does weapon need's update?
	BOOL					IsUpdating			();


	BOOL					IsMisfire			() const;
	BOOL					CheckForMisfire		();


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
			bool IsGrenadeLauncherAttached	() const;
			bool IsScopeAttached			() const;
			bool IsSilencerAttached			() const;

	virtual bool GrenadeLauncherAttachable();
	virtual bool ScopeAttachable();
	virtual bool SilencerAttachable();
			
	ALife::EWeaponAddonStatus	get_GrenadeLauncherStatus	() const { return m_eGrenadeLauncherStatus; }
	ALife::EWeaponAddonStatus	get_ScopeStatus				() const { return m_eScopeStatus; }
	ALife::EWeaponAddonStatus	get_SilencerStatus			() const { return m_eSilencerStatus; }

	virtual bool UseScopeTexture();

	struct SAmmoBonesParams
	{
		SAmmoBonesParams(u32 type) : AmmoType(type) {}
		~SAmmoBonesParams()
		{
			for (auto& it : ConfigurationMap)
			{
				it.second.second.clear();
			}
			ConfigurationMap.clear();
			AllBones.clear();
		}
		u8 AmmoType = undefined_ammo_type;
		xr_hash_map<u32, std::pair<shared_str, RStringVec>> ConfigurationMap{};
		RStringVec AllBones{};
		void Load(const shared_str& section, u32 size);
	};

	//обновление видимости для косточек аддонов
	void UpdateAddonsVisibility();
	void UpdateHUDAddonsVisibility();
	void ProcessScope();
	void UpdateScopePosition();
	void UpdateAmmoBones(xr_vector<SAmmoBonesParams*>& lVector, u32 idx, u8 type);
	void UpdateShellBones(u32 idx, u8 type);
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
	bool bReloadSectionScope(LPCSTR section);
	bool bLoadAltScopesParams(LPCSTR section);
	void LoadOriginalScopesParams(LPCSTR section);
	void LoadCurrentScopeParams(LPCSTR section);
	const shared_str& GetSilencerName			() const{return m_sSilencerName;}
	void UpdateTorch();
	void SwitchTorch(bool status, bool forced = false);

	IC void	ForceUpdateAmmo						()		{ m_BriefInfo_CalcFrame = 0; }

	u8		GetAddonsState						()		const		{return m_flagsAddOnState;};
	void	SetAddonsState						(u8 st)	{m_flagsAddOnState=st;}//dont use!!! for buy menu only!!!

	bool	NeedBlockSprint						() const;

	virtual void OnMotionMark(u32 state, const motion_marks&);

	struct conditional_breaking_params
	{
		float start_condition = 0.0f;     // при каком состоянии начнутся проблемы
		float end_condition = 0.0f;       // при каком состоянии отрубится вообще
		float start_probability = 0.0f;   // вероятность проблем в стартовом состоянии
	};

	conditional_breaking_params CollimatorBreakingParams;
	conditional_breaking_params TorchBreakingParams;

	float m_fCollimatorLevelsProblem;

	bool bUpdateHUDBonesVisibility = false;
	u32 _last_update_time;

	bool bReloadKeyPressed;
	bool bAmmotypeKeyPressed;
	bool bStopReloadSignal;
	bool m_bUseSilHud = false;
	bool m_bUseScopeHud = false;
	bool m_bUseGLHud = false;
	bool m_bHideColimSightInAlter;
	bool m_bIsAimStarted = false;
	bool m_bRestGlSil = false;
	bool m_bTacticalTorchStatus = false;
	bool m_bJustAfterReload = false;
	bool m_bIsPreloaded = false;
	bool m_bAddCartridgeInOpen = false;
	bool m_bBlockUpdateAmmoBonesShooting = false;
	bool m_bUseLastAmmoType = false;
	bool m_bUseChamberInUpdateBones = false;
	bool m_bBlockReload = false;

	shared_str hud_silencer;
	shared_str hud_scope;
	shared_str hud_gl;
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
		CWeaponNightVision*		m_pNight_vision;

	} m_zoom_params;
	
		float			m_fRTZoomFactor; //run-time zoom factor
		CUIWindow*		m_UIScope;

	InertionData	m_base_inertion;
	InertionData	m_zoom_inertion;
	bool m_bIAmWeaponRPG7;
	shared_str GetCurrentScopeSection() const { return m_scopes[m_cur_scope]; }
	shared_str GetScopeSection(int idx) const { return m_scopes[idx]; }

protected:

	u8 m_LastShotAmmoType = undefined_ammo_type;

	xr_vector<SAmmoBonesParams*> m_ammo_bones_mag{}, m_ammo_bones_gl{}, m_shell_bones{};

	RStringVec m_bDefHideBones {}, m_bDefShowBones {}, m_bHideBonesOverride {}, m_bDefHideBonesGLAttached {},
		m_bHideBonesGLAttached {}, m_bHideBonesSilAttached {}, m_bHideBonesScopeAttached {},
		m_bHideBonesUpgrade {}, m_bScopeShowBones{}, m_bScopeHideBones{}, m_bShowBonesUpgToHide{}, m_bShowBonesUpgToShow{},
		m_sCollimatorSightsBones{};

	bool m_bDisableFireModeAim = false;
	bool m_bBlockEmptyClick = false;
	bool m_bIsReloaded = false;

	void HideOneUpgradeLevel(const char* section);
	void LoadUpgradeBonesToHide(const char* section, const char* line);
	u32 FakeReload();
	virtual void ForceUpdateHUD();

public:
	virtual bool IsGrenadeMode() const { return false; }

	IC bool					IsZoomEnabled		()	const		{return m_zoom_params.m_bZoomEnabled;}
	virtual	void			ZoomInc				();
	virtual	void			ZoomDec				();
	virtual void			OnZoomIn			();
	virtual void			OnZoomOut			();
	IC		bool			IsZoomed			()	const		{return m_zoom_params.m_bIsZoomModeNow;};
	CUIWindow*				ZoomTexture			();	

	CWeaponNightVision*		GetNightVision()	{ return m_zoom_params.m_pNight_vision; }

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

	virtual	u8				GetCurrentHudOffsetIdx ();

	virtual float				Weight			() const;		
	virtual	u32					Cost			() const;
public:
    virtual EHandDependence		HandDependence		()	const		{	return eHandDependence;}
			bool				IsSingleHanded		()	const		{	return m_bIsSingleHanded; }
			void				SetMisfireStatus	(bool b)		{ bMisfire = b; }

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

	u8 GetCurrentHudOffsetIdx() const;

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

public:
	CameraRecoil			cam_recoil;			// simple mode (walk, run)
	CameraRecoil			zoom_cam_recoil;	// using zoom =(ironsight or scope)

protected:
	bool					useLegacyMisfire;
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
	IC int					GetAmmoElapsed		()	const		{ return iAmmoElapsed; }
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

	virtual void			UnloadChamber(bool spawn_ammo = true);

protected:
	int						iAmmoElapsed;		// ammo in magazine, currently
	int						iMagazineSize;		// size (in bullets) of magazine

	int						iAmmoChamberElapsed;
	int						iChamberSize;

	bool					m_bAmmoInChamber;

	//для подсчета в GetSuitableAmmoTotal
	mutable int				m_iAmmoCurrentTotal;
	mutable u32				m_BriefInfo_CalcFrame;	//кадр на котором просчитали кол-во патронов
	bool					m_bAmmoWasSpawned;

	virtual bool			IsNecessaryItem	    (const shared_str& item_sect);

	virtual void			GiveAmmoFromMagToChamber();
	virtual void			DeleteAmmoInChamber();

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

	SCOPES_VECTOR			m_scopes;
	u8						m_cur_scope;

	CWeaponAmmo*			m_pCurrentAmmo;
	u8						m_ammoType;
	u8						m_ChamberAmmoType;
//-	shared_str				m_ammoName; <== deleted
	bool					m_bHasTracers;
	u8						m_u8TracerColorID;
	u8						m_set_next_ammoType_on_reload;
	// Multitype ammo support
	xr_vector<CCartridge>	m_magazine;
	xr_vector<CCartridge>	m_chamber;
	CCartridge				m_DefaultCartridge;
	CCartridge				m_DefaultCartridgeInChamber;
	float					m_fCurrentCartirdgeDisp;

		bool				unlimited_ammo				();
		bool				infinite_fire();
	IC	bool				can_be_strapped				() const {return m_can_be_strapped;};

	float GetMagazineWeight(const decltype(m_magazine)& mag) const;

protected:
	u32						m_ef_main_weapon_type;
	u32						m_ef_weapon_type;

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
	virtual bool			use_crosshair				()	const {return true;}
			bool			show_crosshair				();
			bool			show_indicators				();
	virtual BOOL			ParentMayHaveAimBullet		();
	virtual BOOL			ParentIsActor				();
	
private:
	virtual	bool			install_upgrade_ammo_class	( LPCSTR section, bool test );
			bool			install_upgrade_disp		( LPCSTR section, bool test );
			bool			install_upgrade_hit			( LPCSTR section, bool test );
			bool			install_upgrade_addon		( LPCSTR section, bool test );
			
			bool			install_upgrade_hud_sect(LPCSTR section, bool test);
			bool			install_upgrade_hud_sect_silencer(LPCSTR section, bool test);
			bool			install_upgrade_hud_sect_scope(LPCSTR section, bool test);
			bool			install_upgrade_hud_sect_gl(LPCSTR section, bool test);

			bool			install_upgrade_bones		( LPCSTR section, bool test );
			bool			install_upgrade_ammo_bones	( LPCSTR section, bool test );

			bool			install_upgrade_torch_laser	( LPCSTR section, bool test );
protected:
	virtual bool			install_upgrade_impl		( LPCSTR section, bool test );

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
	bool GetScopeBack();
	void UpdateCollimatorSight();
};
