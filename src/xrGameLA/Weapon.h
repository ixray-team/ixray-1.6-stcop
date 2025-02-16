// Weapon.h: interface for the CWeapon class.
#pragma once

#include "PhysicsShell.h"
#include "weaponammo.h"
#include "PHShellCreator.h"

#include "ShootingObject.h"
#include "hud_item_object.h"
#include "Actor_Flags.h"
#include "../../Include/xrRender/KinematicsAnimated.h"
#include "game_cl_single.h"
#include "firedeps.h"

// refs
class CEntity;
class ENGINE_API CMotionDef;
class CSE_ALifeItemWeapon;
class CSE_ALifeItemWeaponAmmo;
class CWeaponMagazined;
class CParticlesObject;
class CUIWindow;

class CNightVisionEffector;
class CBinocularsVision;

class CWeapon : public CHudItemObject,
				public CShootingObject
{
private:
	typedef CHudItemObject inherited;

public:
							CWeapon				(LPCSTR name);
	virtual					~CWeapon			();

	// Generic
	virtual void			Load				(LPCSTR section);

	// Happens when object gets online. Import saved data from server here. Do oject initialization based on data imported from server
	virtual BOOL			net_Spawn			(CSE_Abstract* DC);
	virtual void			net_Destroy			();
	// constant export of various weapon data to server. Here should be stuff, needed to be saved before object switches offline. Than it should be imported in net_spawn, when object gets online again
	virtual void			net_Export			(NET_Packet& P);
	// not happens in Single Player
	virtual void			net_Import			(NET_Packet& P);

			void			net_Relcase			(CObject *object) override;
	
	virtual CWeapon			*cast_weapon			()					{return this;}
	virtual CWeaponMagazined*cast_weapon_magazined	()					{return 0;}


	// save stuff that is needed while weapon is in alife radius
	virtual void			save				(NET_Packet &output_packet);
	// load stuff needed in alife radius
	virtual void			load				(IReader &input_packet);

	virtual BOOL			net_SaveRelevant	()								{return inherited::net_SaveRelevant();}

	virtual void			UpdateCL			();
	virtual void			shedule_Update		(u32 dt);

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

	virtual void			Hide				();
	virtual void			Show				();

	//инициализация если вещь в активном слоте или спрятана на OnH_B_Chield
	virtual void			OnActiveItem		();
	virtual void			OnHiddenItem		();
	virtual void			SendHiddenItem		();

	IC void	ForceUpdateAmmo()		{ m_dwAmmoCurrentCalcFrame = 0; }
//////////////////////////////////////////////////////////////////////////
//  Network
//////////////////////////////////////////////////////////////////////////

public:
	virtual bool			can_kill			() const;
	virtual CInventoryItem	*can_kill			(CInventory *inventory) const;
	virtual const CInventoryItem *can_kill		(const xr_vector<const CGameObject*> &items) const;
	virtual bool			ready_to_kill		() const;
	virtual bool			NeedToDestroyObject	() const; 
	virtual ALife::_TIME_ID	TimePassedAfterIndependant() const;
protected:
	//время удаления оружия
	ALife::_TIME_ID			m_dwWeaponRemoveTime;
	ALife::_TIME_ID			m_dwWeaponIndependencyTime;

	virtual bool			IsHudModeNow		();

//////////////////////////////////////////////////////////////////////////
//  Animation 
//////////////////////////////////////////////////////////////////////////
public:
	void					signal_HideComplete	();

//////////////////////////////////////////////////////////////////////////
//  InventoryItem methods
//////////////////////////////////////////////////////////////////////////
public:
	virtual bool			Action(u16 cmd, u32 flags);

//////////////////////////////////////////////////////////////////////////
//  Weapon state
//////////////////////////////////////////////////////////////////////////
public:
	enum EWeaponStates {
		eFire		= eLastBaseState+1,
		eFire2,
		eReload,
		eMisfire,
		eMagEmpty,
		eSwitch,
		ePreFire,
	};
	enum EWeaponSubStates{
		eSubstateReloadBegin		=0,
		eSubstateReloadInProcess,
		eSubstateReloadEnd,
	};

	IC BOOL					IsValid				()	const		{	return iAmmoElapsed;						}
	// Does weapon need's update?
	BOOL					IsUpdating			();


	BOOL					IsMisfire			() const;
	BOOL					CheckForMisfire		();

	virtual bool			MovingAnimAllowedNow	();

	BOOL					AutoSpawnAmmo		() const		{ return m_bAutoSpawnAmmo; };
	bool					IsTriStateReload	() const		{ return m_bTriStateReload;}
	EWeaponSubStates		GetReloadState		() const		{ return (EWeaponSubStates)m_sub_state;}
protected:
	bool					m_bTriStateReload;
	u8						m_sub_state;
	// Weapon fires now
	bool					bWorking2;
	// a misfire happens, you'll need to rearm weapon
	bool					bMisfire;				

	BOOL					m_bAutoSpawnAmmo;

	virtual bool			AllowBore		();

//////////////////////////////////////////////////////////////////////////
//  Weapon Addons
//////////////////////////////////////////////////////////////////////////
public:
	///////////////////////////////////////////
	// работа с аддонами к оружию
	//////////////////////////////////////////


			bool IsGrenadeLauncherAttached	() const;
			bool IsScopeAttached			() const;
			bool IsSilencerAttached			() const;

	virtual bool GrenadeLauncherAttachable();
	virtual bool ScopeAttachable();
	virtual bool SilencerAttachable();
	virtual bool UseScopeTexture() {return true;};

	ALife::EWeaponAddonStatus	get_GrenadeLauncherStatus	() const { return m_eGrenadeLauncherStatus; }
	ALife::EWeaponAddonStatus	get_ScopeStatus				() const { return m_eScopeStatus; }
	ALife::EWeaponAddonStatus	get_SilencerStatus			() const { return m_eSilencerStatus; }

	//обновление видимости для косточек аддонов
			void UpdateAddonsVisibility();
			void UpdateHUDAddonsVisibility();
	//инициализация свойств присоединенных аддонов
	virtual void InitAddons();

	//для отоброажения иконок апгрейдов в интерфейсе
	int	GetScopeX() {return pSettings->r_s32(m_scopes[m_cur_scope], "scope_x");}
	int	GetScopeY() {return pSettings->r_s32(m_scopes[m_cur_scope], "scope_y");}
	int	GetSilencerX() {return m_iSilencerX;}
	int	GetSilencerY() {return m_iSilencerY;}
	int	GetGrenadeLauncherX() {return m_iGrenadeLauncherX;}
	int	GetGrenadeLauncherY() {return m_iGrenadeLauncherY;}

	const shared_str& GetGrenadeLauncherName	() const		{return m_sGrenadeLauncherName;}
	const shared_str GetScopeName				() const		{ return pSettings->r_string(m_scopes[m_cur_scope], "scope_name"); }
	const shared_str& GetSilencerName			() const		{return m_sSilencerName;}

	bool	IsScopeIconForced					() const		{ return m_bScopeForceIcon; }
	bool	IsSilencerIconForced				() const		{ return m_bSilencerForceIcon; }
	bool	IsGrenadeLauncherIconForced			() const		{ return m_bGrenadeLauncherForceIcon; }

	u8		GetAddonsState						()		const		{return m_flagsAddOnState;};
	void	SetAddonsState						(u8 st)	{m_flagsAddOnState=st;}//dont use!!! for buy menu only!!!
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

	//рисовать ли иконку аддона вне зависимости от статуса (например для permanent)
	bool m_bScopeForceIcon;
	bool m_bSilencerForceIcon;
	bool m_bGrenadeLauncherForceIcon;

///////////////////////////////////////////////////
//	для режима приближения и снайперского прицела
///////////////////////////////////////////////////
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
//		Fvector4		m_ReloadDof;
		BOOL			m_bUseDynamicZoom;
		shared_str		m_sUseZoomPostprocess;
		shared_str		m_sUseBinocularVision;
		CBinocularsVision*		m_pVision;
		CNightVisionEffector*	m_pNight_vision;

		SZoomParams();

	} m_zoom_params;
	
		float			m_fRTZoomFactor; //run-time zoom factor
		CUIWindow*		m_UIScope;
public:

	IC bool					IsZoomEnabled		()	const		{return m_zoom_params.m_bZoomEnabled;}
	virtual	void			ZoomInc				();
	virtual	void			ZoomDec				();
	virtual void			OnZoomIn			();
	virtual void			OnZoomOut			();
	IC		bool			IsZoomed			()	const		{return m_zoom_params.m_bIsZoomModeNow;};
	CUIWindow*				ZoomTexture			();	


			bool			ZoomHideCrosshair	()				{return m_zoom_params.m_bHideCrosshairInZoom || ZoomTexture();}

	IC float				GetZoomFactor		() const		{return m_zoom_params.m_fCurrentZoomFactor;}
	IC void					SetZoomFactor		(float f) 		{m_zoom_params.m_fCurrentZoomFactor = f;}

	virtual	float			CurrentZoomFactor	();
	//показывает, что оружие находится в соостоянии поворота для приближенного прицеливания
			bool			IsRotatingToZoom	() const		{	return (m_zoom_params.m_fZoomRotationFactor<1.f);}

	virtual	u8				GetCurrentHudOffsetIdx ();

	virtual float				Weight			() const;		
	virtual	u32					Cost			() const;
//gr1ph:
protected:
	virtual void			GetZoomData			(const float scope_factor, float& delta, float& min_zoom_factor);
public:
    virtual EHandDependence		HandDependence		()	const		{	return eHandDependence;}
			bool				IsSingleHanded		()	const		{	return m_bIsSingleHanded; }

public:
	IC		LPCSTR			strap_bone0			() const {return m_strap_bone0;}
	IC		LPCSTR			strap_bone1			() const {return m_strap_bone1;}
	IC		void			strapped_mode		(bool value) {m_strapped_mode = value;}
	IC		bool			strapped_mode		() const {return m_strapped_mode;}

protected:
	int					m_strap_bone0_id;
	int					m_strap_bone1_id;
	bool					m_strapped_mode_rifle;
	LPCSTR					m_strap_bone0;
	LPCSTR					m_strap_bone1;
	Fmatrix					m_StrapOffset;
	bool					m_strapped_mode;
	bool					m_can_be_strapped;
	bool					m_can_be_strapped_rifle;

	Fmatrix					m_Offset;
	// 0-используется без участия рук, 1-одна рука, 2-две руки
	EHandDependence			eHandDependence;
	bool					m_bIsSingleHanded;

public:
	//загружаемые параметры
	Fvector					vLoadedFirePoint	;
	Fvector					vLoadedFirePoint2	;

private:
	firedeps				m_firedeps;

protected:
	virtual void			UpdateFireDependencies_internal	();
	virtual void			UpdatePosition			(const Fmatrix& transform);	//.
	virtual void			UpdateXForm				();
	virtual void			UpdateHudAdditonal		(Fmatrix&);
	IC		void			UpdateFireDependencies	()			{ if (dwFP_Frame==Device.dwFrame) return; UpdateFireDependencies_internal(); };

	virtual void			LoadFireParams		(LPCSTR section, LPCSTR prefix);
public:	
	IC		const Fvector&	get_LastFP				()			{ UpdateFireDependencies(); return m_firedeps.vLastFP;	}
	IC		const Fvector&	get_LastFP2				()			{ UpdateFireDependencies(); return m_firedeps.vLastFP2;	}
	IC		const Fvector&	get_LastFD				()			{ UpdateFireDependencies(); return m_firedeps.vLastFD;	}
	IC		const Fvector&	get_LastSP				()			{ UpdateFireDependencies(); return m_firedeps.vLastSP;	}

	virtual const Fvector&	get_CurrentFirePoint	()			{ return get_LastFP();				}
	virtual const Fvector&	get_CurrentFirePoint2	()			{ return get_LastFP2();				}
	virtual const Fmatrix&	get_ParticlesXFORM		()			{ UpdateFireDependencies(); return m_firedeps.m_FireParticlesXForm;	}
	virtual void			ForceUpdateFireParticles();
	virtual void			debug_draw_firedeps		();

	//////////////////////////////////////////////////////////////////////////
	// Weapon fire
	//////////////////////////////////////////////////////////////////////////
protected:
	virtual void			SetDefaults			();

	//трассирование полета пули
	virtual	void			FireTrace			(const Fvector& P, const Fvector& D);
	virtual float			GetWeaponDeterioration	();

	virtual void			FireStart			() {CShootingObject::FireStart();}
	virtual void			FireEnd				();// {CShootingObject::FireEnd();}

	virtual void			Fire2Start			();
	virtual void			Fire2End			();
	virtual void			Reload				();
	virtual	void			StopShooting		();
    

	// обработка визуализации выстрела
	virtual void			OnShot				(){};
	virtual void			AddShotEffector		();
	virtual void			RemoveShotEffector	();
	virtual	void			ClearShotEffector	();

public:
	float					GetFireDispersion	(bool with_cartridge);
	virtual float			GetFireDispersion	(float cartridge_k);
	virtual	int				ShotsFired			() { return 0; }
	virtual	int				GetCurrentFireMode	() { return 1; }

	//параметы оружия в зависимоти от его состояния исправности
	float					GetConditionDispersionFactor	() const;
	float					GetConditionMisfireProbability	() const;
	virtual	float			GetConditionToShow				() const;

public:
	//отдача при стрельбе 
	struct CameraRecoil
	{
	public:
		float					camRelaxSpeed;
		float					camRelaxSpeed_AI;
		float					camDispersion;
		float					camDispersionInc;
		float					camDispersionFrac;
		float					camMaxAngleVert; // new
		float					camMaxAngleHorz;
		float					camStepAngleHorz;
		bool					camReturnMode; // new
		bool					camStopReturn; // new
	} cam_recoil, zoom_cam_recoil;
	

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

	// Коэффициент силы покачивания ствола при прицеливании, умножается на разброс Актора (вычисляемый из PDM)
	float					m_fZoomInertCoef;

protected:
	//для отдачи оружия
	Fvector					m_vRecoilDeltaAngle;

	//для сталкеров, чтоб они знали эффективные границы использования 
	//оружия
	float					m_fMinRadius;
	float					m_fMaxRadius;

//////////////////////////////////////////////////////////////////////////
// партиклы
//////////////////////////////////////////////////////////////////////////

protected:	
	//для второго ствола
			void			StartFlameParticles2();
			void			StopFlameParticles2	();
			void			UpdateFlameParticles2();
protected:
	shared_str					m_sFlameParticles2;
	//объект партиклов для стрельбы из 2-го ствола
	CParticlesObject*		m_pFlameParticles2;

//////////////////////////////////////////////////////////////////////////
// Weapon and ammo
//////////////////////////////////////////////////////////////////////////
public:
	IC int					GetAmmoElapsed		()	const		{	return /*int(m_magazine.size())*/iAmmoElapsed;}
	IC int					GetAmmoMagSize		()	const		{ return maxMagazineSize_; }
	int						GetAmmoCurrent		(bool use_item_to_spawn = false)  const;

	void					SetAmmoElapsed		(int ammo_count);

	virtual void			OnMagazineEmpty		();
			void			SpawnAmmo			(u32 boxCurr = 0xffffffff, 
													LPCSTR ammoSect = NULL, 
													u32 ParentID = 0xffffffff);

	virtual	float			Get_PDM_Base		()	const	{ return m_pdm.m_fPDM_disp_base			; };
	virtual	float			Get_PDM_Vel_F		()	const	{ return m_pdm.m_fPDM_disp_vel_factor		; };
	virtual	float			Get_PDM_Accel_F		()	const	{ return m_pdm.m_fPDM_disp_accel_factor	; };
	virtual	float			Get_PDM_Crouch		()	const	{ return m_pdm.m_fPDM_disp_crouch			; };
	virtual	float			Get_PDM_Crouch_NA	()	const	{ return m_pdm.m_fPDM_disp_crouch_no_acc	; };

	virtual float			GetZoomInertion		()	const	{ return m_fZoomInertCoef; };
	//virtual	float			GetCrosshairInertion()	const	{ return m_crosshair_inertion; };
	//		float			GetFirstBulletDisp	()	const	{ return m_first_bullet_controller.get_fire_dispertion(); };
protected:
	int						iAmmoElapsed;		// ammo in magazine, currently
	int						maxMagazineSize_;	// size (in bullets) of magazine

	//для подсчета в GetAmmoCurrent
	mutable int				iAmmoCurrent;
	mutable u32				m_dwAmmoCurrentCalcFrame;	//кадр на котором просчитали кол-во патронов
	//  [10/5/2005]
	bool					m_bAmmoWasSpawned;
	//  [10/5/2005]

	virtual bool			IsNecessaryItem	    (const shared_str& item_sect);

public:
	xr_vector<shared_str>	m_ammoTypes;

	using SCOPES_VECTOR = xr_vector<shared_str>;
	using SCOPES_VECTOR_IT = SCOPES_VECTOR::iterator;
	SCOPES_VECTOR			m_scopes;
	u8						m_cur_scope;
	CWeaponAmmo*			m_pAmmo;
	u8						m_ammoType;
	shared_str				m_ammoName;
	bool					m_bHasTracers;
	u8						m_u8TracerColorID;
	u8						m_set_next_ammoType_on_reload;
	// Multitype ammo support
	xr_vector<CCartridge>	m_magazine;
	CCartridge				m_DefaultCartridge;
	float					m_fCurrentCartirdgeDisp;

		bool				unlimited_ammo				();
	IC	bool				can_be_strapped				() const {return m_can_be_strapped;};

	LPCSTR					GetCurrentAmmo_ShortName	();

protected:
	u32						m_ef_main_weapon_type;
	u32						m_ef_weapon_type;
	u32						m_ai_weapon_rank;

public:
	virtual u32				ef_main_weapon_type	() const;
	virtual u32				ef_weapon_type		() const;
	virtual u32				ai_weapon_rank		() const { return m_ai_weapon_rank; };

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
			bool			install_upgrade_disp		( LPCSTR section, bool test );
			bool			install_upgrade_hit			( LPCSTR section, bool test );
			bool			install_upgrade_addon		( LPCSTR section, bool test );
protected:
	virtual	bool			install_upgrade_ammo_class	( LPCSTR section, bool test );
	virtual bool			install_upgrade_impl		( LPCSTR section, bool test );
			bool			ProcessRpmUpgrade			( LPCSTR section, LPCSTR paramName, float &timeToFireProperty, bool test);
			bool			process_if_exists_deg2rad	( LPCSTR section, LPCSTR name, float& value, bool test );

private:
	float					m_hit_probability[egdCount];

public:
	const float				&hit_probability			() const;
private:
	bool					m_bRememberActorNVisnStatus;
public:
			bool			GetRememberActorNVisnStatus	() {return m_bRememberActorNVisnStatus;};
	virtual void			EnableActorNVisnAfterZoom	();
};


extern bool	m_bDraw_off;
extern bool	m_bHolster_off;
