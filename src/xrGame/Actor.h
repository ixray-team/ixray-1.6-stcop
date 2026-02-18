#pragma once

#include "../xrEngine/Feel_Touch.h"
#include "../xrEngine/Feel_Sound.h"
#include "../xrEngine/IInputReceiver.h"
#include "../xrEngine/IGame_Actor.h"
#include "../Include/xrRender/KinematicsAnimated.h"
#include "Actor_Flags.h"
#include "actor_defs.h"
#include "fire_disp_controller.h"
#include "entity_alive.h"
#include "PHMovementControl.h"
#include "../xrPhysics/PhysicsShell.h"
#include "InventoryOwner.h"
#include "../xrEngine/StatGraph.h"
#include "PhraseDialogManager.h"
#include "../../xrUI/ui_defs.h"
#include "ControllerAutoaim.h"
#include "step_manager.h"
#include "../xrScripts/script_export_space.h"
#include "CustomDevice.h"
#include "EffectorNightVision.h"
#include "HudAnimatorManager.h"
#include "nvg.h"

using namespace ACTOR_DEFS;

class CInfoPortion;
struct GAME_NEWS_DATA;
class CActorCondition;
class CAI_Stalker;
class CInventoryItem;
class CCustomOutfit;
class CGameTaskRegistryWrapper;
class CGameNewsRegistryWrapper;
class CCharacterPhysicsSupport;
class CActorCameraManager;
// refs
class ENGINE_API CCameraBase;
class ENGINE_API CBoneInstance;
class ENGINE_API CBlend;
class CWeaponList;
class CEffectorBobbing;
class CHolderCustom;
class CUsableScriptObject;

class CInventoryBox;

class CHudItem;
class CArtefact;

struct SActorMotions;
struct SActorVehicleAnims;
class  CActorCondition;
class CActorFollowerMngr;

struct CameraRecoil;
class CCameraShotEffector;
class CActorInputHandler;

class CActorMemory;
class CActorStatisticMgr;
class CEncyclopediaRegistryWrapper;
class CLocationManager;
class CPickUpManager;
class CCustomDevice;
class CAutoAim;

class CNightVisionEffector;
class CHudAnimatorManager;

class CAnimatorCamLerpEffectorConst;
class THudVertexAssignedPatricles;

class CActor: 
	public IGame_Actor, 
	public CEntityAlive, 
	public Feel::Touch,
	public CInventoryOwner,
	public CPhraseDialogManager,
	public CStepManager,
	public Feel::Sound,
	public pureFrame
#ifdef DEBUG_DRAW
	,public pureRender
#endif
{
	friend class CActorCondition;
private:
	typedef CEntityAlive	inherited;
	CPickUpManager* pPickup = nullptr;
	CAutoAim* pAutoaim = nullptr;

	const char* m_onBeforeHitCallback = {};
	bool m_isBeforeHitCallback = false;
public:
										CActor				();
	~CActor				() override;
	void OnFrame();
	THudVertexAssignedPatricles* GetHudVertexAssignedPatriclesComponent();

public:
	virtual bool						AlwaysTheCrow				() override { return true; }

	virtual CAttachmentOwner*			cast_attachment_owner		() override						{return this;}
	virtual CInventoryOwner*			cast_inventory_owner		() override						{return this;}
	virtual CActor*						cast_actor					() override						{return this;}
	virtual CGameObject*				cast_game_object			() override						{return this;}
	virtual IInputReceiver*				cast_input_receiver			() override					{return this;}
	virtual CEntityAlive*				cast_entity_alive			() override					{return this;}
	virtual CEntity*					cast_entity					() override					{return this;}
	virtual CPhraseDialogManager*		cast_phrase_dialog_manager	() override					{return this;}
	virtual CCharacterPhysicsSupport*	character_physics_support	() override					{return m_pPhysics_support;}
	virtual CCharacterPhysicsSupport*	character_physics_support	() const override {return m_pPhysics_support;}
	virtual CPHDestroyable*				ph_destroyable				() override;
			CHolderCustom*				Holder						() const {return m_holder;}

	virtual xr_vector<xr_string>		GetKnownPortions() const override;
	virtual xr_vector<xr_string>		GetKnownPortionDialogs(shared_str id) const override;
	virtual xr_vector<xr_string>		GetKnownPortionDisable(shared_str id) const override;
	virtual xr_vector<xr_string>		GetKnownPortionArticles(shared_str id) const override;
	virtual xr_vector<xr_string>		GetKnownPortionArticlesDisable(shared_str id) const override;
	virtual xr_vector<xr_string>		GetKnownPortionTasks(shared_str id) const override;
	virtual void						Load				( const char* section ) override;

	void						shedule_Update		( u32 T ) override;
	void PlayRainOnHelmetSound();
	void						UpdateCL			( ) override;
	void draw_electrical_fur();
			void						UpdateLensFOV		(CWeapon* wpn, float value);
	void CheckFlyhack();
			void						UpdatePlayerView	( );

	void						OnEvent				( NET_Packet& P, u16 type		) override;

	void								Center(Fvector& C)	const override;

	// Render
	void						renderable_Render			() override;
	bool						renderable_ShadowGenerate	() override;
	void						feel_sound_new				(CObject* who, int type, CSound_UserDataPtr user_data, const Fvector& position, float power) override;
	Feel::Sound*				dcast_FeelSound				() override { return this;	}
			float						m_snd_noise;
#ifdef DEBUG_DRAW
	void						OnRender			() override;

#endif


public:
	bool OnReceiveInfo		(shared_str info_id) const override;
	void OnDisableInfo		(shared_str info_id) const override;

	void	 NewPdaContact		(CInventoryOwner*) override;
	void	 LostPdaContact		(CInventoryOwner*) override;

	void GiveInfoPortion(const char* infoPortion) override;
	void DisableInfoPortion(const char* info_id) override;
	void SetActorPosition(Fvector pos) override;
	void SetActorDirection(float dir) override;

#ifdef DEBUG
	void			 DumpTasks();
#endif

protected:
	virtual void	AddEncyclopediaArticle(const CInfoPortion* info_portion) const;
	virtual void	AddGameTask				(const CInfoPortion* info_portion) const;
	struct SDefNewsMsg{
		GAME_NEWS_DATA*	news_data;
		u32				time;
		bool operator < (const SDefNewsMsg& other) const {return time>other.time;}
	};
	xr_vector<SDefNewsMsg> m_defferedMessages;
	void UpdateDefferedMessages();
public:
	void			AddGameNews_deffered	 (GAME_NEWS_DATA& news_data, u32 delay);
	virtual void	AddGameNews				 (GAME_NEWS_DATA& news_data);
protected:
	CActorStatisticMgr*				m_statistic_manager;
public:
	virtual void StartTalk			(CInventoryOwner* talk_partner);
			void RunTalkDialog		(CInventoryOwner* talk_partner, bool disable_break);
	CActorStatisticMgr&				StatisticMgr() const {return *m_statistic_manager;}
			void OnMoneyChanged		(u32 previousMoney, u32 newMoney);
			void AddDistanceMeters	(float deltaMeters);
			void RegisterHeadshotKill();
			void RegisterPlayerDeath();
			void RegisterHelpWounded();
			void TryRegisterHelpWounded(CAI_Stalker* targetStalker, const CInventoryItem* item);
			static void ResetDeathStatCarryOver();
			void OnDeathStatLoadedFromSave(u32 savedDeaths);
			void OnDeathStatSavedToGame();
			u32 GetStatMoneyEarned	() const { return m_statMoneyEarned; }
			u32 GetStatMoneySpent	() const { return m_statMoneySpent; }
			float GetStatDistanceMeters() const { return m_statDistanceMeters; }
			u32 GetStatHeadshots	() const { return m_statHeadshots; }
			u32 GetStatDeaths		() const { return m_statDeaths; }
			u32 GetStatHelpWounded	() const { return m_statHelpWounded; }
			u32 GetPdaRankingStatRevision() const { return m_pdaRankingStatRevision; }
    CEncyclopediaRegistryWrapper*	encyclopedia_registry;
	CGameNewsRegistryWrapper		*game_news_registry;
	CCharacterPhysicsSupport		*m_pPhysics_support;

	const char*	Name        () const override {return CInventoryOwner::Name();}

public:
	//PhraseDialogManager
	void ReceivePhrase				(DIALOG_SHARED_PTR& phrase_dialog) override;
	void UpdateAvailableDialogs		(CPhraseDialogManager* partner) override;
	virtual void TryToTalk					();
			bool OnDialogSoundHandlerStart	(CInventoryOwner *inv_owner, const char* phrase);
			bool OnDialogSoundHandlerStop	(CInventoryOwner *inv_owner);


	void reinit			() override;
	void reload			(const char* section) override;
	bool use_bolts		() const override;

	void OnItemTake		(CInventoryItem *inventory_item) override;
	void OnItemTakeFromGround(CInventoryItem* inventory_item);

	void OnItemRuck		(CInventoryItem *inventory_item, const SInvItemPlace& previous_place) override;
	void OnItemBelt		(CInventoryItem *inventory_item, const SInvItemPlace& previous_place) override;

	void OnItemDrop		(CInventoryItem *inventory_item, bool just_before_destroy) override;
	void OnItemDropUpdate () override;

	virtual	void OnPlayHeadShotParticle (NET_Packet P);

	void						Die				(CObject* who) override;
	void						Hit				(SHit* pHDS) override;
	void						PHHit			(SHit &H) override;
	virtual float						CalcHitDamage	(SHit* pHDS);
	void						HitSignal		(float P, Fvector &vLocalDir,	CObject* who, s16 element) override;
			void						HitSector		(CObject* who, CObject* weapon);
			void						HitMark			(float P, Fvector dir,			CObject* who, s16 element, Fvector position_in_bone_space, float impulse,  ALife::EHitType hit_type);
	void						FootStepCallback(float power, bool b_play, bool b_on_ground, bool b_hud_view) override;

			void						Feel_Grenade_Update( float rad );

	float						GetMass				() override;
	float						Radius				() const override;
	virtual void						g_PerformDrop		();

	bool						use_default_throw_force	() override;
	float						missile_throw_force		() override;

	bool						unlimited_ammo			() override;
	bool						infinite_fire() override;
	bool						NeedToDestroyObject()  const override;
	virtual ALife::_TIME_ID				TimePassedAfterDeath() const;

	CPickUpManager* GetPickupManager() const { return pPickup; }

public:

	//свойства артефактов
	virtual void		UpdateArtefactsOnBeltAndOutfit();
	void				UpdateConditionArtefacts();
	void				HitArtefactsCondition(SHit& hit);
			float		HitArtefactsOnBelt		(float hit_power, ALife::EHitType hit_type);
			float		HitArtefactsOnBeltLegacy(float hit_power, ALife::EHitType hit_type);
			float		GetProtection_ArtefactsOnBelt(ALife::EHitType hit_type);
			float		GetArtefactEquipmentDurabilityModifier() const;
			float		GetArtefactInventoryWeightModifier() const;
			float		GetArtefactJumpHeightModifier() const;
			float		GetArtefactMovementSpeedModifier() const;
	virtual void		MoveArtefactBelt		(const CArtefact* artefact, bool on_belt);
	const xr_vector<const CArtefact*>& ArtefactsOnBelt() {return m_ArtefactsOnBelt;}

protected:
	//звук тяжелого дыхания
	ref_sound			m_HeavyBreathSnd = {};
	ref_sound			m_BloodSnd = {};
	ref_sound			m_DangerSnd = {};
	u32					m_statMoneyEarned = 0;
	u32					m_statMoneySpent = 0;
	float				m_statDistanceMeters = 0.0f;
	u32					m_statHeadshots = 0;
	u32					m_statDeaths = 0;
	u32					m_statDeathsSavedInLastLoad = 0;
	u32					m_statHelpWounded = 0;
	u16					m_lastHelpWoundedStalkerId = u16(-1);
	u32					m_lastHelpWoundedGameTime = 0;
	u32					m_pdaRankingStatRevision = 0;
	bool				m_isMoneyStatInitialized = false;
			void BumpPdaRankingStatRevision();
	Fvector				m_lastStatPosition = {};
	bool				m_hasLastStatPosition = false;
	ref_sound			m_rainOnHelmetSnd = {};

	xr_vector<const CArtefact*> m_ArtefactsOnBelt;

protected:
	// Death
	float					m_hit_slowmo;
	float					m_hit_probability;
	s8						m_block_sprint_counter;

	bool IsWaunded = false;

	// media
	xr_vector<ref_sound>	sndHit[ALife::eHitTypeMax];
	ref_sound				sndDie[SND_DIE_COUNT];


	float					m_fLandingTime;
	float					m_fJumpTime;
	float					m_fFallTime;
	float					m_fCamHeightFactor;

	// Dropping
	bool					b_DropActivated;
	float					f_DropPower;

	//random seed для Zoom mode
	s32						m_ZoomRndSeed;
	//random seed для Weapon Effector Shot
	s32						m_ShotRndSeed;

	bool					m_bOutBorder;
private:
	void					SwitchOutBorder(bool new_border_state);

	void LookoutFunctionReplace(float& cur_roll, float tgt_roll, float dt);

	CHudAnimatorManager* m_hud_animator = nullptr;
	u32 _jitter_time_remains = 0;
	u32 _last_update_time = 0;
	float m_last_camera_height = 0.0f;
	u32 m_last_cam_update_time = 0;
	u32 m_landing_effect_time_remains = 0;
	u32 m_landing2_effect_time_remains = 0;
	u32 m_landing_effect_finish_time_remains = 0;

	float m_fActorCameraLanding2Time = 0.0f;
	float m_fActorCameraLandingTime = 0.0f;
	float m_fActorCameraSpeedPow = 0.0f;
	float m_fDefaultActorCameraSpeed = 0.0f;
	float m_fActorCameraLandingOffset = 0.0f;
	float m_fActorCameraLandingSpeedFactor = 0.0f;
	float m_fActorCameraLandingSpeedPowFactor = 0.0f;
	float m_fActorCameraLanding2Offset = 0.0f;
	float m_fActorCameraLanding2SpeedFactor = 0.0f;
	float m_fActorCameraLanding2SpeedPowFactor = 0.0f;
	float m_fActorCameraFinishLandingSpeedFactor = 0.0f;
	float m_fActorCameraFinishLandingSpeedPowFactor = 0.0f;
	float m_fActorCameraFinishLandingTime = 0.0f;
	float m_fLookOutSpeed = 0.0f;
	float m_fLookOutAmplK = 0.0f;
	float m_fLookOutSpeedAmplDXPow = 0.0f;

	float m_fNoclipSpeedScale = 3.0f;

	bool m_bIsSafemode = false;

	CAnimatorCamLerpEffectorConst* m_pCameraIdle = nullptr;

	void UpdateCameraIdleAnimation();

 public:
	bool					m_bAllowDeathRemove;
	float					m_fLegs_shift;

	shared_str				m_sNVGAnimator;
	shared_str				m_sHeadlampAnimator;
	shared_str				m_sClearMaskAnimator;
	shared_str				m_sQuickKickAnimator;
	shared_str				m_sBurerKickAnimator;
	shared_str				m_sFrontKickAnimator;
	shared_str				m_sBackKickAnimator;

	void SetHandsJitterTime(u32 time) { _jitter_time_remains = time; }
	bool IsHandJitter() const { return _jitter_time_remains > 0; }
	float GetHandJitterScale(CHudItem* itm) const;

	float GetNoclipSpeedScale() const;
	void  SetNoclipSpeedScale(float scale);

	void					SetZoomRndSeed			(s32 Seed = 0);
	s32						GetZoomRndSeed			() const { return m_ZoomRndSeed;	};
	void					SetShotRndSeed			(s32 Seed = 0);
	s32						GetShotRndSeed			() const { return m_ShotRndSeed;	};

	CHudAnimatorManager*	HudAnimator() const { return m_hud_animator; }
	void					StartAnimator			(const shared_str& section);

public:
	void					detach_Vehicle			();
	void					steer_Vehicle			(float angle);
	void					attach_Vehicle			(CHolderCustom* vehicle);
	bool					use_HolderEx			(CHolderCustom* object, bool bForce);

	virtual bool			can_attach				(const CInventoryItem *inventory_item) const override;

	virtual void UpdatePlayerHud() final override;

protected:
	CHolderCustom*			m_holder;
	ALife::_OBJECT_ID		m_holderID;
	bool					use_Holder				(CHolderCustom* holder);

	bool					use_Vehicle				(CHolderCustom* object);
	void					ActorUse				();
	void					ActorQuickSlotUse		(int cmd);

protected:
	bool					m_bAnimTorsoPlayed;
	static void				AnimTorsoPlayCallBack(CBlend* B);

	// Rotation
	SRotation				r_torso;
	float					r_torso_tgt_roll;
	//положение торса без воздействия эффекта отдачи оружия
	SRotation				unaffected_r_torso;

	//ориентация модели
	float					r_model_yaw_dest;
	float					r_model_yaw;			// orientation of model
	float					r_model_yaw_delta;		// effect on multiple "strafe"+"something"


public:
	SActorMotions*			m_anims;
	SActorVehicleAnims*		m_vehicle_anims;

	CBlend*					m_current_legs_blend;
	CBlend*					m_current_torso_blend;
	CBlend*					m_current_jump_blend;
	MotionID				m_current_legs;
	MotionID				m_current_torso;
	MotionID				m_current_head;

	// callback на анимации модели актера
	void					SetCallbacks		();
	void					ResetCallbacks		();
	static void		_BCL	Spin0Callback		(CBoneInstance*);
	static void		_BCL	Spin1Callback		(CBoneInstance*);
	static void		_BCL	ShoulderCallback	(CBoneInstance*);
	static void		_BCL	HeadCallback		(CBoneInstance*);
	static void		_BCL	VehicleHeadCallback	(CBoneInstance*);

	const SRotation	Orientation			()	const override { return r_torso; };
	SRotation				&Orientation		()			 { return r_torso; };

	void					g_SetAnimation		(u32 mstate_rl);
	void					g_SetSprintAnimation(u32 mstate_rl,MotionID &head,MotionID &torso,MotionID &legs);
public:
	void			OnHUDDraw			(CCustomHUD* hud) override;
			bool			HUDview				( )const ;

	//visiblity 
	float			ffGetFov			()	const override { return 90.f;		}
	float			ffGetRange			()	const override { return 500.f;		}

	IC bool IsSafemode() const { return m_bIsSafemode; }
	IC void SetSafemodeStatus(bool status) { m_bIsSafemode = status; }

public:
	bool					HasCameraEffector	() const { return m_pActorEffector != nullptr; };
	CActorCameraManager&	Cameras				() const
	{VERIFY(HasCameraEffector()); return *m_pActorEffector;}

	CCameraBase*	cam_Active			() override	{return cameras[cam_active];}
	IC CCameraBase*			cam_FirstEye		() const {return cameras[eacFirstEye];}
	IC EActorCameras active_cam() const { return cam_active; }
	virtual void cam_Set(EActorCameras style);

	float					currentFOV();
protected:
	void					cam_Update				(float dt, float fFOV);
	void					cam_Lookout				( const Fmatrix &xform, float camera_height );
	void					camUpdateLadder			(float dt);
	void					cam_SetLadder			();
	void					cam_UnsetLadder			();
	void					CorrectActorCameraHeight(float& h);

	// Cameras
	CCameraBase*			cameras[eacMaxCam];
	EActorCameras			cam_active;
	float					fPrevCamPos;
	float					current_ik_cam_shift;
	Fvector					vPrevCamDir;
	float					fCurAVelocity;
	CEffectorBobbing*		pCamBobbing;
	u32						lastTimeAutoAimStarted = 0;


	//менеджер эффекторов, есть у каждого актрера
	CActorCameraManager*	m_pActorEffector;
	static float			f_Ladder_cam_limit;
public:
	//--#SM+#--
	float fFPCamYawMagnitude;
	float fFPCamPitchMagnitude;

	void			feel_touch_new				(CObject* O) override;
	void			feel_touch_delete			(CObject* O) override;
	bool			feel_touch_contact			(CObject* O) override;
	bool			feel_touch_on_contact		(CObject* O) override;

	CGameObject*			ObjectWeLookingAt			() const {return m_pObjectWeLookingAt;}
	CInventoryOwner*		PersonWeLookingAt			() const {return m_pPersonWeLookingAt;}
	const char*					GetDefaultActionForObject	() const {return *m_sDefaultObjAction;}
protected:
	CUsableScriptObject*	m_pUsableObject;
	// Person we're looking at
	CInventoryOwner*		m_pPersonWeLookingAt;
	CHolderCustom*			m_pVehicleWeLookingAt;
	CGameObject*			m_pObjectWeLookingAt;
	CInventoryBox*			m_pInvBoxWeLookingAt;

	// Tip for action for object we're looking at
	shared_str				m_sDefaultObjAction;
	shared_str				m_sCarTrunk;
	shared_str				m_sCarUse;
	shared_str				m_sCharacterUseAction;
	shared_str				m_sDeadCharacterUseAction;
	shared_str				m_sDeadCharacterUseOrDragAction;
	shared_str				m_sDeadCharacterUseOrDragActionGamepad = "dead_character_use_or_drag_gp";
	shared_str				m_sDeadCharacterDontUseAction;
	shared_str				m_sCarCharacterUseAction;
	shared_str				m_sInventoryItemUseAction;
	shared_str				m_sInventoryBoxUseAction;
	
	//расстояние (в метрах) на котором актер чувствует гранату (любую)
	float					m_fFeelGrenadeRadius;
	float					m_fFeelGrenadeTime; 	//время гранаты (сек) после которого актер чувствует гранату

	void					PickupModeUpdate	();
	void					PickupModeUpdate_COD ();

	//////////////////////////////////////////////////////////////////////////
	// Motions (передвижения актрера)
	//////////////////////////////////////////////////////////////////////////
public:
	void					g_cl_CheckControls		(u32 mstate_wf, Fvector &vControlAccel, float &Jump, float dt);
	void					g_cl_ValidateMState		(float dt, u32 mstate_wf);
	void					g_cl_Orientate			(u32 mstate_rl, float dt);
	void					g_sv_Orientate			(u32 mstate_rl, float dt);
	void					g_Orientate				(u32 mstate_rl, float dt);
	bool					g_LadderOrient			() ;
	void					UpdateMotionIcon		(u32 mstate_rl);

	void					SetMovementState		(const ACTOR_DEFS::EMovementStates& state, const ACTOR_DEFS::EMoveCommand& mask, bool status);
	u32						GetMovementState		(const ACTOR_DEFS::EMovementStates& state) const;

	bool					CanAccelerate			();
	bool					CanJump					();
	bool					CanMove					();
	float					CameraHeight			();
	float					CurrentHeight; // Alex ADD: for smooth crouch
	bool					CanSprint				();
	bool					CanRun					();
	void			StopAnyMove				() override;

	bool					AnyAction				() const {return (mstate_real & mcAnyAction) != 0;};
	bool					AnyMove					() const {return (mstate_real & mcAnyMove) != 0;};

	bool					is_jump					();
public:
	u32						mstate_wishful;
	u32						mstate_old;
	u32						mstate_real;

	bool					m_bJumpKeyPressed;

	float					m_fWalkAccel;
	float					m_fJumpSpeed;
	float					m_fRunFactor;
	float					m_fRunBackFactor;
	float					m_fWalkBackFactor;
	float					m_fCrouchFactor;
	float					m_fClimbFactor;
	float					m_fSprintFactor;

	float					m_fWalk_StrafeFactor;
	float					m_fRun_StrafeFactor;

	bool					bBlockSprint;
	u32						m_iKeyFlags = 0;

public:
	Fvector					GetMovementSpeed		() const {return NET_SavedAccel;};
	//////////////////////////////////////////////////////////////////////////
	// User input/output
	//////////////////////////////////////////////////////////////////////////
public:
	void			IR_OnMouseMove			(int x, int y) override;
	void			IR_GamepadUpdateStick	(int id, Fvector2 value) override;
	void			IR_GamepadKeyPress		(int id) override;
	void			IR_GamepadKeyRelease    (int id) override;
	void			IR_GamepadKeyHold		(int id) override;
	virtual void			IR_OnGyroscopeMove		(Fvector3 value);
	virtual void			IR_OnTouchpadMove		(Fvector2 value);
	virtual void			IR_OnKeyboardPress		(int dik) override;
	virtual void			IR_OnKeyboardRelease	(int dik) override;
	virtual void			IR_OnKeyboardHold		(int dik) override;
	virtual void			IR_OnMouseWheel			(int direction) override;
	virtual	float			GetLookFactor			();
	bool					IsActionKeyPressedInGame(const EGameActions& EGameAction) const;
	void					SetActorKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags mask, bool state, bool ignore_suicide = false);
	void					ProcessKeys(CHudItem* itm = nullptr);

public:
	void						g_WeaponBones		(u16 &L, u16 &R1, u16 &R2) final override;
	void						g_fireParams		(const CHudItem* pHudItem, Fvector& P, Fvector& D) final override;
	bool						g_stateFire			() override {return ! ((mstate_wishful & mcLookout) && !IsGameTypeSingle() );}

	virtual bool						g_State				(SEntityState& state) const override;
	virtual	float						GetWeaponAccuracy	() const override;
	virtual	float						GetAgility() const;
			float						GetFireDispertion	() const {return m_fdisp_controller.GetCurrentDispertion();}
			bool						IsZoomAimingMode	() const {return m_bZoomAimingMode;}
	float						MaxCarryWeight		() const override;
			float						MaxWalkWeight		() const;
			float						get_additional_weight() const;

			int	m_head;

protected:
	CFireDispertionController			m_fdisp_controller;
	//если актер целится в прицел
	void								SetZoomAimingMode	(bool val)	{m_bZoomAimingMode = val;}
	bool								m_bZoomAimingMode;

	//настройки аккуратности стрельбы
	//базовая дисперсия (когда игрок стоит на месте)
	float								m_fDispBase;
	float								m_fDispAim;
	//коэффициенты на сколько процентов увеличится базовая дисперсия
	//учитывает скорость актера 
	float								m_fDispVelFactor;
	//если актер бежит
	float								m_fDispAccelFactor;
	//если актер сидит
	float								m_fDispCrouchFactor;
	//crouch+no acceleration
	float								m_fDispCrouchNoAccelFactor;

	//коэффициенты на сколько процентов увеличится или уменьшиться ловкость для отдачи оружия, также учитывая скорость актера
	float								m_fAgilityVelFactor;
	//если актер бежит
	float								m_fAgilityAccelFactor;
	//если актер сидит
	float								m_fAgilityCrouchFactor;
	//crouch+no acceleration
	float								m_fAgilityCrouchNoAccelFactor;

protected:
	//косточки используемые при стрельбе
	u16									m_r_hand = BI_NONE;
	u16									m_l_finger1 = BI_NONE;
    u16									m_r_finger2 = BI_NONE;
	u16									m_eye_left = BI_NONE;
	u16									m_eye_right = BI_NONE;

	u16									m_l_clavicle = BI_NONE;
	u16									m_r_clavicle = BI_NONE;
	u16									m_spine2 = BI_NONE;
	u16									m_spine1 = BI_NONE;
	u16									m_spine = BI_NONE;
	u16									m_neck = BI_NONE;



	//////////////////////////////////////////////////////////////////////////
	// Network
	//////////////////////////////////////////////////////////////////////////
			void						ConvState			(u32 mstate_rl, string128 *buf);
public:
	bool						net_Spawn			( CSE_Abstract* DC) override;

	void						net_Export			( NET_Packet& P) override;				// export to server
	void						net_Import			( NET_Packet& P) override;				// import from server

	void						SyncRead(NET_Packet& Packet) override;
	void						SyncWrite(NET_Packet& Packet) override;

	void						net_Destroy			() override;
	bool						net_Relevant		() override;//	{ return getSVU() | getLocal(); };		// relevant for export to server
	void						net_Relcase			( CObject* O ) override;					//
	virtual void 				on_requested_spawn  (CObject *object);
	//object serialization
	void						save				(NET_Packet &output_packet) override;
	void						load				(IReader &input_packet) override;
	void						net_Save			(NET_Packet& P) override;
	bool						net_SaveRelevant	() override;
protected:
	xr_deque<net_update>	NET;
	Fvector					NET_SavedAccel;
	net_update				NET_Last;
	bool					NET_WasInterpolating;	// previous update was by interpolation or by extrapolation
	u32						NET_Time;				// server time of last update

	//---------------------------------------------
	void					net_Import_Base				( NET_Packet& P);
	void					net_Import_Physic			( NET_Packet& P);
	void					net_Import_Base_proceed		( );
	void					net_Import_Physic_proceed	( );
	//---------------------------------------------
	


////////////////////////////////////////////////////////////////////////////
	bool				can_validate_position_on_spawn	() override {return false;}
	///////////////////////////////////////////////////////
	// апдайт с данными физики
	xr_deque<net_update_A>	NET_A;
	
	//---------------------------------------------
//	bool					m_bHasUpdate;	
	/// spline coeff /////////////////////
	float			SCoeff[3][4];			//коэффициэнты для сплайна Бизье
	float			HCoeff[3][4];			//коэффициэнты для сплайна Эрмита
	Fvector			IPosS, IPosH, IPosL;	//положение актера после интерполяции Бизье, Эрмита, линейной

#ifdef DEBUG
	using VIS_POSITION = xr_deque<Fvector>;
	using VIS_POSITION_it = VIS_POSITION::iterator;
	
	VIS_POSITION	LastPosS;
	VIS_POSITION	LastPosH;
	VIS_POSITION	LastPosL;
#endif

	
	SPHNetState				LastState;
	SPHNetState				RecalculatedState;
	SPHNetState				PredictedState;
	
	InterpData				IStart;
	InterpData				IRec;
	InterpData				IEnd;
	
	bool					m_bInInterpolation;
	bool					m_bInterpolate;
	u32						m_dwIStartTime;
	u32						m_dwIEndTime;
	u32						m_dwILastUpdateTime;

	//---------------------------------------------
	using PH_STATES = xr_deque<SPHNetState>;
	using PH_STATES_it = PH_STATES::iterator;

	PH_STATES				m_States;
	u16						m_u16NumBones;
	void					net_ExportDeadBody		(NET_Packet &P);
	//---------------------------------------------
	void					CalculateInterpolationParams();
	//---------------------------------------------
	void			make_Interpolation () override;
#ifdef DEBUG
	//---------------------------------------------
	virtual void			OnRender_Network();
	//---------------------------------------------
#endif

// Igor	ref_geom 				hFriendlyIndicator;
	//////////////////////////////////////////////////////////////////////////
	// Actor physics
	//////////////////////////////////////////////////////////////////////////
public:
			void			g_Physics		(Fvector& accel, float jump, float dt);
	void			ForceTransform	(const Fmatrix &m) override;
			void			SetPhPosition	(const Fmatrix& pos);
	void			PH_B_CrPr		() override; // actions & operations before physic correction-prediction steps
	void			PH_I_CrPr		() override; // actions & operations after correction before prediction steps
	void			PH_A_CrPr		() override; // actions & operations after phisic correction-prediction steps
//	virtual void			UpdatePosStack	( u32 Time0, u32 Time1 );
	virtual void			MoveActor		(Fvector NewPos, Fvector NewDir);

	void			spawn_supplies			() override;

	bool			human_being				() const override
	{
		return				(true);
	}

	virtual	shared_str			GetDefaultVisualOutfit	() const	{return m_DefaultVisualOutfit;};
	virtual	void			SetDefaultVisualOutfit	(shared_str DefaultOutfit) {m_DefaultVisualOutfit = DefaultOutfit;};
	virtual void			UpdateAnimation			() 	{ g_SetAnimation(mstate_real); };

	virtual void			ChangeVisual			( shared_str NewVisual );
	void			OnChangeVisual			() override;

	virtual void			RenderIndicator			(Fvector dpos, float r1, float r2, const ui_shader &IndShader);
	virtual void			RenderText				(const char* Text, Fvector dpos, float* pdup, u32 color);
	void			RenderItemUI() override;

	//////////////////////////////////////////////////////////////////////////
	// Controlled Routines
	//////////////////////////////////////////////////////////////////////////

			void			set_input_external_handler			(CActorInputHandler *handler);
			bool			input_external_handler_installed	() const {return (m_input_external_handler != 0);}
			
	IC		void			lock_accel_for						(u32 time){m_time_lock_accel = Device.dwTimeGlobal + time;}

private:	
	CActorInputHandler		*m_input_external_handler;
	u32						m_time_lock_accel;

	/////////////////////////////////////////
	// DEBUG INFO
protected:
		CStatGraph				*pStatGraph;

		const char*					invincibility_fire_shield_3rd;
		const char*					invincibility_fire_shield_1st;
		shared_str				m_sHeadShotParticle;
		u32						last_hit_frame;
#ifdef DEBUG
		friend class CLevelGraph;
#endif
		Fvector							m_AutoPickUp_AABB;
		Fvector							m_AutoPickUp_AABB_Offset;

		void							Check_for_AutoPickUp			();
		void							SelectBestWeapon				(CObject* O);
public:
		shared_str				m_DefaultVisualOutfit;

		void							SetWeaponHideState				(u16 State, bool bSet);
private://IPhysicsShellHolder

	void	_BCL	HideAllWeapons					( bool v ) override { SetWeaponHideState(INV_STATE_BLOCK_ALL,v); }	

public:
		void							SetCantRunState					(bool bSet);
private:
	CActorCondition				*m_entity_condition;

	CNightVisionEffector*		m_night_vision;

protected:
	CEntityConditionSimple	*create_entity_condition	(CEntityConditionSimple* ec) override;

public:
	IC		CActorCondition		&conditions					() const;
	DLL_Pure			*_construct					() override;
	bool				natural_weapon				() const override {return false;}
	bool				natural_detector			() const override {return false;}
	bool				use_center_to_aim			() const override;
protected:
	ALife::_OBJECT_ID			m_iLastHitterID;
	ALife::_OBJECT_ID			m_iLastHittingWeaponID;
	s16							m_s16LastHittedElement;
	Fvector						m_vLastHitDir;
	Fvector						m_vLastHitPos;
	float						m_fLastHealth;
	bool						m_bWasHitted;
	bool						m_bWasBackStabbed;

	virtual		bool			Check_for_BackStab_Bone			(u16 element);
public:
	void				SetHitInfo						(CObject* who, CObject* weapon, s16 element, Fvector Pos, Fvector Dir) override;

	void				OnHitHealthLoss					(float NewHealth) override;
	void				OnCriticalHitHealthLoss			() override;
	void				OnCriticalWoundHealthLoss		() override;
	void				OnCriticalRadiationHealthLoss	() override;

	virtual	bool				InventoryAllowSprint			();
	virtual void				OnNextWeaponSlot				();
	virtual void				OnPrevWeaponSlot				();
			void				SwitchNightVision				();
			void				StartNVPPE						();
			void				SwitchTorch						();
			void				ClearMask						();
			void				ClearMaskCB						();
			void				MakeKick						();
			void				UpdatePickupMode				();
	CNightVisionEffector*		GetNightVisionEffector			() const { return m_night_vision;}

	CCustomDevice*				GetDevice						(bool in_slot = false);

#ifndef MASTER_GOLD
			void				NoClipFly						(int cmd);
			void				NoClipFlyGamepad				(int cmd);
			void				NoClipFlyStick					(Fvector2 val);
#endif //DEBUG

public:
	void				on_weapon_shot_start			(CWeapon *weapon) override;
	void				on_weapon_shot_update			() override;
	void				on_weapon_shot_stop				() override;
	void				on_weapon_shot_remove			(CWeapon *weapon) override;
	void				on_weapon_hide					(CWeapon *weapon) override;
			Fvector				weapon_recoil_delta_angle		();
			Fvector				weapon_recoil_last_delta		();
protected:
	virtual	void				update_camera					(CCameraShotEffector* effector);
	//step manager
	bool				is_on_ground					() override;

private:
	CActorMemory				*m_memory;

public:
	IC		CActorMemory		&memory							() const {VERIFY(m_memory); return(*m_memory); };

	void						OnDifficultyChanged				();

	IC float					HitProbability					() const {return m_hit_probability;}
	CVisualMemoryManager*visual_memory					() const override;

	bool				BonePassBullet					(u16 boneID) final override;
	void				On_B_NotCurrentEntity			() override;

private:
	xr_vector<ISpatialShared>		ISpatialResult;

private:
	CLocationManager				*m_location_manager;

public:
	IC		const CLocationManager	&locations					() const
	{
		VERIFY						(m_location_manager);
		return						(*m_location_manager);
	}

private:
	ALife::_OBJECT_ID	m_holder_id;

public:
	bool				register_schedule				() const override {return false;}
	bool				is_ai_obstacle					() const override;
	
			float				GetRestoreSpeed					(ALife::EConditionRestoreType const& type);

public:
	void			On_SetEntity() override;
	void			On_LostEntity() override {};

			void			DisableHitMarks(bool disable)		{m_disabled_hitmarks = disable;};
			bool			DisableHitMarks() const {return m_disabled_hitmarks;};

			void			set_inventory_disabled (bool is_disabled) { m_inventory_disabled = is_disabled; }
			bool			inventory_disabled () const { return m_inventory_disabled; }
			void			set_pda_disabled(bool is_disabled) { m_pda_disabled = is_disabled; }
			bool			pda_disabled() const { return m_pda_disabled; }
			void			set_use_disabled(bool is_disabled) { m_use_disabled = is_disabled; }
	IInputReceiver* GetIIR() override { return this; }
private:
			void			set_state_box(u32	mstate);
private:
	bool					m_disabled_hitmarks;
	bool					m_inventory_disabled;
	bool					m_pda_disabled;
	bool					m_use_disabled;
//static CPhysicsShell		*actor_camera_shell;

	DECLARE_SCRIPT_REGISTER_FUNCTION

private:
	CScriptGameObject* m_pBestEnemy = nullptr;
	xr_vector<const char*> m_burn_restore_materials{};
	float m_burn_restore_material_speed = 0.0f;
	float m_actor_burn_restore_speed = 0.0f;
	float GetMaterialBurnRestoreSpeed(const char* mtl);
	bool m_need_fire_particle = false;
	Fvector2 leftStickThreshold;

	bool isGamepadShooting = false;
	bool isGamepadZooming = false;
public:
	virtual void SetActorSleepiness(const float value);
	virtual void SetActorSatiety(const float value);
	virtual void SetActorThirst(const float value);
	virtual void SetActorHealth(const float value);
	virtual void SetActorPower(const float value);
	virtual void SetActorRadiation(const float value);
	virtual void SetActorPsyHealth(const float value);
	virtual void SetActorMorale(const float value);

	void SetBestEnemy(CScriptGameObject* enemy);
	CScriptGameObject* GetBestEnemy();

	bool OnLadder = false;
	IC bool is_ladder() const { return OnLadder; };

	float fSprintFactor = 0.f;
	float fSprintFactorIncreaseFactor = 4.f;
	float fSprintFactorDecreaseFactor = 8.f;
	float m_SprintFovFactor = 3.f;

	bool IsActorBurning();
};

extern bool		isActorAccelerated			(u32 mstate, bool ZoomMode);

IC	CActorCondition	&CActor::conditions	() const{ VERIFY(m_entity_condition); return(*m_entity_condition);}

extern Fvector      g_start_position;
extern int          g_start_game_vertex_id;
extern shared_str   g_start_position_smart;
extern CActor*		g_actor;
extern CActor*		g_actor_single;
CActor*				Actor		();
extern const float	s_fFallTime;
