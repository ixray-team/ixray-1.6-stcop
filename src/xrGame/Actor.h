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
#include "Wristwatch/WristwatchController.h"

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
	virtual								~CActor				();
	void OnFrame();
	THudVertexAssignedPatricles* GetHudVertexAssignedPatriclesComponent();

									public:
	virtual bool						AlwaysTheCrow				()						{ return TRUE; }

	virtual CAttachmentOwner*			cast_attachment_owner		() override						{return this;}
	virtual CInventoryOwner*			cast_inventory_owner		() override						{return this;}
	virtual CActor*						cast_actor					() override						{return this;}
	virtual CGameObject*				cast_game_object			() override						{return this;}
	virtual IInputReceiver*				cast_input_receiver			() override					{return this;}
	virtual CEntityAlive*				cast_entity_alive			() override					{return this;}
	virtual CEntity*					cast_entity					() override					{return this;}
	virtual CPhraseDialogManager*		cast_phrase_dialog_manager	() override					{return this;}
	virtual	CCharacterPhysicsSupport*	character_physics_support	() override					{return m_pPhysics_support;}
	virtual	CCharacterPhysicsSupport*	character_physics_support	() const				{return m_pPhysics_support;}
	virtual CPHDestroyable*				ph_destroyable				()						;
			CHolderCustom*				Holder						()						{return m_holder;}

	virtual xr_vector<xr_string>		GetKnownPortions() const ;
	virtual xr_vector<xr_string>		GetKnownPortionDialogs(shared_str id) const ;
	virtual xr_vector<xr_string>		GetKnownPortionDisable(shared_str id) const;
	virtual xr_vector<xr_string>		GetKnownPortionArticles(shared_str id) const;
	virtual xr_vector<xr_string>		GetKnownPortionArticlesDisable(shared_str id) const;
	virtual xr_vector<xr_string>		GetKnownPortionTasks(shared_str id) const;
	virtual void						Load				( const char* section );

	virtual void						shedule_Update		( u32 T );
	void PlayRainOnHelmetSound();
	virtual void						UpdateCL			( );
	void draw_electrical_fur();
			void						UpdateLensFOV		(CWeapon* wpn, float value);
	void CheckFlyhack();
			void						UpdatePlayerView	( );

	virtual void						OnEvent				( NET_Packet& P, u16 type		);

	void								Center(Fvector& C)	const;

	// Render
	virtual void						renderable_Render			();
	virtual bool						renderable_ShadowGenerate	();
	virtual	void						feel_sound_new				(CObject* who, int type, CSound_UserDataPtr user_data, const Fvector& position, float power);
	virtual	Feel::Sound*				dcast_FeelSound				()	{ return this;	}
			float						m_snd_noise;
#ifdef DEBUG_DRAW
	virtual void						OnRender			();

#endif


public:
	virtual bool OnReceiveInfo		(shared_str info_id) const;
	virtual void OnDisableInfo		(shared_str info_id) const;

	virtual void	 NewPdaContact		(CInventoryOwner*);
	virtual void	 LostPdaContact		(CInventoryOwner*);

	virtual void GiveInfoPortion(const char* infoPortion) override;
	virtual void DisableInfoPortion(const char* info_id) override;
	virtual void SetActorPosition(Fvector pos) override;
	virtual void SetActorDirection(float dir) override;

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
	CActorStatisticMgr&				StatisticMgr()	{return *m_statistic_manager;}
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

	virtual const char*	Name        () const {return CInventoryOwner::Name();}

public:
	//PhraseDialogManager
	virtual void ReceivePhrase				(DIALOG_SHARED_PTR& phrase_dialog);
	virtual void UpdateAvailableDialogs		(CPhraseDialogManager* partner);
	virtual void TryToTalk					();
			bool OnDialogSoundHandlerStart	(CInventoryOwner *inv_owner, const char* phrase);
			bool OnDialogSoundHandlerStop	(CInventoryOwner *inv_owner);


	virtual void reinit			();
	virtual void reload			(const char* section);
	virtual bool use_bolts		() const;

	virtual void OnItemTake		(CInventoryItem *inventory_item);
	void OnItemTakeFromGround(CInventoryItem* inventory_item);

	virtual void OnItemRuck		(CInventoryItem *inventory_item, const SInvItemPlace& previous_place);
	virtual void OnItemBelt		(CInventoryItem *inventory_item, const SInvItemPlace& previous_place);

	virtual void OnItemDrop		(CInventoryItem *inventory_item, bool just_before_destroy);
	virtual void OnItemDropUpdate ();

	virtual	void OnPlayHeadShotParticle (NET_Packet P);

	virtual void						Die				(CObject* who);
	virtual	void						Hit				(SHit* pHDS);
	virtual	void						PHHit			(SHit &H);
	virtual float						CalcHitDamage	(SHit* pHDS);
	virtual void						HitSignal		(float P, Fvector &vLocalDir,	CObject* who, s16 element);
			void						HitSector		(CObject* who, CObject* weapon);
			void						HitMark			(float P, Fvector dir,			CObject* who, s16 element, Fvector position_in_bone_space, float impulse,  ALife::EHitType hit_type);
	virtual void						FootStepCallback(float power, bool b_play, bool b_on_ground, bool b_hud_view);

			void						Feel_Grenade_Update( float rad );

	virtual float						GetMass				() ;
	virtual float						Radius				() const;
	virtual void						g_PerformDrop		();

	virtual	bool						use_default_throw_force	();
	virtual	float						missile_throw_force		();

	virtual bool						unlimited_ammo			();
	virtual bool						infinite_fire();
	virtual bool						NeedToDestroyObject()  const;
	virtual ALife::_TIME_ID				TimePassedAfterDeath() const;

	CPickUpManager* GetPickupManager() { return pPickup; }

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
	s32						GetZoomRndSeed			()	{ return m_ZoomRndSeed;	};
	void					SetShotRndSeed			(s32 Seed = 0);
	s32						GetShotRndSeed			()	{ return m_ShotRndSeed;	};

	CHudAnimatorManager*	HudAnimator()			{ return m_hud_animator; }
	void					StartAnimator			(const shared_str& section);

public:
	void					detach_Vehicle			();
	void					steer_Vehicle			(float angle);
	void					attach_Vehicle			(CHolderCustom* vehicle);
	bool					use_HolderEx			(CHolderCustom* object, bool bForce);

	virtual bool			can_attach				(const CInventoryItem *inventory_item) const;

	virtual void UpdatePlayerHud() final override;

protected:
	CHolderCustom*			m_holder;
	u16						m_holderID;
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

	virtual const SRotation	Orientation			()	const	{ return r_torso; };
	SRotation				&Orientation		()			 { return r_torso; };

	void					g_SetAnimation		(u32 mstate_rl);
	void					g_SetSprintAnimation(u32 mstate_rl,MotionID &head,MotionID &torso,MotionID &legs);
public:
	virtual void			OnHUDDraw			(CCustomHUD* hud);
			bool			HUDview				( )const ;

	//visiblity 
	virtual	float			ffGetFov			()	const	{ return 90.f;		}	
	virtual	float			ffGetRange			()	const	{ return 500.f;		}

	IC bool IsSafemode() const { return m_bIsSafemode; }
	IC void SetSafemodeStatus(bool status) { m_bIsSafemode = status; }

public:
	bool					HasCameraEffector	() const { return m_pActorEffector != nullptr; };
	CActorCameraManager&	Cameras				() 	{VERIFY(HasCameraEffector()); return *m_pActorEffector;}
	virtual CCameraBase*	cam_Active			() override	{return cameras[cam_active];}
	IC CCameraBase*			cam_FirstEye		()	{return cameras[eacFirstEye];}
	IC EActorCameras active_cam() { return cam_active; }
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

	virtual void			feel_touch_new				(CObject* O);
	virtual void			feel_touch_delete			(CObject* O);
	virtual bool			feel_touch_contact			(CObject* O);
	virtual bool			feel_touch_on_contact		(CObject* O);

	CGameObject*			ObjectWeLookingAt			() {return m_pObjectWeLookingAt;}
	CInventoryOwner*		PersonWeLookingAt			() {return m_pPersonWeLookingAt;}
	const char*					GetDefaultActionForObject	() {return *m_sDefaultObjAction;}
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
	virtual void			StopAnyMove				() override;

	bool					AnyAction				()	{return (mstate_real & mcAnyAction) != 0;};
	bool					AnyMove					()	{return (mstate_real & mcAnyMove) != 0;};

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
	Fvector					GetMovementSpeed		() {return NET_SavedAccel;};
	//////////////////////////////////////////////////////////////////////////
	// User input/output
	//////////////////////////////////////////////////////////////////////////
public:
	virtual void			IR_OnMouseMove			(int x, int y);
	virtual void			IR_GamepadUpdateStick	(int id, Fvector2 value);
	virtual void			IR_GamepadKeyPress		(int id);
    virtual void			IR_GamepadKeyRelease    (int id);
	virtual void			IR_GamepadKeyHold		(int id);
	virtual void			IR_OnGyroscopeMove		(Fvector3 value);
	virtual void			IR_OnTouchpadMove		(Fvector2 value);
	virtual void			IR_OnKeyboardPress		(int dik);
	virtual void			IR_OnKeyboardRelease	(int dik);
	virtual void			IR_OnKeyboardHold		(int dik);
	virtual void			IR_OnMouseWheel			(int direction);
	virtual	float			GetLookFactor			();
	bool					IsActionKeyPressedInGame(const EGameActions& EGameAction) const;
	void					SetActorKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags mask, bool state, bool ignore_suicide = false);
	void					ProcessKeys(CHudItem* itm = nullptr);

public:
	virtual void						g_WeaponBones		(u16 &L, u16 &R1, u16 &R2) final override;
	virtual void						g_fireParams		(const CHudItem* pHudItem, Fvector& P, Fvector& D) final override;
	virtual bool						g_stateFire			() {return ! ((mstate_wishful & mcLookout) && !IsGameTypeSingle() );}

	virtual bool						g_State				(SEntityState& state) const;
	virtual	float						GetWeaponAccuracy	() const;
	virtual	float						GetAgility() const;
			float						GetFireDispertion	() const {return m_fdisp_controller.GetCurrentDispertion();}
			bool						IsZoomAimingMode	() const {return m_bZoomAimingMode;}
	virtual float						MaxCarryWeight		() const;
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
	virtual bool						net_Spawn			( CSE_Abstract* DC);

	virtual void						net_Export			( NET_Packet& P);				// export to server
	virtual void						net_Import			( NET_Packet& P);				// import from server

	virtual void						SyncRead(NET_Packet& Packet);
	virtual void						SyncWrite(NET_Packet& Packet);

	virtual void						net_Destroy			();
	virtual bool						net_Relevant		();//	{ return getSVU() | getLocal(); };		// relevant for export to server
	virtual	void						net_Relcase			( CObject* O );					//
	virtual void 				on_requested_spawn  (CObject *object);
	//object serialization
	virtual void						save				(NET_Packet &output_packet);
	virtual void						load				(IReader &input_packet);
	virtual void						net_Save			(NET_Packet& P)																	;
	virtual	bool						net_SaveRelevant	()																				;
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
virtual	bool				can_validate_position_on_spawn	(){return false;}
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
	virtual void			make_Interpolation ();
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
	virtual void			ForceTransform	(const Fmatrix &m);
			void			SetPhPosition	(const Fmatrix& pos);
	virtual void			PH_B_CrPr		(); // actions & operations before physic correction-prediction steps
	virtual void			PH_I_CrPr		(); // actions & operations after correction before prediction steps
	virtual void			PH_A_CrPr		(); // actions & operations after phisic correction-prediction steps
//	virtual void			UpdatePosStack	( u32 Time0, u32 Time1 );
	virtual void			MoveActor		(Fvector NewPos, Fvector NewDir);

	virtual	void			spawn_supplies			();
	virtual bool			human_being				() const
	{
		return				(true);
	}

	virtual	shared_str			GetDefaultVisualOutfit	() const	{return m_DefaultVisualOutfit;};
	virtual	void			SetDefaultVisualOutfit	(shared_str DefaultOutfit) {m_DefaultVisualOutfit = DefaultOutfit;};
	virtual void			UpdateAnimation			() 	{ g_SetAnimation(mstate_real); };

	virtual void			ChangeVisual			( shared_str NewVisual );
	virtual void			OnChangeVisual			();

	virtual void			RenderIndicator			(Fvector dpos, float r1, float r2, const ui_shader &IndShader);
	virtual void			RenderText				(const char* Text, Fvector dpos, float* pdup, u32 color);
	virtual void			RenderItemUI();

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

virtual	 void	_BCL	HideAllWeapons					( bool v ){ SetWeaponHideState(INV_STATE_BLOCK_ALL,v); }	

public:
		void							SetCantRunState					(bool bSet);
private:
	CActorCondition				*m_entity_condition;

	CNightVisionEffector*		m_night_vision;

protected:
	virtual	CEntityConditionSimple	*create_entity_condition	(CEntityConditionSimple* ec);

public:
	IC		CActorCondition		&conditions					() const;
	virtual DLL_Pure			*_construct					();
	virtual bool				natural_weapon				() const {return false;}
	virtual bool				natural_detector			() const {return false;}
	virtual bool				use_center_to_aim			() const;
protected:
	u16							m_iLastHitterID;
	u16							m_iLastHittingWeaponID;
	s16							m_s16LastHittedElement;
	Fvector						m_vLastHitDir;
	Fvector						m_vLastHitPos;
	float						m_fLastHealth;
	bool						m_bWasHitted;
	bool						m_bWasBackStabbed;

	virtual		bool			Check_for_BackStab_Bone			(u16 element);
public:
	virtual void				SetHitInfo						(CObject* who, CObject* weapon, s16 element, Fvector Pos, Fvector Dir);

	virtual	void				OnHitHealthLoss					(float NewHealth);	
	virtual	void				OnCriticalHitHealthLoss			();
	virtual	void				OnCriticalWoundHealthLoss		();
	virtual void				OnCriticalRadiationHealthLoss	();

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
	CNightVisionEffector*		GetNightVisionEffector			() { return m_night_vision;}

	CCustomDevice*				GetDevice						(bool in_slot = false);

#ifndef MASTER_GOLD
			void				NoClipFly						(int cmd);
			void				NoClipFlyGamepad				(int cmd);
			void				NoClipFlyStick					(Fvector2 val);
#endif //DEBUG

public:
	
	virtual	void				on_weapon_shot_start			(CWeapon *weapon);
	virtual	void				on_weapon_shot_update			();
	virtual	void				on_weapon_shot_stop				();
	virtual	void				on_weapon_shot_remove			(CWeapon *weapon);
	virtual	void				on_weapon_hide					(CWeapon *weapon);
			Fvector				weapon_recoil_delta_angle		();
			Fvector				weapon_recoil_last_delta		();
protected:
	virtual	void				update_camera					(CCameraShotEffector* effector);
	//step manager
	virtual bool				is_on_ground					();

private:
	CActorMemory				*m_memory;

public:
	IC		CActorMemory		&memory							() const {VERIFY(m_memory); return(*m_memory); };

	void						OnDifficultyChanged				();

	IC float					HitProbability					() {return m_hit_probability;}
	virtual	CVisualMemoryManager*visual_memory					() const;

	virtual	bool				BonePassBullet					(u16 boneID) final override;
	virtual	void				On_B_NotCurrentEntity			();

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
	virtual bool				register_schedule				() const {return false;}
	virtual	bool				is_ai_obstacle					() const;
	
			float				GetRestoreSpeed					(ALife::EConditionRestoreType const& type);

public:
	virtual void			On_SetEntity();
	virtual void			On_LostEntity() {};

			void			DisableHitMarks(bool disable)		{m_disabled_hitmarks = disable;};
			bool			DisableHitMarks()					{return m_disabled_hitmarks;};

			void			set_inventory_disabled (bool is_disabled) { m_inventory_disabled = is_disabled; }
			bool			inventory_disabled () const { return m_inventory_disabled; }
			void			set_pda_disabled(bool is_disabled) { m_pda_disabled = is_disabled; }
			bool			pda_disabled() const { return m_pda_disabled; }
			void			set_use_disabled(bool is_disabled) { m_use_disabled = is_disabled; }
			virtual IInputReceiver* GetIIR() override { return this; }
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
	CWristwatchController _wristwatchController;
	Fvector2 leftStickThreshold;

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

	float fSprintFactor = 0;
	float m_SprintFovFactor = 7.0f;
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
