#pragma once

#include "../xrEngine/feel_touch.h"
#include "../xrEngine/feel_sound.h"
#include "../xrEngine/IInputReceiver.h"
#include "../xrEngine/IGame_Actor.h"
#include "../Include/xrRender/KinematicsAnimated.h"
#include "actor_flags.h"
#include "actor_defs.h"
#include "fire_disp_controller.h"
#include "entity_alive.h"
#include "PHMovementControl.h"
#include "../xrPhysics/PhysicsShell.h"
#include "InventoryOwner.h"
#include "../xrEngine/StatGraph.h"
#include "PhraseDialogManager.h"
#include "ui_defs.h"

#include "step_manager.h"
#include "../xrScripts/script_export_space.h"
#include "HudItem.h"
#include "CustomDetector.h"
#include "player_hud.h"

using namespace ACTOR_DEFS;

class CInfoPortion;
struct GAME_NEWS_DATA;
class CActorCondition;
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

struct SShootingEffector;
struct SSleepEffector;
class  CSleepEffectorPP;
class CInventoryBox;

class	CHudItem;
class   CArtefact;

struct SActorMotions;
struct SActorVehicleAnims;
class  CActorCondition;
class SndShockEffector;
class CActorFollowerMngr;

struct CameraRecoil;
class CCameraShotEffector;
class CActorInputHandler;

class CActorMemory;
class CActorStatisticMgr;

class CLocationManager;
class CPickUpManager;
class CCustomDetector;
class CController;

class	CActor: 
	public IGame_Actor, 
	public CEntityAlive, 
	public IInputReceiver,
	public Feel::Touch,
	public CInventoryOwner,
	public CPhraseDialogManager,
	public CStepManager,
	public Feel::Sound
#ifdef DEBUG_DRAW
	,public pureRender
#endif
{
	friend class CActorCondition;
private:
	typedef CEntityAlive	inherited;
	CPickUpManager* pPickup = nullptr;
public:
										CActor				();
	virtual								~CActor				();

public:
	virtual BOOL						AlwaysTheCrow				()						{ return TRUE; }

	virtual CAttachmentOwner*			cast_attachment_owner		()						{return this;}
	virtual CInventoryOwner*			cast_inventory_owner		()						{return this;}
	virtual CActor*						cast_actor					()						{return this;}
	virtual CGameObject*				cast_game_object			()						{return this;}
	virtual IInputReceiver*				cast_input_receiver			()						{return this;}
	virtual	CCharacterPhysicsSupport*	character_physics_support	()						{return m_pPhysics_support;}
	virtual	CCharacterPhysicsSupport*	character_physics_support	() const				{return m_pPhysics_support;}
	virtual CPHDestroyable*				ph_destroyable				()						;
			CHolderCustom*				Holder						()						{return m_holder;}
public:
	virtual xr_vector<xr_string>		GetKnowedPortions() const ;
	virtual void						Load				( LPCSTR section );

	virtual void						shedule_Update		( u32 T ); 
	virtual void						UpdateCL			( );
			void						UpdatePlayerView	( );
	
	virtual void						OnEvent				( NET_Packet& P, u16 type		);

	// Render
	virtual void						renderable_Render			();
	virtual BOOL						renderable_ShadowGenerate	();
	virtual	void						feel_sound_new				(CObject* who, int type, CSound_UserDataPtr user_data, const Fvector& Position, float power);
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
	CGameNewsRegistryWrapper		*game_news_registry;
	CCharacterPhysicsSupport		*m_pPhysics_support;

	virtual LPCSTR	Name        () const {return CInventoryOwner::Name();}

public:
	//PhraseDialogManager
	virtual void ReceivePhrase				(DIALOG_SHARED_PTR& phrase_dialog);
	virtual void UpdateAvailableDialogs		(CPhraseDialogManager* partner);
	virtual void TryToTalk					();
			bool OnDialogSoundHandlerStart	(CInventoryOwner *inv_owner, LPCSTR phrase);
			bool OnDialogSoundHandlerStop	(CInventoryOwner *inv_owner);


	virtual void reinit			();
	virtual void reload			(LPCSTR section);
	virtual bool use_bolts		() const;

	virtual void OnItemTake		(CInventoryItem *inventory_item);
	
	virtual void OnItemRuck		(CInventoryItem *inventory_item, const SInvItemPlace& previous_place);
	virtual void OnItemBelt		(CInventoryItem *inventory_item, const SInvItemPlace& previous_place);
	
	virtual void OnItemDrop		(CInventoryItem *inventory_item, bool just_before_destroy);
	virtual void OnItemDropUpdate ();

	virtual	void OnPlayHeadShotParticle (NET_Packet P);
	void legs_shift_callback(CBoneInstance* K);

	virtual void						Die				(CObject* who);
	virtual	void						Hit				(SHit* pHDS);
	virtual	void						PHHit			(SHit &H);
	virtual void						HitSignal		(float P, Fvector &vLocalDir,	CObject* who, s16 element);
			void						HitSector		(CObject* who, CObject* weapon);
			void						HitMark			(float P, Fvector dir,			CObject* who, s16 element, Fvector position_in_bone_space, float impulse,  ALife::EHitType hit_type);

			void						Feel_Grenade_Update( float rad );

	virtual float						GetMass				() ;
	virtual float						Radius				() const;
	virtual void						g_PerformDrop		();
	
	virtual	bool						use_default_throw_force	();
	virtual	float						missile_throw_force		(); 

	virtual bool						unlimited_ammo			();

	virtual bool						NeedToDestroyObject()  const;
	virtual ALife::_TIME_ID				TimePassedAfterDeath() const;

	struct controller_input_correction_params
	{
		bool active = false;
		float rotate_angle = 0.0f;
		float sense_scaler_x = 1.0f;
		float sense_scaler_y = 1.0f;
		bool reverse_axis_y = false;
	};

	struct controller_input_random_offset
	{
		int offset_x = 0;
		int offset_y = 0;
	};

	struct controller_psiunblock_params
	{
		float min_dist = 0.0f;
		float min_dist_prob = 0.0f;
		float max_dist = 0.0f;
		float max_dist_prob = 0.0f;

	};

	struct controller_mouse_control_params
	{
		float min_sense_scale = 0.0f;
		float max_sense_scale = 0.0f;
		int min_offset = 0;
		int max_offset = 0;
		float keyboard_move_k = 0.0f;
	};

	u32 _controlled_time_remains;
	bool _suicide_now;
	bool _planning_suicide;
	u32 _lastshot_done_time;
	bool _death_action_started;
	bool _inventory_disabled_set;
	u32 _controller_preparing_starttime;
	xr_vector<CController*> _active_controllers;
	u32 _last_update_time;
	u32 _jitter_time_remains;

	bool _psi_block_failed;
	controller_input_correction_params _input_correction;

	controller_psiunblock_params _controller_psiunblock_params;
	controller_mouse_control_params _controller_mouse_control_params;

	float DistToSelectedContr(CController* controller);
	float DistToContr();
	void UpdatePsiBlockFailedState(CController* monster_controller);
	controller_psiunblock_params GetControllerPsiUnblockProb();
	controller_mouse_control_params GetControllerMouseControlParams();
	void ChangeInputRotateAngle();
	controller_input_correction_params GetCurrentControllerInputCorrectionParams();
	controller_input_random_offset GetControllerInputRandomOffset();
	bool IsControllerPreparing() const;
	float GetCurrentSuicideWalkKoef() const;
	void AddActiveController(CController* monster_controller);
	void ClearActiveControllers();

	bool IsPsiBlockFailed() const { return _psi_block_failed; }
	bool IsPsiBlocked() const;
	bool IsActorPlanningSuicide() const { return _planning_suicide; }
	bool IsActorSuicideNow() const { return _suicide_now; }
	bool IsActorControlled() const { return _controlled_time_remains > 0; }
	bool IsSuicideInreversible() const { return _lastshot_done_time > 0 || _death_action_started; }
	bool IsHandJitter(CHudItemObject* itm) const;
	float GetHandJitterScale(CHudItemObject* itm) const;
	void SetHandsJitterTime(u32 time) { _jitter_time_remains = time; }
	void ResetActorControl();
	bool IsControllerSeeActor(CController* monster_controller);
	bool CanUseItemForSuicide(CHudItemObject* item);
	void UpdateSuicide(u32 dt);
	void DoSuicideShot();
	bool CheckActorVisibilityForController();
	void OnSuicideAnimEnd();
	void NotifySuicideShotCallbackIfNeeded() const;
	void NotifySuicideStopCallbackIfNeeded() const;
	void UpdateFOV();

	cached_cfg_param_float cached_fov_factor;
	cached_cfg_param_float cached_hud_fov_factor_wpn;
	cached_cfg_param_float cached_hud_fov_factor_scope;
	cached_cfg_param_float cached_hud_fov_gl_zoom_factor;
	cached_cfg_param_float cached_hud_fov_zoom_factor;
	//player_hud::cached_cfg_param_float cached_hud_fov_alter_zoom_factor;

public:

	//свойства артефактов
	virtual void		UpdateArtefactsOnBeltAndOutfit();
			float		HitArtefactsOnBelt		(float hit_power, ALife::EHitType hit_type);
			float		GetProtection_ArtefactsOnBelt(ALife::EHitType hit_type);

protected:
	//звук тяжелого дыхания
	ref_sound			m_HeavyBreathSnd;
	ref_sound			m_BloodSnd;
	ref_sound			m_DangerSnd;

protected:
	// Death
	float					m_hit_slowmo;
	float					m_hit_probability;
	s8						m_block_sprint_counter;

	// media
	SndShockEffector*		m_sndShockEffector;
	xr_vector<ref_sound>	sndHit[ALife::eHitTypeMax];
	ref_sound				sndDie[SND_DIE_COUNT];


	float					m_fLandingTime;
	float					m_fJumpTime;
	float					m_fFallTime;
	float					m_fCamHeightFactor;

	// Dropping
	BOOL					b_DropActivated;
	float					f_DropPower;

	//random seed для Zoom mode
	s32						m_ZoomRndSeed;
	//random seed для Weapon Effector Shot
	s32						m_ShotRndSeed;

	bool					m_bOutBorder;
	//сохраняет счетчик объектов в feel_touch, для которых необходимо обновлять размер колижена с актером 
	u32						m_feel_touch_characters;
private:
	void					SwitchOutBorder(bool new_border_state);
public:
	bool					m_bAllowDeathRemove;
	float					m_fLegs_shift;

	void					SetZoomRndSeed			(s32 Seed = 0);
	s32						GetZoomRndSeed			()	{ return m_ZoomRndSeed;	};
	void					SetShotRndSeed			(s32 Seed = 0);
	s32						GetShotRndSeed			()	{ return m_ShotRndSeed;	};

public:
	void					detach_Vehicle			();
	void					steer_Vehicle			(float angle);
	void					attach_Vehicle			(CHolderCustom* vehicle);

	virtual bool			can_attach				(const CInventoryItem *inventory_item) const;
protected:
	CHolderCustom*			m_holder;
	u16						m_holderID;
	bool					use_Holder				(CHolderCustom* holder);

	bool					use_Vehicle				(CHolderCustom* object);
	bool					use_MountedWeapon		(CHolderCustom* object);
	void					ActorUse				();

protected:
	BOOL					m_bAnimTorsoPlayed;
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
			BOOL			HUDview				( )const ;

	//visiblity 
	virtual	float			ffGetFov			()	const	{ return 90.f;		}	
	virtual	float			ffGetRange			()	const	{ return 500.f;		}

	
public:
	CActorCameraManager&	Cameras				() 	{VERIFY(m_pActorEffector); return *m_pActorEffector;}
	IC CCameraBase*			cam_Active			()	{return cameras[cam_active];}
	IC CCameraBase*			cam_FirstEye		()	{return cameras[eacFirstEye];}
	IC EActorCameras active_cam() { return cam_active; }
	virtual void cam_Set(EActorCameras style);

protected:
	void					CorrectActorCameraHeight(float& h);
	void					cam_Update				(float dt, float fFOV);
	void					cam_Lookout				( const Fmatrix &xform, float camera_height );
	void					camUpdateLadder			(float dt);
	void					cam_SetLadder			();
	void					cam_UnsetLadder			();
	float					currentFOV				();

	// Cameras
	CCameraBase*			cameras[eacMaxCam];
	EActorCameras			cam_active;
	float					fPrevCamPos;
	float					current_ik_cam_shift;
	Fvector					vPrevCamDir;
	float					fCurAVelocity;
	CEffectorBobbing*		pCamBobbing;

	float _last_camera_height;
	u32 _last_cam_update_time;
	u32 _landing_effect_time_remains;
	u32 _landing2_effect_time_remains;
	u32 _landing_effect_finish_time_remains;

	//менеджер эффекторов, есть у каждого актрера
	CActorCameraManager*	m_pActorEffector;
	static float			f_Ladder_cam_limit;
public:
	//--#SM+#--
	float fFPCamYawMagnitude;
	float fFPCamPitchMagnitude;

	virtual void			feel_touch_new				(CObject* O);
	virtual void			feel_touch_delete			(CObject* O);
	virtual BOOL			feel_touch_contact			(CObject* O);
	virtual BOOL			feel_touch_on_contact		(CObject* O);

	CGameObject*			ObjectWeLookingAt			() {return m_pObjectWeLookingAt;}
	CInventoryOwner*		PersonWeLookingAt			() {return m_pPersonWeLookingAt;}
	LPCSTR					GetDefaultActionForObject	() {return *m_sDefaultObjAction;}
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
	void					LookoutFunctionReplace	(float& cur_roll, float tgt_roll, float dt);
	bool					g_LadderOrient			() ;
//	void					UpdateMotionIcon		(u32 mstate_rl);

	void					SetMovementState		(const ACTOR_DEFS::EMovementStates& state, const ACTOR_DEFS::EMoveCommand& mask, bool status);
	u32						GetMovementState		(const ACTOR_DEFS::EMovementStates& state) const;

	bool					CanAccelerate			();
	bool					CanJump					();
	bool					CanMove					();
	float					CameraHeight			();
	float					CurrentHeight; // Alex ADD: for smooth crouch
	bool					CanSprint				();
	bool					CanRun					();
	void					StopAnyMove				();

	bool					AnyAction				()	{return (mstate_real & mcAnyAction) != 0;};
	bool					AnyMove					()	{return (mstate_real & mcAnyMove) != 0;};

	bool					is_jump					();
protected:
	u32						mstate_wishful;
	u32						mstate_old;
	u32						mstate_real;

	BOOL					m_bJumpKeyPressed;

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
	u32						_keyflags;

public:
	Fvector					GetMovementSpeed		() {return NET_SavedAccel;};
	//////////////////////////////////////////////////////////////////////////
	// User input/output
	//////////////////////////////////////////////////////////////////////////
public:
			void			IR_OnMouseMove_CorrectMouseSense(int& p_dx, int& p_dy, float& sense);
	virtual void			IR_OnMouseMove			(int x, int y);
	virtual void			IR_GamepadUpdateStick	(int id, Fvector2 value);
	virtual void			IR_GamepadKeyPress(int id);
	virtual void			IR_OnKeyboardPress		(int dik);
	virtual void			IR_OnKeyboardRelease	(int dik);
	virtual void			IR_OnKeyboardHold		(int dik);
	virtual void			IR_OnMouseWheel			(int direction);
	virtual	float			GetLookFactor			();
	void					SetActorKeyRepeatFlag(ACTOR_DEFS::EActorKeyflags mask, bool state, bool ignore_suicide = false);
	u32						GetActorKeyRepeatFlag() const { return _keyflags; }
	void					ProcessKeys				();

public:
	virtual void						g_WeaponBones		(int &L, int &R1, int &R2);
	virtual void						g_fireParams		(const CHudItem* pHudItem, Fvector& P, Fvector& D);
	virtual bool						g_stateFire			() {return ! ((mstate_wishful & mcLookout) && !IsGameTypeSingle() );}

	virtual BOOL						g_State				(SEntityState& state) const;
	virtual	float						GetWeaponAccuracy	() const;
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

protected:
	//косточки используемые при стрельбе
	int									m_r_hand;
	int									m_l_finger1;
    int									m_r_finger2;
	int									m_eye_left;
	int									m_eye_right;

	int									m_l_clavicle;
	int									m_r_clavicle;
	int									m_spine2;
	int									m_spine1;
	int									m_spine;
	int									m_neck;



	//////////////////////////////////////////////////////////////////////////
	// Network
	//////////////////////////////////////////////////////////////////////////
			void						ConvState			(u32 mstate_rl, string128 *buf);
public:
	virtual BOOL						net_Spawn			( CSE_Abstract* DC);
	virtual void						net_Export			( NET_Packet& P);				// export to server
	virtual void						net_Import			( NET_Packet& P);				// import from server
	virtual void						net_Destroy			();
	virtual BOOL						net_Relevant		();//	{ return getSVU() | getLocal(); };		// relevant for export to server
	virtual	void						net_Relcase			( CObject* O );					//
	virtual void 				on_requested_spawn  (CObject *object);
	//object serialization
	virtual void						save				(NET_Packet &output_packet);
	virtual void						load				(IReader &input_packet);
	virtual void						net_Save			(NET_Packet& P)																	;
	virtual	BOOL						net_SaveRelevant	()																				;
protected:
	xr_deque<net_update>	NET;
	Fvector					NET_SavedAccel;
	net_update				NET_Last;
	BOOL					NET_WasInterpolating;	// previous update was by interpolation or by extrapolation
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

	virtual void			SpawnAmmoForWeapon		(CInventoryItem *pIItem);
	virtual void			RemoveAmmoForWeapon		(CInventoryItem *pIItem);
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
	virtual void			RenderText				(LPCSTR Text, Fvector dpos, float* pdup, u32 color);

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

		shared_str				m_DefaultVisualOutfit;

		LPCSTR					invincibility_fire_shield_3rd;
		LPCSTR					invincibility_fire_shield_1st;
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
		void							SetWeaponHideState				(u16 State, bool bSet);
private://IPhysicsShellHolder

virtual	 void	_BCL	HideAllWeapons					( bool v ){ SetWeaponHideState(INV_STATE_BLOCK_ALL,v); }	

public:
		void							SetCantRunState					(bool bSet);
private:
	CActorCondition				*m_entity_condition;

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
			void				SwitchTorch						();
			void				HeadlampCallback				();
			void				NVCallback						();
			bool				OnActorSwitchesSmth				(const shared_str& restrictor_config_param, const shared_str& animator_item_section, const ACTOR_DEFS::EActorKeyflags& key_repeat, const CHudItem::TAnimationEffector& callback, u32 state, u32 device = 0, bool sup_det = false);
	CCustomDetector*			GetDetector						(bool in_slot = false);
#ifndef MASTER_GOLD
			void				NoClipFly						(int cmd);
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

	virtual	BOOL				BonePassBullet					(int boneID);
	virtual	void				On_B_NotCurrentEntity			();

private:
	xr_vector<ISpatial*>		ISpatialResult;

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
private:
			void			set_state_box(u32	mstate);
private:
	bool					m_disabled_hitmarks;
	bool					m_inventory_disabled;
//static CPhysicsShell		*actor_camera_shell;

DECLARE_SCRIPT_REGISTER_FUNCTION
};

extern bool		isActorAccelerated			(u32 mstate, bool ZoomMode);

IC	CActorCondition	&CActor::conditions	() const{ VERIFY(m_entity_condition); return(*m_entity_condition);}

extern CActor*		g_actor;
extern CActor*		g_actor_single;
CActor*				Actor		();
extern const float	s_fFallTime;
