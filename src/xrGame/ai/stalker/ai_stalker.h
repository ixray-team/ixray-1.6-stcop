////////////////////////////////////////////////////////////////////////////
//	Module 		: ai_stalker.h
//	Created 	: 25.02.2003
//  Modified 	: 25.02.2003
//	Author		: Dmitriy Iassenev
//	Description : AI Behaviour for monster "Stalker"
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../../Creature.h"
#include "../../ObjectHandler.h"
#include "../../AI_PhraseDialogManager.h"
#include "../../step_manager.h"
#include "../../../xrScripts/script_export_space.h"

#include "ai_stalker_state_net.h"
#include "ai_stalker_space.h"
#include "ai_monster_space.h"
#include "stalker_animation_names.h"

#ifdef DEBUG
	template <typename _object_type>
	class CActionBase;

	template <typename _object_type>
	class CPropertyEvaluator;

	template <
		typename _object_type,
		bool	 _reverse_search,
		typename _world_operator,
		typename _condition_evaluator,
		typename _world_operator_ptr,
		typename _condition_evaluator_ptr
	>
	class CActionPlanner;

	typedef CActionPlanner<
		CScriptGameObject,
		false,
		CActionBase<CScriptGameObject>,
		CPropertyEvaluator<CScriptGameObject>,
		CActionBase<CScriptGameObject>*,
		CPropertyEvaluator<CScriptGameObject>*
	>								script_planner;
#endif

namespace smart_cover {
	class cover;
	class loophole;

	namespace transitions {
		class action;
	};
};

namespace stalker_interpolation 
{
	struct InterpData
	{
		Fvector Pos;
		Fvector Vel;
		SRotation o_torso;
		SRotation head;
	};

	struct net_update_A
	{
		SPHNetState State;
		SRotation o_torso;
		SRotation head;
		u32 dwTimeStamp = 0;
	};
};

class CALifeSimulator;
class CCharacterPhysicsSupport;
class CWeapon;
class CCoverPoint;
class CCoverEvaluatorCloseToEnemy;
class CCoverEvaluatorFarFromEnemy;
class CCoverEvaluatorBest;
class CCoverEvaluatorAngle;
class CCoverEvaluatorSafe;
class CCoverEvaluatorAmbush;
class CAgentManager;
class CMotionDef;
class CStalkerAnimationManager;
class CStalkerPlanner;
class CSightManager;
class stalker_movement_manager_smart_cover;
class CStalkerSoundDataVisitor;
class CWeaponShotEffector;
struct SBoneProtections;
class CDangerLocation;
class CRestrictedObject;

class CAI_Stalker final : 
	public CCreature, 
	public CObjectHandler,
	public CAI_PhraseDialogManager,
	public CStepManager
{
private:
	typedef CCreature								inherited;

public:
	using inherited::useful;
	using inherited::evaluate;
	
private:
	CStalkerAnimationManager		*m_animation_manager;
	CStalkerPlanner					*m_brain;
	CSightManager					*m_sight_manager;
	stalker_movement_manager_smart_cover			*m_movement_manager;

#ifdef DEBUG
	const script_planner			*m_debug_planner;
#endif

	// ALife
private:
	SBoneProtections*				m_boneHitProtection;
	bool m_isUpdateBestWeapon = false;
	const char* m_onUpdateBestWeaponName = {};
	// weapon dispersion
private:
	float							m_disp_walk_stand;
	float							m_disp_walk_crouch;
	float							m_disp_run_stand;
	float							m_disp_run_crouch;
	float							m_disp_stand_stand;
	float							m_disp_stand_crouch;
	float							m_disp_stand_stand_zoom;
	float							m_disp_stand_crouch_zoom;

private:
	float							m_power_fx_factor;

	Fvector HeadLookOrientation;
	u32 HeadLookTime;
private:
	float							m_fRankDisperison;
	float							m_fRankVisibility;
	float							m_fRankImmunity;

	// for interpolation
	SPHNetState						LastState;
	SPHNetState						RecalculatedState;
	SPHNetState						PredictedState;

	float							SCoeff[3][4];		
	float							HCoeff[3][4];		
	Fvector							IPosS, IPosH, IPosL;


public:
	xr_deque<stalker_interpolation::net_update_A>	NET_A;

	stalker_interpolation::net_update_A				NET_A_Last;

	stalker_interpolation::InterpData		IStart;
	stalker_interpolation::InterpData		IRec;
	stalker_interpolation::InterpData		IEnd;

	bool							m_bInInterpolation;
	bool							m_bInterpolate;
	u32								m_dwIStartTime;
	u32								m_dwIEndTime;
	u32								m_dwILastUpdateTime;
	// best item/ammo selection members
public:
	bool							m_item_actuality;
	CInventoryItem					*m_best_item_to_kill;
	float							m_best_item_value;
	CInventoryItem					*m_best_ammo;
	const CInventoryItem			*m_best_found_item_to_kill;
	const CInventoryItem			*m_best_found_ammo;

	// covers being used
public:
	CCoverEvaluatorCloseToEnemy		*m_ce_close;
	CCoverEvaluatorFarFromEnemy		*m_ce_far;
	CCoverEvaluatorBest				*m_ce_best;
	CCoverEvaluatorAngle			*m_ce_angle;
	CCoverEvaluatorSafe				*m_ce_safe;
	CCoverEvaluatorAmbush			*m_ce_ambush;

	// physics support
public:
	CCharacterPhysicsSupport		*m_pPhysics_support;

public:
	bool							m_wounded;

public:
										CAI_Stalker					();
	virtual ~CAI_Stalker				() override;

public:
	virtual CCharacterPhysicsSupport*	character_physics_support	() override {return m_pPhysics_support;}
	virtual const CCharacterPhysicsSupport*	character_physics_support() const override {return m_pPhysics_support;}
	virtual CPHDestroyable*				ph_destroyable				() override;
	virtual CAttachmentOwner*			cast_attachment_owner		() override				{return this;}
	virtual CInventoryOwner*			cast_inventory_owner		() override				{return this;}
	virtual CEntityAlive*				cast_entity_alive			() override {return this;}
	virtual CEntity*					cast_entity					() override {return this;}
	virtual CGameObject*				cast_game_object			() override				{return this;}
	virtual CPhysicsShellHolder*		cast_physics_shell_holder	() override {return this;}
	virtual Feel::Sound*				dcast_FeelSound				() override {return this;}
	virtual CAI_Stalker*				cast_stalker				() override {return this;}
	virtual CCreature*				cast_creature			() override {return this;}
	virtual CScriptEntity*				cast_script_entity			() override {return this;}
	virtual CPhraseDialogManager*		cast_phrase_dialog_manager	() override {return this;}
	virtual CAI_PhraseDialogManager*	cast_ai_phrase_dialog_manager() override {return this;}

public:
			void						init								();
	virtual void						Load								(const char*	section ) override;
	virtual void						reinit								() override;
	virtual void						reload								(const char*	section ) override;				
	virtual void						LoadSounds							(const char* section );
	
	static void BoneCallback(CBoneInstance* HeadBone);
	void LookAtActor(CBoneInstance* HeadBone, float Weight = 1.f);

	virtual bool						net_Spawn							(CSE_Abstract* DC) override;

	virtual void						net_Export							(NET_Packet& P) override;
	virtual void						net_Import							(NET_Packet& P) override;

	virtual void SyncRead(NET_Packet& Packet) override;
	virtual void SyncWrite(NET_Packet& Packet) override;

	virtual void						net_Destroy							() override;
	virtual void						net_Save							(NET_Packet& P) override;
	virtual bool						net_SaveRelevant					() override;
	virtual void						net_Relcase							(CObject*	 O) override;

	//save/load server serialization
	virtual void						save								(NET_Packet &output_packet) override;
	virtual void						load								(IReader &input_packet) override;
	virtual void Serialize(ISaveObject& Object) override;

	virtual void						UpdateCL							() override;
	virtual void						shedule_Update						(u32 dt) override;
	virtual void						Think								() override;
	virtual void						SelectAnimation						(const Fvector& _view, const Fvector& _move, float speed ) override;
	virtual bool						UsedAI_Locations					() override;

	virtual void						g_WeaponBones						(u16 &L, u16 &R1, u16 &R2) final override;
	virtual void						g_fireParams						(const CHudItem* pHudItem, Fvector& P, Fvector& D) final override;
	virtual void						HitSignal							(float P,	Fvector& vLocalDir, CObject* who, s16 element) override;
	virtual void						Die									(CObject* who) override;

	virtual void						OnEvent								(NET_Packet& P, u16 type) override;
	virtual void						feel_touch_new						(CObject* O) override;
	virtual void						feel_touch_delete					(CObject* O) override;
			void						on_ownership_reject					( CObject*O, bool just_before_destroy );
	virtual void						renderable_Render					() override;
	virtual void						Exec_Look							(float dt) override;
	virtual void						Hit									(SHit* pHDS) override;
	virtual void						PHHit								(SHit &H) override;
	virtual bool						feel_vision_isRelevant				(CObject* who) override;
	virtual float						Radius								() const override;

	virtual void						ChangeVisual						( shared_str NewVisual );
	virtual void OnChangeVisual() override;

#ifdef DEBUG_DRAW
	virtual void						OnHUDDraw							(CCustomHUD* hud) override;
	virtual void						OnRender							() override;
			void						debug_text							();
			bool						m_dbg_hud_draw						;
#endif

	virtual bool						useful								(const CItemManager *manager, const CGameObject *object) const override;
	virtual float						evaluate							(const CItemManager *manager, const CGameObject *object) const override;
	virtual bool						useful								(const CEnemyManager *manager, const CEntityAlive *object) const override;
	
	// PDA && Dialogs
//	virtual void						ReceivePdaMessage					(u16 who, EPdaMsg msg, shared_str info_id);
	virtual void						UpdateAvailableDialogs				(CPhraseDialogManager* partner) override;

	// scripts
	virtual CWeapon						*GetCurrentWeapon		() const;
	virtual u32							GetWeaponAmmo			() const;
//	virtual CInventoryItem				*GetCurrentEquipment	() const; <- moved to InventoryOwner::GetCurrentOutfit
	virtual CInventoryItem				*GetMedikit				() const;
	virtual CInventoryItem				*GetFood				() const;
	virtual bool						bfAssignMovement		(CScriptEntityAction			*tpEntityAction) override;
	virtual bool						bfAssignWatch			(CScriptEntityAction			*tpEntityAction) override;
	virtual void						ResetScriptData			(void					*P = 0) override;
	virtual bool						bfAssignObject			(CScriptEntityAction			*tpEntityAction) override;
	virtual bool						bfAssignAnimation		(CScriptEntityAction			*tpEntityAction) override;
	
	// physics
	virtual u16							PHGetSyncItemsNumber	() override {return inherited ::PHGetSyncItemsNumber();}
	virtual CPHSynchronize*				PHGetSyncItem			(u16 item) override {return inherited ::PHGetSyncItem(item);}
	virtual void						PHUnFreeze				() override {return inherited ::PHUnFreeze();}
	virtual void						PHFreeze				() override {return inherited ::PHFreeze();}

	// miscellanious functions
			void						DropItemSendMessage		(CObject *O);
			bool						bfCheckForNodeVisibility(u32 dwNodeID, bool bIfRyPick = false);
	virtual ALife::ERelationType 		tfGetRelationType		(const CEntityAlive *tpEntityAlive) const override;
	virtual const SRotation				Orientation				() const override;
	virtual const MonsterSpace::SBoneRotation &head_orientation	() const override;

	//InventoryOwner stuff
	virtual bool						CanPutInSlot			(PIItem item, u32 slot) override;

	//////////////////////////////////////////////////////////////////////////
	// action/evaluators support functions
	//////////////////////////////////////////////////////////////////////////
public:
	virtual void						OnItemTake				(CInventoryItem *inventory_item) override;
	virtual void						OnItemDrop				(CInventoryItem *inventory_item, bool just_before_destroy) override;
			bool						item_to_kill			();
			bool						item_can_kill			();
			bool						remember_item_to_kill	();
			bool						remember_ammo			();
			bool						ready_to_kill			();
			bool						ready_to_detour			();
			void						update_best_item_info	();
			void						update_best_item_info_impl();
			void						ResetBoneProtections	(const char* imm_sect, const char* bone_sect);
	virtual float						GetWeaponAccuracy		() const override;
	virtual bool						unlimited_ammo			() override;
	virtual bool						infinite_fire() override;
	virtual void						spawn_supplies			() override;
	IC		CAgentManager				&agent_manager			() const;

	virtual bool						human_being				() const override
	{
		return							(true);
	}
	
			bool						undetected_anomaly		();
			bool						inside_anomaly			();

public:

	// State Manager Se7Kills
	aistalker_state_net state_manager;

	// NET EXPORT-IMPORT APPLY
	u32 LastUpdateAnims = 0;
	Fmatrix UpdateMatrix;

	void ApplyAnimation(StalkerMotionData& anims);
	void UpdateScriptAnim(NET_Packet& packet);
	virtual void make_Interpolation() override;

	virtual void PH_B_CrPr() override;
	virtual void PH_A_CrPr() override;
	virtual void PH_I_CrPr() override;

	void CalculateInterpolationParams();
	void postprocess_packet(stalker_interpolation::net_update_A& packet);
	
	void OnAnimationChangeTorso(const MotionID& motionID, bool mix, bool continue_position, bool m_step_dependence, bool anim_stand);
	void OnAnimationChangeHead(const MotionID& motionID, bool mix, bool continue_position, bool m_step_dependence, bool anim_stand);
	void OnAnimationChangeLegs(const MotionID& motionID, bool mix, bool continue_position, bool m_step_dependence, bool anim_stand);
	void OnAnimationChangeScript(u8 boneID, const MotionID& motionID, bool mix, bool use_controller, bool local_animation, Fmatrix* target_matrix);
	void OnAnimationChangeGlobal(u8 boneID, const MotionID& motionID, bool mix, bool use_controller, bool local_animation, Fmatrix* target_matrix);
private:
	bool				m_can_kill_member;
	bool				m_can_kill_enemy;
	float				m_pick_distance;
	u32					m_pick_frame_id;
	collide::rq_results	rq_storage;

private:
			void						can_kill_entity			(const Fvector &position, const Fvector &direction, float distance, collide::rq_results& rq_storage);
			void						can_kill_entity_from	(const Fvector &position, Fvector direction, float distance);
			void						update_can_kill_info	();

public:
			bool						can_kill_member			();
			bool						can_kill_enemy			();
			float						pick_distance			();
	IC		float						start_pick_distance		() const;
			bool						fire_make_sense			();

	virtual const char*						Name					() const override;
	virtual bool						feel_touch_contact		(CObject* O) override;
	virtual bool						feel_touch_on_contact	(CObject* O) override;

	//флаги, какие действия совершал актер по отношению к сталкеру
	//(помог, атаковал и т.д.)
	Flags32								m_actor_relation_flags;

	// ALife
private:
	struct CTradeItem {
		CInventoryItem					*m_item;
		ALife::_OBJECT_ID				m_owner_id;
		ALife::_OBJECT_ID				m_new_owner_id;

		IC					CTradeItem	(
			CInventoryItem		*item,
			ALife::_OBJECT_ID	owner_id,
			ALife::_OBJECT_ID	new_owner_id
		)
		{
			m_item			= item;
			m_owner_id		= owner_id;
			m_new_owner_id	= new_owner_id;
		}

		IC	bool			operator<	(const CTradeItem &trade_item) const;
		IC	bool			operator==	(ALife::_OBJECT_ID id) const;
	};

private:
	CGameObject							*m_trader_game_object;
	CInventoryOwner						*m_current_trader;
	xr_vector<CTradeItem>				m_temp_items;
	u32									m_total_money;
	bool								m_sell_info_actuality;
	bool								m_can_select_weapon;
public:
	bool								can_select_weapon				() {return m_can_select_weapon;};
	void								can_select_weapon				(bool can) {m_can_select_weapon = can;};
			bool						can_take						(CInventoryItem const * item);

protected:
			u32							fill_items						(CInventory &inventory, CGameObject *old_owner, ALife::_OBJECT_ID new_owner_id);
			
	IC		void						buy_item_virtual				(CTradeItem &item);
			void						attach_available_ammo			(CWeapon *weapon);
			void						choose_food						();
			void						choose_weapon					(ALife::EWeaponPriorityType weapon_priority_type);
			void						choose_medikit					();
			void						choose_detector					();
			void						choose_equipment				();

			void						select_items					();
			void						transfer_item					(CInventoryItem *item, CGameObject *old_owner, CGameObject *new_owner);

			void						update_sell_info				();
			bool						tradable_item					(CInventoryItem *inventory_item, const ALife::_OBJECT_ID &current_owner_id);
			bool						can_sell						(CInventoryItem* item);

			bool						non_conflicted					(const CInventoryItem *item, const CWeapon *new_weapon) const;
			bool						enough_ammo						(const CWeapon *new_weapon) const;
			bool						conflicted						(const CInventoryItem *item, const CWeapon *new_weapon, bool new_wepon_enough_ammo, int new_weapon_rank) const;
			void						update_conflicted				(CInventoryItem *item, const CWeapon *new_weapon);
			void						remove_personal_only_ammo		(const CInventoryItem *item);
			void						on_after_take					(const CGameObject *object);
	virtual bool						AllowItemToTrade 				(CInventoryItem const * item, const SInvItemPlace& place) const override;
public:
	IC		CStalkerAnimationManager	&animation						() const;
	IC		CStalkerPlanner				&brain							() const;
	IC		CSightManager				&sight							() const;

private:
	CStalkerSoundDataVisitor			*m_sound_user_data_visitor;

protected:
	virtual CSound_UserDataVisitor		*create_sound_visitor			() override;
	virtual CMemoryManager				*create_memory_manager			() override;
	virtual CMovementManager			*create_movement_manager		() override;

public:
	IC		stalker_movement_manager_smart_cover		&movement						() const;
	virtual DLL_Pure					*_construct						() override;

private:
	IC		bool						frame_check						(u32 &frame);
	virtual bool						natural_weapon					() const override {return false;}
	virtual bool						natural_detector				() const override {return false;}
	virtual bool						use_center_to_aim				() const override;
			void						process_enemies					();

private:
	bool								m_group_behaviour;

public:
	IC		bool						group_behaviour					() const;
	virtual void						update_range_fov				(float &new_range, float &new_fov, float start_range, float start_fov) override;
			void 				update_object_handler			();
			bool						zoom_state						() const;
			void						react_on_grenades				();
			void						react_on_member_death			();
private:
	CWeaponShotEffector					*m_weapon_shot_effector;
	s32									m_weapon_shot_random_seed;

public:
	virtual void						on_weapon_shot_start			(CWeapon *weapon) override;
	virtual void						on_weapon_shot_update			() override;
	virtual void						on_weapon_shot_stop				() override;
	virtual void						on_weapon_shot_remove			(CWeapon *weapon) override;
	virtual void						on_weapon_hide					(CWeapon *weapon) override;
	IC		CWeaponShotEffector			&weapon_shot_effector			() const;
	IC		Fvector						weapon_shot_effector_direction	(const Fvector &current) const;
	virtual void						UpdateCamera					() override;
	virtual bool						can_attach						(const CInventoryItem *inventory_item) const override;
#ifdef DEBUG
			void						debug_planner					(const script_planner *planner);
#endif

private:
	u32				m_pstl_min_queue_size_far;
	u32				m_pstl_max_queue_size_far;
	u32				m_pstl_min_queue_interval_far;
	u32				m_pstl_max_queue_interval_far;

	u32				m_pstl_min_queue_size_medium;
	u32				m_pstl_max_queue_size_medium;
	u32				m_pstl_min_queue_interval_medium;
	u32				m_pstl_max_queue_interval_medium;

	u32				m_pstl_min_queue_size_close;
	u32				m_pstl_max_queue_size_close;
	u32				m_pstl_min_queue_interval_close;
	u32				m_pstl_max_queue_interval_close;


	u32				m_shtg_min_queue_size_far;
	u32				m_shtg_max_queue_size_far;
	u32				m_shtg_min_queue_interval_far;
	u32				m_shtg_max_queue_interval_far;

	u32				m_shtg_min_queue_size_medium;
	u32				m_shtg_max_queue_size_medium;
	u32				m_shtg_min_queue_interval_medium;
	u32				m_shtg_max_queue_interval_medium;

	u32				m_shtg_min_queue_size_close;
	u32				m_shtg_max_queue_size_close;
	u32				m_shtg_min_queue_interval_close;
	u32				m_shtg_max_queue_interval_close;


	u32				m_snp_min_queue_size_far;
	u32				m_snp_max_queue_size_far;
	u32				m_snp_min_queue_interval_far;
	u32				m_snp_max_queue_interval_far;

	u32				m_snp_min_queue_size_medium;
	u32				m_snp_max_queue_size_medium;
	u32				m_snp_min_queue_interval_medium;
	u32				m_snp_max_queue_interval_medium;

	u32				m_snp_min_queue_size_close;
	u32				m_snp_max_queue_size_close;
	u32				m_snp_min_queue_interval_close;
	u32				m_snp_max_queue_interval_close;


	u32				m_mchg_min_queue_size_far;
	u32				m_mchg_max_queue_size_far;
	u32				m_mchg_min_queue_interval_far;
	u32				m_mchg_max_queue_interval_far;

	u32				m_mchg_min_queue_size_medium;
	u32				m_mchg_max_queue_size_medium;
	u32				m_mchg_min_queue_interval_medium;
	u32				m_mchg_max_queue_interval_medium;

	u32				m_mchg_min_queue_size_close;
	u32				m_mchg_max_queue_size_close;
	u32				m_mchg_min_queue_interval_close;
	u32				m_mchg_max_queue_interval_close;


	u32				m_auto_min_queue_size_far;
	u32				m_auto_max_queue_size_far;
	u32				m_auto_min_queue_interval_far;
	u32				m_auto_max_queue_interval_far;

	u32				m_auto_min_queue_size_medium;
	u32				m_auto_max_queue_size_medium;
	u32				m_auto_min_queue_interval_medium;
	u32				m_auto_max_queue_interval_medium;

	u32				m_auto_min_queue_size_close;
	u32				m_auto_max_queue_size_close;
	u32				m_auto_min_queue_interval_close;
	u32				m_auto_max_queue_interval_close;

//	float			m_pstl_queue_fire_dist_close;
	float			m_pstl_queue_fire_dist_med;
	float			m_pstl_queue_fire_dist_far;

//	float			m_shtg_queue_fire_dist_close;
	float			m_shtg_queue_fire_dist_med;
	float			m_shtg_queue_fire_dist_far;

//	float			m_snp_queue_fire_dist_close;
	float			m_snp_queue_fire_dist_med;
	float			m_snp_queue_fire_dist_far;

//	float			m_mchg_queue_fire_dist_close;
	float			m_mchg_queue_fire_dist_med;
	float			m_mchg_queue_fire_dist_far;

//	float			m_auto_queue_fire_dist_close;
	float			m_auto_queue_fire_dist_med;
	float			m_auto_queue_fire_dist_far;
public:
	IC		u32							pstl_min_queue_size_far			() const;
	IC		u32							pstl_max_queue_size_far			() const;
	IC		u32							pstl_min_queue_interval_far		() const;
	IC		u32							pstl_max_queue_interval_far		() const;

	IC		u32							pstl_min_queue_size_medium		() const;
	IC		u32							pstl_max_queue_size_medium		() const;
	IC		u32							pstl_min_queue_interval_medium	() const;
	IC		u32							pstl_max_queue_interval_medium	() const;

	IC		u32							pstl_min_queue_size_close		() const;
	IC		u32							pstl_max_queue_size_close		() const;
	IC		u32							pstl_min_queue_interval_close	() const;
	IC		u32							pstl_max_queue_interval_close	() const;


	IC		u32							shtg_min_queue_size_far			() const;
	IC		u32							shtg_max_queue_size_far			() const;
	IC		u32							shtg_min_queue_interval_far		() const;
	IC		u32							shtg_max_queue_interval_far		() const;

	IC		u32							shtg_min_queue_size_medium		() const;
	IC		u32							shtg_max_queue_size_medium		() const;
	IC		u32							shtg_min_queue_interval_medium	() const;
	IC		u32							shtg_max_queue_interval_medium	() const;

	IC		u32							shtg_min_queue_size_close		() const;
	IC		u32							shtg_max_queue_size_close		() const;
	IC		u32							shtg_min_queue_interval_close	() const;
	IC		u32							shtg_max_queue_interval_close	() const;


	IC		u32							snp_min_queue_size_far			() const;
	IC		u32							snp_max_queue_size_far			() const;
	IC		u32							snp_min_queue_interval_far		() const;
	IC		u32							snp_max_queue_interval_far		() const;

	IC		u32							snp_min_queue_size_medium		() const;
	IC		u32							snp_max_queue_size_medium		() const;
	IC		u32							snp_min_queue_interval_medium	() const;
	IC		u32							snp_max_queue_interval_medium	() const;

	IC		u32							snp_min_queue_size_close		() const;
	IC		u32							snp_max_queue_size_close		() const;
	IC		u32							snp_min_queue_interval_close	() const;
	IC		u32							snp_max_queue_interval_close	() const;

	IC		u32							mchg_min_queue_size_far			() const;
	IC		u32							mchg_max_queue_size_far			() const;
	IC		u32							mchg_min_queue_interval_far		() const;
	IC		u32							mchg_max_queue_interval_far		() const;

	IC		u32							mchg_min_queue_size_medium		() const;
	IC		u32							mchg_max_queue_size_medium		() const;
	IC		u32							mchg_min_queue_interval_medium	() const;
	IC		u32							mchg_max_queue_interval_medium	() const;

	IC		u32							mchg_min_queue_size_close		() const;
	IC		u32							mchg_max_queue_size_close		() const;
	IC		u32							mchg_min_queue_interval_close	() const;
	IC		u32							mchg_max_queue_interval_close	() const;


	IC		u32							auto_min_queue_size_far			() const;
	IC		u32							auto_max_queue_size_far			() const;
	IC		u32							auto_min_queue_interval_far		() const;
	IC		u32							auto_max_queue_interval_far		() const;

	IC		u32							auto_min_queue_size_medium		() const;
	IC		u32							auto_max_queue_size_medium		() const;
	IC		u32							auto_min_queue_interval_medium	() const;
	IC		u32							auto_max_queue_interval_medium	() const;

	IC		u32							auto_min_queue_size_close		() const;
	IC		u32							auto_max_queue_size_close		() const;
	IC		u32							auto_min_queue_interval_close	() const;
	IC		u32							auto_max_queue_interval_close	() const;

//	IC		float						pstl_queue_fire_dist_close		() const;
	IC		float						pstl_queue_fire_dist_med		() const;
	IC		float						pstl_queue_fire_dist_far		() const;

//	IC		float						shtg_queue_fire_dist_close		() const;
	IC		float						shtg_queue_fire_dist_med		() const;
	IC		float						shtg_queue_fire_dist_far		() const;

//	IC		float						snp_queue_fire_dist_close		() const;
	IC		float						snp_queue_fire_dist_med			() const;
	IC		float						snp_queue_fire_dist_far			() const;

//	IC		float						mchg_queue_fire_dist_close		() const;
	IC		float						mchg_queue_fire_dist_med			() const;
	IC		float						mchg_queue_fire_dist_far			() const;

//	IC		float						auto_queue_fire_dist_close		() const;
	IC		float						auto_queue_fire_dist_med		() const;
	IC		float						auto_queue_fire_dist_far		() const;
public:
	typedef xr_delegate<void (const CCoverPoint *, const CCoverPoint *)>	on_best_cover_changed_delegate;
	typedef	xr_delegate<bool (SHit const*)>								HitCallback;

private:
	typedef xr_vector<on_best_cover_changed_delegate>	cover_delegates;

private:
	cover_delegates						m_cover_delegates;
	const CCoverPoint					*m_best_cover;
	float								m_best_cover_value;
	bool								m_best_cover_actual;
	bool								m_best_cover_can_try_advance;
	const CCoverPoint					*m_best_cover_advance_cover;

public:
			const CCoverPoint			*find_best_cover					(const Fvector &position_to_cover_from);

private:
			float						best_cover_value					(const Fvector &position_to_cover_from);
			void						compute_enemy_distances				(float &minimum_enemy_distance, float &maximum_enemy_distance);
			void						update_best_cover_actuality			(const Fvector &position_to_cover_from);
			void						on_best_cover_changed				(const CCoverPoint *new_cover, const CCoverPoint *old_cover);

public:
			void						best_cover_invalidate				();
			void						best_cover_can_try_advance			();
			const CCoverPoint			*best_cover							(const Fvector &position_to_cover_from);
	IC		CCoverPoint const* const&	current_best_cover					() const;

public:
			void						subscribe_on_best_cover_changed		(const on_best_cover_changed_delegate &delegate);
			void						unsubscribe_on_best_cover_changed	(const on_best_cover_changed_delegate &delegate);

public:
	virtual void						on_enemy_change						(const CEntityAlive *enemy) override;
	virtual void						on_restrictions_change				() override;
			void						on_cover_blocked					(const CCoverPoint *cover);
			void						on_danger_location_add				(const CDangerLocation &location);
			void						on_danger_location_remove			(const CDangerLocation &location);

public:
			void						wounded								(bool value);
			bool						wounded								(const CRestrictedObject *object) const;
	IC		bool						wounded								() const;

// throwing grenades
private:
	// actuality parameters
	bool								m_throw_actual;
	Fvector								m_computed_object_position;
	Fvector								m_computed_object_direction;
	// target parameters
	Fvector								m_throw_target_position;
	CObject								*m_throw_ignore_object;
	// computed
	Fvector								m_throw_position;
	Fvector								m_throw_velocity;
	// collision prediction
	Fvector								m_throw_collide_position;
	bool								m_throw_enabled;

	u32									m_last_throw_time;
	u32									m_throw_time_interval;

#ifdef DEBUG
	xr_vector<trajectory_pick>			m_throw_picks;
	xr_vector<Fvector>					m_throw_collide_tris;
#endif // DEBUG

public:
	IC		const bool					&throw_enabled						();

private:
	bool								m_can_throw_grenades;

public:
	IC		const bool					&can_throw_grenades					() const;
	IC		void						can_throw_grenades					(const bool &value);

private:
			bool						throw_check_error					(
											float low,
											float high,
											const Fvector &start,
											const Fvector &velocity,
											const Fvector &gravity
										);
			void						check_throw_trajectory				(const float &throw_time);
			void						throw_target_impl					(const Fvector &position, CObject *throw_ignore_object );
			void						compute_throw_miss					( u32 const vertex_id );

public:
	virtual bool						use_default_throw_force				() override;
	virtual float						missile_throw_force					() override;
	virtual bool						use_throw_randomness				() override;
			void						throw_target						(const Fvector &position, CObject *throw_ignore_object );
			void						throw_target						(const Fvector &position, u32 const vertex_id, CObject *throw_ignore_object );
	IC		const Fvector				&throw_target						() const;
			void						update_throw_params					(); 
			void						on_throw_completed					();
	IC		const u32					&last_throw_time					() const;

#ifdef DEBUG
public:
			void						dbg_draw_vision						();
			void						dbg_draw_visibility_rays			();
#endif


	//////////////////////////////////////////////////////////////////////////
	// Critical Wounds
	//////////////////////////////////////////////////////////////////////////	
private:
	virtual void						load_critical_wound_bones					() override;
	virtual bool						critical_wound_external_conditions_suitable	() override;
	virtual void						critical_wounded_state_start				() override;

			void						fill_bones_body_parts						(const char* bone_id, const ECriticalWoundType &wound_type);
public:
	typedef xr_vector<float>			CRITICAL_WOUND_WEIGHTS;

private:
	CRITICAL_WOUND_WEIGHTS				m_critical_wound_weights;

public:
			bool						critically_wounded							();
	IC		const CRITICAL_WOUND_WEIGHTS&critical_wound_weights						() const;

private:
			bool						can_cry_enemy_is_wounded					() const;
			void						on_critical_wound_initiator					(const CAI_Stalker *critically_wounded);
			void						on_enemy_wounded_or_killed					(const CAI_Stalker *wounded_or_killed);
			void						notify_on_wounded_or_killed					(CObject *object);
			void						notify_on_wounded_or_killed					();
			void				remove_critical_hit							();
//////////////////////////////////////////////////////////////////////////
private:
	bool	m_registered_in_combat_on_migration;

public:
	virtual void						on_before_change_team						() override;
	virtual void						on_after_change_team						() override;

public:
	virtual void						create_anim_mov_ctrl						( CBlend *b, Fmatrix *start_pose, bool local_animation ) override;
	virtual void						destroy_anim_mov_ctrl						() override;

private:
	bool	m_can_select_items;

public:
	IC		const u32					&throw_time_interval						() const;
	IC		void						throw_time_interval							(const u32 &value);

public:
			bool						too_far_to_kill_enemy						(const Fvector &position);

public:
	virtual float						shedule_Scale								() override;

private:
	bool	m_sniper_update_rate;
	bool	m_sniper_fire_mode;

public:
	IC		void						sniper_update_rate							(bool value);
	IC		bool						sniper_update_rate							() const;
	IC		void						sniper_fire_mode							(bool value);
	IC		bool						sniper_fire_mode							() const;

private:
	shared_str	m_aim_bone_id;

public:
			void						aim_bone_id									(shared_str const &bone_id);
			shared_str const			&aim_bone_id								() const;
			void						aim_target									(Fvector &result, const CGameObject *object);

private:
	HitCallback							m_hit_callback;

public:
	IC		void						hit_callback								(HitCallback const &callback);
			void						use_smart_covers_only						(bool value);
			bool						use_smart_covers_only						() const;

public:
	typedef xr_delegate<void (Fmatrix& )>							EyeMatrixCallback;

private:
	virtual bool						AlwaysTheCrow								() override;

public:
	IC		void						take_items_enabled							(bool value);
	IC		bool						take_items_enabled							() const;

	IC		void						death_sound_enabled							(bool value);
	IC		bool						death_sound_enabled							() const;

private:
	bool								m_take_items_enabled;
	bool								m_death_sound_enabled;

public:
	smart_cover::cover const*			get_current_smart_cover						( );
	smart_cover::loophole const*		get_current_loophole						( );
	bool								can_fire_right_now							( );

public:
	typedef xr_vector<CObject*>			ignored_touched_objects_type;
	inline	ignored_touched_objects_type& ignored_touched_objects					() { return m_ignored_touched_objects; }
			void						generate_take_event							( CObject const* const object ) const;

private:
	ignored_touched_objects_type		m_ignored_touched_objects;

public:
	DECLARE_SCRIPT_REGISTER_FUNCTION
};

#include "ai_stalker_inline.h"
