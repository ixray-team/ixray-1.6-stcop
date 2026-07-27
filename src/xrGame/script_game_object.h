////////////////////////////////////////////////////////////////////////////
//	Module 		: script_game_object.h
//	Created 	: 25.09.2003
//  Modified 	: 29.06.2004
//	Author		: Dmitriy Iassenev
//	Description : Script game object class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../xrScripts/script_space_forward.h"
#include "script_bind_macroses.h"
#include "../xrScripts/script_export_space.h"
#include "xr_time.h"
#include "game_graph_space.h"
#include "game_location_selector.h"
#include "Artefact.h"
#include "medkit.h"
#include "antirad.h"
#include "CustomOutfit.h"
#include "Scope.h"
#include "Silencer.h"
#include "GrenadeLauncher.h"
#include "InventoryBox.h"
#include "InventoryOwner.h"
#include "Actor.h"
#include "Explosive.h"
#include "script_zone.h"
#include "ai/trader/ai_trader.h"
#include "ai/stalker/ai_stalker.h"
#include "Creature.h"
#include "Torch.h"
#include "space_restrictor.h"
#include "AnomalyZone.h"
#include "HudItem.h"
#include "FoodItem.h"
#include "PhysicsShellHolder.h"
#include "BottleItem.h"
#include "danger_object.h"
#include "danger_manager.h"		
#include "GameTaskDefs.h"
#include "antigas_filter.h"
#include "sight_manager_space.h"
#include "detail_path_manager_space.h"
#include "movement_manager_space.h"
#include "script_entity_space.h"
#include "alife_space.h"
#include "pda_space.h"
#include "../xrSound/ai_sounds.h"
#include "GameTaskDefs.h"
#include "patrol_path_manager_space.h"
#include "ai_monster_space.h"
#include "game_object_space.h"
#include "EntityCondition.h"
#include "actor_defs.h"

namespace smart_cover { class object; }
namespace doors { class door; }

class NET_Packet;
class CGameTask;

namespace MemorySpace {
	struct CMemoryInfo;
	struct CVisibleObject;
	struct CSoundObject;
	struct CHitObject;
	struct CNotYetVisibleObject;
};

class CGameObject;
class CScriptHit;
class CScriptEntityAction;
class CScriptTask;
class CScriptSoundInfo;
class CScriptMonsterHitInfo;
class CScriptBinderObject;
class CCoverPoint;
class CScriptIniFile;
class cphysics_shell_scripted;
class CHelicopter;
class CHangingLamp;
class CHolderCustom;
struct ScriptCallbackInfo;
struct STasks;
class CCar;
class CDangerObject;
class CScriptGameObject;
class CZoneCampfire;
class CPhysicObject;
class CArtefact;

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
#endif // DEBUG

class CScriptGameObject;

struct CSightParams {
	SightManager::ESightType	m_sight_type;
	CScriptGameObject			*m_object;
	Fvector						m_vector;
};

class CScriptGameObject {
	mutable CGameObject		*m_game_object;
							CScriptGameObject		(CScriptGameObject const& game_object);

public:

							CScriptGameObject		(CGameObject *tpGameObject);
	virtual					~CScriptGameObject		();
							operator CObject*		();

			CGameObject			&object				() const;
			CScriptGameObject	*Parent				() const;
			void				Hit					(CScriptHit *tLuaHit);
			int					clsid				() const;
			bool				IsActorOutdoors		() const;
			void				play_cycle			(const char* anim, bool mix_in);
			void				play_cycle			(const char* anim);
			Fvector				Center				();
	_DECLARE_FUNCTION10	(Position	,	Fvector		);
	_DECLARE_FUNCTION10	(Direction	,	Fvector		);
	_DECLARE_FUNCTION10	(Mass		,	float		);
	_DECLARE_FUNCTION10	(ID			,	u16			);
	_DECLARE_FUNCTION10	(getVisible	,	bool		);
	_DECLARE_FUNCTION10	(getEnabled	,	bool		);
	_DECLARE_FUNCTION10	(story_id	,	ALife::_STORY_ID);
	
			const char*				Name				() const;
			shared_str			cName				() const;
			const char*				Section				() const;
	// CInventoryItem
			u32					Cost				() const;
			float				GetCondition		() const;
			void				SetCondition		(float val);

	// CEntity
	_DECLARE_FUNCTION10	(DeathTime	,	u32		);
	_DECLARE_FUNCTION10	(MaxHealth	,	float	);
	_DECLARE_FUNCTION10	(Accuracy	,	float	);
	_DECLARE_FUNCTION10	(Team		,	int		);
	_DECLARE_FUNCTION10	(Squad		,	int		);
	_DECLARE_FUNCTION10	(Group		,	int		);

    void				Kill(CScriptGameObject* who, bool bypass_actor_check = false /*AVO: added for actor before death callback*/);

	void KillNotBypassActorCheck(CScriptGameObject* who);

	// CEntityAlive
	_DECLARE_FUNCTION10	(GetFOV				,			float);
	_DECLARE_FUNCTION10	(GetRange			,			float);
	_DECLARE_FUNCTION10	(GetHealth			,			float);
	_DECLARE_FUNCTION10	(GetPsyHealth		,			float);
	_DECLARE_FUNCTION10	(GetPower			,			float);
	_DECLARE_FUNCTION10	(GetRadiation		,			float);
	_DECLARE_FUNCTION10	(GetSatiety			,			float);
	_DECLARE_FUNCTION10	(GetSleepiness		,			float);
	_DECLARE_FUNCTION10	(GetThirst			,			float);
	_DECLARE_FUNCTION10	(GetIntoxication	,			float);
	_DECLARE_FUNCTION10	(GetBleeding		,			float);
	_DECLARE_FUNCTION10	(GetMorale			,			float);

	_DECLARE_FUNCTION11	(SetHealth,			void, float);
	_DECLARE_FUNCTION11	(SetPsyHealth,		void, float);
	_DECLARE_FUNCTION11	(SetPower,			void, float);
	_DECLARE_FUNCTION11	(ChangeSatiety,		void, float);
	_DECLARE_FUNCTION11	(SetRadiation,		void, float);
	_DECLARE_FUNCTION11	(SetBleeding,		void, float);
	_DECLARE_FUNCTION11	(SetSleepiness,		void, float);
	_DECLARE_FUNCTION11	(SetThirst,			void, float);
	_DECLARE_FUNCTION11	(SetIntoxication,	void, float);
	_DECLARE_FUNCTION11	(SetCircumspection,	void, float);
	_DECLARE_FUNCTION11	(SetMorale,			void, float);

			void				set_fov				(float new_fov);
			void				set_range			(float new_range);
			bool				Alive				() const;
			ALife::ERelationType	GetRelationType	(CScriptGameObject* who);

	// CScriptEntity
	
	_DECLARE_FUNCTION12	(SetScriptControl,	void, bool,				const char*);
	_DECLARE_FUNCTION10	(GetScriptControl	,			bool	);
	_DECLARE_FUNCTION10	(GetScriptControlName,			const char*	);
	_DECLARE_FUNCTION10	(GetEnemyStrength, int);
	_DECLARE_FUNCTION10	(can_script_capture, bool);
	

			CScriptEntityAction	*GetCurrentAction	() const;
			void				AddAction			(const CScriptEntityAction *tpEntityAction, bool bHighPriority = false);
			void				ResetActionQueue	();
	// Actor only
			void				SetActorPosition	(Fvector pos);
			void				SetActorDirection	(float dir);
			void				CameraMove			(float YawOffset);	// FNAS
			void				SwitchTorch			();					// FNAS
			void				SetActorCrouch		();					// FNAS
			void				SetNpcPosition		(Fvector pos);
			void				DisableHitMarks		(bool disable);
			bool				DisableHitMarks		() const;
			Fvector				GetMovementSpeed	() const;
	
	// CCreature
			bool				CheckObjectVisibility(const CScriptGameObject *tpLuaGameObject);
			bool				CheckTypeVisibility	(const char *section_name);
			const char*				WhoHitName			();
			const char*				WhoHitSectionName	();

			void				ChangeTeam			(u8 team, u8 squad, u8 group);
			void				SetVisualMemoryEnabled	(bool enabled);

	// CAI_Stalker
			CScriptGameObject	*GetCurrentWeapon	() const;
			CScriptGameObject	*GetFood			() const;
			CScriptGameObject	*GetMedikit			() const;
			void				SetPlayShHdRldSounds(bool val);

			void				set_force_anti_aim		(bool force);
			bool				get_force_anti_aim		();

	// Burer
			void				burer_set_force_gravi_attack (bool force);
			bool				burer_get_force_gravi_attack ();

	// Poltergeist
			void				poltergeist_set_actor_ignore	(bool ignore);
			bool				poltergeist_get_actor_ignore	();

	// CAI_Bloodsucker
			void				force_visibility_state	(int state);
			int					get_visibility_state	();

	// CBaseMonster
			void				set_override_animation	(const char* anim_name);
			void				clear_override_animation();

			void				force_stand_sleep_animation	(u32 index);
			void				release_stand_sleep_animation ();

			void				set_invisible			(bool val);
			bool				get_invisible			();
			void				set_manual_invisibility (bool val);
			void				set_alien_control		(bool val);
			void				set_enemy				(CScriptGameObject *e);
			void				set_vis_state			(float value);
			void				off_collision			(bool val);
			void				bloodsucker_drag_jump	(CScriptGameObject* e, const char* e_str, const Fvector &position, float factor);

	// Zombie
			bool				fake_death_fall_down	();
			void				fake_death_stand_up		();

	// CBaseMonster
			void				skip_transfer_enemy		(bool val);
			void				set_home				(const char* name, float r_min, float r_max, bool aggressive, float r_mid);
			void				set_home				(u32 lv_ID, float r_min, float r_max, bool aggressive, float r_mid);
			void				remove_home				();
			void				berserk					();
			void				set_custom_panic_threshold	(float value);
			void				set_default_panic_threshold	();

	// CAI_Trader
			void				set_trader_global_anim	(const char* anim);
			void				set_trader_head_anim	(const char* anim);
			void				set_trader_sound		(const char* sound, const char* anim);
			void				external_sound_start	(const char* sound);
			void				external_sound_stop		();


			template <typename T>
			IC		T			*action_planner			();

	// CProjector
			Fvector				GetCurrentDirection		();
			
			bool				IsInvBoxEmpty			();
			bool				inv_box_closed			(bool status, const char* reason);
			bool				inv_box_closed_status	();
			bool				inv_box_can_take		(bool status);
			bool				inv_box_can_take_status	();

	//передача порции информации InventoryOwner
			bool				GiveInfoPortion		(const char* info_id);
			bool				DisableInfoPortion	(const char* info_id);
			bool				GiveGameNews		(const char* news, const char* texture_name, Frect tex_rect, int delay, int show_time);
			void				GiveGameNews		(const char* caption, const char* news, const char* texture_name, int delay, int show_time);
			void				GiveGameNews		(const char* caption, const char* news, const char* texture_name, int delay, int show_time, int type);

			void				AddIconedTalkMessage_old(const char* text, const char* texture_name, const char* templ_name) {};
			void				AddIconedTalkMessage(const char* text, const char* texture_name, Frect tex_rect, const char* templ_name);
			void				AddIconedTalkMessage(const char* caption, const char* text, const char* texture_name, const char* templ_name);
			//предикаты наличия/отсутствия порции информации у персонажа
			bool				HasInfo				(const char* info_id);
			bool				DontHasInfo			(const char* info_id);
			xrTime				GetInfoTime			(const char* info_id);
			//работа с заданиями
			ETaskState			GetGameTaskState	(const char* task_id, u16 objective_id);
			void				SetGameTaskState	(ETaskState state, const char* task_id, u16 objective_id);
			void				GiveTaskToActor		(CGameTask* t, u32 dt, bool bCheckExisting, u32 t_timer);
			void				SetActiveTask		(CGameTask* t);
			bool				IsActiveTask		(CGameTask* t);
			CGameTask*			GetTask				(const char* id, bool only_inprocess);

			
			bool				IsTalking			();
			void				StopTalk			();
			void				EnableTalk			();	
			void				DisableTalk			();
			bool				IsTalkEnabled		();

			void				EnableTrade			();	
			void				DisableTrade		();
			bool				IsTradeEnabled		();

			void				EnableInvUpgrade	();	
			void				DisableInvUpgrade	();
			bool				IsInvUpgradeEnabled	();


			void				ActorLookAtPoint	(Fvector point);
			void				IterateInventory	(luabind::functor<bool> functor, luabind::object object);
			void				IterateInventoryBox	(luabind::functor<bool> functor, luabind::object object);
			void				MarkItemDropped		(CScriptGameObject *item);
			bool				MarkedDropped		(CScriptGameObject *item);
			void				UnloadMagazine		();

			void				DropItem			(CScriptGameObject* pItem);
			void				DropItemAndTeleport	(CScriptGameObject* pItem, Fvector position);
			void				ForEachInventoryItems(const luabind::functor<void> &functor);
			void				TransferItem		(CScriptGameObject* pItem, CScriptGameObject* pForWho);
			void				TransferMoney		(int money, CScriptGameObject* pForWho);
			void				GiveMoney			(int money);
			u32					Money				();
			u32					GetActorMoneyEarned	();
			u32					GetActorMoneySpent	();
			float				GetActorDistanceKm	();
			u32					GetActorHeadshots	();
			u32					GetActorDeaths		();
			u32					GetActorHelpWounded	();
			void				MakeItemActive		(CScriptGameObject* pItem);
			
			void				SetRelation			(ALife::ERelationType relation, CScriptGameObject* pWhoToSet);
			
			float				GetSympathy			();
			void				SetSympathy			(float sympathy);

			int					GetCommunityGoodwill_obj( const char* community );
			void				SetCommunityGoodwill_obj( const char* community, int goodwill );
			
			int					GetAttitude			(CScriptGameObject* pToWho);

			int					GetGoodwill			(CScriptGameObject* pToWho);
			void				SetGoodwill			(int goodwill, CScriptGameObject* pWhoToSet);
			void				ForceSetGoodwill	(int goodwill, CScriptGameObject* pWhoToSet);
			void				ChangeGoodwill		(int delta_goodwill, CScriptGameObject* pWhoToSet);


			void				SetStartDialog		(const char* dialog_id);
			void				GetStartDialog		();
			void				RestoreDefaultStartDialog();

			void				SwitchToTrade		();
			void				SwitchToUpgrade		();
			void				SwitchToTalk		();	
			void				RunTalkDialog		(CScriptGameObject* pToWho, bool disable_break);
			void				AllowBreakTalkDialog(bool disable_break);

			void				HideWeapon			();
			void				HideDetector		();
			void				SwitchDetector		();
			void				RestoreWeapon		();
			void				AllowSprint			(bool b);

			void				SetPdaDisabled		(bool b);
			bool				IsPdaDisabled		();
			void				SetInventoryDisabled(bool b);
			bool				IsInventoryDisabled	();
			void				SetUseDisabled(bool b);

			bool				Weapon_IsGrenadeLauncherAttached();
			bool				Weapon_IsScopeAttached			();
			bool				Weapon_IsSilencerAttached		();

			int					Weapon_GrenadeLauncher_Status	();
			int					Weapon_Scope_Status				();
			int					Weapon_Silencer_Status			();

			const char*				ProfileName			();
			const char*				CharacterName		();
			const char*				CharacterIcon		();
			const char*				CharacterCommunity	();
			int					CharacterRank		();
			int					CharacterReputation	();


			void SetCharacterRank			(int);
			void ChangeCharacterRank		(int);
			void ChangeCharacterReputation	(int);
			void SetCharacterReputation		(int);
			void SetCharacterCommunity		(const char*,int,int);
		

			u32					GetInventoryObjectCount() const;

			CScriptGameObject	*GetActiveItem		();

			CScriptGameObject	*GetObjectByName	(const char* caObjectName) const;
			CScriptGameObject	*GetObjectByIndex	(int iIndex) const;

			
	// Callbacks			
			void				SetCallback			(GameObject::ECallbackType type, const luabind::functor<void> &functor);
			void				SetCallback			(GameObject::ECallbackType type, const luabind::functor<void> &functor, const luabind::object &object);
			void				SetCallback			(GameObject::ECallbackType type);

			void				set_patrol_extrapolate_callback(const luabind::functor<bool> &functor);
			void				set_patrol_extrapolate_callback(const luabind::functor<bool> &functor, const luabind::object &object);
			void				set_patrol_extrapolate_callback();

			void				set_enemy_callback	(const luabind::functor<bool> &functor);
			void				set_enemy_callback	(const luabind::functor<bool> &functor, const luabind::object &object);
			void				set_enemy_callback	();
	
	//////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////use calback///////////////////////////////////////////////
			void				SetTipText			(const char* tip_text);
			void				SetTipTextDefault	();
			void				SetNonscriptUsable	(bool nonscript_usable);
///////////////////////////////////////////////////////////////////////////////////////////
			void				set_fastcall		(const luabind::functor<bool> &functor, const luabind::object &object);
			void				set_const_force		(const Fvector &dir,float value,u32  time_interval)							;
//////////////////////////////////////////////////////////////////////////

			const char*				GetPatrolPathName	();
			const char*				GetItemAdditionalDescription();
			void				SetItemAdditionalDescription(const char* additionalDescription);
			void				UnsetItemAdditionalDescription();
			bool				IsItemUsedAdditionalDescription();

			u32					GetAmmoElapsed		();

			// FFx0001 ++
			u32					GetAmmoElapsedWithChamber(); 
			bool				IsWeaponUseChamber();
			bool				IsWorldObjectBoneVisible(const char* bone_name);
			bool				SetWorldObjectBoneVisibility(const char* boneName, bool bVisibility);
			bool				IsHudObjectBoneVisible(const char* bone_name);
			bool				SetHudObjectBoneVisibility(const char* boneName, bool bVisibility);
			void				SetActorSleepiness(const float value);
			void				SetActorSatiety(const float value);
			void				SetActorThirst(const float value);
			void				SetActorHealth(const float value);
			void				SetActorPower(const float value);
			void				SetActorRadiation(const float value);
			void				SetActorPsyHealth(const float value);
			void				SetActorMorale(const float value);
			bool				InstallAntigasFilter(CScriptGameObject* antigas_filter_lua_game_object);
			bool				UnInstallAntigasFilter();
			// FFx0001 --

			void				SetAmmoElapsed		(int ammo_elapsed);
			u32					GetSuitableAmmoTotal		() const;
			void				SetQueueSize		(u32 queue_size);
			CScriptGameObject	*GetBestEnemy		();
			const CDangerObject	*GetBestDanger		();
			CScriptGameObject	*GetBestItem		();
			void				SetBestEnemy		(CScriptGameObject* lua_game_object);
	_DECLARE_FUNCTION10			(GetActionCount,u32);
	
			const				CScriptEntityAction	*GetActionByIndex(u32 action_index = 0);

//////////////////////////////////////////////////////////////////////////
// Inventory Owner
//////////////////////////////////////////////////////////////////////////

			//////////////////////////////////////////////////////////////////////////
			Flags32				get_actor_relation_flags	()			const;
			void 				set_actor_relation_flags	(Flags32);
			const char*				sound_voice_prefix	()			const;

			//////////////////////////////////////////////////////////////////////////
			u32						memory_time		(const CScriptGameObject &lua_game_object);
			Fvector					memory_position	(const CScriptGameObject &lua_game_object);
			CScriptGameObject		*best_weapon	();
			void					explode			(u32 level_time);
			CScriptGameObject		*GetEnemy		() const;
			CScriptGameObject		*GetCorpse		() const;
			CScriptSoundInfo		GetSoundInfo	();
			CScriptMonsterHitInfo	GetMonsterHitInfo();
			void					bind_object		(CScriptBinderObject *object);
			CScriptGameObject		*GetCurrentOutfit() const;
			float					GetCurrentOutfitProtection(int hit_type);
			
			u32						BeltSize() const;    
			void					deadbody_closed			(bool status);
			bool					deadbody_closed_status	();
			void					deadbody_can_take		(bool status);
			bool					deadbody_can_take_status();

			void					can_select_weapon		(bool status);
			bool					can_select_weapon		() const;
	//////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////
			void				set_body_state		(MonsterSpace::EBodyState body_state);
			void				set_movement_type	(MonsterSpace::EMovementType movement_type);
			void				set_mental_state	(MonsterSpace::EMentalState mental_state);
			void				set_path_type		(MovementManager::EPathType path_type);
			void				set_detail_path_type(DetailPathManager::EDetailPathType detail_path_type);

	MonsterSpace::EBodyState			body_state			() const;
	MonsterSpace::EBodyState			target_body_state	() const;
	MonsterSpace::EMovementType			movement_type		() const;
	MonsterSpace::EMovementType			target_movement_type() const;
	MonsterSpace::EMentalState			mental_state		() const;
	MonsterSpace::EMentalState			target_mental_state	() const;
	MovementManager::EPathType			path_type			() const;
	DetailPathManager::EDetailPathType	detail_path_type	() const;

			u32					add_sound				(const char* prefix, u32 max_count, ESoundTypes type, u32 priority, u32 mask, u32 internal_type, const char* bone_name);
			u32					add_sound				(const char* prefix, u32 max_count, ESoundTypes type, u32 priority, u32 mask, u32 internal_type);
			u32					add_sound				(const char* prefix, u32 max_count, ESoundTypes type, u32 priority, u32 mask, u32 internal_type, const char* bone_name, const char* head_anim);
			u32					add_combat_sound		(const char* prefix, u32 max_count, ESoundTypes type, u32 priority, u32 mask, u32 internal_type, const char* bone_name);
			void				remove_sound			(u32 internal_type);
			void				set_sound_mask			(u32 sound_mask);
			void				set_sight				(SightManager::ESightType sight_type, Fvector *vector3d, u32 dwLookOverDelay);
			void				set_sight				(SightManager::ESightType sight_type, bool torso_look, bool path);
			void				set_sight				(SightManager::ESightType sight_type, Fvector &vector3d, bool torso_look);
			void 				set_sight				(SightManager::ESightType sight_type, Fvector *vector3d);
			void 				set_sight				(CScriptGameObject *object_to_look);
			void 				set_sight				(CScriptGameObject *object_to_look, bool torso_look);
			void 				set_sight				(CScriptGameObject *object_to_look, bool torso_look, bool fire_object);
			void 				set_sight				(CScriptGameObject *object_to_look, bool torso_look, bool fire_object, bool no_pitch);
			void 				set_sight				(const MemorySpace::CMemoryInfo *memory_object, bool	torso_look);
			s32 GetRank				();
			void				play_sound				(u32 internal_type);
			void				play_sound				(u32 internal_type, u32 max_start_time);
			void				play_sound				(u32 internal_type, u32 max_start_time, u32 min_start_time);
			void				play_sound				(u32 internal_type, u32 max_start_time, u32 min_start_time, u32 max_stop_time);
			void				play_sound				(u32 internal_type, u32 max_start_time, u32 min_start_time, u32 max_stop_time, u32 min_stop_time);
			void				play_sound				(u32 internal_type, u32 max_start_time, u32 min_start_time, u32 max_stop_time, u32 min_stop_time, u32 id);

			void				set_item				(MonsterSpace::EObjectAction object_action);
			void				set_item				(MonsterSpace::EObjectAction object_action, CScriptGameObject *game_object);
			void				set_item				(MonsterSpace::EObjectAction object_action, CScriptGameObject *game_object, u32 queue_size);
			void				set_item				(MonsterSpace::EObjectAction object_action, CScriptGameObject *game_object, u32 queue_size, u32 queue_interval);
			void				set_desired_position	();
			void				set_desired_position	(const Fvector *desired_position);
			void				set_desired_direction	();
			void				set_desired_direction	(const Fvector *desired_direction);
			void				set_patrol_path			(const char* path_name, const PatrolPathManager::EPatrolStartType patrol_start_type, const PatrolPathManager::EPatrolRouteType patrol_route_type, bool random);
			void				inactualize_patrol_path	();
			void				set_dest_level_vertex_id(u32 level_vertex_id);
			void				set_dest_game_vertex_id	(GameGraph::_GRAPH_ID game_vertex_id);
			void				set_movement_selection_type(ESelectionType selection_type);
			u32					level_vertex_id			() const;
			u32					game_vertex_id			() const;
			void				add_animation			(const char* animation, bool hand_usage, bool use_movement_controller);
			void				add_animation			(const char* animation, bool hand_usage, Fvector position, Fvector rotation, bool local_animation);
			void				clear_animations		();
			int					animation_count			() const;
			int					animation_slot			() const;
			CScriptBinderObject	*binded_object			();
			void				set_previous_point		(int point_index);
			void				set_start_point			(int point_index);
			u32					get_current_patrol_point_index();
			bool				path_completed			() const;
			void				patrol_path_make_inactual();
			void				extrapolate_length		(float extrapolate_length);
			float				extrapolate_length		() const;
			void				enable_memory_object	(CScriptGameObject *object, bool enable);
			int					active_sound_count		();
			int					active_sound_count		(bool only_playing);
			bool				RayPick					(const Fvector3& Pos, const Fvector3& Dir, float Range);
			const CCoverPoint	*best_cover				(const Fvector &position, const Fvector &enemy_position, float radius, float min_enemy_distance, float max_enemy_distance);
			const CCoverPoint	*safe_cover				(const Fvector &position, float radius, float min_distance);
			CScriptIniFile		*spawn_ini				() const;
			bool				active_zone_contact		(u16 id);

			///
			void				add_restrictions		(const char* out, const char* in);
			void				remove_restrictions		(const char* out, const char* in);
			void				remove_all_restrictions	();
			const char*				in_restrictions			();
			const char*				out_restrictions		();
			const char*				base_in_restrictions	();
			const char*				base_out_restrictions	();
			bool				accessible_position		(const Fvector &position);
			bool				accessible_vertex_id	(u32 level_vertex_id);
			u32					accessible_nearest		(const Fvector &position, Fvector &result);

			const xr_vector<MemorySpace::CVisibleObject>		&memory_visible_objects	() const;
			const xr_vector<MemorySpace::CSoundObject>			&memory_sound_objects	() const;
			const xr_vector<MemorySpace::CHitObject>			&memory_hit_objects		() const;
			const xr_vector<MemorySpace::CNotYetVisibleObject>	&not_yet_visible_objects() const;
			float				visibility_threshold	() const;
			void				enable_vision			(bool value);
			bool				vision_enabled			() const;
			void				set_sound_threshold		(float value);
			void				restore_sound_threshold	();
			//////////////////////////////////////////////////////////////////////////
			void				enable_attachable_item	(bool value);			
			bool				attachable_item_enabled	() const;
			void				enable_night_vision		(bool value);			
			void				night_vision_allowed	(bool value);															
			bool				night_vision_enabled	() const;
			void				enable_torch			(bool value);
			bool				torch_enabled			() const;
			
			void				attachable_item_load_attach(const char* section);
			// CustomZone
			void				EnableAnomaly			();
			void				DisableAnomaly			();
			float				GetAnomalyPower			();
			void				SetAnomalyPower			(float p);
			
	
			// HELICOPTER
			CHelicopter*		get_helicopter			();
			//CAR
			CCar*				get_car					();
			//LAMP
			CHangingLamp*		get_hanging_lamp		();
			
			// Custom lighting control for CHangingLamp (used in cutscenes and logic)
			bool				IsLightActive		();
			void				SetLightColor		(float r, float g, float b, float multiplier);
			void				SetLightRange		(float range);
			void				SetLightShadows		(bool b_shadows);
			void				SetLightVolumetric	(bool b_volumetric);
			void				SetLightAnim		(const char* anim_name);

			CHolderCustom*		get_custom_holder		();
			CHolderCustom*		get_current_holder		(); //actor only

			void				start_particles			(const char* pname, const char* bone);
			void				stop_particles			(const char* pname, const char* bone);

			Fvector				bone_position			(const char* bone_name);
			Fvector				bone_position			(u16 bone_index);
			const char*				get_bone_name_by_id		(u16 bone_id) const;
			u16					get_bone_id_by_name		(const char* bone_name) const;
			const char*				get_root_bone_name		() const;
			u16					get_root_bone_id()		const;
			Fvector				bone_direction			(const char* bone_name);
			Fvector				bone_direction			(u16 bone_name);

			bool				is_body_turning			() const;
	cphysics_shell_scripted*	get_physics_shell		() const;
			u16					get_bone_id				(const char* bone_name) const;					
			bool				weapon_strapped			() const;
			bool				weapon_unstrapped		() const;
			void				eat						(CScriptGameObject *item);
			bool				inside					(const Fvector &position, float epsilon) const;
			bool				inside					(const Fvector &position) const;

			Fvector				head_orientation		() const;
			u32					vertex_in_direction		(u32 level_vertex_id, Fvector direction, float max_distance) const;
			
			void				info_add				(const char* text);
			void				info_clear				();
			
			// Monster Jumper
			void				jump					(const Fvector &position, float factor);

			void				set_ignore_monster_threshold		(float ignore_monster_threshold);
			void				restore_ignore_monster_threshold	();
			float				ignore_monster_threshold			() const;
			void				set_max_ignore_monster_distance		(const float &max_ignore_monster_distance);
			void				restore_max_ignore_monster_distance	();
			float				max_ignore_monster_distance			() const;

			void				make_object_visible_somewhen		(CScriptGameObject *object);

			CScriptGameObject	*item_in_slot						(u32 slot_id) const;
			CScriptGameObject	*active_detector					() const;
			u32					active_slot							();
			void				activate_slot						(u32 slot_id);
			void				enable_level_changer				(bool b);
			bool				is_level_changer_enabled			();
			void				set_level_changer_invitation		(const char* str);

			//Boosters
			bool				IsBoosterInfluence					(EBoostParams param);

			float				GetBoosterInfluenceTime				(EBoostParams param);

			void				ApplyBooster						(const char* sect);
			void				SetBoosterTime						(float time, EBoostParams param);

			bool				GetActorMovementState				(ACTOR_DEFS::EMovementStates state, ACTOR_DEFS::EMoveCommand mask);
			void				SetActorMovementState				(ACTOR_DEFS::EMovementStates state, ACTOR_DEFS::EMoveCommand mask, bool status);

			void				ActorFire() const;

#ifdef DEBUG
			void				debug_planner						(const script_planner *planner);
#endif

			void				sell_condition						(CScriptIniFile *ini_file, const char* section);
			void				sell_condition						(float friend_factor, float enemy_factor);
			void				buy_condition						(CScriptIniFile *ini_file, const char* section);
			void				buy_condition						(float friend_factor, float enemy_factor);
			void				show_condition						(CScriptIniFile *ini_file, const char* section);
			void				buy_supplies						(CScriptIniFile *ini_file, const char* section);
			void				buy_item_condition_factor			(float factor);

			const char*				sound_prefix						() const;
			void				sound_prefix						(const char* sound_prefix);

			u32					location_on_path					(float distance, Fvector *location);
			bool				is_there_items_to_pickup			() const;

			bool				wounded								() const;
			void				wounded								(bool value);

			CSightParams		sight_params						();

			void				enable_movement						(bool enable);
			bool				movement_enabled					();

			bool				critically_wounded					();

			bool				invulnerable						() const;
			void				invulnerable						(bool invulnerable);
			const char*				get_smart_cover_description			() const;
			void				set_visual_name_notForce			(const char* visual);
			void				set_visual_name						(const char* visual,bool bForce = false);
			const char*				get_visual_name						() const;

			bool				can_throw_grenades					() const;
			void				can_throw_grenades					(bool can_throw_grenades);

			u32					throw_time_interval					() const;
			void				throw_time_interval					(u32 throw_time_interval);

			u32					group_throw_time_interval			() const;
			void				group_throw_time_interval			(u32 throw_time_interval);
			CArtefact*			get_artefact						();
			CZoneCampfire*		get_campfire						();
			CPhysicObject* 		get_physics_object					();

			void				aim_time							(CScriptGameObject *weapon, u32 time);
			u32					aim_time							(CScriptGameObject *weapon);

			void				special_danger_move					(bool value);
			bool				special_danger_move					();

			void				sniper_update_rate					(bool value);
			bool				sniper_update_rate					() const;

			void				sniper_fire_mode					(bool value);
			bool				sniper_fire_mode					() const;

			void				aim_bone_id							(const char* value);
			const char*				aim_bone_id							() const;

			void				register_in_combat					();
			void				unregister_in_combat				();
			CCoverPoint const*	find_best_cover						(Fvector position_to_cover_from);

// approved by Dima smart covers functions
			bool				use_smart_covers_only					() const;
			void				use_smart_covers_only					(bool value);

			bool				in_smart_cover							() const;

			void				set_dest_smart_cover					(const char* cover_id);
			void				set_dest_smart_cover					();
			CCoverPoint const*	get_dest_smart_cover					();
			const char*				get_dest_smart_cover_name				();
			
			void				set_dest_loophole						(const char* loophole_id);
			void				set_dest_loophole						();

			void				set_smart_cover_target					(Fvector position);
			void				set_smart_cover_target					(CScriptGameObject* object);
			void				set_smart_cover_target					();

			void				set_smart_cover_target_selector			();
			void				set_smart_cover_target_selector			(luabind::functor<void> functor);
			void				set_smart_cover_target_selector			(luabind::functor<void> functor, luabind::object object);

			void				set_smart_cover_target_idle				();
			void				set_smart_cover_target_lookout			();
			void				set_smart_cover_target_fire				();
			void				set_smart_cover_target_fire_no_lookout	();
			void				set_smart_cover_target_default			(bool value);

			float const			idle_min_time							() const;
			void				idle_min_time							(float value);
			float const			idle_max_time							() const;
			void				idle_max_time							(float value);
			float const			lookout_min_time						() const;
			void				lookout_min_time						(float value);
			float const			lookout_max_time						() const;
			void				lookout_max_time						(float value);

			bool				in_loophole_fov							(const char* cover_id, const char* loophole_id, Fvector object_position) const;
			bool				in_current_loophole_fov					(Fvector object_position) const;
			bool				in_loophole_range						(const char* cover_id, const char* loophole_id, Fvector object_position) const;
			bool				in_current_loophole_range				(Fvector object_position) const;

			float				apply_loophole_direction_distance		() const;
			void				apply_loophole_direction_distance		(float value);

			bool				movement_target_reached					();
			bool				suitable_smart_cover					(CScriptGameObject* object);

			void				take_items_enabled						(bool value);
			bool				take_items_enabled						() const;

			void				death_sound_enabled						(bool value);
			bool				death_sound_enabled						() const;

			void				register_door							();
			void				unregister_door							();
			void				on_door_is_open							();
			void				on_door_is_closed						();
			bool				is_door_locked_for_npc					() const;
			void				lock_door_for_npc						();
			void				unlock_door_for_npc						();
			bool				is_door_blocked_by_npc					() const;
			bool				is_weapon_going_to_be_strapped			( CScriptGameObject const* object ) const;
			const bool			getMechanic								() const;
			void SetHeadRotate(bool value);
			void				setMechanic								(bool cond);

			void				SetSubIconText(const char* m_custom_text, int item_custom_text_clr_inv, const char* item_custom_text_font, Fvector2 m_custom_text_offset);
			void				SetSubIcon(bool m_custom_mark, Fvector2 m_custom_mark_offset, Fvector2 m_custom_mark_size, const char* m_custom_mark_texture, int m_custom_mark_clr);
			
			CHelmet*			cast_CHelmet();
			AntigasFilter*		cast_AntigasFilter();

			_DECLARE_FUNCTION14(cast_GameObject, CScriptGameObject);
			_DECLARE_FUNCTION14(cast_Car, CCar);
			_DECLARE_FUNCTION14(cast_Heli, CHelicopter);
			_DECLARE_FUNCTION14(cast_HolderCustom, CHolderCustom);
			_DECLARE_FUNCTION14(cast_EntityAlive, CEntityAlive);
			_DECLARE_FUNCTION14(cast_InventoryItem, CInventoryItem);
			_DECLARE_FUNCTION14(cast_InventoryOwner, CInventoryOwner);
			_DECLARE_FUNCTION14(cast_Actor, CActor);
			_DECLARE_FUNCTION14(cast_Medkit, CMedkit);
			_DECLARE_FUNCTION14(cast_EatableItem, CEatableItem);
			_DECLARE_FUNCTION14(cast_Antirad, CAntirad);
			_DECLARE_FUNCTION14(cast_CustomOutfit, CCustomOutfit);
			_DECLARE_FUNCTION14(cast_Scope, CScope);
			_DECLARE_FUNCTION14(cast_Silencer, CSilencer);
			_DECLARE_FUNCTION14(cast_GrenadeLauncher, CGrenadeLauncher);
			_DECLARE_FUNCTION14(cast_SpaceRestrictor, CSpaceRestrictor);
			_DECLARE_FUNCTION14(cast_Stalker, CAI_Stalker);
			_DECLARE_FUNCTION14(cast_CustomZone, CAnomalyZone);
			_DECLARE_FUNCTION14(cast_Monster, CCreature);
			_DECLARE_FUNCTION14(cast_Explosive, CExplosive);
			_DECLARE_FUNCTION14(cast_ScriptZone, CScriptZone);
			//_DECLARE_FUNCTION14(cast_Projector, CProjector);
			_DECLARE_FUNCTION14(cast_Trader, CAI_Trader);
			_DECLARE_FUNCTION14(cast_HudItem, CHudItem);
			_DECLARE_FUNCTION14(cast_FoodItem, CFoodItem);
			_DECLARE_FUNCTION14(cast_Artefact, CArtefact);
			_DECLARE_FUNCTION14(cast_Ammo, CWeaponAmmo);
			//_DECLARE_FUNCTION14(cast_Missile, CMissile);
			_DECLARE_FUNCTION14(cast_PhysicsShellHolder, CPhysicsShellHolder);
			//_DECLARE_FUNCTION14(cast_Grenade, CGrenade);
			_DECLARE_FUNCTION14(cast_BottleItem, CBottleItem);
			_DECLARE_FUNCTION14(cast_Torch, CTorch);
			_DECLARE_FUNCTION14(cast_InventoryBox, CInventoryBox);

			u32					get_dest_level_vertex_id();
			u32					get_dest_game_vertex_id();
			void				inactualize_level_path();
			void				inactualize_game_path();

			void				SetHealthEx(float hp); //AVO
			float				GetLuminocityHemi();
			float				GetLuminocity();
			bool				Use(CScriptGameObject* obj);
			void				StartTrade(CScriptGameObject* obj);
			void				StartUpgrade(CScriptGameObject* obj);
			void				SetWeight(float w);
			u64					GetSpatialType();
			void				SetSpatialType(u64 sptype);
			u8					GetRestrictionType();
			void				SetRestrictionType(u8 typ);

			void				RemoveDanger(const CDangerObject& dobject);

			void				RemoveMemorySoundObject(const MemorySpace::CSoundObject& memory_object);
			void				RemoveMemoryHitObject(const MemorySpace::CHitObject& memory_object);
			void				RemoveMemoryVisibleObject(const MemorySpace::CVisibleObject& memory_object);

			//Weapon
			const char*				Weapon_GetAmmoSection(u8 ammo_type);
			void				Weapon_SetCurrentScope(u8 type);
			u8					Weapon_GetCurrentScope();
			void				Weapon_AddonAttach(CScriptGameObject* item);
			void				Weapon_AddonDetach(const char* item_section, bool b_spawn_item);
			void				AddonsAttacher(u8 addons, u8 scope_idx);
			void				SetAmmoType(u8 type);
			void				SetMainWeaponType(u32 type);
			void				SetWeaponType(u32 type);
			u32					GetMainWeaponType();
			u32					GetWeaponType();
			u8					GetWeaponSubstate();
			u8					GetAmmoType();
			bool				HasAmmoType(u8 type);

			//CWeaponAmmo
			u16					AmmoGetCount();
			int					GetAmmoCount(u8 type);

			bool IsOnBelt(CScriptGameObject* obj) const;
			bool IsDefaultToRuck();
			void SetDefaultToRuck(bool state);
			void SetRemainingUses(u8 value);
			u8 GetRemainingUses();
			u8 GetMaxUses();
			bool IsAmmo() const;
			bool ActorIsJump() const;

			bool				WeaponInGrenadeMode();
			//Car
			CScriptGameObject* GetAttachedVehicle();
			void				AttachVehicle(CScriptGameObject* veh, bool bForce = false);
			void				DetachVehicle(bool bForce = false);

			u32 PlayHudMotion(const char* M, bool bMixIn, u8 state);
			void AmmoSetCount(u16 count);
			u16 AmmoBoxSize();
			float GetTotalTelepaticProtection();

			//Weapon & Outfit
			bool InstallUpgrade(const char* upgrade);
			bool HasUpgrade(const char* upgrade);
			void IterateInstalledUpgrades(const luabind::functor<bool>& functor);
			void SwitchState(u8 state);
			u8 GetState();
			
			//Works for anything with visual
			bool				IsBoneVisible(const char* bone_name);
			void				SetBoneVisible(const char* bone_name, bool bVisibility, bool bRecursive = true);

			//CAI_Stalker
			void				ResetBoneProtections(const char* imm_sect, const char* bone_sect);
			const char*				bones_protection_sect();
			//Anything with PPhysicShell (ie. car, actor, stalker, monster, heli)
			void				ForceSetPosition(Fvector pos, bool bActivate = false);

			//Artifacts
			float				GetArtefactHealthRestoreSpeed();
			bool				SetEntityIgnoredByMonstersState(bool flag);
			bool				GetEntityIgnoredByMonstersState();
			float				GetArtefactRadiationRestoreSpeed();
			float				GetArtefactSatietyRestoreSpeed();
			float				GetArtefactThirstRestoreSpeed();
			float				GetArtefactSleepinessRestoreSpeed();
			float				GetArtefactEquipmentDurabilityModifier();
			float				GetArtefactInventoryWeightModifier();
			float				GetArtefactPowerRestoreSpeed();
			float				GetArtefactBleedingRestoreSpeed();

			void				SetArtefactHealthRestoreSpeed(float value);
			void				SetArtefactRadiationRestoreSpeed(float value);
			void				SetArtefactSatietyRestoreSpeed(float value);
			void				SetArtefactThirstRestoreSpeed(float value);
			void				SetArtefactSleepinessRestoreSpeed(float value);
			void				SetArtefactEquipmentDurabilityModifier(float value);
			void				SetArtefactInventoryWeightModifier(float value);
			void				SetArtefactPowerRestoreSpeed(float value);
			void				SetArtefactBleedingRestoreSpeed(float value);
			CScriptGameObject* ItemOnBelt(u32 item_id) const;


			//Phantom
			void				PhantomSetEnemy(CScriptGameObject*);
			//Actor
			float		GetActorMaxWeight() const;
			void		SetActorMaxWeight(float max_weight);
			float		GetActorMaxWalkWeight() const;
			void		SetActorMaxWalkWeight(float max_walk_weight);
			float		GetAdditionalMaxWeight() const;
			void		SetAdditionalMaxWeight(float add_max_weight);
			float		GetAdditionalMaxWalkWeight() const;
			void		SetAdditionalMaxWalkWeight(float add_max_walk_weight);
			float		GetTotalWeight() const;
			float		Weight() const;
			float		GetInventoryVolume() const;
			float		GetInventoryVolumeCapacity() const;
			float		GetInventoryVolumeOverload() const;
			float		GetItemVolume() const;

			float       GetActorJumpSpeed() const;
			void        SetActorJumpSpeed(float jump_speed);
			float       GetActorSprintKoef() const;
			void        SetActorSprintKoef(float sprint_koef);
			float       GetActorRunCoef() const;
			void        SetActorRunCoef(float run_coef);
			float       GetActorRunBackCoef() const;
			void        SetActorRunBackCoef(float run_back_coef);
			void		SetCharacterIcon(const char* iconName);
			void		SetActorDefaultVisual(const char* visualName);
			void SetCharacterMaxWeight(float value);
	void StartActorAnimator(const char* section);
	void StopActorAnimator();
	const char* GetActorAnimatorSection();
	bool IsAnimatorActive();
	u8 GetActorAnimatorRestoredSlot();
	float GetActorPowerBoostTime();
	const char* GetCutsceneVisual();
	void SetInvulnerable(bool value);
	void SetFire(bool value);
	bool GetGasmaskStatus();
	float GetGasmaskCondition();
	bool GetAnimatorForceHideItems();
	void SetAnimatorForceHideItems(bool status);
	void ShowStateAnimator(const char* section);
	void HideStateAnimator();
	bool IsInCar();
	void IterateFeelTouch(const luabind::functor<bool>& functor);
	bool IsActorLadder() const;
	bool IsActorSafemode() const;
	void SetActorSafemode(bool status);
	doors::door* m_door;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};

extern void sell_condition	(CScriptIniFile *ini_file, const char* section);
extern void sell_condition	(float friend_factor, float enemy_factor);
extern void buy_condition	(CScriptIniFile *ini_file, const char* section);
extern void buy_condition	(float friend_factor, float enemy_factor);
extern void show_condition	(CScriptIniFile *ini_file, const char* section);