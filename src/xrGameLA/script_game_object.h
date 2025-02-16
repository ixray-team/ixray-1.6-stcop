////////////////////////////////////////////////////////////////////////////
//	Module 		: script_game_object.h
//	Created 	: 25.09.2003
//  Modified 	: 29.06.2004
//	Author		: Dmitriy Iassenev
//	Description : Script game object class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "script_space_forward.h"
#include "script_bind_macroses.h"
#include "script_export_space.h"
#include "xr_time.h"
#include "character_info_defs.h"

enum EPdaMsg; 
enum ESoundTypes;
enum EActorState;
enum ETaskState;

namespace ALife {enum ERelationType;};
namespace ScriptEntity {enum EActionType;};
namespace MovementManager { enum EPathType;};
namespace DetailPathManager { enum EDetailPathType;};
namespace SightManager {enum ESightType;};

class NET_Packet;
class CGameTask;

namespace PatrolPathManager { 
	enum EPatrolStartType;
	enum EPatrolRouteType;
};

namespace MemorySpace {
	struct CMemoryInfo;
	struct CVisibleObject;
	struct CSoundObject;
	struct CHitObject;
	struct CNotYetVisibleObject;
};

namespace MonsterSpace {
	enum EBodyState;
	enum EMovementType;
	enum EMovementDirection;
	enum EDirectionType;
	enum EPathState;
	enum EObjectAction;
	enum EMentalState;
	enum EScriptMonsterMoveAction;
	enum EScriptMonsterSpeedParam;
	enum EScriptMonsterAnimAction;
	enum EScriptMonsterGlobalAction;
	enum EScriptSoundAnim;
	enum EMonsterSounds;
	enum EMonsterHeadAnimType;
	struct SBoneRotation;
};

namespace GameObject {
	enum ECallbackType;
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
class CPhysicsShell;
class CHelicopter;
class CHangingLamp;
class CHolderCustom;
struct ScriptCallbackInfo;
struct STasks;
class CCar;
//class CPda;
class CMountedTurret;
class CDangerObject;
class CScriptGameObject;

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

class CScriptGameObject;

namespace SightManager {
	enum ESightType;
}

struct CSightParams {
	SightManager::ESightType	m_sight_type;
	CScriptGameObject			*m_object;
	Fvector						m_vector;
};

class CScriptGameObject {
	mutable CGameObject		*m_game_object;
public:

							CScriptGameObject		(CGameObject *tpGameObject);
	virtual					~CScriptGameObject		();
							operator CObject*		();

	IC		CGameObject			&object				() const;
			CScriptGameObject	*Parent				() const;
			void				Hit					(CScriptHit *tLuaHit);
			int					clsid				() const;
			void				play_cycle			(LPCSTR anim, bool mix_in);
			void				play_cycle			(LPCSTR anim);
			Fvector				Center				();
	_DECLARE_FUNCTION10	(Position	,	Fvector		);
	_DECLARE_FUNCTION10	(Direction	,	Fvector		);
	_DECLARE_FUNCTION10	(Mass		,	float		);
	_DECLARE_FUNCTION10	(ID			,	u32			);
	_DECLARE_FUNCTION10	(getVisible	,	BOOL		);
	_DECLARE_FUNCTION10	(getEnabled	,	BOOL		);
	_DECLARE_FUNCTION10	(story_id	,	ALife::_STORY_ID);
	
			LPCSTR				Name				() const;
			shared_str			cName				() const;
			LPCSTR				Section				() const;
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

			void				Kill				(CScriptGameObject* who);

	// CEntityAlive
	_DECLARE_FUNCTION10	(GetFOV				,			float);
	_DECLARE_FUNCTION10	(GetRange			,			float);
	_DECLARE_FUNCTION10	(GetHealth			,			float);
	_DECLARE_FUNCTION10	(GetPsyHealth		,			float);
	_DECLARE_FUNCTION10	(GetPower			,			float);
	_DECLARE_FUNCTION10	(GetSatiety			,			float);
	_DECLARE_FUNCTION10	(GetThirsty			,			float);
	_DECLARE_FUNCTION10	(GetRadiation		,			float);
	_DECLARE_FUNCTION10	(GetBleeding		,			float);
	_DECLARE_FUNCTION10	(GetMorale			,			float);

	_DECLARE_FUNCTION11	(SetHealth,			void, float);
	//---Добавил возможность обращаться к кровотечению
	_DECLARE_FUNCTION11	(SetBleeding,		void, float);
	_DECLARE_FUNCTION11	(SetPsyHealth,		void, float);
	_DECLARE_FUNCTION11	(SetPower,			void, float);
	_DECLARE_FUNCTION11	(SetSatiety,		void, float);
	_DECLARE_FUNCTION11	(SetThirsty,		void, float);
	_DECLARE_FUNCTION11	(SetRadiation,		void, float);
	_DECLARE_FUNCTION11	(SetCircumspection,	void, float);
	_DECLARE_FUNCTION11	(SetMorale,			void, float);

			void				set_fov				(float new_fov);
			void				set_range			(float new_range);
			bool				Alive				() const;
			ALife::ERelationType	GetRelationType	(CScriptGameObject* who);
			bool MoveToSlot(CScriptGameObject *obj, bool not_activate = false);
			bool MoveToRuck(CScriptGameObject *obj);
			void RemoveFromInventory(u32 obj_id);

	// CScriptEntity
	
	_DECLARE_FUNCTION12	(SetScriptControl,	void, bool,				LPCSTR);
	_DECLARE_FUNCTION10	(GetScriptControl	,			bool	);
	_DECLARE_FUNCTION10	(GetScriptControlName,			LPCSTR	);
	_DECLARE_FUNCTION10	(GetEnemyStrength, int);
	_DECLARE_FUNCTION10	(can_script_capture, bool);
	

	bool has_silencer_installed() const;
	bool has_scope_installed() const;
	bool has_grenade_launcher_installed() const;
	LPCSTR installed_scope_name() const;
	void attach_addon(ALife::_OBJECT_ID addon_id);

	void RemoveFastUpdate(void);
	void SetFastUpdate(const luabind::functor<void> &functor, const luabind::object &object);
	
			CScriptEntityAction	*GetCurrentAction	() const;
			void				AddAction			(const CScriptEntityAction *tpEntityAction, bool bHighPriority = false);
			void				ResetActionQueue	();
	// Actor only
			CScriptGameObject*  GetActorCar			();
			void				SetActorPosition	(Fvector pos);
			void				SetActorDirection	(float dir);
			void				SetActorDirectionVector	(Fvector dir);
			void				SetNpcPosition		(Fvector pos);
			void				SetActorCamActive	(Fvector pos, Fvector dir, Fvector norm);
			void				SetActorDirectionSlowly	(Fvector pos, float time);
			void				SetActorLegsVisible	(bool val);
			void				SetActorCam			(u8 val);
			void				SetHandsOnly		(bool val);
			bool				IsHandsOnly			();
			bool				IsFirstEyeCam		();
			void				SetIconState		(u8 state, bool show);
			bool				GetIconState		(u8 state);
	// CEntityAlive
			void				TeleportEntity		(Fvector pos, Fvector dir);
//			void				TeleportActorVehicle(Fvector pos, u32 level_vertex_id, u16 game_vertex_id);
	// CCustomMonster
			bool				CheckObjectVisibility(const CScriptGameObject *tpLuaGameObject);
			bool				CheckTypeVisibility	(const char *section_name);
			LPCSTR				WhoHitName			();
			LPCSTR				WhoHitSectionName	();

			void				ChangeTeam			(u8 team, u8 squad, u8 group);

	// CAI_Stalker
			CScriptGameObject	*GetCurrentWeapon	() const;
			CScriptGameObject	*GetFood			() const;
			CScriptGameObject	*GetMedikit			() const;

	// CAI_Bloodsucker
	
			void				set_invisible			(bool val);
			bool				get_invisible			();
			void				set_manual_invisibility (bool val);
			void				set_alien_control		(bool val);

	// Zombie
			bool				fake_death_fall_down	();
			void				fake_death_stand_up		();

	// CBaseMonster
			void				skip_transfer_enemy		(bool val);
			void				set_home				(LPCSTR name, float r_min, float r_max, bool aggressive);
			void				remove_home				();
			void				berserk					();
			void				set_custom_panic_threshold	(float value);
			void				set_default_panic_threshold	();
			void				anomaly_detector_enable	(bool state);
			bool				anomaly_detector_enabled();

	// CAI_Trader
			void				set_trader_global_anim	(LPCSTR anim);
			void				set_trader_head_anim	(LPCSTR anim);
			void				set_trader_sound		(LPCSTR sound, LPCSTR anim);
			void				external_sound_start	(LPCSTR sound);
			void				external_sound_stop		();


			template <typename T>
			IC		T			*action_planner			();

	// CProjector
			Fvector				GetCurrentDirection		();
			bool				IsInvBoxEmpty			();
	//передача порции информации InventoryOwner
			bool				GiveInfoPortion		(LPCSTR info_id);
			bool				DisableInfoPortion	(LPCSTR info_id);
			bool				GiveGameNews		(LPCSTR news, LPCSTR texture_name, Frect tex_rect, int delay, int show_time);

			LPCSTR				GetNpcIcon			();

			void				AddIconedTalkMessage(LPCSTR text, LPCSTR texture_name, Frect tex_rect, LPCSTR templ_name);
	//предикаты наличия/отсутствия порции информации у персонажа
			bool				HasInfo				(LPCSTR info_id);
			bool				DontHasInfo			(LPCSTR info_id);
			xrTime				GetInfoTime			(LPCSTR info_id);
	//работа с заданиями
			ETaskState			GetGameTaskState	(LPCSTR task_id, int objective_num);
			void				SetGameTaskState	(ETaskState state, LPCSTR task_id, int objective_num);
			void				GiveTaskToActor		(CGameTask* t, u32 dt, bool bCheckExisting);

			
			bool				IsTalking			();
			void				StopTalk			();
			void				EnableTalk			();	
			void				DisableTalk			();
			bool				IsTalkEnabled		();

			void				EnableTrade			();	
			void				DisableTrade		();
			bool				IsTradeEnabled		();

			bool				HasUpgrade			(LPCSTR upgrade_id);
			luabind::object		InstalledUpgrades	();

			void				IterateInventory	(luabind::functor<void> functor, luabind::object object);
			void				MarkItemDropped		(CScriptGameObject *item);
			bool				MarkedDropped		(CScriptGameObject *item);
			void				UnloadMagazine		();
			void				PlayAnim		(LPCSTR anim, BOOL cycle, u32 slot);

			void				DropItem			(CScriptGameObject* pItem);
			void				DropItemAndTeleport	(CScriptGameObject* pItem, Fvector position);
			void				ForEachInventoryItems(const luabind::functor<void> &functor);
			void				InventoryBoxIterator(const luabind::functor<void> &functor);
			void				TransferItem		(CScriptGameObject* pItem, CScriptGameObject* pForWho);
			void				TransferMoney		(int money, CScriptGameObject* pForWho);
			void				GiveMoney			(int money);
			u32					Money				();
			
			void				SetRelation			(ALife::ERelationType relation, CScriptGameObject* pWhoToSet);
	
			int					GetAttitude			(CScriptGameObject* pToWho);

			int					GetGoodwill			(CScriptGameObject* pToWho);
			void				SetGoodwill			(int goodwill, CScriptGameObject* pWhoToSet);
			void				ChangeGoodwill		(int delta_goodwill, CScriptGameObject* pWhoToSet);


			void				SetStartDialog		(LPCSTR dialog_id);
			LPCSTR				GetStartDialog		();
			void				RestoreDefaultStartDialog();

			void				SwitchToTrade		();
			void				SwitchToTalk		();	
			void				RunTalkDialog		(CScriptGameObject* pToWho);
			void				AllowBreakTalkDialog(bool disable_break);

			void				HideWeapon			();
			void				RestoreWeapon		();
			bool				IsWeaponHidden		();

			u16				BoneNameToId			(LPCSTR name);
			bool				GetBoneVisible		(LPCSTR name);
			void				SetBoneVisible		(LPCSTR name, bool val, bool bRecursive);
			bool				BoneExist		(LPCSTR name);

			LPCSTR				ProfileName			();
			LPCSTR				CharacterName		();
			LPCSTR				CharacterCommunity	();
			LPCSTR				CharacterBio		();
			int					CharacterRank		();
			int					CharacterReputation	();


			void SetCharacterRank			(int);
			void ChangeCharacterRank		(int);
			void ChangeCharacterReputation	(int);
			void SetCharacterCommunity		(LPCSTR,int,int);
		

			u32					GetInventoryObjectCount() const;

			CScriptGameObject	*GetActiveItem		();
			CScriptGameObject	*GetCurrentDetector	();

			CScriptGameObject	*GetObjectByName	(LPCSTR caObjectName) const;
			CScriptGameObject	*GetObjectByIndex	(int iIndex) const;

			u32	GetBeltObjectCount() const;
			CScriptGameObject	*GetBeltObjectByName	(LPCSTR caObjectName) const;
			CScriptGameObject	*GetBeltObjectByIndex	(int iIndex) const;
			CScriptGameObject	*GetBeltObjectById		(int item_id) const;
			
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
			void				SetTipText			(LPCSTR tip_text);
			void				SetTipTextDefault	();
			void				SetNonscriptUsable	(bool nonscript_usable);
			void				SetStashAsSafe		(bool is_safe);
			bool				IsStashASafe		();
			void				SetSafeCode			(LPCSTR code);
			LPCSTR				GetSafeCode			();
			void				SetUnlockInfo		(LPCSTR infoportion);
			LPCSTR				GetUnlockInfo		();
///////////////////////////////////////////////////////////////////////////////////////////
			void				set_fastcall		(const luabind::functor<bool> &functor, const luabind::object &object);
			void				set_const_force		(const Fvector &dir,float value,u32  time_interval);
//////////////////////////////////////////////////////////////////////////

			LPCSTR				GetPatrolPathName	();
			u32					GetAmmoElapsed		();
			LPCSTR				GetAmmoSection		();
			void				SetAmmoElapsed		(int ammo_elapsed);
			u32					GetAmmoCurrent		() const;
			void				SetQueueSize		(u32 queue_size);
			int					GetAmmoMagSize		();
			CScriptGameObject	*GetBestEnemy		();
			const CDangerObject	*GetBestDanger		();
			CScriptGameObject	*GetBestItem		();

			void				SetPortionsNum		(u32 num);
			u32					GetPortionsNum		() const;
			 
	_DECLARE_FUNCTION10			(GetActionCount,u32);
	
			const				CScriptEntityAction	*GetActionByIndex(u32 action_index = 0);

//////////////////////////////////////////////////////////////////////////
// Inventory Owner
//////////////////////////////////////////////////////////////////////////

			//////////////////////////////////////////////////////////////////////////
			Flags32				get_actor_relation_flags	()			const;
			void 				set_actor_relation_flags	(Flags32);
			LPCSTR				sound_voice_prefix	()			const;

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

			u32					add_sound				(LPCSTR prefix, u32 max_count, ESoundTypes type, u32 priority, u32 mask, u32 internal_type, LPCSTR bone_name);
			u32					add_sound				(LPCSTR prefix, u32 max_count, ESoundTypes type, u32 priority, u32 mask, u32 internal_type);
			u32					add_sound				(LPCSTR prefix, u32 max_count, ESoundTypes type, u32 priority, u32 mask, u32 internal_type, LPCSTR bone_name, LPCSTR head_anim);
			void				remove_sound			(u32 internal_type);
			void				set_sound_mask			(u32 sound_mask);
			void				set_sight				(SightManager::ESightType sight_type, const Fvector *vector3d, u32 dwLookOverDelay);
			void				set_sight				(SightManager::ESightType sight_type, bool torso_look, bool path);
			void				set_sight				(SightManager::ESightType sight_type, const Fvector &vector3d, bool torso_look);
			void 				set_sight				(SightManager::ESightType sight_type, const Fvector *vector3d);
			void 				set_sight				(CScriptGameObject *object_to_look);
			void 				set_sight				(CScriptGameObject *object_to_look, bool torso_look);
			void 				set_sight				(CScriptGameObject *object_to_look, bool torso_look, bool fire_object);
			void 				set_sight				(CScriptGameObject *object_to_look, bool torso_look, bool fire_object, bool no_pitch);
			void 				set_sight				(const MemorySpace::CMemoryInfo *memory_object, bool	torso_look);
			CHARACTER_RANK_VALUE GetRank				();
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
			void				set_patrol_path			(LPCSTR path_name, const PatrolPathManager::EPatrolStartType patrol_start_type, const PatrolPathManager::EPatrolRouteType patrol_route_type, bool random);
			void				set_dest_level_vertex_id(u32 level_vertex_id);
			u32					level_vertex_id			() const;
			float				level_vertex_light		(const u32 &level_vertex_id) const;
			u32					game_vertex_id			() const;
			void				add_animation			(LPCSTR animation, bool hand_usage, bool use_movement_controller);
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
			const CCoverPoint	*best_cover				(const Fvector &position, const Fvector &enemy_position, float radius, float min_enemy_distance, float max_enemy_distance);
			const CCoverPoint	*safe_cover				(const Fvector &position, float radius, float min_distance);
			CScriptIniFile		*spawn_ini				() const;
			bool				active_zone_contact		(u16 id);

			///
			void				add_restrictions		(LPCSTR out, LPCSTR in);
			void				remove_restrictions		(LPCSTR out, LPCSTR in);
			void				remove_all_restrictions	();
			LPCSTR				in_restrictions			();
			LPCSTR				out_restrictions		();
			LPCSTR				base_in_restrictions	();
			LPCSTR				base_out_restrictions	();
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
			// CustomZone
			void				EnableAnomaly			();
			void				DisableAnomaly			();
			float				GetAnomalyPower			();
			void				SetAnomalyPower			(float p);
			
	
			// HELICOPTER
			CHelicopter*		get_helicopter			();
			//CAR
			CCar*				get_car					();
			//PDA
			//CPda*				get_pda					();
			//LAMP
			CHangingLamp*		get_hanging_lamp		();
			CHolderCustom*		get_custom_holder		();
			CHolderCustom*		get_current_holder		(); //actor only
			u16			get_current_holder_id		(); //actor only

			void				detach_actor_Vehicle		(); //actor only
			void				attach_actor_Vehicle		(u32 id); //actor only
			// Turret
			CMountedTurret		*get_turret				();

			Fvector				bone_position			(LPCSTR bone_name) const;
			bool				is_body_turning			() const;
			CPhysicsShell*		get_physics_shell		() const;
			bool				weapon_strapped			() const;
			bool				weapon_unstrapped		() const;
			void				eat						(CScriptGameObject *item);
			bool				inside					(const Fvector &position, float epsilon) const;
			bool				inside					(const Fvector &position) const;

			Fvector				head_orientation		() const;
			u32					vertex_in_direction		(u32 level_vertex_id, Fvector direction, float max_distance) const;
			
			void				info_add				(LPCSTR text);
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
			u32					active_slot							();
			void				activate_slot						(u32 slot_id);
			void				deactivate_slot						();

#ifdef DEBUG
			void				debug_planner						(const script_planner *planner);
#endif

			void				sell_condition						(luabind::object tbl);
			void				sell_condition						(CScriptIniFile *ini_file, LPCSTR section);
			void				sell_condition						(float friend_factor, float enemy_factor);
			void				sell_condition						(bool);
			void				buy_condition						(luabind::object tbl);
			void				buy_condition						(CScriptIniFile *ini_file, LPCSTR section);
			void				buy_condition						(float friend_factor, float enemy_factor);
			void				buy_condition						(bool);
			void				show_condition						(luabind::object tbl);
			void				show_condition						(CScriptIniFile *ini_file, LPCSTR section);
			void				buy_supplies						(luabind::object tbl);
			void				buy_supplies						(CScriptIniFile *ini_file, LPCSTR section);
			void				buy_item_condition_factor			(float factor);

			LPCSTR				sound_prefix						() const;
			void				sound_prefix						(LPCSTR sound_prefix);

			u32					location_on_path					(float distance, Fvector *location);

			bool				wounded								() const;
			void				wounded								(bool value);
			void				wounded								(bool value, bool dest);

			CSightParams		sight_params						();

			void				enable_movement						(bool enable);
			bool				movement_enabled					();

			bool				critically_wounded					();
			
			// check if object is invulnerable by default
			bool				default_invulnerable				() const;
			// check if object is currently invulnerable
			bool				invulnerable						() const;
			// toggle object invulnerability
			void				invulnerable						(bool invulnerable);
			// lost alpha start
			u16					GetTorchBatteryStatus				();
			void				SetTorchBatteryStatus				(u16 value);
			void				SetTorchState						(bool state);
			bool				GetTorchState						(void);
			void				SetVisual						(LPCSTR visual);
			LPCSTR				GetVisual						() const;

			float				SetActorWalkAccel					(float new_value);
			float				GetActorMaxWeight				();	
			void				SetActorMaxWeight					(float value);
			float				GetActorWalkWeight				();	
			void				SetActorWalkWeight				(float value);
			void				SetActorHitImmunity				(float value);
			void				SetActorCamDispersionCoeff			(float value);
			void				SetActorZoomInertCoeff				(float value);
			void				SetActorPriceFactor				(float value);
			void				SetActorSprintFactor				(float value);

			void				OpenSafe();

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
add_to_type_list(CScriptGameObject)
#undef script_type_list
#define script_type_list save_type_list(CScriptGameObject)

extern void sell_condition	(luabind::object);
extern void sell_condition	(CScriptIniFile *ini_file, LPCSTR section);
extern void sell_condition	(float friend_factor, float enemy_factor);
extern void buy_condition	(luabind::object);
extern void buy_condition	(CScriptIniFile *ini_file, LPCSTR section);
extern void buy_condition	(float friend_factor, float enemy_factor);
extern void show_condition	(CScriptIniFile *ini_file, LPCSTR section);