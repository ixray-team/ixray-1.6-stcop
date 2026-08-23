////////////////////////////////////////////////////////////////////////////
//	Module 		: alife_space.h
//	Created 	: 08.01.2002
//  Modified 	: 08.01.2003
//	Author		: Dmitriy Iassenev
//	Description : ALife space
////////////////////////////////////////////////////////////////////////////

#ifndef XRAY_ALIFE_SPACE
#define XRAY_ALIFE_SPACE
//#include "../xrcore/_std_extensions.h"

// ALife objects, events and tasks
#define ALIFE_VERSION				0x0006
#define ALIFE_CHUNK_DATA			0x0000
#define SPAWN_CHUNK_DATA			0x0001
#define OBJECT_CHUNK_DATA			0x0002
#define GAME_TIME_CHUNK_DATA		0x0005
#define REGISTRY_CHUNK_DATA			0x0009
#define MARSHAL_CHUNK_DATA			0x000A
#define SECTION_HEADER				"location_"

#define IXRAY_DEF_SAVE_EXTENSION ".scop"

#define SPAWN_NAME					"game.spawn"
// inventory rukzak size
#define MAX_ITEM_VOLUME				100
#define INVALID_STORY_ID			ALife::_STORY_ID(-1)
#define INVALID_SPAWN_STORY_ID		ALife::_SPAWN_STORY_ID(-1)

class CSE_ALifeDynamicObject;
class CSE_ALifeMonsterAbstract;
class CSE_ALifeTrader;
class CSE_ALifeInventoryItem;
class CSE_ALifeItemWeapon;
class CSE_ALifeSchedulable;
class CGameGraph;

namespace ALife
{
	using _CLASS_ID = u64;		 // Class ID
	using _OBJECT_ID = u16;		 // Object ID
	using _TIME_ID = u64;		 // Time  ID
	using _EVENT_ID = u32;		 // Event ID
	using _TASK_ID = u32;		 // Event ID
	using _SPAWN_ID = u16;		 // Spawn ID
	using _TERRAIN_ID = u16;	 // Terrain ID
	using _STORY_ID = u32;		 // Story ID
	using _SPAWN_STORY_ID = u32; // Spawn Story ID

	constexpr _OBJECT_ID _ACTOR_ID = 0; // Actor net id is always 0.

	struct SSumStackCell {
		int						i1;
		int						i2;
		int						iCurrentSum;
	};

	enum ECombatResult {
		eCombatResultRetreat1 = u32(0),
		eCombatResultRetreat2,
		eCombatResultRetreat12,
		eCombatResult1Kill2,
		eCombatResult2Kill1,
		eCombatResultBothKilled,
		eCombatDummy = u32(-1),
	};

	enum ECombatAction {
		eCombatActionAttack = u32(0),
		eCombatActionRetreat,
		eCombatActionDummy = u32(-1),
	};

	enum EMeetActionType {
		eMeetActionTypeAttack = u32(0),
		eMeetActionTypeInteract,
		eMeetActionTypeIgnore,
		eMeetActionSmartTerrain,
		eMeetActionTypeDummy = u32(-1),
	};

	enum ERelationType {
		eRelationTypeFriend = u32(0),
		eRelationTypeNeutral,
		eRelationTypeEnemy,
		eRelationTypeWorstEnemy,
		eRelationTypeLast,
		eRelationTypeDummy = u32(-1),
	};

	enum EHitType {
		eHitTypeBurn = u32(0),
		eHitTypeShock,
		eHitTypeChemicalBurn,
		eHitTypeRadiation,
		eHitTypeTelepatic,
		eHitTypeWound,
		eHitTypeFireWound,
		eHitTypeStrike,
		eHitTypeExplosion,
		eHitTypeWound_2,		//knife's alternative fire
		eHitTypePhysicStrike,
		eHitTypeLightBurn,
		eHitTypeMax,
	};

	enum EInfluenceType	{
		infl_rad = u32(0),
		infl_fire,
		infl_acid,
		infl_psi,
		infl_electra,
		infl_max_count
	};

	enum EConditionRestoreType {
		eHealthRestoreSpeed = u32(0),
		eSatietyRestoreSpeed,
		eThirstRestoreSpeed,
		ePowerRestoreSpeed,
		eBleedingRestoreSpeed,
		eRadiationRestoreSpeed,
		eRestoreTypeMax,
	};

	enum ETakeType {
		eTakeTypeAll,
		eTakeTypeMin,
		eTakeTypeRest,
	};

	enum EWeaponPriorityType {
		eWeaponPriorityTypeKnife = u32(0),
		eWeaponPriorityTypeSecondary,
		eWeaponPriorityTypePrimary,
		eWeaponPriorityTypeGrenade,
		eWeaponPriorityTypeDummy = u32(-1),
	};

	enum ECombatType {
		eCombatTypeMonsterMonster = u32(0),
		eCombatTypeMonsterAnomaly,
		eCombatTypeAnomalyMonster,
		eCombatTypeSmartTerrain,
		eCombatTypeDummy = u32(-1),
	};

	//возможность подключения аддонов
	enum EWeaponAddonStatus{
		eAddonDisabled				= 0,	//нельзя присоеденить
		eAddonPermanent				= 1,	//постоянно подключено по умолчанию
		eAddonAttachable			= 2		//можно присоединять
	};

	IC EHitType	g_tfString2HitType(const char* caHitType)
	{
		if (!_stricmp(caHitType,"burn"))
			return(eHitTypeBurn);
		else if (!_stricmp(caHitType,"light_burn"))
			return(eHitTypeLightBurn);
		else if (!_stricmp(caHitType,"shock"))
				return(eHitTypeShock);
		else if (!_stricmp(caHitType,"strike"))
            	return(eHitTypeStrike);
		else if (!_stricmp(caHitType,"wound"))
				return(eHitTypeWound);
		else if (!_stricmp(caHitType,"radiation"))
				return(eHitTypeRadiation);
		else if (!_stricmp(caHitType,"telepatic"))
				return(eHitTypeTelepatic);
		else if (!_stricmp(caHitType,"fire_wound"))
				return(eHitTypeFireWound);
		else if (!_stricmp(caHitType,"chemical_burn"))
				return(eHitTypeChemicalBurn);
		else if (!_stricmp(caHitType,"explosion"))
				return(eHitTypeExplosion);
		else if (!_stricmp(caHitType,"wound_2"))
				return(eHitTypeWound_2);
		else if (!_stricmp(caHitType, "physic_strike"))
			return eHitTypePhysicStrike;
		else
				FATAL	("Unsupported hit type!");
		NODEFAULT;
		return(eHitTypeMax);
	}

	extern ENGINE_API xr_token hit_types_token[ ];

	IC const char* g_cafHitType2String(EHitType tHitType)
	{
		return get_token_name(hit_types_token, tHitType);
	}

	using INT_VECTOR = xr_vector<int>;
	using INT_IT = INT_VECTOR::iterator;

	using OBJECT_VECTOR = xr_vector<_OBJECT_ID>;
	using OBJECT_IT = OBJECT_VECTOR::iterator;

	using ITEM_P_VECTOR = xr_vector<CSE_ALifeInventoryItem*>;
	using ITEM_P_IT = ITEM_P_VECTOR::iterator;

	using WEAPON_P_VECTOR = xr_vector<CSE_ALifeItemWeapon*>;
	using WEAPON_P_IT = WEAPON_P_VECTOR::iterator;

	using SCHEDULE_P_VECTOR = xr_vector<CSE_ALifeSchedulable*>;
	using SCHEDULE_P_IT = SCHEDULE_P_VECTOR::iterator;

	using D_OBJECT_P_MAP = xr_map<_OBJECT_ID, CSE_ALifeDynamicObject*>;
	using D_OBJECT_P_PAIR_IT = D_OBJECT_P_MAP::iterator;
	
	using STORY_P_MAP = xr_map<_STORY_ID, CSE_ALifeDynamicObject*>;
	using STORY_P_PAIR_IT = STORY_P_MAP::iterator;
};

#endif //XRAY_ALIFE_SPACE