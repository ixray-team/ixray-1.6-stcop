//////////////////////////////////////////////////////////////////////////
// relation_registry.h: реестр для хранения данных об отношении персонажа к 
//						другим персонажам
//////////////////////////////////////////////////////////////////////////

#pragma once

class CRelationRegistryWrapper;

class CInventoryOwner;
class CEntityAlive;

//////////////////////////////////////////////////////////////////////////

#define GAME_RELATIONS_SECT "game_relations"
#define ACTIONS_POINTS_SECT "action_points"

//////////////////////////////////////////////////////////////////////////
template<typename T>
concept RELATION_REGISTRY_TemplateType =
	requires(T a)
{
	std::is_pointer_v<T>;
	{ a->object_id() } -> std::same_as<ALife::_OBJECT_ID>;
	{ a->Reputation() } -> std::same_as<s32>;
	{ a->Rank() } -> std::same_as<s32>;
	{ a->Community() } -> std::same_as<s32>;
};


struct RELATION_REGISTRY 
{
public:
	RELATION_REGISTRY  ();
	virtual ~RELATION_REGISTRY ();

public:	

	template<RELATION_REGISTRY_TemplateType T>
	ALife::ERelationType GetRelationBetween(T char1,T char2) const;

	template<RELATION_REGISTRY_TemplateType T>
	ALife::ERelationType GetRelationType(T from, T to) const ;
	template<RELATION_REGISTRY_TemplateType T>
	void SetRelationType(T from, T to, ALife::ERelationType new_relation);

	//общее отношение  одного персонажа к другому, вычисленное по формуле
	//с учетом всех факторов - величина от 
	//-100< (крайне враждебное) до >100 (очень дрюжелюбное)
	
	template<RELATION_REGISTRY_TemplateType T>
	s32	 GetAttitude(T from, T to) const ;

	//личное отношение (благосклонность) одного персонажа к другому - 
	//величина от -100< (крайне враждебное) до >100 (очень дрюжелюбное)
	s32	 GetGoodwill				(ALife::_OBJECT_ID from, ALife::_OBJECT_ID to) const ;
	void				 SetGoodwill				(ALife::_OBJECT_ID from, ALife::_OBJECT_ID to, s32 goodwill);
	void				 ForceSetGoodwill			(ALife::_OBJECT_ID from, ALife::_OBJECT_ID to, s32 goodwill);
	void				 ChangeGoodwill 			(ALife::_OBJECT_ID from, ALife::_OBJECT_ID to, s32 delta_goodwill);

	//отношения группировки к персонажу (именно так, а не наоборот)
	//т.е. персонаж сам помнит, как к нему какая группировка отностися
	s32	 GetCommunityGoodwill		(s32 from_community, ALife::_OBJECT_ID to_character) const ;
	void				 SetCommunityGoodwill		(s32 from_community, ALife::_OBJECT_ID to_character, s32 goodwill);
	void				 ChangeCommunityGoodwill	(s32 from_community, ALife::_OBJECT_ID to_character, s32 delta_goodwill);
	
	void				 ClearRelations				(ALife::_OBJECT_ID person_id);

	s32	 GetCommunityRelation		(s32, s32) const;	
	void				 SetCommunityRelation		(s32 index1, s32 index2, s32 goodwill);

private:
	s32	 GetRankRelation			(s32, s32) const;
	s32	 GetReputationRelation		(s32, s32) const;


	//реакцией на действия персонажей и соответствующее изменение отношения
public:
	
	//список действий актера, за которые начисляются
	//очки рейтинга, репутации или меняется отношения персонажа
	//к группировке
	enum ERelationAction
	{
		KILL				= 0x00,		//убийство персонажа
		ATTACK				= 0x01,		//атака персонажа
		FIGHT_HELP_HUMAN	= 0x02,		//помощь в драке персонажу с другим персонажем
		FIGHT_HELP_MONSTER	= 0x04,		//помощь в драке персонажу c монстром
		SOS_HELP			= 0x08		//приход на помощь по сигналу SOS
	};
	void Action (CEntityAlive* from, CEntityAlive* to, ERelationAction action);
	
public:	

	struct FIGHT_DATA
	{
		FIGHT_DATA			();
		ALife::_OBJECT_ID					attacker;
		ALife::_OBJECT_ID					defender;
		float				total_hit;
		u32					time;
		u32					time_old;
		
		u32						attack_time;			//время фиксирования события "атака"
		ALife::ERelationType	defender_to_attacker;	//как относился атакованый к нападавшему во время начальной атаки
	};

	struct RELATION_MAP_SPOTS
	{
		RELATION_MAP_SPOTS	();
		shared_str			spot_names[ALife::eRelationTypeLast+1];
		const shared_str&	GetSpotName (ALife::ERelationType& type){
									if(type<ALife::eRelationTypeLast)return spot_names[type];
									else return spot_names[ALife::eRelationTypeLast];};
	};
	//зарегистрировать драку (реакция на Hit в EntityAlive)
	void FightRegister (ALife::_OBJECT_ID attacker, ALife::_OBJECT_ID defender, ALife::ERelationType defender_to_attacker, float hit_amount);
	void UpdateFightRegister ();

private:
	using FIGHT_VECTOR = xr_vector<FIGHT_DATA>;
	using FIGHT_VECTOR_IT = FIGHT_VECTOR::iterator;

	static FIGHT_VECTOR*						m_fight_registry;
	static FIGHT_VECTOR&						fight_registry();
	
	FIGHT_DATA*									FindFight(ALife::_OBJECT_ID object_id, bool by_attacker/* = true*/);
	static RELATION_MAP_SPOTS*					m_spot_names;
public:
	const shared_str&							GetSpotName			(ALife::ERelationType& type);
	static CRelationRegistryWrapper&			relation_registry();
	static void									clear_relation_registry();
private:
	static CRelationRegistryWrapper				*m_relation_registry;
};

//////////////////////////////////////////////////////////////////////////
template<RELATION_REGISTRY_TemplateType T>
ALife::ERelationType RELATION_REGISTRY::GetRelationBetween		(T char1,T char2) const 
{
	ALife::ERelationType rel12 = GetRelationType(char1, char2);
	ALife::ERelationType rel21 = GetRelationType(char2, char1);

	if(ALife::eRelationTypeEnemy == rel12 || ALife::eRelationTypeEnemy == rel21)
		return ALife::eRelationTypeEnemy;
	else if(ALife::eRelationTypeNeutral == rel12 || ALife::eRelationTypeNeutral == rel21)
		return ALife::eRelationTypeNeutral;
	else
		return ALife::eRelationTypeFriend;
}

template<RELATION_REGISTRY_TemplateType T>
void				 RELATION_REGISTRY::SetRelationType		(T from, T to, ALife::ERelationType new_relation)
{
	static int goodwill_enemy	= pSettings->r_s16(GAME_RELATIONS_SECT, "goodwill_enemy");
	static int goodwill_neutral = pSettings->r_s16(GAME_RELATIONS_SECT, "goodwill_neutal");
	static int goodwill_friend	= pSettings->r_s16(GAME_RELATIONS_SECT, "goodwill_friend");

	switch(new_relation)
	{
	case ALife::eRelationTypeEnemy:
		SetGoodwill(from->object_id(), to->object_id(), goodwill_enemy);
		break;
	case ALife::eRelationTypeNeutral:
		SetGoodwill(from->object_id(), to->object_id(), goodwill_neutral);
		break;
	case ALife::eRelationTypeFriend:
		SetGoodwill(from->object_id(), to->object_id(), goodwill_friend);
		break;
	default:
		NODEFAULT;
	}
}

template<RELATION_REGISTRY_TemplateType T>
ALife::ERelationType RELATION_REGISTRY::GetRelationType		(T from, T to) const 
{
	static int attitude_neutral = pSettings->r_s16(GAME_RELATIONS_SECT, "attitude_neutal_threshold");
	static int attitude_friend = pSettings->r_s16(GAME_RELATIONS_SECT, "attitude_friend_threshold");

	s32 attitude = GetAttitude(from, to);

	if(attitude == -type_max(s32))
		return ALife::eRelationTypeNeutral;

	if(attitude<attitude_neutral)
		return ALife::eRelationTypeEnemy;
	else if(attitude<attitude_friend)
		return ALife::eRelationTypeNeutral;
	else
		return ALife::eRelationTypeFriend;
}


//////////////////////////////////////////////////////////////////////////
template<RELATION_REGISTRY_TemplateType T>
s32	 RELATION_REGISTRY::GetAttitude	(T from, T to) const 
{
	//личное отношение from к to
	s32 presonal_goodwill		= GetGoodwill(from->object_id(), to->object_id()); VERIFY(presonal_goodwill != -type_max(s32));
	//влияние репутации персонажей
	s32 reputation_goodwill		= GetReputationRelation(from->Reputation(), to->Reputation());
	//влияние рангов персонажей
	s32 rank_goodwill			= GetRankRelation(from->Rank(), to->Rank());


	//отношение группировки from персонально к to
	s32 community_goodwill		= GetCommunityGoodwill(from->Community(), to->object_id()); VERIFY(community_goodwill != -type_max(s32));
	//отношение группировки from к группировки to
	s32 community_to_community	= GetCommunityRelation(from->Community(), to->Community());

	s32 attitude = presonal_goodwill + 
		reputation_goodwill +
		rank_goodwill +
		community_goodwill + 
		community_to_community;

	return attitude;
}