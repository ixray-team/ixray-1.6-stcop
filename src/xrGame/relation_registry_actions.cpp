#include "StdAfx.h"
#include "pch_script.h"
#include "relation_registry.h"
#include "alife_registry_wrappers.h"

#include "Actor.h"
#include "ai/stalker/ai_stalker.h"

#include "seniority_hierarchy_holder.h"
#include "team_hierarchy_holder.h"
#include "squad_hierarchy_holder.h"
#include "group_hierarchy_holder.h"

#include "memory_manager.h"
#include "enemy_manager.h"

struct SAttackGoodwillStorage
{
	s32	friend_attack_goodwill;
	s32	neutral_attack_goodwill;
	s32	enemy_attack_goodwill;
	s32	community_member_attack_goodwill;

	s32	friend_attack_reputation;
	s32	neutral_attack_reputation;
	s32	enemy_attack_reputation;
	
	void load(const char* prefix)
	{
		string128					s;
		xr_strconcat(s,prefix,"friend_attack_goodwill");
		friend_attack_goodwill		= pSettings->r_s32(ACTIONS_POINTS_SECT, s);

		xr_strconcat(s,prefix,"neutral_attack_goodwill");
		neutral_attack_goodwill		= pSettings->r_s32(ACTIONS_POINTS_SECT, s);

		xr_strconcat(s,prefix,"enemy_attack_goodwill");
		enemy_attack_goodwill		= pSettings->r_s32(ACTIONS_POINTS_SECT, s);

		xr_strconcat(s,prefix,"community_member_attack_goodwill");
		community_member_attack_goodwill = READ_IF_EXISTS(pSettings, r_s32, ACTIONS_POINTS_SECT, s, 0);


		xr_strconcat(s,prefix,"friend_attack_reputation");
		friend_attack_reputation	= pSettings->r_s32(ACTIONS_POINTS_SECT, s);

		xr_strconcat(s,prefix,"neutral_attack_reputation");
		neutral_attack_reputation	= pSettings->r_s32(ACTIONS_POINTS_SECT, s);
		
		xr_strconcat(s,prefix,"enemy_attack_reputation");
		enemy_attack_reputation		= pSettings->r_s32(ACTIONS_POINTS_SECT, s);
	}
};
SAttackGoodwillStorage gw_danger,gw_free;

void load_attack_goodwill()
{
	gw_danger.load("danger_");
	gw_free.load("free_");
}

void RELATION_REGISTRY::Action (CEntityAlive* from, CEntityAlive* to, ERelationAction action)
{
	if (!from || !to) return;
	static s32 friend_kill_goodwill				= pSettings->r_s32(ACTIONS_POINTS_SECT, "friend_kill_goodwill");
	static s32 neutral_kill_goodwill				= pSettings->r_s32(ACTIONS_POINTS_SECT, "neutral_kill_goodwill");
	static s32 enemy_kill_goodwill				= pSettings->r_s32(ACTIONS_POINTS_SECT, "enemy_kill_goodwill");
	static s32 community_member_kill_goodwill	= pSettings->r_s32(ACTIONS_POINTS_SECT, "community_member_kill_goodwill");

	static s32 friend_kill_reputation	= pSettings->r_s32(ACTIONS_POINTS_SECT, "friend_kill_reputation");
	static s32 neutral_kill_reputation	= pSettings->r_s32(ACTIONS_POINTS_SECT, "neutral_kill_reputation");
	static s32 enemy_kill_reputation		= pSettings->r_s32(ACTIONS_POINTS_SECT, "enemy_kill_reputation");

	//(с) мин. время через которое снова будет зарегестрировано сообщение об атаке на персонажа
	static u32	 min_attack_delta_time							= u32(1000.f * pSettings->r_float(ACTIONS_POINTS_SECT, "min_attack_delta_time"));

	static s32 friend_fight_help_goodwill		= pSettings->r_s32(ACTIONS_POINTS_SECT, "friend_fight_help_goodwill");
	static s32 neutral_fight_help_goodwill		= pSettings->r_s32(ACTIONS_POINTS_SECT, "neutral_fight_help_goodwill");
	static s32 enemy_fight_help_goodwill			= pSettings->r_s32(ACTIONS_POINTS_SECT, "enemy_fight_help_goodwill");
	static s32 community_member_fight_help_goodwill	= READ_IF_EXISTS(pSettings, r_s32, ACTIONS_POINTS_SECT, "community_member_fight_help_goodwill", 1000);

	static s32 friend_fight_help_reputation	= pSettings->r_s32(ACTIONS_POINTS_SECT, "friend_fight_help_reputation");
	static s32 neutral_fight_help_reputation = pSettings->r_s32(ACTIONS_POINTS_SECT, "neutral_fight_help_reputation");
	static s32 enemy_fight_help_reputation	= pSettings->r_s32(ACTIONS_POINTS_SECT, "enemy_fight_help_reputation");

	CActor*				actor			= from->cast_actor();
	CInventoryOwner*	inv_owner_from	= from->cast_inventory_owner();
	CAI_Stalker*		stalker_from	= from->cast_stalker();
	CAI_Stalker*		stalker			= to->cast_stalker();

	//вычисление изменения репутации и рейтинга пока ведется 
	//только для актера
	if(!inv_owner_from || from->cast_base_monster()) return;
	
	ALife::ERelationType relation = ALife::eRelationTypeDummy;
	if(stalker)
	{
		stalker->m_actor_relation_flags.set(action, TRUE);
		relation = GetRelationType(stalker->cast_inventory_owner(), inv_owner_from);
	}

	switch(action)
	{
	case ATTACK:
		{
			if(actor)
			{
				//учитывать ATTACK и FIGHT_HELP, только если прошло время
				//min_attack_delta_time 
				FIGHT_DATA* fight_data_from = FindFight (from->ID(), true);
				if(Device.dwTimeGlobal - fight_data_from->attack_time < min_attack_delta_time)
					break;

				fight_data_from->attack_time = Device.dwTimeGlobal;

				//если мы атаковали персонажа или монстра, который 
				//кого-то атаковал, то мы помогли тому, кто защищался
				FIGHT_DATA* fight_data = FindFight (to->ID(),true);
				if(fight_data)
				{
					CObject* OD = Level().Objects.net_Find(fight_data->defender);
					if (!OD) break;
					CGameObject* GOD = OD->cast_game_object();
					if (!GOD) break;
					CAI_Stalker* defending_stalker = GOD->cast_stalker();
					if(defending_stalker)	
					{
						CObject* OA = Level().Objects.net_Find(fight_data->defender);
						if (!OA) break;
						CGameObject* GOA = OA->cast_game_object();
						if (!GOA) break;
						CAI_Stalker*	attacking_stalker = GOA->cast_stalker();
						Action(actor, defending_stalker, attacking_stalker?FIGHT_HELP_HUMAN:FIGHT_HELP_MONSTER);
					}
				}
			}

			if(stalker)
			{
				bool bDangerScheme = false;
				CEntityAlive* stalker_enemy = const_cast<CEntityAlive*>(stalker->memory().enemy().selected());
				if(actor && stalker_enemy)
				{
					CInventoryOwner* const_inv_owner_from				= inv_owner_from;
					if(stalker_enemy->human_being())
					{
						CInventoryOwner* const_inv_owner_stalker_enemy	= stalker_enemy->cast_inventory_owner();
						ALife::ERelationType relation_to_actor = GetRelationType(const_inv_owner_stalker_enemy, const_inv_owner_from);

						if(relation_to_actor == ALife::eRelationTypeEnemy)
							bDangerScheme = true;
					}
				}
				SAttackGoodwillStorage*		st		= bDangerScheme?&gw_danger:&gw_free;

				s32 delta_goodwill		= 0;
				s32	delta_reputation = 0;
				switch (relation)
				{
					case ALife::eRelationTypeEnemy:
						{
							delta_goodwill		= st->enemy_attack_goodwill;
							delta_reputation	= st->enemy_attack_reputation;
						}break;
					case ALife::eRelationTypeNeutral:
						{
							delta_goodwill		= st->neutral_attack_goodwill;
							delta_reputation	= st->neutral_attack_reputation;
						}break;
					case ALife::eRelationTypeFriend:
						{
							delta_goodwill		= st->friend_attack_goodwill;
							delta_reputation	= st->friend_attack_reputation;
						}break;
				};

				//сталкер при нападении на членов своей же группировки отношения не меняют
				//(считается, что такое нападение всегда случайно)
				// change relation only for pairs actor->stalker, do not use pairs stalker->stalker
				bool stalker_attack_team_mate = stalker && stalker_from;
				if (delta_goodwill && !stalker_attack_team_mate) {
					//изменить отношение ко всем членам атакованой группы (если такая есть)
					//как к тому кого атаковали
					CGroupHierarchyHolder& group = Level().seniority_holder().team(stalker->g_Team()).squad(stalker->g_Squad()).group(stalker->g_Group());
					for(std::size_t i = 0;  i < group.members().size(); i++)
					{
						ChangeGoodwill(group.members()[i]->ID(), from->ID(), delta_goodwill);
					}
						
					//*(s32)( stalker->Sympathy() * (float)(delta_goodwill));
					s32 community_goodwill = (s32)( stalker->Sympathy() * (float)(st->community_member_attack_goodwill) );
					if (community_goodwill)
					{
						ChangeCommunityGoodwill(stalker->Community(), from->ID(), community_goodwill);
					}
				}
				if(delta_reputation)
				{
					inv_owner_from->ChangeReputation(delta_reputation);
				}
			}
		}
		break;
	case KILL:
		{
			if(stalker)
			{
				//FIGHT_DATA* fight_data_from = FindFight (from->ID(), true);	
				
				//мы помним то, какое отношение обороняющегося к атакующему
				//было перед началом драки
				ALife::ERelationType relation_before_attack = ALife::eRelationTypeDummy;
				//if(fight_data_from)
				//	relation_before_attack = fight_data_from->defender_to_attacker;
				//else
					relation_before_attack = relation;

				s32			delta_goodwill		= 0;
				s32	delta_reputation	= 0;

				switch (relation_before_attack)
				{
					case ALife::eRelationTypeEnemy:
						{
							delta_goodwill		= enemy_kill_goodwill;
							delta_reputation	= enemy_kill_reputation;
						}break;
					case ALife::eRelationTypeNeutral:
						{
							delta_goodwill		= neutral_kill_goodwill;
							delta_reputation	= neutral_kill_reputation;
						}break;
					case ALife::eRelationTypeFriend:
						{
							delta_goodwill		= friend_kill_goodwill;
							delta_reputation	= friend_kill_reputation;
						}break;
				};

				//сталкер при нападении на членов своей же группировки отношения не меняют
				//(считается, что такое нападение всегда случайно)
				bool stalker_kills_team_mate = stalker_from && (stalker_from->Community() == stalker->Community());

				if(delta_goodwill && !stalker_kills_team_mate)
				{
					//изменить отношение ко всем членам группы (если такая есть)
					//убитого, кроме него самого
					CGroupHierarchyHolder& group = Level().seniority_holder().team(stalker->g_Team()).squad(stalker->g_Squad()).group(stalker->g_Group());
					for(std::size_t i = 0;  i < group.members().size(); i++)
					{
						if(stalker->ID() != group.members()[i]->ID())
						{
							ChangeGoodwill(group.members()[i]->ID(), from->ID(), delta_goodwill);
						}
					}

					//(s32)( stalker->Sympathy() * (float)(delta_goodwill+community_member_kill_goodwill));
					s32 community_goodwill = (s32)( stalker->Sympathy() * (float)(community_member_kill_goodwill) );
					if (community_goodwill)
					{
						ChangeCommunityGoodwill(stalker->Community(), from->ID(), community_goodwill);
					}
				}

				if(delta_reputation)
				{
					inv_owner_from->ChangeReputation(delta_reputation);
				}

				s32		delta_rank = 0;
				delta_rank = CHARACTER_RANK::rank_kill_points(CHARACTER_RANK::ValueToIndex(stalker->Rank()));
				if(delta_rank)
					inv_owner_from->ChangeRank(delta_rank);
			}
		}
		break;
	case FIGHT_HELP_HUMAN:
	case FIGHT_HELP_MONSTER:
		{
			if(stalker && stalker->g_Alive())
			{
				s32			delta_goodwill		= 0;
				s32	delta_reputation	= 0;
				
				switch (relation)
				{
					case ALife::eRelationTypeEnemy:
						{
							delta_goodwill = enemy_fight_help_goodwill;
							delta_reputation = enemy_fight_help_reputation;
						}break;
					case ALife::eRelationTypeNeutral:
						{
							delta_goodwill = neutral_fight_help_goodwill;
							delta_reputation = neutral_fight_help_reputation;
						}break;
					case ALife::eRelationTypeFriend:
						{
							delta_goodwill = friend_fight_help_goodwill;
							delta_reputation = friend_fight_help_reputation;
						}break;
				};

				if(delta_goodwill)
				{
					//изменить отношение ко всем членам атакованой группы (если такая есть)
					//как к тому кого атаковали
					CGroupHierarchyHolder& group = Level().seniority_holder().team(stalker->g_Team()).squad(stalker->g_Squad()).group(stalker->g_Group());
					for(std::size_t i = 0;  i < group.members().size(); i++)
					{
						ChangeGoodwill(group.members()[i]->ID(), from->ID(), delta_goodwill);
					}

//*					ChangeCommunityGoodwill(stalker->Community(), from->ID(), (s32)( stalker->Sympathy() * (float)delta_goodwill ));
					s32 community_goodwill = (s32)( stalker->Sympathy() * (float)(community_member_fight_help_goodwill) );
					if (community_goodwill)
					{
						ChangeCommunityGoodwill(stalker->Community(), from->ID(), community_goodwill);
					}
				}

				if(delta_reputation)
				{
					inv_owner_from->ChangeReputation(delta_reputation);
				}
			}
		}
		break;
	}
}