#include "stdafx.h"
#include "base_monster.h"
#include "../../../level.h"
#include "../../../level_debug.h"
#include "../../../entitycondition.h"
#include "../../../ai_debug.h"
#include "../state_defs.h"
#include "../state_manager.h"
#include "../../../phmovementcontrol.h"
#include "../../../characterphysicssupport.h"
#include "../../../actor.h"

#ifdef DEBUG
CBaseMonster::SDebugInfo CBaseMonster::show_debug_info()
{
	if (!g_Alive()) return SDebugInfo();

	if (m_show_debug_info == 0) {
		DBG().text(this).clear();
		return SDebugInfo();
	}

	float y				= 200;
	float x				= (m_show_debug_info == 1) ? 40.f : float(::Render->getTarget()->get_width() / 2) + 40.f;
	const float delta_y	= 12;

	string256	text;

	u32			color			= D3DCOLOR_XRGB(0,255,0);
	u32			delimiter_color	= D3DCOLOR_XRGB(0,0,255);

	DBG().text(this).clear	 ();
	DBG().text(this).add_item("---------------------------------------", x, y+=delta_y, delimiter_color);

	xr_sprintf(text, "-- Monster : [%s]  Current Time = [%u]", *cName(), Device.dwTimeGlobal);
	DBG().text(this).add_item(text, x, y+=delta_y, color);
	DBG().text(this).add_item("-----------   PROPERTIES   ------------", x, y+=delta_y, delimiter_color);

	xr_sprintf(text, "Health = [%f]", conditions().GetHealth());
	DBG().text(this).add_item(text,										 x, y+=delta_y, color);

	xr_sprintf(text, "Morale = [%f]", Morale.get_morale());
	DBG().text(this).add_item(text,										 x, y+=delta_y, color);


	DBG().text(this).add_item("-----------   MEMORY   ----------------", x, y+=delta_y, delimiter_color);

	if (EnemyMan.get_enemy()) {
		xr_sprintf(text, "Current Enemy = [%s]", *EnemyMan.get_enemy()->cName());
	} else 
		xr_sprintf(text, "Current Enemy = [NONE]");
	DBG().text(this).add_item(text,										 x, y+=delta_y, color);
	
	if (EnemyMan.get_enemy()) {
		xr_sprintf(text, "SeeEnemy[%u] EnemySeeMe[%u] TimeLastSeen[%u]", EnemyMan.see_enemy_now(),EnemyMan.enemy_see_me_now(),EnemyMan.get_enemy_time_last_seen());
		DBG().text(this).add_item(text,									x, y+=delta_y, color);
	}

	if (CorpseMan.get_corpse()) {
		xr_sprintf(text, "Current Corpse = [%s] Satiety = [%.2f]", *CorpseMan.get_corpse()->cName(), GetSatiety());
	} else 
		xr_sprintf(text, "Current Corpse = [NONE] Satiety = [%.2f]", GetSatiety());

	DBG().text(this).add_item(text,										 x, y+=delta_y, color);

	// Sound
	if (SoundMemory.IsRememberSound()) {
		SoundElem	sound_elem;
		bool		dangerous_sound;
		SoundMemory.GetSound(sound_elem, dangerous_sound);

		string128	s_type;

		switch(sound_elem.type){
			case WEAPON_SHOOTING:			xr_strcpy(s_type,"WEAPON_SHOOTING"); break;
			case MONSTER_ATTACKING:			xr_strcpy(s_type,"MONSTER_ATTACKING"); break;
			case WEAPON_BULLET_RICOCHET:	xr_strcpy(s_type,"WEAPON_BULLET_RICOCHET"); break;
			case WEAPON_RECHARGING:			xr_strcpy(s_type,"WEAPON_RECHARGING"); break;

			case WEAPON_TAKING:				xr_strcpy(s_type,"WEAPON_TAKING"); break;
			case WEAPON_HIDING:				xr_strcpy(s_type,"WEAPON_HIDING"); break;
			case WEAPON_CHANGING:			xr_strcpy(s_type,"WEAPON_CHANGING"); break;
			case WEAPON_EMPTY_CLICKING:		xr_strcpy(s_type,"WEAPON_EMPTY_CLICKING"); break;

			case MONSTER_DYING:				xr_strcpy(s_type,"MONSTER_DYING"); break;
			case MONSTER_INJURING:			xr_strcpy(s_type,"MONSTER_INJURING"); break;
			case MONSTER_WALKING:			xr_strcpy(s_type,"MONSTER_WALKING"); break;
			case MONSTER_JUMPING:			xr_strcpy(s_type,"MONSTER_JUMPING"); break;
			case MONSTER_FALLING:			xr_strcpy(s_type,"MONSTER_FALLING"); break;
			case MONSTER_TALKING:			xr_strcpy(s_type,"MONSTER_TALKING"); break;

			case DOOR_OPENING:				xr_strcpy(s_type,"DOOR_OPENING"); break;
			case DOOR_CLOSING:				xr_strcpy(s_type,"DOOR_CLOSING"); break;
			case OBJECT_BREAKING:			xr_strcpy(s_type,"OBJECT_BREAKING"); break;
			case OBJECT_FALLING:			xr_strcpy(s_type,"OBJECT_FALLING"); break;
			case NONE_DANGEROUS_SOUND:		xr_strcpy(s_type,"NONE_DANGEROUS_SOUND"); break;
		}

		if (sound_elem.who)
			xr_sprintf(text,"Sound: type[%s] time[%u] power[%.3f] val[%i] src[+]", s_type, sound_elem.time, sound_elem.power, sound_elem.value);
		else 
			xr_sprintf(text,"Sound: type[%s] time[%u] power[%.3f] val[%i] src[?]", s_type, sound_elem.time, sound_elem.power, sound_elem.value);


	} else 
		xr_sprintf(text, "Sound: NONE");

	DBG().text(this).add_item(text,										 x, y+=delta_y, color);

	// Hit
	if (HitMemory.is_hit()) {
		if (HitMemory.get_last_hit_object()) {
			xr_sprintf(text,"Hit Info: object=[%s] time=[%u]", *(HitMemory.get_last_hit_object()->cName()), HitMemory.get_last_hit_time());
		} else {
			xr_sprintf(text,"Hit Info: object=[NONE] time=[%u]", HitMemory.get_last_hit_time());
		}
	} else 
		xr_sprintf(text, "Hit Info: NONE");

	DBG().text(this).add_item(text,										 x, y+=delta_y, color);

	DBG().text(this).add_item("-----------   MOVEMENT   ------------", x, y+=delta_y, delimiter_color);

	xr_sprintf(text, "Actual = [%u] Enabled = [%u]",			 control().path_builder().actual(), control().path_builder().enabled());
	DBG().text(this).add_item(text,										x, y+=delta_y, color);
	
	xr_sprintf(text, "Speed: Linear = [%.3f] Angular = [%.3f]", control().movement().velocity_current(), 0.f);
	DBG().text(this).add_item(text,										x, y+=delta_y, color);
	
	DBG().text(this).add_item("------- Attack Distances -------------", x, y+=delta_y, delimiter_color);
	xr_sprintf(text, "MinDist[%.3f] MaxDist[%.3f] As_Step[%.3f] As_MinDist[%.3f]", 
		MeleeChecker.get_min_distance(),
		MeleeChecker.get_max_distance(),
		MeleeChecker.dbg_as_step(),
		MeleeChecker.dbg_as_min_dist()
	);
	DBG().text(this).add_item(text,										x, y+=delta_y, color);


	if (EnemyMan.get_enemy()) {
		xr_sprintf(text, "Current Enemy = [%s]", *EnemyMan.get_enemy()->cName());
	} else 
		xr_sprintf(text, "Current Enemy = [NONE]");
	DBG().text(this).add_item(text,										 x, y+=delta_y, color);



	return SDebugInfo(x, y, delta_y, color, delimiter_color);
}

void CBaseMonster::debug_fsm()
{
	if (!g_Alive()) return;

	if (!psAI_Flags.test(aiMonsterDebug)) {
		DBG().object_info(this,this).clear ();
		return;
	}
	
	EMonsterState state = StateMan->get_state_type();
	
	string128 st;

	switch (state) {
		case eStateRest_WalkGraphPoint:					xr_sprintf(st,"Rest :: Walk Graph");			break;
		case eStateRest_Idle:							xr_sprintf(st,"Rest :: Idle");					break;
		case eStateRest_Fun:							xr_sprintf(st,"Rest :: Fun");					break;
		case eStateRest_Sleep:							xr_sprintf(st,"Rest :: Sleep");				break;
		case eStateRest_MoveToHomePoint:				xr_sprintf(st,"Rest :: MoveToHomePoint");		break;
		case eStateRest_WalkToCover:					xr_sprintf(st,"Rest :: WalkToCover");			break;
		case eStateRest_LookOpenPlace:					xr_sprintf(st,"Rest :: LookOpenPlace");		break;

		case eStateEat_CorpseApproachRun:				xr_sprintf(st,"Eat :: Corpse Approach Run");	break;
		case eStateEat_CorpseApproachWalk:				xr_sprintf(st,"Eat :: Corpse Approach Walk");	break;
		case eStateEat_CheckCorpse:						xr_sprintf(st,"Eat :: Check Corpse");			break;
		case eStateEat_Eat:								xr_sprintf(st,"Eat :: Eating");				break;
		case eStateEat_WalkAway:						xr_sprintf(st,"Eat :: Walk Away");				break;
		case eStateEat_Rest:							xr_sprintf(st,"Eat :: Rest After Meal");		break;
		case eStateEat_Drag:							xr_sprintf(st,"Eat :: Drag");					break;
		
		case eStateAttack_Run:							xr_sprintf(st,"Attack :: Run");				break;
		case eStateAttack_Melee:						xr_sprintf(st,"Attack :: Melee");				break;
		case eStateAttack_RunAttack:					xr_sprintf(st,"Attack :: Run Attack");			break;
		case eStateAttack_RunAway:						xr_sprintf(st,"Attack :: Run Away");			break;
		case eStateAttack_FindEnemy:					xr_sprintf(st,"Attack :: Find Enemy");			break;
		case eStateAttack_Steal:						xr_sprintf(st,"Attack :: Steal");				break;
		case eStateAttack_AttackHidden:					xr_sprintf(st,"Attack :: Attack Hidden");		break;
		
		case eStateAttackCamp_Hide:						xr_sprintf(st,"Attack Camp:: Hide");			break;
		case eStateAttackCamp_Camp:						xr_sprintf(st,"Attack Camp:: Camp");			break;
		case eStateAttackCamp_StealOut:					xr_sprintf(st,"Attack Camp:: Steal Out");		break;

		case eStateAttack_HideInCover:					xr_sprintf(st,"Attack :: Hide In Cover");		break;
		case eStateAttack_MoveOut:						xr_sprintf(st,"Attack :: Move Out From Cover");break;
		case eStateAttack_CampInCover:					xr_sprintf(st,"Attack :: Camp In Cover");		break;

		case eStateAttack_Psy:							xr_sprintf(st,"Attack :: Psy");				break;
		case eStateAttack_MoveToHomePoint:				xr_sprintf(st,"Attack :: Move To Home Point");	break;
		case eStateAttack_HomePoint_Hide:				xr_sprintf(st,"Attack :: Home Point :: Hide");	break;
		case eStateAttack_HomePoint_Camp:				xr_sprintf(st,"Attack :: Home Point :: Camp");	break;
		case eStateAttack_HomePoint_LookOpenPlace:		xr_sprintf(st,"Attack :: Home Point :: Look Open Place");	break;
		
		case eStatePanic_Run:							xr_sprintf(st,"Panic :: Run Away");				break;
		case eStatePanic_FaceUnprotectedArea:			xr_sprintf(st,"Panic :: Face Unprotected Area");	break;
		case eStatePanic_HomePoint_Hide:				xr_sprintf(st,"Panic :: Home Point :: Hide");		break;
		case eStatePanic_HomePoint_LookOpenPlace:		xr_sprintf(st,"Panic :: Home Point :: Look Open Place");	break;
		case eStatePanic_HomePoint_Camp:				xr_sprintf(st,"Panic :: Home Point :: Camp");		break;

		case eStateHitted_Hide:							xr_sprintf(st,"Hitted :: Hide");					break;
		case eStateHitted_MoveOut:						xr_sprintf(st,"Hitted :: MoveOut");				break;
		case eStateHitted_Home:							xr_sprintf(st,"Hitted :: Home");				break;

		case eStateHearDangerousSound_Hide:				xr_sprintf(st,"Dangerous Snd :: Hide");			break;
		case eStateHearDangerousSound_FaceOpenPlace:	xr_sprintf(st,"Dangerous Snd :: FaceOpenPlace");	break;
		case eStateHearDangerousSound_StandScared:		xr_sprintf(st,"Dangerous Snd :: StandScared");		break;
		case eStateHearDangerousSound_Home:				xr_sprintf(st,"Dangerous Snd :: Home");			break;

		case eStateHearInterestingSound_MoveToDest:		xr_sprintf(st,"Interesting Snd :: MoveToDest");	break;
		case eStateHearInterestingSound_LookAround:		xr_sprintf(st,"Interesting Snd :: LookAround");	break;
		
		case eStateHearHelpSound:						xr_sprintf(st,"Hear Help Sound");	break;
		case eStateHearHelpSound_MoveToDest:			xr_sprintf(st,"Hear Help Sound :: MoveToDest");	break;
		case eStateHearHelpSound_LookAround:			xr_sprintf(st,"Hear Help Sound :: LookAround");	break;

		case eStateControlled_Follow_Wait:				xr_sprintf(st,"Controlled :: Follow : Wait");			break;
		case eStateControlled_Follow_WalkToObject:		xr_sprintf(st,"Controlled :: Follow : WalkToObject");	break;
		case eStateControlled_Attack:					xr_sprintf(st,"Controlled :: Attack");					break;
		case eStateThreaten:							xr_sprintf(st,"Threaten :: ");							break;
		case eStateFindEnemy_Run:						xr_sprintf(st,"Find Enemy :: Run");							break;
		case eStateFindEnemy_LookAround_MoveToPoint:	xr_sprintf(st,"Find Enemy :: Look Around : Move To Point");	break;
		case eStateFindEnemy_LookAround_LookAround:		xr_sprintf(st,"Find Enemy :: Look Around : Look Around");		break;
		case eStateFindEnemy_LookAround_TurnToPoint:	xr_sprintf(st,"Find Enemy :: Look Around : Turn To Point");	break;
		case eStateFindEnemy_Angry:						xr_sprintf(st,"Find Enemy :: Angry");							break;
		case eStateFindEnemy_WalkAround:				xr_sprintf(st,"Find Enemy :: Walk Around");					break;
		case eStateSquad_Rest_Idle:						xr_sprintf(st,"Squad :: Rest : Idle");					break;
		case eStateSquad_Rest_WalkAroundLeader:			xr_sprintf(st,"Squad :: Rest : WalkAroundLeader");		break;
		case eStateSquad_RestFollow_Idle:				xr_sprintf(st,"Squad :: Follow Leader : Idle");		break;
		case eStateSquad_RestFollow_WalkToPoint:		xr_sprintf(st,"Squad :: Follow Leader : WalkToPoint");	break;
		case eStateCustom_Vampire:						xr_sprintf(st,"Attack :: Vampire");					break;
		case eStateVampire_ApproachEnemy:				xr_sprintf(st,"Vampire :: Approach to enemy");			break;
		case eStateVampire_Execute:						xr_sprintf(st,"Vampire :: Hit");						break;
		case eStateVampire_RunAway:						xr_sprintf(st,"Vampire :: Run Away");					break;
		case eStateVampire_Hide:						xr_sprintf(st,"Vampire :: Hide");						break;
		case eStatePredator:							xr_sprintf(st,"Predator");								break;
		case eStatePredator_MoveToCover:				xr_sprintf(st,"Predator :: MoveToCover");				break;
		case eStatePredator_LookOpenPlace:				xr_sprintf(st,"Predator :: Look Open Place");			break;
		case eStatePredator_Camp:						xr_sprintf(st,"Predator :: Camp");						break;
		case eStateBurerAttack_Tele:					xr_sprintf(st,"Attack :: Telekinesis");			break;
		case eStateBurerAttack_Gravi:					xr_sprintf(st,"Attack :: Gravi Wave");				break;
		case eStateBurerAttack_RunAround:				xr_sprintf(st,"Attack :: Run Around");			break;
		case eStateBurerAttack_FaceEnemy:				xr_sprintf(st,"Attack :: Face Enemy");			break;
		case eStateBurerAttack_Melee:					xr_sprintf(st,"Attack :: Melee");				break;
		case eStateBurerScanning:						xr_sprintf(st,"Attack :: Scanning");			break;
		case eStateGhostBossAttack_Tele:				xr_sprintf(st,"GhostBoss::Attack :: Telekinesis");			break;
		case eStateGhostBossAttack_Gravi:				xr_sprintf(st,"GhostBoss::Attack :: Gravi Wave");				break;
		case eStateGhostBossAttack_RunAround:			xr_sprintf(st,"GhostBoss::Attack :: Run Around");			break;
		case eStateGhostBossAttack_FaceEnemy:			xr_sprintf(st,"GhostBoss::Attack :: Face Enemy");			break;
		case eStateGhostBossAttack_Melee:				xr_sprintf(st,"GhostBoss::Attack :: Melee");				break;
		case eStateGhostBossScanning:					xr_sprintf(st,"GhostBoss::Attack :: Scanning");			break;
		case eStateCustomMoveToRestrictor:				xr_sprintf(st,"Moving To Restrictor :: Position not accessible");	break;
		case eStateSmartTerrainTask:					xr_sprintf(st,"ALIFE");	break;
		case eStateSmartTerrainTaskGamePathWalk:		xr_sprintf(st,"ALIFE :: Game Path Walk");	break;
		case eStateSmartTerrainTaskLevelPathWalk:		xr_sprintf(st,"ALIFE :: Level Path Walk");	break;
		case eStateSmartTerrainTaskWaitCapture:			xr_sprintf(st,"ALIFE :: Wait till smart terrain will capture me");	break;
		case eStateUnknown:								xr_sprintf(st,"Unknown State :: ");			break;
		default:										xr_sprintf(st,"Undefined State ::");			break;
	}
	
	DBG().object_info(this,this).remove_item (u32(0));
	DBG().object_info(this,this).remove_item (u32(1));
	DBG().object_info(this,this).remove_item (u32(2));

	DBG().object_info(this,this).add_item	 (*cName(), D3DCOLOR_XRGB(255,0,0), 0);
	DBG().object_info(this,this).add_item	 (st, D3DCOLOR_XRGB(255,0,0), 1);
	
	xr_sprintf(st, "Team[%u]Squad[%u]Group[%u]", g_Team(), g_Squad(), g_Group());
	DBG().object_info(this,this).add_item	 (st, D3DCOLOR_XRGB(255,0,0), 2);

	CEntityAlive *entity = smart_cast<CEntityAlive *>(Level().CurrentEntity());
	if (entity && entity->character_physics_support()->movement()) {
		xr_sprintf(st,"VELOCITY [%f,%f,%f] Value[%f]",VPUSH(entity->character_physics_support()->movement()->GetVelocity()),entity->character_physics_support()->movement()->GetVelocityActual());
		DBG().text(this).clear();
		DBG().text(this).add_item(st,200,100,COLOR_GREEN,100);
	}
}


#endif
