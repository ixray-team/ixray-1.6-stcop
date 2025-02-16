#include "stdafx.h"
#include "HUDmanager.h"
#include "../xr_ioconsole.h"
#include "entity_alive.h"
#include "game_sv_single.h"
#include "alife_simulator.h"
#include "alife_simulator_header.h"
#include "level_graph.h"
#include "../fdemorecord.h"
#include "level.h"
#include "../xrEngine/xr_level_controller.h"
#include "game_cl_base.h"
#include "stalker_movement_manager.h"
#include "Inventory.h"
#include "player_hud.h"
#include "xrServer.h"
#include "autosave_manager.h"
#include "actor.h"
#include "huditem.h"
#include "UIGameCustom.h"
#include "ui/UIDialogWnd.h"
#include "pch_script.h"
#include "ui/UIGameTutorial.h"
#include "clsid_game.h"
#include "../xr_input.h"
#include "saved_game_wrapper.h"
#include "ai_space.h"
#include "../xrScripts/script_engine.h"
#include "UI/UIinventoryutilities.h"

#ifdef DEBUG
#	include "ai/monsters/BaseMonster/base_monster.h"
#endif
#include "../../Include/xrRender/DebugRender.h"

#ifdef LOG_PLANNER
	extern void try_change_current_entity();
	extern void restore_actor();
#endif

bool g_bDisableAllInput			= false;
bool g_bDisableKeyboardInput	= false;
extern	float	g_fTimeFactor;

extern int quick_save_counter;
extern int max_quick_saves;

#define CURRENT_ENTITY()	(game?((GameID() == GAME_SINGLE) ? CurrentEntity() : CurrentControlEntity()):NULL)

void CLevel::IR_OnMouseWheel( int direction )
{
	if(	g_bDisableAllInput	) return;

	if (CurrentGameUI()->IR_UIOnMouseWheel(direction)) return;

	if( Device.Paused()		) return;
	if (CURRENT_ENTITY())		{
			IInputReceiver*		IR	= smart_cast<IInputReceiver*>	(smart_cast<CGameObject*>(CURRENT_ENTITY()));
			if (IR)				IR->IR_OnMouseWheel(direction);
		}

}

static int mouse_button_2_key []	=	{MOUSE_1,MOUSE_2,MOUSE_3};

void CLevel::IR_OnMousePress(int btn)
{	IR_OnKeyboardPress(mouse_button_2_key[btn]);}
void CLevel::IR_OnMouseRelease(int btn)
{	IR_OnKeyboardRelease(mouse_button_2_key[btn]);}
void CLevel::IR_OnMouseHold(int btn)
{	IR_OnKeyboardHold(mouse_button_2_key[btn]);}

void CLevel::IR_OnMouseMove( int dx, int dy )
{
	if(g_bDisableAllInput)						return;
	if (CurrentGameUI()->IR_UIOnMouseMove(dx,dy))		return;
	if (Device.Paused())							return;
	if (CURRENT_ENTITY())		{
		IInputReceiver*		IR	= smart_cast<IInputReceiver*>	(smart_cast<CGameObject*>(CURRENT_ENTITY()));
		if (IR)				IR->IR_OnMouseMove					(dx,dy);
	}
}

extern CUISequencer* g_tutorial;

#include "../igame_persistent.h"


void CLevel::IR_OnKeyboardPress	(int key)
{
	bool b_ui_exist = (!!CurrentGameUI());

//.	if (DIK_F10 == key)		vtune.enable();
//.	if (DIK_F11 == key)		vtune.disable();
//	Msg("CLevel::IR_OnKeyboardPress(%d)", key);


	EGameActions _curr = get_binded_action(key);
	switch ( _curr ) 
	{

	case kSCREENSHOT:
		string256 scr_additional_name;

		xr_sprintf(scr_additional_name, sizeof(scr_additional_name), "w_%s_to_%s_gt_%s", g_pGamePersistent->Environment().Current[0]->m_identifier.c_str(),
		g_pGamePersistent->Environment().Current[1]->m_identifier.c_str(), InventoryUtilities::GetGameTimeAsString(InventoryUtilities::etpTimeToMinutes, '-').c_str());

		Msg("^ Additional screenshot descr.: %s", scr_additional_name);

		Render->Screenshot(IRender_interface::SM_NORMAL, scr_additional_name);
		return;
		break;

	case kCONSOLE:
		Console->Show				();
		return;
		break;

	case kQUIT: 
		{
			if(b_ui_exist && CurrentGameUI()->TopInputReceiver() )
			{
					if(CurrentGameUI()->IR_UIOnKeyboardPress(key))	return;//special case for mp and main_menu
					CurrentGameUI()->TopInputReceiver()->HideDialog();
			}else
			{
				Console->Execute("main_menu");
			}return;
		}break;
	case kPAUSE:
			Device.Pause(!Device.Paused(), TRUE, TRUE, "li_pause_key");
		return;

		break;
/*	
	case DIK_DIVIDE:
		if( OnServer() ){
			if (GameID() == GAME_SINGLE)
				Server->game->SetGameTimeFactor(g_fTimeFactor);
			else
			{
				Server->game->SetEnvironmentGameTimeFactor(g_fTimeFactor);
				Server->game->SetGameTimeFactor(g_fTimeFactor);
			};
		}
	
		break;	

case DIK_MULTIPLY:
		if( OnServer() ){
			float NewTimeFactor				= 1000.f;
			if (GameID() == GAME_SINGLE)
				Server->game->SetGameTimeFactor(NewTimeFactor);
			else
			{
				Server->game->SetEnvironmentGameTimeFactor(NewTimeFactor);
			};
		}

		break;
*/

	};
	
	if(	g_bDisableAllInput || g_bDisableKeyboardInput)	return;
	if ( !bReady || !b_ui_exist )			return;

	if ( b_ui_exist && CurrentGameUI()->IR_UIOnKeyboardPress(key)) return;

	if( Device.Paused() )		return;

	if ( game && game->OnKeyboardPress(get_binded_action(key)) )	return;


	if(_curr == kQUICK_SAVE && IsGameTypeSingle())
	{
		luabind::functor<bool> lua_bool;
		bool script_handeled = false;

		R_ASSERT2(ai().script_engine().functor("la_input.quick_save", lua_bool), "la_input.quick_save");

		if (lua_bool.is_valid()){
			script_handeled = lua_bool();
		}

		if (script_handeled) { Msg("# Quick Save handeled by script"); return; } // Return if script handeled quick save


		if (Actor() && Actor()->b_saveAllowed && Actor()->g_Alive())
		{
			quick_save_counter++;
			if (quick_save_counter > max_quick_saves) //bimd the index to max_quick_saves
			{
				quick_save_counter = 0;
			}

			string_path					saved_game, command;

			xr_sprintf(saved_game, "%s_quicksave_%i", Core.UserName, quick_save_counter); //make a savefile name using user name and save index

			xr_sprintf(command, "save %s", saved_game); //make a command for console
			Msg("%s", command); //temporary

			Console->Execute(command);
		}
		else
		{
			// Calls refuse message on hud from script
			luabind::functor<void> lua_func;
			R_ASSERT2(ai().script_engine().functor("la_input.quicksave_refuse", lua_func), "Can't find la_input.quicksave_refuse");
			lua_func();
		}
		return;
	}


	if(_curr == kQUICK_LOAD && IsGameTypeSingle())
	{
#ifdef DEBUG
		FS.get_path					("$game_config$")->m_Flags.set(FS_Path::flNeedRescan, TRUE);
		FS.get_path					("$game_scripts$")->m_Flags.set(FS_Path::flNeedRescan, TRUE);
		FS.rescan_pathes			();
#endif // DEBUG

		luabind::functor<bool> lua_bool;
		bool script_handeled = false;

		R_ASSERT2(ai().script_engine().functor("la_input.quick_load", lua_bool), "la_input.quick_load");

		if (lua_bool.is_valid()){
			script_handeled = lua_bool();
		}

		if (script_handeled) { Msg("# Quick Load handeled by script"); return; } // Return if script handeled quick load


		string_path					saved_game, command;

		xr_sprintf(saved_game, "%s_quicksave_%i", Core.UserName, quick_save_counter); //make a savefile name using user name and save index

		if (!CSavedGameWrapper::valid_saved_game(saved_game)){
			Msg("!Invalid save %s", saved_game);
			return;
		}

		if (g_tutorial && g_tutorial->IsActive()) {
			g_tutorial->Stop();
		}

		xr_sprintf(command, "load %s", saved_game); //make a command for console
		Msg("%s", command); //temporary

		Console->Execute(command);
		return;
	}


#ifndef MASTER_GOLD
	switch (key) {
	case SDL_SCANCODE_KP_5:
		{
			if (GameID() != GAME_SINGLE) 
			{
				Msg("For this game type Demo Record is disabled.");
///				return;
			};
			Console->Hide	();
			Console->Execute("demo_record 1");
		}
		break;
#endif // MASTER_GOLD

#ifdef DEBUG
	case SDL_SCANCODE_RETURN:
			bDebug	= !bDebug;
		return;

	case SDL_SCANCODE_BACKSPACE:
		if (GameID() == GAME_SINGLE)
			DRender->NextSceneMode();
		return;

	case SDL_SCANCODE_KP_4: {
		if (pInput->iGetAsyncKeyState(SDL_SCANCODE_LALT))
			break;

		if (pInput->iGetAsyncKeyState(SDL_SCANCODE_RALT))
			break;

		bool bOk = false;
		u32 i=0, j, n=Objects.o_count();
		if (pCurrentEntity)
			for ( ; i<n; ++i)
				if (Objects.o_get_by_iterator(i) == pCurrentEntity)
					break;
		if (i < n) {
			j = i;
			bOk = false;
			for (++i; i <n; ++i) {
				CEntityAlive* tpEntityAlive = smart_cast<CEntityAlive*>(Objects.o_get_by_iterator(i));
				if (tpEntityAlive && tpEntityAlive->g_Alive()) {
					bOk = true;
					break;
				}
			}
			if (!bOk)
				for (i = 0; i <j; ++i) {
					CEntityAlive* tpEntityAlive = smart_cast<CEntityAlive*>(Objects.o_get_by_iterator(i));
					if (tpEntityAlive && tpEntityAlive->g_Alive()) {
						bOk = true;
						break;
					}
				}
			if (bOk) {
				CObject *tpObject = CurrentEntity();
				CObject *__I = Objects.o_get_by_iterator(i);
				CObject **I = &__I;
				
				SetEntity(*I);
				if (tpObject != *I)
				{
					CActor* pActor = smart_cast<CActor*> (tpObject);
					if (pActor)
						pActor->inventory().Items_SetCurrentEntityHud(false);
				}
				if (tpObject)
				{
					Engine.Sheduler.Unregister	(tpObject);
					Engine.Sheduler.Register	(tpObject, TRUE);
				};
				Engine.Sheduler.Unregister	(*I);
				Engine.Sheduler.Register	(*I, TRUE);

				CActor* pActor = smart_cast<CActor*> (*I);
				if (pActor)
				{
					pActor->inventory().Items_SetCurrentEntityHud(true);

					CHudItem* pHudItem = smart_cast<CHudItem*>(pActor->inventory().ActiveItem());
					if (pHudItem) 
					{
						pHudItem->OnStateSwitch(pHudItem->GetState());
					}
				}
			}
		}
		return;
	}
#ifdef LOG_PLANNER
	case MOUSE_1: {
		if (GameID() != GAME_SINGLE)
			break;
		if (pInput->iGetAsyncKeyState(DIK_LALT)) {
			if (CurrentEntity()->CLS_ID == CLSID_OBJECT_ACTOR)
				try_change_current_entity	();
			else
				restore_actor				();
			return;
		}
		break;
	}
#endif  
				/*
	case DIK_DIVIDE:
		if( OnServer() ){
//			float NewTimeFactor				= pSettings->r_float("alife","time_factor");
			
			if (GameID() == GAME_SINGLE)
				Server->game->SetGameTimeFactor(g_fTimeFactor);
			else
			{
				Server->game->SetEnvironmentGameTimeFactor(g_fTimeFactor);
				Server->game->SetGameTimeFactor(g_fTimeFactor);
			};
		}
		break;	
	case DIK_MULTIPLY:
		if( OnServer() ){
			float NewTimeFactor				= 1000.f;
			if (GameID() == GAME_SINGLE)
				Server->game->SetGameTimeFactor(NewTimeFactor);
			else
			{
				Server->game->SetEnvironmentGameTimeFactor(NewTimeFactor);
//				Server->game->SetGameTimeFactor(NewTimeFactor);
			};
		}
		break;
	/**/
				
#endif
#ifdef DEBUG
//#ifdef NDEBUG
	case SDL_SCANCODE_F9:{
//		if (!ai().get_alife())
//			break;
//		const_cast<CALifeSimulatorHeader&>(ai().alife().header()).set_state(ALife::eZoneStateSurge);
		if (GameID() != GAME_SINGLE)
		{
			extern INT g_sv_SendUpdate;
			g_sv_SendUpdate = 1;
		};
		break;
	}
		return;
//	case DIK_F10:{
//		ai().level_graph().set_dest_point();
//		ai().level_graph().build_detail_path();
//		if (!Objects.FindObjectByName("m_stalker_e0000") || !Objects.FindObjectByName("localhost/dima"))
//			return;
//		if (!m_bSynchronization) {
//			m_bSynchronization	= true;
//			ai().level_graph().set_start_point();
//			m_bSynchronization	= false;
//		}
//		luabind::functor<void>	functor;
//		ai().script_engine().functor("alife_test.set_switch_online",functor);
//		functor(0,false);
//	}
//		return;
//	case DIK_F11:
//		ai().level_graph().build_detail_path();
//		if (!Objects.FindObjectByName("m_stalker_e0000") || !Objects.FindObjectByName("localhost/dima"))
//			return;
//		if (!m_bSynchronization) {
//			m_bSynchronization	= true;
//			ai().level_graph().set_dest_point();
//			ai().level_graph().select_cover_point();
//			m_bSynchronization	= false;
//		}
//		return;
#endif // DEBUG
#ifndef MASTER_GOLD
	}
#endif // MASTER_GOLD

	if (bindConsoleCmds.execute(key))
		return;

	if (CURRENT_ENTITY())
	{
			IInputReceiver*		IR	= smart_cast<IInputReceiver*>	(smart_cast<CGameObject*>(CURRENT_ENTITY()));
			if (IR)				IR->IR_OnKeyboardPress(get_binded_action(key));
	}


	#ifdef _DEBUG
		CObject *obj = Level().Objects.FindObjectByName("monster");
		if (obj) {
			CBaseMonster *monster = smart_cast<CBaseMonster *>(obj);
			if (monster) 
				monster->debug_on_key(key);
		}
	#endif
}

void CLevel::IR_OnKeyboardRelease(int key)
{
	if (!bReady || g_bDisableAllInput	)								return;
	if ( CurrentGameUI() && CurrentGameUI()->IR_UIOnKeyboardRelease(key)) return;
	if (game && game->OnKeyboardRelease(get_binded_action(key)) )		return;
	if (Device.Paused() )				return;

	if (CURRENT_ENTITY())		
	{
		IInputReceiver*		IR	= smart_cast<IInputReceiver*>	(smart_cast<CGameObject*>(CURRENT_ENTITY()));
		if (IR)				IR->IR_OnKeyboardRelease			(get_binded_action(key));
	}
}

void CLevel::IR_OnKeyboardHold(int key)
{
	if (g_bDisableAllInput || g_bDisableKeyboardInput) return;
	if (CurrentGameUI() && CurrentGameUI()->IR_UIOnKeyboardHold(key)) return;
	if (Device.Paused() && !Level().IsDemoPlay() ) return;
	if (CURRENT_ENTITY())
	{
		IInputReceiver*		IR	= smart_cast<IInputReceiver*>	(smart_cast<CGameObject*>(CURRENT_ENTITY()));
		if (IR)				IR->IR_OnKeyboardHold				(get_binded_action(key));
	}
}

void CLevel::IR_OnMouseStop( int /**axis/**/, int /**value/**/)
{
}

void CLevel::IR_OnActivate()
{
	if(!pInput) return;
	int i;
	for (i = 0; i < CInput::COUNT_KB_BUTTONS; i++ )
	{
		if(IR_GetKeyState(i))
		{

			EGameActions action = get_binded_action(i);
			switch (action){
			case kFWD			:
			case kBACK			:
			case kL_STRAFE		:
			case kR_STRAFE		:
			case kLEFT			:
			case kRIGHT			:
			case kUP			:
			case kDOWN			:
			case kCROUCH		:
			case kACCEL			:
			case kL_LOOKOUT		:
			case kR_LOOKOUT		:	
			case kWPN_FIRE		:
				{
					IR_OnKeyboardPress	(i);
				}break;
			};
		};
	}
}