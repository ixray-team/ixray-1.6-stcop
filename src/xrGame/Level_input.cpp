#include "stdafx.h"
#include "../xrEngine/xr_ioconsole.h"
#include "entity_alive.h"
#include "game_sv_single.h"
#include "alife_simulator.h"
#include "alife_simulator_header.h"
#include "level_graph.h"
#include "../xrEngine/fdemorecord.h"
#include "Level.h"
#include "../xrEngine/xr_level_controller.h"
#include "game_cl_base.h"
#include "stalker_movement_manager_smart_cover.h"
#include "Inventory.h"
#include "xrServer.h"
#include "autosave_manager.h"

#include "Actor.h"
#include "huditem.h"
#include "UIGameCustom.h"
#include "../../xrUI/Widgets/UIDialogWnd.h"
#include "../xrEngine/xr_input.h"
#include "saved_game_wrapper.h"

#include "../Include/xrRender/DebugRender.h"

#ifdef DEBUG
#	include "ai/monsters/BaseMonster/base_monster.h"

// Lain: add
#   include "level_debug.h"
#endif

#ifdef DEBUG
	extern void try_change_current_entity();
	extern void restore_actor();
#endif

bool g_bDisableAllInput = false;
extern	float	g_fTimeFactor;

#define CURRENT_ENTITY()	(game?((IsGameTypeSingle()) ? CurrentEntity() : CurrentControlEntity()):nullptr)

void CLevel::IR_OnMouseWheel( int direction )
{
	if(	g_bDisableAllInput	) return;

	/* avo: script callback */
	if (g_actor) g_actor->callback(GameObject::eMouseWheel)(direction);
	/* avo: end */

	if (CurrentGameUI()->IR_UIOnMouseWheel(direction)) return;
	if( Device.Paused()
#ifdef DEBUG
		&& !psActorFlags.test(AF_NO_CLIP) 
#endif //DEBUG
		) return;

	if (CURRENT_ENTITY())		
	{
		IInputReceiver*		IR	= smart_cast<IInputReceiver*>	(smart_cast<CGameObject*>(CURRENT_ENTITY()));
		if (IR)				IR->IR_OnMouseWheel(direction);
	}
}

static int mouse_button_2_key[] = { MOUSE_1,MOUSE_2,MOUSE_3, MOUSE_4 , MOUSE_5 };

void CLevel::IR_OnMousePress(int btn)
{	IR_OnKeyboardPress(mouse_button_2_key[btn]);}
void CLevel::IR_OnMouseRelease(int btn)
{	IR_OnKeyboardRelease(mouse_button_2_key[btn]);}
void CLevel::IR_OnMouseHold(int btn)
{	IR_OnKeyboardHold(mouse_button_2_key[btn]);}

void CLevel::IR_OnMouseMove( int dx, int dy )
{
	if(g_bDisableAllInput)							return;

	/* avo: script callback */
	if (g_actor) g_actor->callback(GameObject::eMouseMove)(dx, dy);
	/* avo: end */

	if (CurrentGameUI()->IR_UIOnMouseMove(dx,dy))		return;
	if (Device.Paused() && !IsDemoPlay() 
#ifdef DEBUG
		&& !psActorFlags.test(AF_NO_CLIP) 
#endif //DEBUG
		)	return;
	if (CURRENT_ENTITY())		
	{
		IInputReceiver*		IR	= smart_cast<IInputReceiver*>	(smart_cast<CGameObject*>(CURRENT_ENTITY()));
		if (IR)				IR->IR_OnMouseMove					(dx,dy);
	}
}

// Обработка нажатия клавиш
extern bool g_block_pause;

// Lain: added TEMP!!!
extern float g_separate_factor;
extern float g_separate_radius;

#include <luabind/functor.hpp>
#include "../xrScripts/script_engine.h"
#include "ai_space.h"
#include "../xrEngine/string_table.h"

void CLevel::IR_OnKeyboardPress	(int key)
{
	auto _curr = get_binded_action(key);
	if (_curr != kNOTBINDED) {
		if (is_block_action(static_cast<int>(_curr))) {
			return;
		}
	}

	if(Device.dwPrecacheFrame)
		return;

	bool b_ui_exist = (!!CurrentGameUI());

	if(_curr==kPAUSE)
	{
		if (!g_block_pause && (IsGameTypeSingle() || IsDemoPlay()))
		{
#ifdef DEBUG
			if(psActorFlags.test(AF_NO_CLIP))
				Device.Pause(!Device.Paused(), TRUE, TRUE, "li_pause_key_no_clip");
			else
#endif //DEBUG
				Device.Pause(!Device.Paused(), TRUE, TRUE, "li_pause_key");
		}
		return;
	}

	if(	g_bDisableAllInput )	return;

	if (g_actor)
		g_actor->callback(GameObject::eKeyPress)(key);

	switch ( _curr ) 
	{
	case kSCREENSHOT:
		Render->Screenshot();
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
	};

	if ( !bReady || !b_ui_exist )			return;

	if ( b_ui_exist && CurrentGameUI()->IR_UIOnKeyboardPress(key)) return;

	if ( Device.Paused() && !IsDemoPlay() 
#ifdef DEBUG
		&& !psActorFlags.test(AF_NO_CLIP) 
#endif //DEBUG
		)	return;

	if ( game && game->OnKeyboardPress(get_binded_action(key)) )	return;

	if(_curr == kQUICK_SAVE && IsGameTypeSingle())
	{
		Console->Execute			("save");
		return;
	}
	if(_curr == kQUICK_LOAD && IsGameTypeSingle())
	{
#ifdef DEBUG
		FS.get_path					("$game_config$")->m_Flags.set(FS_Path::flNeedRescan, TRUE);
		FS.get_path					("$game_scripts$")->m_Flags.set(FS_Path::flNeedRescan, TRUE);
		FS.rescan_pathes			();
#endif // DEBUG
		string_path					saved_game,command;
		xr_strconcat(saved_game, Core.UserName, " - ", g_pStringTable->translate("quicksave").c_str());
		if (!CSavedGameWrapper::valid_saved_game(saved_game))
			return;

		xr_strconcat(command,"load ",saved_game);
		Console->Execute			(command);
		return;
	}

#ifndef MASTER_GOLD
	switch (key) {
	case SDL_SCANCODE_F7: {
		if (!IsGameTypeSingle()) return;
		FS.get_path					("$game_config$")->m_Flags.set(FS_Path::flNeedRescan, TRUE);
		FS.get_path					("$game_scripts$")->m_Flags.set(FS_Path::flNeedRescan, TRUE);
		FS.rescan_pathes			();
		NET_Packet					net_packet;
		net_packet.w_begin			(M_RELOAD_GAME);
		Send						(net_packet,net_flags(TRUE));
		return;
	}
	case SDL_SCANCODE_KP_DIVIDE: {
		if (!Server)
			break;

		SetGameTimeFactor			(g_fTimeFactor);

#ifdef DEBUG
		if(!m_bEnvPaused)
			SetEnvironmentGameTimeFactor(GetEnvironmentGameTime(), g_fTimeFactor);
#else //DEBUG
		SetEnvironmentGameTimeFactor(GetEnvironmentGameTime(), g_fTimeFactor);
#endif //DEBUG
		
		break;	
	}
	case SDL_SCANCODE_KP_MULTIPLY: {
		if (!Server)
			break;

		SetGameTimeFactor			(1000.f);
#ifdef DEBUG
		if(!m_bEnvPaused)
			SetEnvironmentGameTimeFactor(GetEnvironmentGameTime(), 1000.f);
#else //DEBUG
		SetEnvironmentGameTimeFactor(GetEnvironmentGameTime(), 1000.f);
#endif //DEBUG
		
		break;
	}
#ifdef DEBUG
	case SDL_SCANCODE_KP_MINUS:{
		if (!Server)
			break;
		if(m_bEnvPaused)
			SetEnvironmentGameTimeFactor(GetEnvironmentGameTime(), g_fTimeFactor);
		else
			SetEnvironmentGameTimeFactor(GetEnvironmentGameTime(), 0.00001f);

		m_bEnvPaused = !m_bEnvPaused;
		break;
	}
#endif //DEBUG
	case SDL_SCANCODE_KP_5:
		{
			if (GameID()!=eGameIDSingle) 
			{
				Msg("For this game type Demo Record is disabled.");
///				return;
			};
			if(!pInput->iGetAsyncKeyState(SDL_SCANCODE_LSHIFT))
			{
				Console->Hide	();
				Console->Execute("demo_record 1");
			}
		}
		break;

#ifdef DEBUG_DRAW
	case SDL_SCANCODE_RETURN:
	{
		if (!Console->bVisible)
		{
			bDebug = !bDebug;
		}

		return;
	}
#endif

#ifdef DEBUG

	// Lain: added TEMP!!!
	case SDL_SCANCODE_UP:
	{
		g_separate_factor /= 0.9f;
		break;
	}
	case SDL_SCANCODE_DOWN:
	{
		g_separate_factor *= 0.9f;
		if ( g_separate_factor < 0.1f )
		{
			g_separate_factor = 0.1f;
		}
		break;
	}
	case SDL_SCANCODE_LEFT:
	{
		g_separate_radius *= 0.9f;
		if ( g_separate_radius < 0 )
		{
			g_separate_radius = 0;
		}
		break;
	}
	case SDL_SCANCODE_RIGHT:
	{
		g_separate_radius /= 0.9f;
		break;
	}
	case SDL_SCANCODE_BACKSPACE:
		if (IsGameTypeSingle())
			DRender->NextSceneMode();
			//HW.Caps.SceneMode			= (HW.Caps.SceneMode+1)%3;
		return;

	case SDL_SCANCODE_F4: {
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
				if (tpEntityAlive) {
					bOk = true;
					break;
				}
			}
			if (!bOk)
				for (i = 0; i <j; ++i) {
					CEntityAlive* tpEntityAlive = smart_cast<CEntityAlive*>(Objects.o_get_by_iterator(i));
					if (tpEntityAlive) {
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
	// Lain: added
	case SDL_SCANCODE_F5: 
	{
		if ( CBaseMonster* pBM = smart_cast<CBaseMonster*>(CurrentEntity()))
		{
			DBG().log_debug_info();			
		}
		break;
	}

	case MOUSE_1: {
		if (!IsGameTypeSingle())
			break;

		if (pInput->iGetAsyncKeyState(SDL_SCANCODE_LALT)) {
			if (smart_cast<CActor*>(CurrentEntity()))
				try_change_current_entity	();
			else
				restore_actor				();
			return;
		}
		break;
	}
	/**/
#endif
#ifdef DEBUG
	case SDL_SCANCODE_F9:{
//		if (!ai().get_alife())
//			break;
//		const_cast<CALifeSimulatorHeader&>(ai().alife().header()).set_state(ALife::eZoneStateSurge);
		break;
	}
		return;
#endif // DEBUG
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
	auto bind = get_binded_action(key);
	if (bind != kNOTBINDED) {
		if (is_block_action(static_cast<int>(bind))) {
			return;
		}
	}

	if (!bReady || g_bDisableAllInput	)								return;
	if ( CurrentGameUI() && CurrentGameUI()->IR_UIOnKeyboardRelease(key)) return;
	if (game && game->OnKeyboardRelease(get_binded_action(key)) )		return;
	if (Device.Paused() 
#ifdef DEBUG
		&& !psActorFlags.test(AF_NO_CLIP)
#endif //DEBUG
		)				return;

	if (g_actor)
		g_actor->callback(GameObject::eKeyRelease)(key);

	if (CURRENT_ENTITY())		
	{
		IInputReceiver*		IR	= smart_cast<IInputReceiver*>	(smart_cast<CGameObject*>(CURRENT_ENTITY()));
		if (IR != nullptr) {
			IR->IR_OnKeyboardRelease(bind);
		}
	}
}

void CLevel::IR_OnKeyboardHold(int key)
{
	auto bind = get_binded_action(key);
	if (bind != kNOTBINDED) {
		if (is_block_action(static_cast<int>(bind))) {
			return;
		}
	}
	if(g_bDisableAllInput) return;

	if (g_actor)
		g_actor->callback(GameObject::eKeyHold)(key);

#ifdef DEBUG
	// Lain: added
	if ( key == SDL_SCANCODE_UP )
	{
		static u32 time = Device.dwTimeGlobal;
		if ( Device.dwTimeGlobal - time > 20 )
		{
			if ( CBaseMonster* pBM = smart_cast<CBaseMonster*>(CurrentEntity()) )
			{
				DBG().debug_info_up();
				time = Device.dwTimeGlobal;
			}
		}
	}
	else if ( key == SDL_SCANCODE_DOWN )
	{
		static u32 time = Device.dwTimeGlobal;
		if ( Device.dwTimeGlobal - time > 20 )
		{
			if ( CBaseMonster* pBM = smart_cast<CBaseMonster*>(CurrentEntity()) )
			{
				DBG().debug_info_down();
				time = Device.dwTimeGlobal;
			}
		}
	}

#endif // DEBUG

	if (CurrentGameUI() && CurrentGameUI()->IR_UIOnKeyboardHold(key)) return;
	if (Device.Paused() && !Level().IsDemoPlay() 
#ifdef DEBUG
		&& !psActorFlags.test(AF_NO_CLIP)
#endif //DEBUG
		) return;
	if (CURRENT_ENTITY())		{
		IInputReceiver*		IR	= smart_cast<IInputReceiver*>	(smart_cast<CGameObject*>(CURRENT_ENTITY()));
		if (IR != nullptr) {
			IR->IR_OnKeyboardHold(bind);
		}
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

void CLevel::IR_GamepadUpdateStick(int id, Fvector2 value)
{
	if (g_bDisableAllInput)
		return;

	if (Device.Paused())
		return;

	if (g_actor != nullptr && g_actor->g_Alive())
	{
		g_actor->IR_GamepadUpdateStick(id, value);
	}
}

void CLevel::IR_GamepadKeyPress(int id)
{
	if (g_bDisableAllInput)
		return;

	if (Device.Paused())
		return;

	// TODO
	//if (CurrentGameUI() && CurrentGameUI()->IR_GamepadKeyPress(key)) return;

	if (g_actor != nullptr && g_actor->g_Alive())
	{
		g_actor->IR_GamepadKeyPress(id);
	}
}

void CLevel::block_action(int cmd) {
	++blocked_bings[cmd];
}

void CLevel::unblock_action(int cmd) {
	--blocked_bings[cmd];
}

bool CLevel::is_block_action(int cmd) {
	return blocked_bings[cmd] > 0;
}

void CLevel::LockActorWithCameraRotation() {
	for (int i = kJUMP; i < kLASTACTION; ++i) {
		block_action(i);
	}
}

void CLevel::UnLockActor() {
	for (int i = 0; i < kLASTACTION; ++i) {
		unblock_action(i);
	}
}
