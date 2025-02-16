#include "pch_script.h"
#include "uigamesp.h"
#include "actor.h"
#include "level.h"
#include "../xr_input.h"

#include "game_cl_Single.h"
#include "ui/UIPdaAux.h"
#include "../xrEngine/xr_level_controller.h"
#include "actorcondition.h"
#include "../xr_ioconsole.h"
#include "../xrCore/object_broker.h"
#include "GameTaskManager.h"
#include "GameTask.h"
#include "inventory.h"
#include "ui/UITradeWnd.h"
#include "ui/UITalkWnd.h"
#include "ui/UIMessageBox.h"
#include "ui/UIInventoryWnd.h"
#include "ui/UIPdaWnd.h"
#include "ui/UICarBodyWnd.h"
#include "ui/UIMainIngameWnd.h"

CUIGameSP::CUIGameSP()
{
	m_game			= NULL;
	StoredInvBox	= NULL;

	TalkMenu		= new CUITalkWnd			();
	UIChangeLevelWnd= new CChangeLevelWnd		();
}

CUIGameSP::~CUIGameSP() 
{	
	delete_data(TalkMenu);
	delete_data(UIChangeLevelWnd);
}

void CUIGameSP::HideShownDialogs()
{
	CUIDialogWnd* mir	= TopInputReceiver();
  	if(mir && (mir==m_InventoryMenu || mir==m_PdaMenu || mir==TalkMenu || mir==m_UICarBodyMenu || mir == m_WeatherEditor))
		mir->HideDialog();
}

void CUIGameSP::SetClGame (game_cl_GameState* g)
{
	inherited::SetClGame				(g);
	m_game = smart_cast<game_cl_Single*>(g);
	R_ASSERT							(m_game);
}
#ifdef DEBUG
	void hud_adjust_mode_keyb(int dik);
	void hud_draw_adjust_mode();
#endif

void hud_draw_adjust_mode();

void CUIGameSP::ReInitShownUI() 
{ 
	if (m_InventoryMenu->IsShown()) 
		m_InventoryMenu->InitInventory_delayed(); 
	else if(m_UICarBodyMenu->IsShown())
		m_UICarBodyMenu->UpdateLists_delayed();
	
};

bool CUIGameSP::IR_UIOnKeyboardPress(int dik) 
{
	if(inherited::IR_UIOnKeyboardPress(dik)) return true;
	if( Device.Paused()		) return false;
	
#ifdef DEBUG
	hud_adjust_mode_keyb	(dik);
#endif

	CInventoryOwner* pInvOwner  = smart_cast<CInventoryOwner*>( Level().CurrentEntity() );
	if ( !pInvOwner )			return false;
	CEntityAlive* EA			= smart_cast<CEntityAlive*>(Level().CurrentEntity());
	if (!EA || !EA->g_Alive() )	return false;

	CActor *pActor = smart_cast<CActor*>(pInvOwner);
	if( !pActor ) 
		return false;

	if( !pActor->g_Alive() )	
		return false;

	if(UIMainIngameWnd->OnKeyboardPress(dik))
		return true;

	switch ( get_binded_action(dik) )
	{
	case kINVENTORY: 
		if((!TopInputReceiver() || TopInputReceiver()==m_InventoryMenu) && !pActor->inventory().IsHandsOnly())
		{
			if (!m_InventoryMenu->IsShown())
				m_InventoryMenu->ShowDialog		(true);
			else
				m_InventoryMenu->HideDialog		();
			break;
		}

	case kACTIVE_JOBS:
		if( !TopInputReceiver() || TopInputReceiver()==m_PdaMenu)
		{
			if (!m_PdaMenu->IsShown())
				m_PdaMenu->ShowDialog		(true, eptQuests);
			else
				m_PdaMenu->HideDialog		();
		}break;

	case kMAP:
		if( !TopInputReceiver() || TopInputReceiver()==m_PdaMenu)
		{
			if (!m_PdaMenu->IsShown())
				m_PdaMenu->ShowDialog		(true, eptMap);
			else
				m_PdaMenu->HideDialog		();
		}break;

	case kCONTACTS:
		if( !TopInputReceiver() || TopInputReceiver()==m_PdaMenu)
		{
			if (!m_PdaMenu->IsShown())
				m_PdaMenu->ShowDialog		(true, eptContacts);
			else
				m_PdaMenu->HideDialog		();
			break;
		}break;

	case kSCORES:
		{
			SDrawStaticStruct* ss	= AddCustomStatic("main_task", true);
			SGameTaskObjective* o	= pActor->GameTaskManager().ActiveObjective();
			if(!o)
				ss->m_static->TextItemControl()->SetTextST	("st_no_active_task");
			else
				ss->m_static->TextItemControl()->SetTextST	(*(o->description));
		}break;
	}

	return false;
}

void CUIGameSP::Render()
{
	inherited::Render();
	hud_draw_adjust_mode();
}

bool CUIGameSP::IR_UIOnKeyboardRelease(int dik) 
{
	if(inherited::IR_UIOnKeyboardRelease(dik)) return true;

	if( is_binded(kSCORES, dik))
		RemoveCustomStatic		("main_task");

	return false;
}

void CUIGameSP::StartTalk(bool disable_break)
{
	RemoveCustomStatic		("main_task");

	TalkMenu->b_disable_break = disable_break;
	TalkMenu->ShowDialog		(true);
}

void CUIGameSP::StartStashUI(CInventoryOwner* pActorInv, CInventoryOwner* pOtherOwner) //Труп, богажник ...
{
	if( TopInputReceiver() )		return;

	m_UICarBodyMenu->InitCustomInventory	(pActorInv, pOtherOwner);
	m_UICarBodyMenu->ShowDialog				(true);
}

void CUIGameSP::StartStashUI(CInventoryOwner* pActorInv, CInventoryBox* pBox) //Ящик, Сэйф
{
	if( TopInputReceiver() )		return;
	
	m_UICarBodyMenu->InitInventoryBox		(pActorInv, pBox);
	m_UICarBodyMenu->ShowDialog				(true);
}
//Нужно проверить что будет, если гг убьют
void CUIGameSP::OpenSafe()
{
	if (TopInputReceiver())		return;

	if (StoredInvBox){
		m_UICarBodyMenu->InitInventoryBox(Actor(), StoredInvBox);
		m_UICarBodyMenu->ShowDialog(true);
	}

}


extern ENGINE_API BOOL bShowPauseString;
void CUIGameSP::ChangeLevel(	GameGraph::_GRAPH_ID game_vert_id, 
								u32 level_vert_id, 
								Fvector pos, 
								Fvector ang, 
								Fvector pos2, 
								Fvector ang2, 
								bool b_use_position_cancel)
{
	if(TopInputReceiver()!=UIChangeLevelWnd)
	{
		UIChangeLevelWnd->m_game_vertex_id		= game_vert_id;
		UIChangeLevelWnd->m_level_vertex_id		= level_vert_id;
		UIChangeLevelWnd->m_position			= pos;
		UIChangeLevelWnd->m_angles				= ang;
		UIChangeLevelWnd->m_position_cancel		= pos2;
		UIChangeLevelWnd->m_angles_cancel		= ang2;
		UIChangeLevelWnd->m_b_position_cancel	= b_use_position_cancel;

		UIChangeLevelWnd->ShowDialog		(true);
	}
}

void CUIGameSP::EnableSkills(bool val)
{
	m_PdaMenu->EnableSkills(val);
}

void CUIGameSP::EnableDownloads(bool val)
{
	m_PdaMenu->EnableDownloads(val);
}

void CUIGameSP::ReinitDialogs()
{
	delete_data(m_InventoryMenu);
	m_InventoryMenu		= new CUIInventoryWnd();
	
	delete_data(TalkMenu);
	TalkMenu		= new CUITalkWnd();
}

CChangeLevelWnd::CChangeLevelWnd		()
{
	m_messageBox			= new CUIMessageBox();	
	m_messageBox->SetAutoDelete(true);
	AttachChild				(m_messageBox);
}

void CChangeLevelWnd::SendMessage(CUIWindow *pWnd, s16 msg, void *pData)
{
	if(pWnd==m_messageBox){
		if(msg==MESSAGE_BOX_YES_CLICKED){
			OnOk									();
		}else
		if(msg==MESSAGE_BOX_NO_CLICKED || msg==MESSAGE_BOX_OK_CLICKED)
		{
			OnCancel								();
		}
	}else
		inherited::SendMessage(pWnd, msg, pData);
}

void CChangeLevelWnd::OnOk()
{
	HideDialog								();
	NET_Packet								p;
	p.w_begin								(M_CHANGE_LEVEL);
	p.w										(&m_game_vertex_id,sizeof(m_game_vertex_id));
	p.w										(&m_level_vertex_id,sizeof(m_level_vertex_id));
	p.w_vec3								(m_position);
	p.w_vec3								(m_angles);

	Level().Send							(p,net_flags(TRUE));
}

void CChangeLevelWnd::OnCancel()
{
	HideDialog();
	if(m_b_position_cancel)
		Actor()->MoveActor(m_position_cancel, m_angles_cancel);
}

bool CChangeLevelWnd::OnKeyboardAction(int dik, EUIMessages keyboard_action)
{
	if(keyboard_action==WINDOW_KEY_PRESSED)
	{
		if(is_binded(kQUIT, dik) )
			OnCancel		();
		return true;
	}
	return inherited::OnKeyboardAction(dik, keyboard_action);
}

#include "ai_space.h"
#include "script_engine.h"

bool g_block_pause	= false;
void CChangeLevelWnd::ShowDialog(bool bDoHideIndicators)
{
	luabind::functor<bool> lua_bool;
	bool call_result = false;

	R_ASSERT2(ai().script_engine().functor("level_weathers.is_blowout_active", lua_bool), "Can't find level_weathers.set_weather_manualy");

	if (lua_bool.is_valid()){
		call_result = lua_bool();
	}

	if (call_result)
	{ 
		m_messageBox->InitMessageBox("message_box_change_level_blowout");	//if blowout - show rejection message 
	}
	else
	{
		m_messageBox->InitMessageBox("message_box_change_level");	//else ask if player wants to move to other level
	}

	SetWndPos				(m_messageBox->GetWndPos());
	m_messageBox->SetWndPos	(Fvector2().set(0.0f,0.0f));
	SetWndSize				(m_messageBox->GetWndSize());

	g_block_pause							= true;
	Device.Pause							(TRUE, TRUE, TRUE, "CChangeLevelWnd_show");
	bShowPauseString						= FALSE;

	inherited::ShowDialog(bDoHideIndicators);
}

void CChangeLevelWnd::HideDialog()
{
	g_block_pause							= false;
	Device.Pause							(FALSE, TRUE, TRUE, "CChangeLevelWnd_hide");

	inherited::HideDialog();
}

