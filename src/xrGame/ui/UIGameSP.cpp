#include "StdAfx.h"
#include "pch_script.h"
#include "UIGameSP.h"
#include "Actor.h"
#include "Level.h"
#include "../xrEngine/xr_input.h"

#ifdef DEBUG
#include "attachable_item.h"
#endif

#include "game_cl_single.h"
#include "ActorCondition.h"
#include "../xrEngine/XR_IOConsole.h"
#include "object_broker.h"
#include "GametaskManager.h"
#include "GameTask.h"

#include "ui/UIActorMenu.h"
#include "ui/UIPdaWnd.h"
#include "ui/UIMessageBox.h"


CUIGameSP::CUIGameSP()
:m_game(nullptr),m_game_objective(nullptr)
{
	UIChangeLevelWnd= new CChangeLevelWnd	();
}

CUIGameSP::~CUIGameSP() 
{
	delete_data(UIChangeLevelWnd);
}
 
void CUIGameSP::SetClGame (game_cl_GameState* g)
{
	inherited::SetClGame				(g);
	m_game = smart_cast<game_cl_Single*>(g);
	R_ASSERT							(m_game);
}

#ifndef MASTER_GOLD
	void attach_adjust_mode_keyb(int dik);
	void attach_draw_adjust_mode();
	void hud_adjust_mode_keyb(int dik);
	void hud_draw_adjust_mode();
#endif

void CUIGameSP::OnFrame()
{
	inherited::OnFrame();
	
	if(Device.Paused())	return;

	if(m_game_objective)
	{
		bool b_remove = false;
		int dik = get_action_dik(kSCORES, 0);
		if(dik && !pInput->iGetAsyncKeyState(dik))
			b_remove=true;
		
		dik = get_action_dik(kSCORES, 1);
		if(!b_remove && dik && !pInput->iGetAsyncKeyState(dik))
			b_remove=true;

		if(b_remove)
		{
			RemoveCustomStatic		("main_task");
			RemoveCustomStatic		("secondary_task");
			m_game_objective		= nullptr;
		}
	}

#ifndef MASTER_GOLD
	hud_draw_adjust_mode();
	attach_draw_adjust_mode();
#endif
}

bool CUIGameSP::IR_UIOnKeyboardPress(int dik) 
{
	if(inherited::IR_UIOnKeyboardPress(dik)) return true;
	if( Device.Paused()		) return false;

#ifndef MASTER_GOLD
	hud_adjust_mode_keyb	(dik);
	attach_adjust_mode_keyb	(dik);
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

	switch ( get_binded_action(dik) )
	{
	case kACTIVE_JOBS:
		{
			if ( !pActor->inventory_disabled() )
				ShowPdaMenu();
			break;
		}

	case kINVENTORY:
		{
			if ( !pActor->inventory_disabled() )
				ShowActorMenu();

			break;
		}

	case kSCORES:
        if (!pActor->inventory_disabled())
        {
            m_game_objective = AddCustomStatic("main_task", true);
            CGameTask* t1 = Level().GameTaskManager()->ActiveTask(eTaskTypeStoryline);
            CGameTask* t2 = Level().GameTaskManager()->ActiveTask(eTaskTypeAdditional);

            if (Level().GameTaskManager()->IsMultipleTask() && t1 && t2)
            {
                m_game_objective->m_static->TextItemControl()->SetTextST(t1->m_Title.c_str());
                SDrawStaticStruct* sm2 = AddCustomStatic("secondary_task", true);
                sm2->m_static->TextItemControl()->SetTextST(t2->m_Title.c_str());
            }
            else
            {
                if (t1 || t2)
                {
                    CGameTask* t = (t1) ? t1 : t2;
                    m_game_objective->m_static->TextItemControl()->SetTextST(t->m_Title.c_str());
					SDrawStaticStruct* sm2 = AddCustomStatic("secondary_task", true);
                    sm2->m_static->TextItemControl()->SetTextST(t->m_Description.c_str());
                }
                else
                {
                    m_game_objective->m_static->TextItemControl()->SetTextST("st_no_active_task");
                }
            }
        }break;
	}

	return false;
}
#ifdef DEBUG
void CUIGameSP::Render()
{
	inherited::Render();
}
#endif