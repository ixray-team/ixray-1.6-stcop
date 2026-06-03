#include "stdafx.h"
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
#include "../xrEngine/string_table.h"
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
	m_game = g->cast_game_cl_single();
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

bool CUIGameSP::IR_UIOnGamepadKeyPress(int id)
{
	if(inherited::IR_UIOnGamepadKeyPress(id)) 
		return true;

	if(Device.Paused() ) 
		return false;

	CObject* current_entity = Level().CurrentEntity();

	CInventoryOwner* pInvOwner = current_entity != nullptr ? current_entity->cast_inventory_owner() : nullptr;
	if (pInvOwner == nullptr)
	{
		return false;
	}

	CEntityAlive* EA = current_entity->cast_entity_alive();
	if (EA == nullptr || !EA->g_Alive())
	{
		return false;
	}

	CActor* pActor = pInvOwner->cast_actor();
	if (pActor == nullptr)
	{
		return false;
	}

	if (!pActor->g_Alive())
	{
		return false;
	}

	OnAction(pActor, get_binded_action(id));
	return false;
}

bool CUIGameSP::IR_UIOnKeyboardPress(int dik) 
{
	if(inherited::IR_UIOnKeyboardPress(dik)) return true;
	if( Device.Paused()		) return false;

#ifndef MASTER_GOLD
	hud_adjust_mode_keyb	(dik);
	attach_adjust_mode_keyb	(dik);
#endif

	CObject* current_entity = Level().CurrentEntity();

	CInventoryOwner* pInvOwner = current_entity != nullptr ? current_entity->cast_inventory_owner() : nullptr;
	if (pInvOwner == nullptr)
	{
		return false;
	}

	CEntityAlive* EA = current_entity->cast_entity_alive();
	if (EA == nullptr || !EA->g_Alive())
	{
		return false;
	}

	CActor* pActor = pInvOwner->cast_actor();
	if (pActor == nullptr)
	{
		return false;
	}

	if (!pActor->g_Alive())
	{
		return false;
	}

	OnAction(pActor, get_binded_action(dik));
	return false;
}

void CUIGameSP::OnAction(CActor* actor, EGameActions action)
{
	switch (action)
	{
	case kACTIVE_JOBS:
	{
		if (!actor->pda_disabled())
		{
			if (actor->HudAnimator()->PdaAnimator() != nullptr)
			{
				actor->HudAnimator()->PdaAnimator()->SwitchAnimator();
			}
			else
			{
				ShowPdaMenu();
			}
		}
		break;
	}

	case kMAP:
	{
		if (!actor->pda_disabled())
		{
			if (!PdaMenu()->IsShown())
			{
				PdaMenu()->SetActiveSubdialog("eptMap");
				if (actor->HudAnimator()->PdaAnimator() != nullptr)
				{
					actor->HudAnimator()->PdaAnimator()->SwitchAnimator();
				}
				else
				{
					ShowPdaMenu();
				}
			}
			else
			{
				HidePdaMenu();
			}
		}
		break;
	}

	case kCONTACTS:
	{
		if (!actor->pda_disabled())
		{
			if (!PdaMenu()->IsShown())
			{
				PdaMenu()->SetActiveSubdialog("eptContacts");
				if (actor->HudAnimator()->PdaAnimator() != nullptr)
				{
					actor->HudAnimator()->PdaAnimator()->SwitchAnimator();
				}
				else
				{
					ShowPdaMenu();
				}
			}
			else
			{
				HidePdaMenu();
			}
		}
		break;
	}

	case kINVENTORY:
	{
		if (!actor->inventory_disabled())
		{
			if (actor->HudAnimator()->BackpackAnimator() != nullptr)
			{
				actor->HudAnimator()->BackpackAnimator()->SwitchAnimator();
			}
			else
			{
				ShowActorMenu();
			}
		}
		break;
	}
	case kSCORES:
		if (!actor->pda_disabled())
		{
			m_game_objective = AddCustomStatic("main_task", true);
			CGameTask* t1 = Level().GameTaskManager()->ActiveTask(eTaskTypeStoryline);
			CGameTask* t2 = Level().GameTaskManager()->ActiveTask(eTaskTypeAdditional);

			if (Level().GameTaskManager()->IsMultipleTask() && t1 && t2)
			{
				m_game_objective->m_static->TextItemControl()->SetTextST(g_pStringTable->ParseStringFromScript(t1->m_Title).c_str());
				SDrawStaticStruct* sm2 = AddCustomStatic("secondary_task", true);
				sm2->m_static->TextItemControl()->SetTextST(g_pStringTable->ParseStringFromScript(t2->m_Title).c_str());
			}
			else
			{
				if (t1 || t2)
				{
					CGameTask* t = (t1) ? t1 : t2;
					if (m_msgs_xml->NavigateToNode("secondary_task"))
					{
						m_game_objective->m_static->TextItemControl()->SetTextST(g_pStringTable->ParseStringFromScript(t->m_Title).c_str());
						SDrawStaticStruct* sm2 = AddCustomStatic("secondary_task", true);
						sm2->m_static->TextItemControl()->SetTextST(g_pStringTable->ParseStringFromScript(t->m_Description).c_str());
					}
					else
					{
						m_game_objective->m_static->TextItemControl()->SetTextST(g_pStringTable->ParseStringFromScript(Level().GameTaskManager()->ActiveObjective()->m_Description).c_str());
					}
				}
				else
				{
					m_game_objective->m_static->TextItemControl()->SetTextST("st_no_active_task");
				}
			}
		}break;
	}

}

#ifdef DEBUG
void CUIGameSP::Render()
{
	inherited::Render();
}
#endif