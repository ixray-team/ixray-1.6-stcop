#include "stdafx.h"
#include "UIGameFreeMP.h"
#include "game_cl_freemp.h"

#include "../xrEngine/xr_input.h"
#include "../xrEngine/xr_level_controller.h"

#include "Actor.h"
#include "Level.h"

CUIGameFMP::CUIGameFMP()
{
	m_game = NULL;
}

CUIGameFMP::~CUIGameFMP()
{
}

void CUIGameFMP::SetClGame(game_cl_GameState* g)
{
	inherited::SetClGame(g);
	m_game = smart_cast<game_cl_freemp*>(g);
	R_ASSERT(m_game);
}

bool CUIGameFMP::IR_UIOnKeyboardPress(int dik)
{
	if (inherited::IR_UIOnKeyboardPress(dik)) return true;
	if (Device.Paused()) return false;

	CInventoryOwner* pInvOwner = smart_cast<CInventoryOwner*>(Level().CurrentEntity());
	if (!pInvOwner)			return false;

	CEntityAlive* EA = smart_cast<CEntityAlive*>(Level().CurrentEntity());
	if (!EA || !EA->g_Alive())	return false;

	CActor* pActor = smart_cast<CActor*>(pInvOwner);
	if (!pActor)
		return false;

	if (!pActor->g_Alive())
		return false;

	switch (get_binded_action(dik))
	{
	case kACTIVE_JOBS:
	{
		if (!pActor->inventory_disabled())
			ShowPdaMenu();
	} break;
	case kINVENTORY:
	{
		if (!pActor->inventory_disabled())
			ShowActorMenu();
	} break;
	default:
		break;
	}
	return false;
}