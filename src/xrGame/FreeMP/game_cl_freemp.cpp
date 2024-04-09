#include "stdafx.h"
#include "game_cl_freemp.h"
#include "clsid_game.h"
#include "../../xrEngine/xr_level_controller.h"
#include "UIGameFreeMP.h"
#include "actor_mp_client.h"

game_cl_freemp::game_cl_freemp()
{
}

game_cl_freemp::~game_cl_freemp()
{
}


CUIGameCustom* game_cl_freemp::createGameUI()
{
	if (g_dedicated_server)
		return nullptr;

	CLASS_ID clsid = CLSID_GAME_UI_FREEMP;
	m_game_ui = smart_cast<CUIGameFMP*> (NEW_INSTANCE(clsid));
	R_ASSERT(m_game_ui);
	m_game_ui->Load();
	m_game_ui->SetClGame(this);

	return m_game_ui;
}

void game_cl_freemp::SetGameUI(CUIGameCustom* uigame)
{
	inherited::SetGameUI(uigame);
	m_game_ui = smart_cast<CUIGameFMP*>(uigame);
	R_ASSERT(m_game_ui);
}


void game_cl_freemp::net_import_state(NET_Packet& P)
{
	inherited::net_import_state(P);
}

void game_cl_freemp::net_import_update(NET_Packet& P)
{
	inherited::net_import_update(P);
}

bool game_cl_freemp::OnKeyboardPress(int key)
{
	if (kJUMP == key)
	{
		bool b_need_to_send_ready = false;

		CObject* curr = Level().CurrentControlEntity();
		if (!curr) return(false);

		bool is_actor = !!smart_cast<CActor*>(curr);
		bool is_spectator = !!smart_cast<CSpectator*>(curr);

		game_PlayerState* ps = local_player;

		if (is_spectator || (is_actor && ps && ps->testFlag(GAME_PLAYER_FLAG_VERY_VERY_DEAD)))
		{
			b_need_to_send_ready = true;
		}

		if (b_need_to_send_ready)
		{
			CGameObject* GO = smart_cast<CGameObject*>(curr);
			NET_Packet			P;
			GO->u_EventGen(P, GE_GAME_EVENT, GO->ID());
			P.w_u16(GAME_EVENT_PLAYER_READY);
			GO->u_EventSend(P);
			return				true;
		}
		else
		{
			return false;
		}
	};

	return inherited::OnKeyboardPress(key);
}

LPCSTR game_cl_freemp::GetGameScore(string32& score_dest)
{
	s32 frags = local_player ? local_player->frags() : 0;
	xr_sprintf(score_dest, "[%d]", frags);
	return score_dest;
}