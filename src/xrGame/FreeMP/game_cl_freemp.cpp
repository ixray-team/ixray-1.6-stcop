#include "stdafx.h"
#include "game_cl_freemp.h"
#include "clsid_game.h"
#include "../../xrEngine/xr_level_controller.h"
#include "UIGameFreeMP.h"
#include "actor_mp_client.h"
#include "game_sv_freemp.h"
#include "ui/UIActorMenu.h"
#include "ui/UIMainIngameWnd.h"

game_cl_freemp::game_cl_freemp()
{
	if (!g_dedicated_server)
	{
		m_pVoiceChat = new CVoiceChat();
	}
}

game_cl_freemp::~game_cl_freemp()
{
	xr_delete(m_pVoiceChat);
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

	if (m_pVoiceChat)
	{
		m_game_ui->UIMainIngameWnd->SetVoiceDistance(m_pVoiceChat->GetDistance());
	}
}

void game_cl_freemp::OnConnected()
{
	inherited::OnConnected();

	if (m_game_ui)
	{
		R_ASSERT(!g_dedicated_server);
		m_game_ui = smart_cast<CUIGameFMP*>	(CurrentGameUI());
		m_game_ui->SetClGame(this);
	}

	//luabind::functor<void>	funct;
	//R_ASSERT(ai().script_engine().functor("mp_game_cl.on_connected", funct));
	//funct();
}

void game_cl_freemp::net_import_state(NET_Packet& P)
{
	inherited::net_import_state(P);
}

void game_cl_freemp::net_import_update(NET_Packet& P)
{
	inherited::net_import_update(P);
}

void game_cl_freemp::shedule_Update(u32 dt)
{
	game_cl_GameState::shedule_Update(dt);

	if (!local_player)
		return;

	if (!g_dedicated_server && m_pVoiceChat)
	{
		const bool started = m_pVoiceChat->IsStarted();
		const bool is_dead = !local_player || local_player->testFlag(GAME_PLAYER_FLAG_VERY_VERY_DEAD);
		const bool has_shown_dialogs = CurrentGameUI()->HasShownDialogs();
		if (started && (is_dead || has_shown_dialogs))
		{
			m_pVoiceChat->Stop();
			CurrentGameUI()->UIMainIngameWnd->SetActiveVoiceIcon(false);
		}
		m_pVoiceChat->Update();
	}

	for (auto cl : players)
	{
		game_PlayerState* ps = cl.second;
		if (!ps || ps->testFlag(GAME_PLAYER_FLAG_VERY_VERY_DEAD)) continue;

		CActor* pActor = smart_cast<CActor*>(Level().Objects.net_Find(ps->GameID));
		if (!pActor || !pActor->g_Alive()) continue;

		pActor->SetName(ps->getName());
		pActor->cName_set(ps->getName());

		if (ps->team != pActor->Community())
		{
			CHARACTER_COMMUNITY	community;
			community.set(ps->team);
			pActor->SetCommunity(community.index());
			pActor->ChangeTeam(community.team(), 0, 0);
		}

		if (local_player->GameID == ps->GameID)
		{
			pActor->set_money((u32)ps->money_for_round, false);
		}
	}
}

void game_cl_freemp::TranslateGameMessage(u32 msg, NET_Packet& P)
{
	switch (msg)
	{
	case GAME_EVENT_MP_REPAIR_SUCCESS:
	{
		if (m_game_ui && m_game_ui->ActorMenu().IsShown() && m_game_ui->ActorMenu().GetMenuMode() == mmUpgrade)
		{
			u16 itemId = P.r_u16();
			PIItem item = smart_cast<PIItem>(Level().Objects.net_Find(itemId));
			if (item)
			{
				m_game_ui->ActorMenu().OnSuccessRepairMP(item);
			}
		}
	}break;
	default:
		inherited::TranslateGameMessage(msg, P);
	};
}

bool game_cl_freemp::OnKeyboardPress(int key)
{
	switch (key)
	{
	case kVOICE_CHAT:
	{
		if (local_player && !local_player->testFlag(GAME_PLAYER_FLAG_VERY_VERY_DEAD))
		{
			if (!m_pVoiceChat->IsStarted())
			{
				m_pVoiceChat->Start();
				CurrentGameUI()->UIMainIngameWnd->SetActiveVoiceIcon(true);
			}
		}
		return true;
	}break;

	case kVOICE_DISTANCE:
	{
		if (local_player && !local_player->testFlag(GAME_PLAYER_FLAG_VERY_VERY_DEAD))
		{
			u8 distance = m_pVoiceChat->SwitchDistance();
			CurrentGameUI()->UIMainIngameWnd->SetVoiceDistance(distance);
		}
		return true;
	}break;
	case kJUMP:
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
	}
	}

	return inherited::OnKeyboardPress(key);
}

bool game_cl_freemp::OnKeyboardRelease(int key)
{
	switch (key)
	{
	case kVOICE_CHAT:
	{
		m_pVoiceChat->Stop();
		CurrentGameUI()->UIMainIngameWnd->SetActiveVoiceIcon(false);
		return true;
	}break;

	default:
		break;
	}

	return inherited::OnKeyboardRelease(key);
}
LPCSTR game_cl_freemp::GetGameScore(string32& score_dest)
{
	s32 frags = local_player ? local_player->frags() : 0;
	xr_sprintf(score_dest, "[%d]", frags);
	return score_dest;
}

void game_cl_freemp::OnRender()
{
	inherited::OnRender();

	if (m_pVoiceChat)
		m_pVoiceChat->OnRender();
}

void game_cl_freemp::OnVoiceMessage(NET_Packet* P)
{
	m_pVoiceChat->ReceiveMessage(P);
}

bool IsGameTypeSingleCompatible()
{
	return IsGameTypeSingle() || (g_pGamePersistent->GameType() == eGameIDFreeMP);
}
