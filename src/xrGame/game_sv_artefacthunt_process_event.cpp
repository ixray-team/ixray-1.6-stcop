#include "StdAfx.h"
#include "game_sv_artefacthunt.h"
#include "xrMessages.h"

void	game_sv_ArtefactHunt::OnEvent (NET_Packet &P, u16 type, u32 time, ClientID sender )
{

	switch	(type)
	{
	case GAME_EVENT_PLAYER_ENTER_TEAM_BASE:
		{
			ALife::_OBJECT_ID pl_id = P.r_u32();
			u8 z_t_id = P.r_u8();
			OnObjectEnterTeamBase(pl_id, z_t_id);
		}break;

	case GAME_EVENT_PLAYER_LEAVE_TEAM_BASE:
		{
			ALife::_OBJECT_ID pl_id = P.r_u32();
			u8 z_t_id = P.r_u8();
			OnObjectLeaveTeamBase(pl_id, z_t_id);
		}break;

	default:
		inherited::OnEvent(P, type, time, sender);
	}
}