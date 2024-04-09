#pragma once

#include "game_sv_mp.h"
#include "../../xrEngine/pure_relcase.h"

class game_sv_freemp : public game_sv_mp, private pure_relcase
{
	using inherited = game_sv_mp;

public:
	game_sv_freemp();
	virtual							~game_sv_freemp();

	virtual		void				Create(shared_str& options);


	virtual		bool				UseSKin() const { return false; }

	virtual		LPCSTR				type_name() const { return "freemp"; };
	void							net_Relcase(CObject* O) {};


	virtual		void				OnPlayerReady(ClientID id_who);
	virtual		void				OnPlayerConnect(ClientID id_who);
	virtual		void				OnPlayerConnectFinished(ClientID id_who);
	virtual		void				OnPlayerDisconnect(ClientID id_who, LPSTR Name, u16 GameID);

	virtual		void				OnPlayerKillPlayer(game_PlayerState* ps_killer, game_PlayerState* ps_killed, KILL_TYPE KillType, SPECIAL_KILL_TYPE SpecialKillType, CSE_Abstract* pWeaponA);

	virtual		void				OnEvent(NET_Packet& tNetPacket, u16 type, u32 time, ClientID sender);

	virtual		void				Update();
	virtual		BOOL OnTouch(u16 eid_who, u16 eid_what, BOOL bForced = false);
};