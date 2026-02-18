#pragma once

#include "game_sv_mp.h"
#include "../../xrEngine/pure_relcase.h"

class game_sv_freemp final :
	public game_sv_mp, 
	private pure_relcase
{
	using inherited = game_sv_mp;
	xr_hash_map<xr_string, int> map_quest;
	xr_hash_map<xr_string, int> map_items;
public:
	game_sv_freemp();
	virtual							~game_sv_freemp();

	virtual		void				Create(shared_str& options);


	virtual		bool				UseSKin() const { return false; }
	virtual		void				SetSkin(CSE_Abstract* E, u16 Team, u16 ID);

	virtual		const char*				type_name() const { return "freemp"; };
	void							net_Relcase(CObject* O) {};

	void									AddMoneyToPlayer(game_PlayerState* ps, s32 amount);
	void									SpawnItemToActor(ALife::_OBJECT_ID actorId, const char* name);
	virtual		void				on_death(CSE_Abstract* e_dest, CSE_Abstract* e_src);
	virtual		void				OnTransferMoney(NET_Packet& P, ClientID const& clientID);

	virtual		void				OnPlayerReady(ClientID id_who);
	virtual		void				OnPlayerConnect(ClientID id_who);
	virtual		void				OnPlayerConnectFinished(ClientID id_who);
	void				OnPlayerDisconnect(ClientID id_who, LPSTR Name, ALife::_OBJECT_ID GameID) override;
	virtual		void				OnPlayerKillPlayer(game_PlayerState* ps_killer, game_PlayerState* ps_killed, KILL_TYPE KillType, SPECIAL_KILL_TYPE SpecialKillType, CSE_Abstract* pWeaponA);
	virtual		void				OnPlayerRepairItem(NET_Packet& P, ClientID const& clientID);
	virtual		void				OnEvent(NET_Packet& tNetPacket, u16 type, u32 time, ClientID sender);

	virtual		void				Update();

	virtual		void				RespawnPlayer(ClientID id_who, bool NoSpectator);
	bool                OnTouch(ALife::_OBJECT_ID eid_who, ALife::_OBJECT_ID eid_what, bool bForced = false) override;
	void				OnDetach(ALife::_OBJECT_ID eid_who, ALife::_OBJECT_ID eid_what) override;
	virtual		void				OnPlayerTrade(NET_Packet& P, ClientID const& clientID);

	// drop items after death
	virtual		void				FillDeathActorRejectItems(CSE_ActorMP* actor, xr_vector<CSE_Abstract*>& to_reject);
	bool			 	            OnTouchPlayersBag(CSE_ActorMP* actor, CSE_Abstract* item);
	void			             	OnDetachPlayersBag(CSE_ActorMP* actor, CSE_Abstract* item);

	virtual game_sv_freemp* cast_game_sv_freemp() override { return this; }

private:
	xr_hash_map<ALife::_OBJECT_ID, xr_vector<shared_str>> DoSpawnList;
	xrCriticalSection SpawnGuard;
};