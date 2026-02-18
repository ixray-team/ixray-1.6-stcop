#pragma once

#include "game_sv_mp.h"

class game_sv_mp_script : public game_sv_mp
{
	typedef game_sv_mp inherited;
private:
	virtual		void				Create					(shared_str &options);

public:
									game_sv_mp_script		():inherited(){};
	virtual							~game_sv_mp_script		(){};
	virtual		void				Create					(const char* options){};
	void				Update					() override {inherited::Update();};
	void				OnPlayerConnect			(ClientID id_who) override;
	void				OnPlayerDisconnect		(ClientID id_who, LPSTR Name, ALife::_OBJECT_ID GameID) override;

	void				net_Export_State		(NET_Packet& P, ClientID id_to) override;
	void				OnEvent					(NET_Packet &P, u16 type, u32 time, ClientID sender ) override;
	game_PlayerState*	createPlayerState(NET_Packet* account_info) override {return inherited::createPlayerState(account_info); };


	virtual		void				OnPlayerKillPlayer		(ClientID id_killer, ClientID id_killed){};
	void				OnPlayerHitPlayer		(ALife::_OBJECT_ID id_hitter, ALife::_OBJECT_ID id_hitted, NET_Packet& P) override {}; //игрок получил Hit
	bool				OnTouch					(ALife::_OBJECT_ID eid_who, ALife::_OBJECT_ID eid_target, bool bForced = false) override {return true;};			// true=allow ownership, false=denied
	void				OnDetach				(ALife::_OBJECT_ID eid_who, ALife::_OBJECT_ID eid_target) override {};

protected:
				void				SetHitParams			(NET_Packet* P, float impulse, float power);
				float				GetHitParamsPower		(NET_Packet* P);
				float				GetHitParamsImpulse		(NET_Packet* P);
virtual		void				switch_Phase			(u32 new_phase);
				void				SpawnPlayer				(ClientID id, const char* N, const char* SkinName, RPoint rp);


	DECLARE_SCRIPT_REGISTER_FUNCTION
};
