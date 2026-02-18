#pragma once

#include "game_sv_deathmatch.h"

class game_sv_TeamDeathmatch : public game_sv_Deathmatch
{
private:
	using inherited = game_sv_Deathmatch;
	bool	teams_swaped;

protected:
	bool				checkForFragLimit		() override;
	bool				HasChampion				() override;

	void				ReadOptions				(shared_str &options) override;
	void				ConsoleCommands_Create	() override;
	void				ConsoleCommands_Clear	() override;

public:	
									game_sv_TeamDeathmatch	(){m_type = eGameIDTeamDeathmatch;}
	void				Create					(shared_str& options) override;

	void				OnEvent					(NET_Packet &tNetPacket, u16 type, u32 time, ClientID sender ) override;

	const char*				type_name			() const override { return "teamdeathmatch";};

	void				Update					() override;
	void				net_Export_State		(NET_Packet& P, ClientID id_to) override;				// full state
	// Events	
	void				OnPlayerConnect			(ClientID id_who) override;
	void				OnPlayerConnectFinished	(ClientID id_who) override;

	void				OnPlayerSelectTeam		(NET_Packet& P, ClientID sender) override;
	virtual		void				OnPlayerChangeTeam		(ClientID id_who, s16 team);
	KILL_RES			GetKillResult			(game_PlayerState* pKiller, game_PlayerState* pVictim) override;
	bool				OnKillResult			(KILL_RES KillResult, game_PlayerState* pKiller, game_PlayerState* pVictim) override;
	void				OnPlayerKillPlayer		(game_PlayerState* ps_killer, game_PlayerState* ps_killed, KILL_TYPE KillType, SPECIAL_KILL_TYPE SpecialKillType, CSE_Abstract* pWeaponA) override;
	virtual		void				UpdateTeamScore			(game_PlayerState* ps_killer, s16 OldKills);
	bool				CheckTeams				() override { return true; };

	void				OnPlayerHitPlayer		(ALife::_OBJECT_ID id_hitter, ALife::_OBJECT_ID id_hitted, NET_Packet& P) override;
	void				OnPlayerHitPlayer_Case	(game_PlayerState* ps_hitter, game_PlayerState* ps_hitted, SHit* pHitS) override;

	void				OnRoundStart			() override;
	void				OnRoundEnd				() override;
	virtual		void				AutoBalanceTeams		();
	virtual		void				AutoSwapTeams			();

	virtual		u8					AutoTeam				( );
	virtual		u32					GetPlayersCountInTeams	(u8 team);
	virtual		bool				TeamSizeEqual			();
	u32					RP_2_Use				(CSE_Abstract* E) override;

	void				LoadTeams				() override;

	char*				GetAnomalySetBaseName	() override {return (char*) "teamdeathmatch_game_anomaly_sets";};
	bool				CanHaveFriendlyFire		() override {return true;}
	void				OnFraglimitExceed		() override;
	void				OnTimelimitExceed		() override;

	bool				isFriendlyFireEnabled	() override;
	virtual		float				GetFriendlyFire			();

	virtual		bool				Get_AutoTeamBalance		();
	virtual		bool				Get_AutoTeamSwap		();
	virtual		bool				Get_FriendlyIndicators	();
	virtual		bool				Get_FriendlyNames		();

	virtual		int					Get_TeamKillLimit		();
	virtual		bool				Get_TeamKillPunishment	();

				bool				OnTouchItem(CSE_ActorMP *actor, CSE_Abstract *item);
				void				OnDetachItem(CSE_ActorMP *actor, CSE_Abstract *item);

	bool OnTouch(ALife::_OBJECT_ID eid_who, ALife::_OBJECT_ID eid_what, bool bForced = false) override;
	void OnDetach(ALife::_OBJECT_ID eid_who, ALife::_OBJECT_ID eid_what) override;

	virtual void				OnObjectEnterTeamBase	(ALife::_OBJECT_ID id, u16 zone_team);
	virtual void				OnObjectLeaveTeamBase	(ALife::_OBJECT_ID id, u16 zone_team);
	void				RespawnPlayer			(ClientID id_who, bool NoSpectator) override;

	virtual game_sv_mp* cast_game_sv_mp() override { return this; }
	virtual game_sv_Deathmatch* cast_game_sv_deathmatch() override { return this; }
	virtual game_sv_TeamDeathmatch* cast_game_sv_teamdeathmatch() override { return this; }

protected:
	void				WriteGameState			(CInifile& ini, const char* sect, bool bRoundResult) override;
};
