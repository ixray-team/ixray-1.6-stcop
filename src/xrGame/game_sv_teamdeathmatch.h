#pragma once

#include "game_sv_deathmatch.h"

class game_sv_TeamDeathmatch : public game_sv_Deathmatch
{
private:
	using inherited = game_sv_Deathmatch;
	bool	teams_swaped;

protected:

	virtual		bool				checkForFragLimit		();
	virtual		bool				HasChampion				();
		
	virtual		void				ReadOptions				(shared_str &options);
	virtual		void				ConsoleCommands_Create	();
	virtual		void				ConsoleCommands_Clear	();

public:	
									game_sv_TeamDeathmatch	(){m_type = eGameIDTeamDeathmatch;}
	virtual		void				Create					(shared_str& options);

	virtual		void				OnEvent					(NET_Packet &tNetPacket, u16 type, u32 time, ClientID sender );

	virtual		const char*				type_name			() const { return "teamdeathmatch";};

	virtual		void				Update					();
	virtual		void				net_Export_State		(NET_Packet& P, ClientID id_to);				// full state
	// Events	
	virtual		void				OnPlayerConnect			(ClientID id_who);
	virtual		void				OnPlayerConnectFinished	(ClientID id_who);

	virtual		void				OnPlayerSelectTeam		(NET_Packet& P, ClientID sender);
	virtual		void				OnPlayerChangeTeam		(ClientID id_who, s16 team);
	virtual		KILL_RES			GetKillResult			(game_PlayerState* pKiller, game_PlayerState* pVictim);
	virtual		bool				OnKillResult			(KILL_RES KillResult, game_PlayerState* pKiller, game_PlayerState* pVictim);
	virtual		void				OnPlayerKillPlayer		(game_PlayerState* ps_killer, game_PlayerState* ps_killed, KILL_TYPE KillType, SPECIAL_KILL_TYPE SpecialKillType, CSE_Abstract* pWeaponA);
	virtual		void				UpdateTeamScore			(game_PlayerState* ps_killer, s16 OldKills);
	virtual		bool				CheckTeams				() { return true; };

	virtual		void				OnPlayerHitPlayer		(u16 id_hitter, u16 id_hitted, NET_Packet& P);
	virtual		void				OnPlayerHitPlayer_Case	(game_PlayerState* ps_hitter, game_PlayerState* ps_hitted, SHit* pHitS);

	virtual		void				OnRoundStart			();
	virtual		void				OnRoundEnd				();
	virtual		void				AutoBalanceTeams		();
	virtual		void				AutoSwapTeams			();

	virtual		u8					AutoTeam				( );
	virtual		u32					GetPlayersCountInTeams	(u8 team);
	virtual		bool				TeamSizeEqual			();
	virtual		u32					RP_2_Use				(CSE_Abstract* E);

	virtual		void				LoadTeams				();

	virtual		char*				GetAnomalySetBaseName	()	{return (char*) "teamdeathmatch_game_anomaly_sets";};	
	virtual		bool				CanHaveFriendlyFire		()	{return true;}
	virtual		void				OnFraglimitExceed		();
	virtual		void				OnTimelimitExceed		();

	virtual		bool				isFriendlyFireEnabled	();
	virtual		float				GetFriendlyFire			();

	virtual		bool				Get_AutoTeamBalance		();
	virtual		bool				Get_AutoTeamSwap		();
	virtual		bool				Get_FriendlyIndicators	();
	virtual		bool				Get_FriendlyNames		();

	virtual		int					Get_TeamKillLimit		();
	virtual		bool				Get_TeamKillPunishment	();

				bool				OnTouchItem(CSE_ActorMP *actor, CSE_Abstract *item);
				void				OnDetachItem(CSE_ActorMP *actor, CSE_Abstract *item);
	
	virtual		bool				OnTouch					(u16 eid_who, u16 eid_what, bool bForced = false);
	virtual		void				OnDetach				(u16 eid_who, u16 eid_what);

				void				OnObjectEnterTeamBase	(u16 id, u16 zone_team);
				void				OnObjectLeaveTeamBase	(u16 id, u16 zone_team);
	virtual		void				RespawnPlayer			(ClientID id_who, bool NoSpectator);

	virtual game_sv_mp* cast_game_sv_mp() override { return this; }
	virtual game_sv_Deathmatch* cast_game_sv_deathmatch() override { return this; }
	virtual game_sv_TeamDeathmatch* cast_game_sv_teamdeathmatch() override { return this; }

protected:
	virtual		void				WriteGameState			(CInifile& ini, const char* sect, bool bRoundResult);
};
