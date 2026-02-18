#pragma once

#include "game_sv_teamdeathmatch.h"

class game_sv_ArtefactHunt final : public game_sv_TeamDeathmatch
{
private:
	using inherited = game_sv_TeamDeathmatch;

	enum	ARTEFACT_STATE
	{
		NONE,
		NOARTEFACT,
		ON_FIELD,
		IN_POSSESSION,
	};

protected:
	
	u32								m_dwNextReinforcementTime;
	int								m_iMoney_for_BuySpawn;

	u32								m_dwArtefactSpawnTime;
	u32								m_dwArtefactRemoveTime;

	u16								m_ArtefactsSpawnedTotal;
	ALife::_OBJECT_ID				m_dwArtefactID;	

	ARTEFACT_STATE					m_eAState;
	bool							m_bArtefactWasTaken;
	bool							m_bArtefactWasDropped;

	xr_vector<RPoint>				Artefact_rpoints;
//.	xr_vector<u8>					ArtefactsRPoints_ID;
//.	u8								m_LastRespawnPointID;
	CRandom							ArtefactChooserRandom;

	ALife::_OBJECT_ID artefactBearerID;//ah,ZoneMap
	ALife::_OBJECT_ID m_iAfBearerMenaceID;
	u8								teamInPossession;//ah,ZoneMap

	bool							bNoLostMessage;
	bool							m_bArtefactWasBringedToBase;

	bool							m_bSwapBases;

	void							Artefact_PrepareForSpawn	();
	void							Artefact_PrepareForRemove	();

	bool							Artefact_NeedToSpawn	();
	bool							Artefact_NeedToRemove	();
	bool							Artefact_MissCheck		();

	void							CheckForAnyAlivePlayer	();
	void							UpdatePlayersNotSendedMoveRespond();
	void							ReplicatePlayersStateToPlayer(ClientID CID);

	void				check_Player_for_Invincibility	(game_PlayerState* ps) override;
	void				Check_ForClearRun		(game_PlayerState* ps) override;

	void				ReadOptions				(shared_str &options) override;
	void				ConsoleCommands_Create	() override;
	void				ConsoleCommands_Clear	() override;

	bool				Player_Check_Rank		(game_PlayerState* ps) override;
	//virtual		void			DestroyAllPlayerItems(ClientID id_who);

	bool		assign_rp_tmp		(game_PlayerState* ps_who, xr_vector<RPoint>& points_vec, xr_vector<u32>& dest, xr_vector<u32>& rpIDEnemy, xr_vector<ClientID>& EnemyIt,  bool use_safe_dist);
public:

									game_sv_ArtefactHunt	(){m_type = eGameIDArtefactHunt;}
	void				Create					(shared_str& options) override;

	const char*				type_name			() const override { return "artefacthunt";};
	// Events	
	void				OnEvent					(NET_Packet &tNetPacket, u16 type, u32 time, ClientID sender ) override;
	void				OnRoundStart			() override;							// старт раунда
	KILL_RES			GetKillResult			(game_PlayerState* pKiller, game_PlayerState* pVictim) override;
	bool				OnKillResult			(KILL_RES KillResult, game_PlayerState* pKiller, game_PlayerState* pVictim) override;
	void				OnGiveBonus				(KILL_RES KillResult, game_PlayerState* pKiller, game_PlayerState* pVictim, KILL_TYPE KillType, SPECIAL_KILL_TYPE SpecialKillType, CSE_Abstract* pWeaponA) override;
	void				OnPlayerHitPlayer		(ALife::_OBJECT_ID id_hitter, ALife::_OBJECT_ID id_hitted, NET_Packet& P) override;
	void				OnPlayerHitPlayer_Case	(game_PlayerState* ps_hitter, game_PlayerState* ps_hitted, SHit* pHitS) override;
	void				OnPlayerKillPlayer		(game_PlayerState* ps_killer, game_PlayerState* ps_killed, KILL_TYPE KillType, SPECIAL_KILL_TYPE SpecialKillType, CSE_Abstract* pWeaponA) override;
	void				OnPlayerFire			(ClientID id_who, NET_Packet &P) override {};
	void				Victim_Exp				(game_PlayerState* pVictim) override {};
	void				UpdateTeamScore			(game_PlayerState* ps_killer, s16 OldKills) override {};
	void				OnPlayerReady			(ClientID id_who) override;
	void				OnPlayerBuySpawn		(ClientID sender) override;

	void				OnTimelimitExceed		() override;

	void				assign_RP				(CSE_Abstract* E, game_PlayerState* ps_who) override;
	u32					RP_2_Use				(CSE_Abstract* E) override;	
	virtual		void				CheckRPUnblock			();
	virtual		void				SetRP					(CSE_Abstract* E, RPoint* pRP);

	void				LoadTeams				() override;

	char*				GetAnomalySetBaseName	() override {return (char*) "artefacthunt_game_anomaly_sets";};

	void				OnObjectEnterTeamBase	(ALife::_OBJECT_ID id, u16 zone_team) override;
	void				OnObjectLeaveTeamBase	(ALife::_OBJECT_ID id, u16 zone_team) override;
	
	void							OnArtefactOnBase		(ClientID id_who);

	bool				OnTouch					(ALife::_OBJECT_ID eid_who, ALife::_OBJECT_ID eid_what, bool bForced = false) override;
	void				OnDetach				(ALife::_OBJECT_ID eid_who, ALife::_OBJECT_ID eid_what) override;
	void				OnCreate				(ALife::_OBJECT_ID id_who) override;


	virtual		void				Update					();
				
				void				SpawnArtefact			();
				void				RemoveArtefact			();
				void				Assign_Artefact_RPoint	(CSE_Abstract* E);

	virtual		void				net_Export_State		(NET_Packet& P, ClientID id_to);				// full state
				bool				ArtefactSpawn_Allowed	();
	//-------------------------------------------------------------------------------
	virtual		void				RespawnAllNotAlivePlayers	();
	virtual		bool				CheckAlivePlayersInTeam		(s16 Team);
	virtual		void				MoveAllAlivePlayers			();
	virtual		void				CheckForTeamElimination		();
	virtual		void				CheckForTeamWin				();
	virtual		bool				CanHaveFriendlyFire		()	{return true;}

	//-----------------------------------------------------------------------------
	virtual		int					Get_ArtefactsCount			();
	virtual		u32					Get_ArtefactsRespawnDelta	();
	virtual		u32					Get_ArtefactsStayTime		();
	virtual		int					Get_ReinforcementTime		();
	virtual		bool				Get_ShieldedBases			();
	virtual		bool				Get_ReturnPlayers			();
	virtual		bool				Get_BearerCantSprint		();

				void				SwapTeams					();

	//  [7/5/2005]
#ifdef DEBUG_DRAW
	virtual		void				OnRender				();
#endif
	//  [7/5/2005]

	virtual game_sv_mp* cast_game_sv_mp() override { return this; }
	virtual game_sv_Deathmatch* cast_game_sv_deathmatch() override { return this; }
	virtual game_sv_TeamDeathmatch* cast_game_sv_teamdeathmatch() override { return this; }
	virtual game_sv_ArtefactHunt* cast_game_sv_artefacthunt() override { return this; }

protected:
	virtual		void				WriteGameState			(CInifile& ini, const char* sect, bool bRoundResult);
};
