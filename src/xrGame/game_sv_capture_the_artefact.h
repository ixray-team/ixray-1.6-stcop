#pragma once

#include "game_sv_mp.h"
#include "actor_mp_server.h"
#include "xrServer.h"
#include "xrServer_Object_Base.h"
#include "AnomalyZone.h"



class game_sv_CaptureTheArtefact final : public game_sv_mp
{
private:
	using inherited = game_sv_mp;

	struct MyTeam
	{
		TEAM_DATA_LIST::size_type indexOfTeamInList;
		u16 playersCount;
		s32 score;
		// warning teamName must be <= 256 bytes !
		shared_str teamName;
		bool rPointInitialized;
		bool artefactActivated;
		
		RPoint artefactRPoint;
		shared_str artefactName;
		CSE_ALifeItemArtefact *artefact;
		CSE_ActorMP *artefactOwner;
		u32	freeArtefactTimeStart;
		ALife::_OBJECT_ID last_activator_id;
		
		u32 activationArtefactTimeStart;

		MyTeam();
		MyTeam(const MyTeam & clone);
		MyTeam(TEAM_DATA_LIST::size_type indexInTeamList, u16 pCount, const shared_str & tName, const shared_str & aName);
		void SetArtefactRPoint(const RPoint & rpoint);
		void OnPlayerAttachArtefact(CSE_ActorMP * newArtefactOwner);
		void OnPlayerDetachArtefact(CSE_ActorMP * oldArtefactOwner);
		void OnPlayerActivateArtefact(ALife::_OBJECT_ID eid_who);
		bool IsArtefactActivated();
		void DeactivateArtefact();
		CSE_ActorMP * GetArtefactOwner() const;
	};
	using TeamPair = std::pair<ETeam, MyTeam>;
	// For balancing team players count
	struct MinPlayersFunctor {
		bool operator()(const TeamPair & left, const TeamPair & right) const;
	};
	struct SearchArtefactIdFunctor {
		bool operator()(const TeamPair & tr, ALife::_OBJECT_ID artefactId) const;
	};
	struct SearchOwnerIdFunctor {
		bool operator()(const TeamPair & tr, ALife::_OBJECT_ID actorId) const;
	};
	
	using TeamsMap = xr_map<ETeam, MyTeam>;
	TeamsMap teams;

	//todo: transmit work with anomalies into other class...
	//----------------------------------------------------
	using TNameGameIDAnomalyPair = std::pair<xr_string, u16>;
	using TAnomaliesVector = xr_vector<TNameGameIDAnomalyPair>;

	using TAnomalyStartedPair = std::pair<TAnomaliesVector, u8>;
	using TAnomalySet = xr_vector<TAnomalyStartedPair>;

	using TGIDCPair = std::pair<u16, u8>; //GameIDCountPair
	using TMultiMap = xr_multimap<xr_string, TGIDCPair>;

	using TGameIDToBoughtFlag = xr_map<ClientID, int>;		//this map shows what player already bought items when he was dead...
	

	TAnomaliesVector				m_AnomaliesPermanent;
	TAnomalySet						m_AnomalySet;
	TMultiMap						m_AnomalyIds;

	u32								m_dwLastAnomalyStartTime;
	s32								m_iMoney_for_BuySpawn;
	
	TGameIDToBoughtFlag				m_dead_buyers;
	bool							m_bSpectatorMode;

	u32								m_dwWarmUp_CurTime;
	bool							m_bInWarmUp;
	
	u32								m_dwSM_SwitchDelta;
	u32								m_dwSM_LastSwitchTime;
	u32								m_dwSM_CurViewEntity;
	CObject	*						m_pSM_CurViewEntity;
	//static const float			spectr_cam_inert_value;
	//float							prev_cam_inert_value;
	void							SM_SwitchOnNextActivePlayer			();
	void							SM_SwitchOnPlayer		(CObject* pNewObject);
	void							SM_CheckViewSwitching();

	void	LoadAnomalySet();
	bool	LoadAnomaliesItems(const char* ini_set_id, TAnomaliesVector & dest_vector);
	
	
	void	StopPreviousAnomalies();
	void	ReStartRandomAnomaly();
	void	AddAnomalyChanges(
				NET_Packet & packet,
				TAnomaliesVector const & anomalies,
				CAnomalyZone::EZoneState state);

	void	SendAnomalyStates();
	void	CheckAnomalyUpdate(u32 current_time);
	void	CheckForWarmap(u32 current_time);
	
	u16		GetMinUsedAnomalyID(const char* zone_name);
	//----------------------------------------------------

	void LoadTeamData(ETeam eteam, const shared_str& caSection);
	void LoadArtefactRPoints();




	s32	GetMoneyAmount(const shared_str& caSection, char* caMoneyStr);
	void OnPlayerChangeSkin(ClientID id_who, s8 skin);
	void OnPlayerChangeTeam(game_PlayerState *playerState, s8 team);
	void ProcessPlayerDeath(game_PlayerState *playerState);
	//void ProcessPlayerKill(game_PlayerState *playerState);
	void Money_SetStart(game_PlayerState* ps);

	void ReSpawnArtefacts();
	void MoveArtefactToPoint(CSE_ALifeItemArtefact *artefact, RPoint const & toPoint);
	void MoveLifeActors();
	void RespawnDeadPlayers();
	void RespawnClient(xrClientData const * pclient);
	void  PrepareClientForNewRound(IClient* client);
	void BalanceTeams();
	void ClearReadyFlagFromAll();

	enum buyMenuPlayerState
	{
		buyMenuPlayerClosesBuyMenu		= 0,	// this value set in OnCloseBuyMenu
		buyMenuPlayerOpensBuyMenu		= 1,	// this value set in OnPlayerOpenBuyMenu
		buyMenuPlayerReadyToSpawn		= 2		// this value set in RespawnDeadPlayers
	};

	using TBuyMenuPlayerStates = associative_vector<xrClientData const *, buyMenuPlayerState>;
	TBuyMenuPlayerStates				m_buyMenuPlayerStates;
	void OnPlayerOpenBuyMenu(xrClientData const * pclient) override;				//this method invokes only if player dead
	void OnPlayerCloseBuyMenu(xrClientData const * pclient) override;			//if client state buyMenuPlayerReadyToSpawn respawn player
	void OnCloseBuyMenuFromAll();										//just clears buy menu player states associative vector
	bool CheckIfPlayerInBuyMenu(xrClientData const * pclient);
	void SetReadyToSpawnPlayer(xrClientData const * pclient);
	
	void OnPlayerBuyFinished(ClientID id_who, NET_Packet& P);
	//void DestroyAllPlayerItems(ClientID id_who);	//except rukzak and artefact :)
	
	//void DestroyGameItem(CSE_Abstract* entity);
	//void RejectGameItem(CSE_Abstract* entity);

	bool OnTouchItem(CSE_ActorMP *actor, CSE_Abstract *item);
	void OnDetachItem(CSE_ActorMP *actor, CSE_Abstract *item);

	void OnObjectEnterTeamBase(ALife::_OBJECT_ID id, u16 zone_team);
	void OnObjectLeaveTeamBase(ALife::_OBJECT_ID id, u16 zone_team);

	/// Moves and prepears all player for new round (invokes 
	/// PrepareActorForNewRound, MoveActorToPoint.
	void StartNewRound();
	void ActorDeliverArtefactOnBase(CSE_ActorMP *actor, ETeam actorTeam, ETeam teamOfArtefact);
	void DropArtefact(CSE_ActorMP *aOwner, CSE_ALifeItemArtefact *artefact, Fvector const *dropPosition = NULL);
	void ReturnArtefactToBase();
	void CheckForArtefactDelivering();
	void CheckForArtefactReturning(u32 currentTime);
	bool CheckForAllPlayersReady();
	bool CheckForRoundStart();
	bool CheckForRoundEnd();
	
	KILL_RES GetKillResult(game_PlayerState* pKiller, game_PlayerState* pVictim);
	bool OnKillResult(KILL_RES KillResult, game_PlayerState* pKiller, game_PlayerState* pVictim);
	void OnGiveBonus(KILL_RES KillResult, game_PlayerState* pKiller, game_PlayerState* pVictim, KILL_TYPE KillType, SPECIAL_KILL_TYPE SpecialKillType, CSE_Abstract* pWeaponA);
	
	bool roundStarted;
	u32 nextReinforcementTime;
	u32 currentTime;
	bool teams_swaped;


	using InvincibilityTimeouts = associative_vector<ClientID, u32>;
	InvincibilityTimeouts m_invTimeouts;
	void ResetTimeoutInvincibility(u32 currentTime);
	bool ResetInvincibility(ClientID const clientId);

	bool	isFriendlyFireEnabled	() override;
	float	GetFriendlyFire			();
	int		Get_TeamKillLimit		();
	bool	Get_TeamKillPunishment	();
	bool	Get_FriendlyIndicators	();
	bool	Get_FriendlyNames		();
	int		Get_ReinforcementTime_msec	();
	u32		GetWarmUpTime			();
	s32		GetTimeLimit			();

	bool	isAnomaliesEnabled				();
	bool	isPDAHuntEnabled				();
	u32		Get_InvincibilityTime_msec		();
	u32		Get_AnomalySetLengthTime_msec	();
	u32		Get_ArtefactReturningTime_msec	();
	u32		Get_ActivatedArtefactRet		();
	u32		Get_PlayerScoresDelayTime_msec	();
	s32		Get_ScoreLimit();
	bool	Get_BearerCanSprint				();
protected:
	void ReadOptions(shared_str &options) override;
	void FillDeathActorRejectItems(CSE_ActorMP *actor, xr_vector<CSE_Abstract*> & to_reject) override;
	shared_str m_not_free_ammo_str;
	bool CanChargeFreeAmmo(char const * ammo_section) override;
	void WriteGameState(CInifile& ini, const char* sect, bool bRoundResult) override;
public:
	game_sv_CaptureTheArtefact();
	virtual ~game_sv_CaptureTheArtefact();

	const char* type_name() const override;
	void Create(shared_str &options) override;
	void OnPlayerConnect(ClientID id_who) override;
	void OnPlayerDisconnect(ClientID id_who, LPSTR Name, ALife::_OBJECT_ID GameID) override;
	void OnPlayerConnectFinished(ClientID id_who) override;
	void OnPlayerHitted(NET_Packet P) override;

	void OnPlayerReady(ClientID id_who) override;

	void OnPlayerSelectSkin(NET_Packet& P, ClientID sender) override;
	void OnPlayerSelectTeam(NET_Packet& P, ClientID sender) override;
	void OnPlayerSelectSpectator(NET_Packet& P, ClientID sender) override;
	void OnRoundStart() override;
	void OnRoundEnd() override;

	bool OnPreCreate	(CSE_Abstract* E) override;
	void OnCreate		(ALife::_OBJECT_ID eid_who) override;
	void OnPostCreate	(ALife::_OBJECT_ID id_who) override;
	void OnDestroyObject(ALife::_OBJECT_ID eid_who) override;

	void Update() override;

	void net_Export_State(NET_Packet& P, ClientID id_to) override;
	void net_Export_Update(NET_Packet& P, ClientID id_to, ClientID id) override;
	
	virtual	void LoadSkinsForTeam(const shared_str& caSection, TEAM_SKINS_NAMES* pTeamSkins);
	virtual void LoadDefItemsForTeam(const shared_str& caSection, DEF_ITEMS_LIST* pDefItems);
	virtual void SpawnWeaponsForActor(CSE_Abstract* pE, game_PlayerState*	ps);
	void OnPlayerKillPlayer(game_PlayerState* ps_killer, game_PlayerState* ps_killed, KILL_TYPE KillType, SPECIAL_KILL_TYPE SpecialKillType, CSE_Abstract* pWeaponA) override;

	void OnPlayerHitPlayer(ALife::_OBJECT_ID id_hitter, ALife::_OBJECT_ID id_hitted, NET_Packet& P) override;
	virtual void OnPlayerHitPlayer_Case(game_PlayerState* ps_hitter, game_PlayerState* ps_hitted, SHit* pHitS);

	bool OnTouch(ALife::_OBJECT_ID eid_who, ALife::_OBJECT_ID eid_target, bool bForced = false) override;			// true=allow ownership, false=denied
	void OnDetach(ALife::_OBJECT_ID eid_who, ALife::_OBJECT_ID eid_target) override;
	bool OnActivate(ALife::_OBJECT_ID eid_who, ALife::_OBJECT_ID eid_target) override;
	void OnEvent(NET_Packet &tNetPacket, u16 type, u32 time, ClientID sender ) override;
	void RespawnPlayer(ClientID id_who, bool NoSpectator) override;
	void OnPlayerBuySpawn(ClientID sender) override;
	bool Player_Check_Rank(game_PlayerState* ps) override;

			void SwapTeams();

	virtual game_sv_mp* cast_game_sv_mp() override { return this; }
	virtual game_sv_CaptureTheArtefact* cast_game_sv_capturetheartefact() override { return this; }
};
