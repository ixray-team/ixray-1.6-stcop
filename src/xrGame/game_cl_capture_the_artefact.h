#pragma once

#include "game_cl_mp.h"
#include "UIGameCTA.h"
#include "game_cl_capture_the_artefact_captions_manager.h"
/// This class describes the client part of "Capture The Artefact" game mode.
class game_cl_CaptureTheArtefact final : public game_cl_mp
{
private:
	using inherited = game_cl_mp;
	
	CUIGameCTA * m_game_ui;

	bool	m_bReadMapDesc;
	bool	m_bTeamSelected;
	bool	m_bSkinSelected;
	bool	m_winnerTeamShowed;
	
	CTAGameClCaptionsManager	m_captions_manager;

	u32		m_curReinforcementTime;
	u32		m_maxReinforcementTime;

	u32		m_currentWarmupTime;
	s32		m_s32TimeLimit;
	bool	m_inWarmup;

	s32 maxScore;
	s32 greenTeamScore;
	s32 blueTeamScore;

	Fvector	greenTeamRPos;
	Fvector	blueTeamRPos;

	ALife::_OBJECT_ID greenArtefact;
	ALife::_OBJECT_ID blueArtefact;
	bool haveGotUpdate;
	//bool sendedSpawnMe;

	ALife::_OBJECT_ID greenArtefactOwner;	//GameID, if 0, then no body took the artefact
	ALife::_OBJECT_ID blueArtefactOwner;
	
	const shared_str & GetLocalPlayerTeamSection() const;
	ETeam GetLocalPlayerTeam() const;
	
	bool		m_player_on_base;
	bool		m_allow_buy;
	bool		m_bFriendlyIndicators;
	bool		m_bFriendlyNames;
	bool		m_bBearerCantSprint;
	bool		m_bCanActivateArtefact;
	bool		m_bShowPlayersNames;
	u32			m_dwVoteEndTime;
	float		m_baseRadius;

	s32		spawn_cost;
	s32		buy_amount;
	s32		total_money;
	s32		last_money;

	void UpdateMoneyIndicator();
	
	virtual void LoadSndMessages() override;
	void OnPlayerEnterBase();
	void OnPlayerLeaveBase();
	void SetInvinciblePlayer(ALife::_OBJECT_ID const gameId, bool const invincible);
	void SpawnMe();

	void PlayCapturedTheArtefact(game_PlayerState const * capturer);
	void PlayDeliveredTheArtefact(game_PlayerState const * deliverer);
	void PlayReturnedTheArtefact(game_PlayerState const * returnerer);
	void PlayRankChangedSnd();

	void UpdateVotingTime(u32 current_time);
	void UpdateWarmupTime(u32 current_time);
	void UpdateTimeLimit(u32 current_time);
public:
	game_cl_CaptureTheArtefact();
	virtual void	SetGameUI(CUIGameCustom * already_created_ui) override;
	virtual ~game_cl_CaptureTheArtefact() override;
	virtual CUIGameCustom * createGameUI() override;
	/// Initializes teams
	virtual void Init() override;
	virtual void shedule_Update(u32 dt) override;
	virtual void net_import_state(NET_Packet& P) override;
	virtual void net_import_update(NET_Packet& P) override;
	virtual void TranslateGameMessage(u32 msg, NET_Packet& P) override;

	virtual void OnGameMenuRespond_ChangeSkin(NET_Packet& P) override;
	virtual void OnGameMenuRespond_ChangeTeam(NET_Packet& P) override;
	virtual void OnPlayerFlagsChanged(game_PlayerState* ps) override;
	virtual void OnNewPlayerConnected(ClientID const & newClient) override;
	virtual void OnRender() override;

	virtual bool IsEnemy(game_PlayerState* ps) override;

	virtual bool CanBeReady() override;
	virtual bool NeedToSendReady_Actor			(int key, game_PlayerState* ps) override;
	virtual bool NeedToSendReady_Spectator		(int key, game_PlayerState* ps) override;

	virtual bool OnKeyboardPress(int key) override;
	virtual bool OnKeyboardRelease(int key) override;

	virtual void OnSpawn(CObject* pObj) override;
	virtual	bool CanCallBuyMenu();
	virtual bool CanCallSkinMenu() {return true;};
	virtual bool CanCallTeamSelectMenu();
	virtual	bool CanCallInventoryMenu();
			bool LocalPlayerCanBuyItem(shared_str const & name_sect);
	
			void	Set_ShowPlayerNames			(bool Show) {m_bShowPlayersNames = Show;};
			bool	Get_ShowPlayerNames			()			{return m_bShowPlayersNames;};
			bool	Get_ShowPlayerNamesEnabled	()			{return m_bFriendlyNames;};

	//ui part
	virtual void OnSpectatorSelect() override;
	virtual void OnMapInfoAccept() override;

	virtual void OnTeamChanged() override;
	virtual void OnRankChanged(u8 OldRank) override;
			void OnTeamScoresChanged();

	virtual void OnTeamMenuBack() override;
	virtual void OnTeamMenu_Cancel() override;
	virtual void OnTeamSelect(int Team) override;
	virtual void OnBuyMenu_Ok() override;
	virtual void OnBuyMenu_Cancel() override;
	virtual void OnBuySpawnMenu_Ok() override;
			
			void OnBuyMenuOpen();

	virtual void OnSkinMenuBack			() override;
	virtual void OnSkinMenu_Ok			() override;
	virtual void OnSkinMenu_Cancel		() override;

	virtual void UpdateMapLocations		() override;

	virtual void	OnVoteStart(NET_Packet& P) override;
	virtual void	OnVoteStop(NET_Packet& P) override;
	virtual void	OnVoteEnd(NET_Packet& P) override;

	virtual char*	getTeamSection			(int Team) override;

	virtual s16	ModifyTeam(s16 Team) override {return Team;};
	virtual bool PlayerCanSprint(CActor* pActor) override;

	//very bad... need to fix problem with team indexes...
	virtual void OnSpeechMessage(NET_Packet& P) override;
	virtual u8 GetTeamCount	() override { return 2; };

	virtual void OnSwitchPhase(u32 old_phase, u32 new_phase) override;
	virtual void OnGameRoundStarted	() override;
	ALife::_OBJECT_ID GetGreenArtefactOwnerID		() const;
	ALife::_OBJECT_ID GetBlueArtefactOwnerID		() const;
	
	Fvector const & GetGreenArtefactRPoint	()	const;
	Fvector const & GetBlueArtefactRPoint	()	const;
	float			GetBaseRadius			()	const;
	ALife::_OBJECT_ID GetGreenArtefactID		()	const;
	ALife::_OBJECT_ID GetBlueArtefactID		()	const;
	bool			CanActivateArtefact		()	const;

	bool			InWarmUp				()	const;
	bool			HasTimeLimit			()	const;

	virtual const char*	GetGameScore			(string32&	score_dest) override;
	virtual void	OnConnected				() override;
	s32				GetGreenTeamScore		() const { return greenTeamScore; };
	s32				GetBlueTeamScore		() const { return blueTeamScore; };

	virtual game_cl_CaptureTheArtefact* cast_game_cl_capturetheartefact() override { return this; }
	virtual game_cl_mp* cast_game_cl_mp() override { return this; }
};