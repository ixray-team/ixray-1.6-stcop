#pragma once
#include "game_cl_deathmatch.h"

class CUISpawnWnd;
class CUIGameTDM;

class game_cl_TeamDeathmatch :public game_cl_Deathmatch
{
	using inherited = game_cl_Deathmatch;
	CUIGameTDM*							m_game_ui;
protected:
	bool								m_bFriendlyIndicators;
	bool								m_bFriendlyNames;

	virtual void				shedule_Update			(u32 dt) override;
	virtual void				TranslateGameMessage	(u32 msg, NET_Packet& P) override;

	virtual void				LoadSndMessages				() override;
public :
										game_cl_TeamDeathmatch	();
	virtual ~game_cl_TeamDeathmatch	() override;
	virtual void				Init					() override;
	virtual void				net_import_state		(NET_Packet& P) override;
	virtual CUIGameCustom*		createGameUI			() override;
	virtual void				SetGameUI				(CUIGameCustom*) override;
	virtual void				GetMapEntities			(xr_vector<SZoneMapEntityData>& dst) override;

	virtual bool					CanCallBuyMenu			() override;
	virtual bool					CanCallSkinMenu			() override;
	virtual bool					CanCallInventoryMenu	() override;
	virtual	bool					CanCallTeamSelectMenu	();


	virtual void					OnSpectatorSelect		() override;
	virtual void				OnSkinMenuBack			() override;
	virtual void				OnTeamMenuBack			() override;
	virtual void				OnMapInfoAccept			() override;

	virtual void				OnGameMenuRespond_ChangeTeam	(NET_Packet& P) override;

	virtual void				OnTeamSelect			(int Result) override;
	virtual char*				getTeamSection			(int Team) override;
	virtual void				OnTeamChanged			() override;
	virtual void				PlayRankChangesSndMessage () override;
	virtual void				OnTeamMenu_Cancel		() override;

	virtual			void				Set_ShowPlayerNames		(bool Show) {m_bShowPlayersNames = Show;};
	virtual			bool				Get_ShowPlayerNames		() {return m_bShowPlayersNames;};
	virtual s16					ModifyTeam				(s16 Team) override {return (Team != -1) ? Team-1 : Team;};
	virtual			bool				Get_ShowPlayerNamesEnabled () {return m_bFriendlyNames;};

	virtual bool				IsPlayerInTeam			(game_PlayerState* ps, ETeam team) override;
	virtual const char*				GetGameScore			(string32&	score_dest) override;
	s32									GetGreenTeamScore		() const { return teams[0].score; };
	s32									GetBlueTeamScore		() const { return teams[1].score; };
//from UIGameTDM
protected:
	virtual const shared_str			GetBaseCostSect			() override {return "teamdeathmatch_base_cost";}
	virtual const shared_str			GetTeamMenu				(s16 team) override;
//	CUISpawnWnd*						pUITeamSelectWnd;

	PRESET_ITEMS						PresetItemsTeam1;
	PRESET_ITEMS						PresetItemsTeam2;

	bool								m_bTeamSelected;
	bool								m_bShowPlayersNames;

	virtual bool						CanBeReady				() override;
	virtual void						SetCurrentBuyMenu		() override;
	virtual void						SetCurrentSkinMenu		() override;

	virtual bool						OnKeyboardPress			(int key) override;

	virtual void					OnRender				() override;
	virtual bool					IsEnemy					(game_PlayerState* ps) override;
	virtual bool					IsEnemy					(CEntityAlive* ea1, CEntityAlive* ea2) override;

	virtual void						UpdateMapLocations		() override;
	virtual void						OnSwitchPhase			(u32 old_phase, u32 new_phase) override;

	virtual void						SetScore				() override;
	virtual void						OnSwitchPhase_InProgress() override;

	virtual u8						GetTeamCount			() override { return 2; };
	virtual void					OnConnected				() override;

	virtual game_cl_mp* cast_game_cl_mp() override { return this; }
	virtual game_cl_Deathmatch* cast_game_cl_deathmatch() override { return this; }
	virtual game_cl_TeamDeathmatch* cast_game_cl_teamdeathmatch() override { return this; }
};

IC bool	TDM_Compare_Players		(game_PlayerState* p1, game_PlayerState* p2)
{
	return DM_Compare_Players(p1, p2);
};