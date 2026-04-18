#pragma once

#include "UIGameMP.h"


class CUIDMPlayerList;
class CUIDMStatisticWnd;
class CUISkinSelectorWnd;
class game_cl_Deathmatch;
class CUIMoneyIndicator;
class CUIRankIndicator;
class UIVoteStatusWnd;
class CUIMapDesc;
class UITeamPanels;
class CUIStatic;
class CUIWindow;

class CUIGameDM: public UIGameMP
{
private:
	game_cl_Deathmatch *	m_game;
	typedef UIGameMP inherited;

public:
	CUIMapDesc*			m_pMapDesc;

protected:
	enum{
		flShowFragList	= (1<<1),
		fl_force_dword	= u32(-1)	};


	CUIWindow*						m_pFragLists;
	CUIWindow*						m_pPlayerLists;
	//-----------------------------------------
	CUIWindow*						m_pStatisticWnds;
	//-----------------------------------------
	UITeamPanels*					m_pTeamPanels;

	CUIStatic*						m_time_caption;
	CUIStatic*						m_spectrmode_caption;
	CUIStatic*						m_spectator_caption;
	CUIStatic*						m_pressjump_caption;
	CUIStatic*						m_pressbuy_caption;
	CUIStatic*						m_round_result_caption;
	CUIStatic*						m_force_respawn_time_caption;
	CUIStatic*						m_demo_play_caption;
	CUIStatic*						m_warm_up_caption;
	
	shared_str						m_time_caption_legacy = "timelimit";
	shared_str						m_spectrmode_caption_legacy = "spetatormode";
	shared_str						m_spectator_caption_legacy = "spectator";
	shared_str						m_pressjump_caption_legacy = "pressjump";
	shared_str						m_pressbuy_caption_legacy = "pressbuy";
	shared_str						m_round_result_caption_legacy = "round_result";
	shared_str						m_force_respawn_time_caption_legacy = "force_respawn_time";
	shared_str						m_demo_play_caption_legacy = "demo_play";
	shared_str						m_warm_up_caption_legacy = "warm_up";

	CUIMoneyIndicator*				m_pMoneyIndicator;
	CUIRankIndicator*				m_pRankIndicator;
	CUIStatic*						m_pFragLimitIndicator;
	UIVoteStatusWnd*				m_voteStatusWnd;
public:
									CUIGameDM				();
	virtual 						~CUIGameDM				();

	virtual void					SetClGame				(game_cl_GameState* g);
	virtual	void					Init					(int stage);
	virtual void					UnLoad					();
	virtual void					Render					();
	virtual void	_BCL			OnFrame					();

	void							SetRank							(s16 team, u8 rank);

	virtual void					ChangeTotalMoneyIndicator		(const char* newMoneyString);
	virtual void					DisplayMoneyChange				(const char* deltaMoney);
	virtual void					DisplayMoneyBonus				(KillMessageStruct* bonus);
	virtual void					SetFraglimit					(int local_frags, int fraglimit);

			void					SetTimeMsgCaption				(const char* str);
			void					SetSpectrModeMsgCaption			(const char* str);
			void					SetSpectatorMsgCaption			(const char* str);
			void					SetPressJumpMsgCaption			(const char* str);
			void					SetPressBuyMsgCaption			(const char* str);
			void					SetRoundResultCaption			(const char* str);
			void					SetForceRespawnTimeCaption		(const char* str);
			void					SetDemoPlayCaption				(const char* str);
			void					SetWarmUpCaption				(const char* str);

			void					SetVoteMessage					(const char* str);
			void					SetVoteTimeResultMsg			(const char* str);

			void					UpdateTeamPanels				();

			void					ShowFragList					(bool bShow);
			void					ShowPlayersList					(bool bShow);
};
