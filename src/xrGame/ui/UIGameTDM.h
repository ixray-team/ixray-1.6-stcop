#pragma once

#include "UIGameCustom.h"
#include "UIGameDM.h"

#include "../../xrUI/Widgets/UIDialogWnd.h"
#include "ui/UISpawnWnd.h"

// refs 

class CUITDMFragList;
class CUITDMPlayerList;
class CUISkinSelectorWnd;
class game_cl_TeamDeathmatch;
class CUIStatic;
class CUISpawnWnd;

class CUIGameTDM: public CUIGameDM
{
private:
	game_cl_TeamDeathmatch * m_game;
	typedef CUIGameDM inherited;

public:
	CUISpawnWnd*		m_pUITeamSelectWnd;

protected:
	CUIStatic*			m_team1_icon;
	CUIStatic*			m_team2_icon;
	CUIStatic*			m_team1_score;
	CUIStatic*			m_team2_score;
	CUIStatic*			m_buy_msg_caption;
	shared_str			m_buy_msg_caption_legacy = "tdm_buy";
public:
	CUIGameTDM			();
	virtual ~CUIGameTDM				() override;
	virtual void		SetClGame				(game_cl_GameState* g) override;
	virtual void		UnLoad					() override;
	virtual void		Init					(int stage) override;
			void		SetScoreCaption			(int t1, int t2);			
			void		SetBuyMsgCaption		(const char* str);
	virtual void		SetFraglimit			(int local_frags, int fraglimit) override;
	virtual void		Render					() override;
	virtual void _BCL	OnFrame					() override;

	virtual bool		IR_UIOnKeyboardPress	(int dik) override;
	virtual bool		IR_UIOnKeyboardRelease	(int dik) override;
};
