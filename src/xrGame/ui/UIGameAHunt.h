#pragma once

#include "UIGameCustom.h"
#include "UIGameTDM.h"

#include "../../xrUI/Widgets/UIDialogWnd.h"
#include "ui/UISpawnWnd.h"

// refs 
class CUIAHuntFragList;
class CUIAHuntPlayerList;
class game_cl_ArtefactHunt;
class CUIStatic;
class CUIProgressShape;
class CUIMessageBoxEx;

class CUIGameAHunt final : public CUIGameTDM
{
private:
	game_cl_ArtefactHunt * m_game;
	typedef CUIGameTDM inherited;

public:
	CUIStatic*	m_pReinforcementInidcator;
	CUIProgressShape* m_pReinforcementInidcator_old;
	CUIMessageBoxEx*	m_pBuySpawnMsgBox;

public:
	virtual void		SetClGame				(game_cl_GameState* g) override;
	CUIGameAHunt								();
	virtual ~CUIGameAHunt			() override;

	virtual void		Init					(int stage) override;
	virtual void		UnLoad					() override;
			void		SetBuyMsgCaption		(const char* str);
	virtual void		Render					() override;
	virtual void		OnFrame					() override;

protected:
	CUIStatic*	m_buy_msg_caption;
	shared_str	m_buy_msg_caption_legacy = "ah_buy";
};
