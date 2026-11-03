#pragma once
#include "UIGameCustom.h"
#include "game_graph_space.h"

class CUITradeWnd;			
class CUITalkWnd;			
class CInventory;

class game_cl_Single;
class CChangeLevelWnd;
class CUIMessageBox;
class CInventoryBox;
class CInventoryOwner;

class CUIGameSP final : public CUIGameCustom
{
private:
	game_cl_Single*		m_game;
	typedef CUIGameCustom inherited;
public:
	CUIGameSP									();
	virtual ~CUIGameSP				() override;

	virtual void		SetClGame				(game_cl_GameState* g) override;
	virtual bool		IR_UIOnKeyboardPress	(int dik) override;
	virtual bool		IR_UIOnGamepadKeyPress	(int dik) override;
	virtual void _BCL	OnFrame					() override;

	void				OnAction				(CActor* actor, EGameActions action);
 
 #ifdef DEBUG
	virtual void		Render					() override;
#endif


	SDrawStaticStruct*	m_game_objective;
};


