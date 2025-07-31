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

class CUIGameSP : public CUIGameCustom
{
private:
	game_cl_Single*		m_game;
	typedef CUIGameCustom inherited;
public:
	CUIGameSP									();
	virtual				~CUIGameSP				();

	virtual void		SetClGame				(game_cl_GameState* g);
	virtual bool		IR_UIOnKeyboardPress	(int dik);
	virtual void _BCL	OnFrame					();

 
 #ifdef DEBUG
	virtual void		Render					();
#endif


	SDrawStaticStruct*	m_game_objective;
};


