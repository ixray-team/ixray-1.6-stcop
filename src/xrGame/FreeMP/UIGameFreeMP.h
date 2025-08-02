#pragma once

#include "UIGameMP.h"

class CUIStatic;
class game_cl_freemp;

class CUIGameFMP :
	public UIGameMP
{
private:
	game_cl_freemp* m_game;
	typedef UIGameMP inherited;
protected:
	CUIStatic* m_stats;
public:
	CUIGameFMP();
	virtual ~CUIGameFMP();

	virtual	void Init(int stage);

	virtual void SetClGame(game_cl_GameState* g);

	virtual void HideShownDialogs();

	virtual void _BCL OnFrame();

	virtual bool IR_UIOnKeyboardPress(int dik);
};