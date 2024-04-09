#pragma once
#include "UIGameMP.h"

class game_cl_freemp;

class CUIGameFMP :
	public UIGameMP
{
private:
	game_cl_freemp* m_game;
	typedef UIGameMP inherited;

public:
	CUIGameFMP();
	virtual ~CUIGameFMP();

	virtual void SetClGame(game_cl_GameState* g);
	virtual bool IR_UIOnKeyboardPress(int dik);
};