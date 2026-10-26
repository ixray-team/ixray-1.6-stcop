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
	virtual ~CUIGameFMP() override;

	virtual void Init(int stage) override;

	virtual void SetClGame(game_cl_GameState* g) override;

	virtual void HideShownDialogs() override;

	virtual void _BCL OnFrame() override;

	virtual bool IR_UIOnKeyboardPress(int dik) override;
};