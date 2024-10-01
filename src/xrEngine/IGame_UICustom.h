#pragma once

class IGame_CustomUI
{
public:
	virtual void ShowGameIndicators(bool) = 0;
	virtual bool GameIndicatorsShown() const = 0;
	virtual class CDialogHolder* GetDialogHolder() = 0;
};

extern ENGINE_API IGame_CustomUI* g_pGameCustom;