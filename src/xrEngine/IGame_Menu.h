#pragma once
class CUIWindow;

class IMainMenu
{
public:
	virtual			~IMainMenu() {};
	virtual void	Activate(bool bActive) = 0;
	virtual	bool	IsActive() = 0;
	virtual	bool	CanSkipSceneRendering() = 0;
	virtual void	DestroyInternal(bool bForce) = 0;

	virtual void RegisterPPDraw(CUIWindow*) = 0;
	virtual void UnregisterPPDraw(CUIWindow*) = 0;

	virtual class CDialogHolder* GetDialogHolder() = 0;
};