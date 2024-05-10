#include "stdafx.h"

class TUI_ControlDOToolsSelect : public TUI_CustomControl
{
public:
	TUI_ControlDOToolsSelect(int st, int act, ESceneToolBase* parent) :TUI_CustomControl(st, act, parent) {}
	virtual bool IsSupportMove() { return false; }
	virtual bool IsSupportRotate() { return false; }
	virtual bool IsSupportScale() { return false; }
};
void EDetailManager::CreateControls()
{
	AddControl		(xr_new<TUI_ControlDOToolsSelect>(estDefault,	etaSelect, this));
	// frame
	pForm = xr_new< UIDOTool>();
	((UIDOTool*)pForm)->DM = this;
   // pFrame 			= xr_new<TfraDetailObject>((TComponent*)0,this);
}

 
void EDetailManager::RemoveControls()
{
	inherited::RemoveControls();
}


                     