#include "stdafx.h"
class TUI_ControlSoundSrcTool : public TUI_CustomControl
{
public:
	TUI_ControlSoundSrcTool(int st, int act, ESceneToolBase* parent) :TUI_CustomControl(st, act, parent) {}
	virtual bool IsSupportRotate() { return false; }
	virtual bool IsSupportScale() { return false; }
};
void ESceneSoundSrcTool::CreateControls()
{
	inherited::CreateDefaultControls(estDefault);
	AddControl(xr_new<TUI_ControlSoundSrcTool>(estDefault, etaSelect, this));
}


void ESceneSoundSrcTool::RemoveControls()
{
	inherited::RemoveControls();
}


CCustomObject* ESceneSoundSrcTool::CreateObject(LPVOID data, LPCSTR name)
{
	CCustomObject* O	= xr_new<ESoundSource>(data,name);
    O->FParentTools		= this;
    return O;
}


