#include "stdafx.h"

void ESceneSoundSrcTool::CreateControls()
{
	inherited::CreateDefaultControls(estDefault);
	// AddControl(xr_new<TUI_CustomControl>(estDefault, etaSelect, this));
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


