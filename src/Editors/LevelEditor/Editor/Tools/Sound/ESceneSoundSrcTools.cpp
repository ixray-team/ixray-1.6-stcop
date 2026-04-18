#include "stdafx.h"

void ESceneSoundSrcTool::CreateControls()
{
	inherited::CreateDefaultControls(estDefault);
	// AddControl(new TUI_CustomControl(estDefault, etaSelect, this));
}


void ESceneSoundSrcTool::RemoveControls()
{
	inherited::RemoveControls();
}


CCustomObject* ESceneSoundSrcTool::CreateObject(LPVOID data, const char* name)
{
	CCustomObject* O	= new ESoundSource(data,name);
    O->FParentTools		= this;
    return O;
}


