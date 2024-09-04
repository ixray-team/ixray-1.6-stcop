#include "stdafx.h"

void EScenePSTool::CreateControls()
{
	inherited::CreateDefaultControls(estDefault); 

	AddControl(new TUI_ControlPSAdd(estDefault, etaAdd, this));
	// AddControl(new TUI_CustomControl(estDefault, etaSelect, this));
	// frame
	pForm = new UIParticlesTool();
}


void EScenePSTool::RemoveControls()
{
	inherited::RemoveControls();
}


CCustomObject* EScenePSTool::CreateObject(LPVOID data, LPCSTR name)
{
	CCustomObject* O	= new EParticlesObject(data,name);
    O->FParentTools		= this;
    return O;
}

bool  EScenePSTool::ExportGame(SExportStreams* F)
{
	return inherited::ExportGame	(F);
}

