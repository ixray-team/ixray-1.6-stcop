#include "stdafx.h"

void ESceneSoundEnvTool::CreateControls()
{
	inherited::CreateDefaultControls(estDefault);
}


void ESceneSoundEnvTool::RemoveControls()
{
	inherited::RemoveControls();
}


void ESceneSoundEnvTool::Clear(bool bSpecific)
{
	inherited::Clear	(bSpecific);
    LSndLib->RefreshEnvGeometry	();
}


CCustomObject* ESceneSoundEnvTool::CreateObject(LPVOID data, LPCSTR name)
{
	CCustomObject* O	= new ESoundEnvironment(data,name);
    O->FParentTools		= this;
    return O;
}