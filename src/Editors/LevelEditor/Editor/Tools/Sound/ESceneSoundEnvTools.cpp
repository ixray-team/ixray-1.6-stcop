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
	inherited::Clear(bSpecific);
	if (LSndLib)
	{
		LSndLib->RefreshEnvGeometry();
	}
}

CCustomObject* ESceneSoundEnvTool::CreateObject(LPVOID data, const char* name)
{
	CCustomObject* O	= new ESoundEnvironment(data,name);
    O->FParentTools		= this;
    return O;
}