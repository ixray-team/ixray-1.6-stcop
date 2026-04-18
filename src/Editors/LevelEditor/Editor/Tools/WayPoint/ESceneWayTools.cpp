#include "stdafx.h"
CCustomObject* ESceneWayTool::CreateObject(LPVOID data, const char* name)
{
	CCustomObject* O	= new CWayObject(data,name);
    O->FParentTools		= this;
    return O;
}


