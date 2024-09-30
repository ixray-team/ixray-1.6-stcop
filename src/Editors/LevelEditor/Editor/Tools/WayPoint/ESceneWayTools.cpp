#include "stdafx.h"
CCustomObject* ESceneWayTool::CreateObject(LPVOID data, LPCSTR name)
{
	CCustomObject* O	= new CWayObject(data,name);
    O->FParentTools		= this;
    return O;
}


