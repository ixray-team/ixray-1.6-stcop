#include "stdafx.h"
#include "ESceneDOTools_Paint.h"

void EDetailManager::CreateControls()
{
    AddControl(new TUI_CustomControl(estDefault, etaSelect, this));
    AddControl(new TUI_ControlDOPaint(estDOPaint, etaAdd, this));
    // frame
    pForm = new UIDOTool();
    ((UIDOTool*)pForm)->DM = this;
    // pFrame = new TfraDetailObject((TComponent*)0, this);
}

void EDetailManager::RemoveControls()
{
    inherited::RemoveControls();
}
