#include "stdafx.h"
#include "ESceneDOTools_Paint.h"

void EDetailManager::CreateControls()
{
    AddControl(new TUI_CustomControl(estDefault, etaSelect, this));
    // changing the sub target re-activates the tool, which resets the action to etaSelect,
    // so the brush has to answer to both actions
    AddControl(new TUI_ControlDOPaint(estDOPaint, etaSelect, this));
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
