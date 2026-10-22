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

void EDetailManager::OnActivate()
{
    // the base resets the action to etaSelect, which runs after the paint button already
    // asked for etaAdd (the sub target change is deferred), so request the brush action last
    inherited::OnActivate();
    if (sub_target == estDOPaint)
    {
        ExecCommand(COMMAND_CHANGE_ACTION, etaAdd);
    }
}

void EDetailManager::RemoveControls()
{
    inherited::RemoveControls();
}
