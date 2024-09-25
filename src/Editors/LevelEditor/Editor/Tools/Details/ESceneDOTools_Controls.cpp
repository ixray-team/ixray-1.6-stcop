#include "stdafx.h"

void EDetailManager::CreateControls()
{
    AddControl(xr_new<TUI_CustomControl>(estDefault, etaSelect, this));
    // frame
    pForm = xr_new< UIDOTool>();
    ((UIDOTool*)pForm)->DM = this;
    // pFrame = xr_new<TfraDetailObject>((TComponent*)0, this);
}

void EDetailManager::RemoveControls()
{
    inherited::RemoveControls();
}
