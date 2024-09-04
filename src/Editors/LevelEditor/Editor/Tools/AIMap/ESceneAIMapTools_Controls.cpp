#include "stdafx.h"

void ESceneAIMapTool::CreateControls()
{
    inherited::CreateDefaultControls(estDefault);
    // node tools
    AddControl(new TUI_ControlAIMapNodeSelect(estDefault,   etaSelect, this));
    AddControl(new TUI_ControlAIMapNodeAdd   (estDefault,   etaAdd,    this));
    AddControl(new TUI_ControlAIMapNodeMove  (estAIMapNode, etaMove,   this));
    AddControl(new TUI_ControlAIMapNodeRotate(estAIMapNode, etaRotate, this));
    // frame
    pForm = new UIAIMapTool();
    ((UIAIMapTool*)pForm)->tool = this;
    // pFrame = new TfraAIMap((TComponent*)0, this);
}

void ESceneAIMapTool::RemoveControls()
{
    inherited::RemoveControls();
}


