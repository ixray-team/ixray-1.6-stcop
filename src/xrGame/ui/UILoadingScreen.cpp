#include "StdAfx.h"
#include "UILoadingScreen.h"

#include "../../xrEngine/GameFont.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/UIXmlInit.h"

UILoadingScreen::UILoadingScreen()
    : loadingProgressBackground(nullptr), loadingProgressPercent(nullptr),
      loadingLogo(nullptr), loadingStage(nullptr), loadingHeader(nullptr),
      loadingTipNumber(nullptr), loadingTip(nullptr), progressUnderBackground(true)
{
    UILoadingScreen::Initialize();
}

void UILoadingScreen::Initialize() 
{
    CUIXml uiXml;
    uiXml.Load(CONFIG_PATH, UI_PATH, "ui_mm_loading_screen.xml");

    const auto loadProgressBar = [&]()
        {
            if (uiXml.NavigateToNode("loading_progress_background", 0))
                loadingProgressBackground = UIHelper::CreateStatic(uiXml, "loading_progress_background", this);

            loadingProgress = new CUILoadingScreenProgress();
            AttachChild(loadingProgress);
            loadingProgress->SetAutoDelete(true);
            CUIXmlInit::InitLoadscreenProgress(uiXml, "loading_progress", 0, loadingProgress);
        };

    const auto loadBackground = [&] { CUIXmlInit::InitWindow(uiXml, "background", 0, this); };

    progressUnderBackground = uiXml.ReadAttribInt("loading_progress", 0, "under_background", 1) ? true : false;
    if (progressUnderBackground)
    {
        loadProgressBar();
        loadBackground();
    }
    else
    {
        loadBackground();
        loadProgressBar();
    }

    loadingLogo = UIHelper::CreateStatic(uiXml, "loading_logo", this);

    if (uiXml.NavigateToNode("loading_progress_percent", 0))
        loadingProgressPercent = UIHelper::CreateStatic(uiXml, "loading_progress_percent", this);

    loadingStage = UIHelper::CreateStatic(uiXml, "loading_stage", this);

    if (uiXml.NavigateToNode("loading_header", 0))
        loadingHeader = UIHelper::CreateStatic(uiXml, "loading_header", this);

    if (uiXml.NavigateToNode("loading_tip_number", 0))
        loadingTipNumber = UIHelper::CreateStatic(uiXml, "loading_tip_number", this);

    if (uiXml.NavigateToNode("loading_tip", 0))
        loadingTip = UIHelper::CreateStatic(uiXml, "loading_tip", this);
}

void UILoadingScreen::Update(const int stagesCompleted, const int stagesTotal) 
{
    loadingProgress->SetPos(stagesCompleted, stagesTotal);

    CUIWindow::Update();
    Draw();
}

void UILoadingScreen::ForceFinish() 
{
    loadingProgress->SetPos(1.f);
}

void UILoadingScreen::SetLevelLogo(const char* name) const 
{ 
    loadingLogo->InitTexture(name); 
}

void UILoadingScreen::SetStageTitle(const char* title) const 
{
    loadingStage->TextItemControl()->SetText(title);
}

void UILoadingScreen::SetStageTip(const char* header, const char* tipNumber,
                                  const char* tip) const 
{
    if (loadingHeader)
        loadingHeader->TextItemControl()->SetText(header);

    if (loadingTipNumber)
        loadingTipNumber->TextItemControl()->SetText(tipNumber);

    if (loadingTip)
        loadingTip->TextItemControl()->SetText(tip);
}

void UILoadingScreen::Draw()
{
    CUIWindow::Draw();
}