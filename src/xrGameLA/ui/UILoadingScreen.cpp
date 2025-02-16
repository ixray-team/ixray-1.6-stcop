#include "StdAfx.h"
#include "UILoadingScreen.h"

#include "../../xrEngine/GameFont.h"
#include "UIHelper.h"
#include "UIXmlInit.h"

UILoadingScreen::UILoadingScreen()
    : loadingProgressBackground(nullptr), loadingProgress(nullptr), loadingProgressPercent(nullptr),
      loadingLogo(nullptr), loadingStage(nullptr), loadingHeader(nullptr),
      loadingTipNumber(nullptr), loadingTip(nullptr) {
    UILoadingScreen::Initialize();
}

void UILoadingScreen::Initialize() {
    CUIXml uiXml;
    uiXml.Load(CONFIG_PATH, UI_PATH, "ui_mm_loading_screen.xml");

    const auto loadProgressBar = [&]() {
        loadingProgressBackground =
            UIHelper::CreateStatic(uiXml, "loading_progress_background", this);
        loadingProgress = UIHelper::CreateProgressBar(uiXml, "loading_progress", this);
    };

    const auto loadBackground = [&] { CUIXmlInit::InitWindow(uiXml, "background", 0, this); };

    const auto node =
        uiXml.NavigateToNodeWithAttribute("loading_progress", "under_background", "0");
    if (node) {
        loadBackground();
        loadProgressBar();
    } else {
        loadProgressBar();
        loadBackground();
    }

    loadingLogo = UIHelper::CreateStatic(uiXml, "loading_logo", this);
    loadingProgressPercent = UIHelper::CreateStatic(uiXml, "loading_progress_percent", this);
    loadingStage = UIHelper::CreateStatic(uiXml, "loading_stage", this);
    loadingHeader = UIHelper::CreateStatic(uiXml, "loading_header", this);
    loadingTipNumber = UIHelper::CreateStatic(uiXml, "loading_tip_number", this);
    loadingTip = UIHelper::CreateStatic(uiXml, "loading_tip", this);
}

void UILoadingScreen::Update(const int stagesCompleted, const int stagesTotal) {
    const float progress = float(stagesCompleted) / stagesTotal * loadingProgress->GetRange_max();
    if (loadingProgress->GetProgressPos() < progress)
        loadingProgress->SetProgressPos(progress);

    CUIWindow::Update();
    Draw();
}

void UILoadingScreen::ForceFinish() {
    loadingProgress->SetProgressPos(loadingProgress->GetRange_max());
}

void UILoadingScreen::SetLevelLogo(const char* name) const { loadingLogo->InitTexture(name); }

void UILoadingScreen::SetStageTitle(const char* title) const {
    loadingStage->TextItemControl()->SetText(title);
}

void UILoadingScreen::SetStageTip(const char* header, const char* tipNumber,
                                  const char* tip) const {
    loadingHeader->TextItemControl()->SetText(header);
    loadingTipNumber->TextItemControl()->SetText(tipNumber);
    loadingTip->TextItemControl()->SetText(tip);
}