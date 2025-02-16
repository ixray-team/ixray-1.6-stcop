#pragma once

#include "../../xrEngine/ILoadingScreen.h"
#include "UIStatic.h"
#include "UIWindow.h"
#include "UIProgressBar.h"

class CApplication;

class UILoadingScreen : 
    public ILoadingScreen, 
    public CUIWindow 
{
    CUIStatic* loadingProgressBackground;
    CUIProgressBar* loadingProgress;
    CUIStatic* loadingLogo;
    CUIStatic* loadingProgressPercent;

    CUIStatic* loadingStage;
    CUIStatic* loadingHeader;
    CUIStatic* loadingTipNumber;
    CUIStatic* loadingTip;

public:
    UILoadingScreen();

    void Initialize() override;

    void Update(const int stagesCompleted, const int stagesTotal) override;
    void ForceFinish() override;

    void SetLevelLogo(const char* name) const override;
    void SetStageTitle(const char* title) const override;
    void SetStageTip(const char* header, const char* tipNumber, const char* tip) const override;
};