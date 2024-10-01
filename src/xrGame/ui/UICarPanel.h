#pragma once

#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UIProgressBar.h"

class CUICarPanel : public CUIWindow
{
private:
    typedef CUIWindow inherited;

    CUIStatic* UIStaticCarHealth;
    CUIProgressBar* UICarHealthBar;
    CUIProgressBar* UICarFuelBar;

    CUITextWnd* UIEngineSpeedMode;
    CUIStatic* UIEngineLamp;
    CUIStatic* UIEngineLampOff;

    CUIStatic* UILightLamp;
    CUIStatic* UILightLampOff;

public:
    // Установить
    ~CUICarPanel();

    void SetCarHealth(float value);
    void SetCarFuel(float value);
    void SetSpeed(float speed);
    void SetRPM(float rmp);
    void SetSpeedMode(const xr_string& Mode);
    void Init();
    void SetEngineLamp(bool On);
    void SetLightLamp(bool On);
    virtual void Draw() override;
};