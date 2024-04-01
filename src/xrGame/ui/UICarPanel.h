#pragma once

#include "UIStatic.h"
#include "UIProgressBar.h"

class CUICarPanel : public CUIWindow
{
private:
    typedef CUIWindow inherited;

    CUIStatic* UIStaticCarHealth;
    CUIProgressBar* UICarHealthBar;
    CUIProgressBar* UICarFuelBar;

    CUIStatic* UIEngineLamp;
    CUIStatic* UIEngineLampOff;

public:
    // Установить
    ~CUICarPanel();

    void SetCarHealth(float value);
    void SetCarFuel(float value);
    void SetSpeed(float speed);
    void SetRPM(float rmp);
    void Init();
    void SetEngineLamp(bool On);
    virtual void Draw() override;
};