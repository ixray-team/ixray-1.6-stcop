#pragma once
#include "UIWindow.h"

class UI_API CUIGamepadLegend :
	public CUIWindow
{
public:
	virtual void Draw() override;

	void SetSpacing(float Val)
	{
		Spacing = Val;
	}
	void ReloadLegend();

	virtual CUIGamepadLegend* ui_cast_gamepad_legend() { return this; }
protected:
	float Spacing = 0.0f;
};
