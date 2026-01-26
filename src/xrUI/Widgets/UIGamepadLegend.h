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

protected:
	float Spacing = 0.0f;
};
