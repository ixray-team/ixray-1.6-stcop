#pragma once
#include "UIWindow.h"

class UI_API CUIStackPanel :
	public CUIWindow
{
public:
	virtual void Draw() override;

	void SetRightAlign(bool Val)
	{
		AlignLeft = !Val;
	}

	bool IsAlignRight() const
	{
		return !AlignLeft;
	}

	void SetSpacing(float Val)
	{
		Spacing = Val;
	}

protected:
	bool AlignLeft = true;
	float Spacing = 0.0f;
};
