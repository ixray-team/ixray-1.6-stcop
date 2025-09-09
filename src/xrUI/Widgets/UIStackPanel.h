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

public:
	bool AlignLeft = true;
};
