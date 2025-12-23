#pragma once
#include "UIWindow.h"

class UI_API CUIStackPanel :
	public CUIWindow
{
public:
	enum EStackPanelAlignment
	{
		eNone,
		eLeft,
		eRight,
		eTop,
		eBottom
	};
	EStackPanelAlignment SPAlignment;

	virtual void Draw() override;

	void SetRightAlign(bool Val)
	{
		SPAlignment = Val ? eRight : eLeft;
	}

	bool IsAlignRight() const
	{
		return SPAlignment == eRight;
	}

	void SetSpacing(float Val)
	{
		Spacing = Val;
	}

protected:
	float Spacing = 0.0f;
};
