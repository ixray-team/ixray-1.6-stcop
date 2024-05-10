#pragma once
class UITopBarForm :public XrUI
{
public:
	UITopBarForm();
	virtual ~UITopBarForm();
	virtual void Draw();
	void RefreshBar();
private:
#define ADD_BUTTON_IMAGE_S(Name) void Click##Name(); ref_texture m_t##Name;u32 m_time##Name;
#include "UITopBarForm_ButtonList.h"
};