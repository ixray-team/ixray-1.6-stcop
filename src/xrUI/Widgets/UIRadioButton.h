#pragma once

#pragma once
#include "UITabButton.h"

class UI_API CUIRadioButton : 
	public CUITabButton
{
	typedef CUITabButton inherited;
public:
	virtual void InitButton(Fvector2 pos, Fvector2 size);
	virtual bool InitTexture(LPCSTR tex_name, bool fatal = true);
	virtual void SetTextX(float x)	{/*do nothing*/}
};