#pragma once

#include "UICustomEdit.h"

class CUIFrameWindow;

class UI_API CUIEditBoxEx :
	/*public CUIMultiTextureOwner, */
	public CUICustomEdit 
{
public:
					CUIEditBoxEx	();
	virtual			~CUIEditBoxEx	();

	virtual void	InitCustomEdit	(Fvector2 pos, Fvector2 size);

	// CUIMultiTextureOwner
	virtual bool	InitTexture		(const char* texture, bool fatal = true);
	virtual bool	InitTextureEx	(const char* texture,const char* shader, bool fatal = true);

	virtual CUIWindow* ui_cast_window() { return this; }
	virtual CUIStatic* ui_cast_static() { return this; }

protected:
	CUIFrameWindow*	m_pFrameWindow;
};
