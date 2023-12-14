#pragma once

#include "ui_base.h"
class CUIStatic;

class CUICursor:	public pureRender, 
					public pureScreenResolutionChanged
{
	bool			bVisible;
	Fvector2		vPos;
	Fvector2		vPrevPos;
	CUIStatic*		m_static;
	void			InitInternal				();
public:
					CUICursor					();
	virtual			~CUICursor					();
	virtual void	OnRender					();
	
	Fvector2		GetCursorPositionDelta		();

	Fvector2		GetCursorPosition			();
	void			SetUICursorPosition			(Fvector2 pos);
	void			UpdateCursorPosition		(int _dx, int _dy);
	virtual void	OnScreenResolutionChanged	();

	bool			IsVisible					() {return bVisible;}
	void			Show						()
	{
		int screenWidth = GetSystemMetrics(SM_CXSCREEN);
		int screenHeight = GetSystemMetrics(SM_CYSCREEN);
		SDL_WarpMouseInWindow(g_AppInfo.Window, screenWidth / 2, screenHeight / 2);
		bVisible = true;
	}
	void			Hide						() {bVisible = false;}
};
