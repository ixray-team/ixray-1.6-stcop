#pragma once

#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/xrUIXmlParser.h"

class CUIStatic;
struct GAME_NEWS_DATA;

class CUINewsItemWnd final :public CUIWindow
{
	typedef	CUIWindow		inherited;

	CUIStatic*				m_UIDate;
	CUIStatic*				m_UICaption;
	CUIStatic*				m_UIText;
	CUIStatic*				m_UIImage;
	bool					m_legacyMode;

public:
					CUINewsItemWnd		();
	virtual			~CUINewsItemWnd		();
			void	Init				(CUIXml& uiXml, const char* start_from);
			void	Setup				(GAME_NEWS_DATA& news_data);
	virtual	void	Update				(){};
	virtual CUIWindow* ui_cast_window() { return this; }
};