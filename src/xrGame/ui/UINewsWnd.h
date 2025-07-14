#pragma once

#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/xrUIXmlParser.h"
class CUIScrollView;
struct GAME_NEWS_DATA;

class CUINewsWnd: public CUIWindow
{
	typedef CUIWindow inherited;
	enum eFlag{eNeedAdd=(1<<0),};
	Flags16			m_flags;
public:
					CUINewsWnd	();
	virtual			~CUINewsWnd	();

			void	Init		();
			void	Init		(LPCSTR xml_name, LPCSTR start_from);
	void			AddNews		();
	virtual void	Show		(bool status);
	virtual void	Update		();

	CUIScrollView*	UIScrollWnd;
	CUIXml			NewsXML;

private:
	void			LoadNews		();
	void			AddNewsItem	(GAME_NEWS_DATA& news_data);
};
