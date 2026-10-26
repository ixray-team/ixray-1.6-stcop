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
	virtual ~CUINewsWnd	() override;

			void	Init		();
			void	Init		(const char* xml_name, const char* start_from);
	void			AddNews		();
	virtual void	Show		(bool status) override;
	virtual void	Update		() override;
	virtual bool	OnGamepadKeyAction(int id, EUIMessages gamepad_action) override;
	virtual bool	OnGamepadKeyHold(int id) override;

	CUIScrollView*	UIScrollWnd;
	CUIXml			NewsXML;

private:
	void			LoadNews		();
	void			AddNewsItem	(GAME_NEWS_DATA& news_data);
};
