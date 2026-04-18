#pragma once
#include "../../xrUI/Widgets/UIWindow.h"

class CUIScrollView;
class CUIFrameWindow;
class CUIFrameLineWnd;
class CUIXml;
class CEncyclopediaArticle;

class CUITaskDescrWnd:public CUIWindow
{
	typedef CUIWindow			inherited;
protected:
	CUIScrollView*				m_UITaskInfoWnd;
	CUIFrameWindow*				m_UIMainFrame;
	CUIFrameLineWnd*			m_UIMainHeader;

public:
				CUITaskDescrWnd				();
	virtual		~CUITaskDescrWnd			();
	virtual void Draw						();
	void		Init						(CUIXml* doc, const char* start_from);
	void		ClearAll					();
	void		AddArticle					(const char* article);
	void		AddArticle					(CEncyclopediaArticle* article);
	
	void		ScrollUp					();
	void		ScrollDown					();
};