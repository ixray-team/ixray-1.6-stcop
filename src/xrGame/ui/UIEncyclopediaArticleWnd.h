#pragma once
#include "../../xrUI/Widgets/UIWindow.h"

class CUIStatic;
class CEncyclopediaArticle;

class CUIEncyclopediaArticleWnd final :public CUIWindow
{
typedef	CUIWindow		inherited;

CUIStatic*				m_UIImage;
CUIStatic*				m_UIText;
CEncyclopediaArticle*	m_Article;

protected:
			void		AdjustLauout				();

public:
					CUIEncyclopediaArticleWnd		();
	virtual			~CUIEncyclopediaArticleWnd		();
			void	Init							(LPCSTR xml_name, LPCSTR start_from);
			void	SetArticle						(CEncyclopediaArticle*);
			void	SetArticle						(LPCSTR);
	virtual CUIWindow* ui_cast_window() { return this; }
};