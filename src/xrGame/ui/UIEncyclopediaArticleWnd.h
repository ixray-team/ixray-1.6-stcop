#pragma once
#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/Widgets/UI3dStatic.h"

class CUIStatic;
class CEncyclopediaArticle;

class CUIEncyclopediaArticleWnd final :public CUIWindow
{
typedef	CUIWindow inherited;

	CUIStatic*				m_UIImage;
	CUI3dStatic*			m_UIModel;
	CUIStatic*				m_UIText;
	CEncyclopediaArticle*	m_Article;
	bool					m_bUsedModel;

protected:
			void	AdjustLauout					();

public:
					CUIEncyclopediaArticleWnd		();
	virtual			~CUIEncyclopediaArticleWnd		();
			void	Init							(LPCSTR xml_name, LPCSTR start_from);
			void	SetArticle						(CEncyclopediaArticle*);
			void	SetArticle						(LPCSTR);
	virtual	bool	OnMouseAction					(float x, float y, EUIMessages mouse_action);
	virtual CUIWindow* ui_cast_window				() { return this; }
};