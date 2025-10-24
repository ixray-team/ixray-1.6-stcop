#include "stdafx.h"
#include "UIEncyclopediaArticleWnd.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../encyclopedia_article.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrEngine/string_table.h"

CUIEncyclopediaArticleWnd::CUIEncyclopediaArticleWnd	()
:m_Article(nullptr)
{
}

CUIEncyclopediaArticleWnd::~CUIEncyclopediaArticleWnd	()
{
}

void CUIEncyclopediaArticleWnd::Init(LPCSTR xml_name, LPCSTR start_from)
{
	CUIXml uiXml;
	uiXml.Load(CONFIG_PATH, UI_PATH, xml_name);

	CUIXmlInit				xml_init;

	string512				str;

	strcpy_s				(str,sizeof(str),start_from);
	xml_init.InitWindow		(uiXml,str,0,this);

	xr_strconcat			(str,start_from,":image");
	m_UIImage				= new CUIStatic();	m_UIImage->SetAutoDelete(true);
	xml_init.InitStatic			(uiXml,str,0,m_UIImage);
	AttachChild				(m_UIImage);

	xr_strconcat			(str,start_from,":text_cont");
	m_UIText				= new CUIStatic();	m_UIText->SetAutoDelete(true);
	xml_init.InitStatic		(uiXml,str,0,m_UIText);
	AttachChild				(m_UIText);
}

void CUIEncyclopediaArticleWnd::SetArticle(CEncyclopediaArticle* article)
{
	if( article->data()->image.GetShader() && article->data()->image.GetShader()->inited())
	{
		m_UIImage->SetShader			(article->data()->image.GetShader());
		m_UIImage->SetTextureRect		(article->data()->image.GetStaticItem()->GetTextureRect());
		m_UIImage->SetWndSize			(article->data()->image.GetWndSize());
		m_UIImage->SetWidth				(m_UIImage->GetWidth() * UI().get_current_kx());

		float img_x						= (GetWidth()-m_UIImage->GetWidth())/2.0f;
		img_x							= _max(0.0f, img_x);
		m_UIImage->SetWndPos			(Fvector2().set(img_x, m_UIImage->GetWndPos().y));
	};
	m_UIText->SetTextST					(article->data()->text.c_str());
	m_UIText->AdjustHeightToText		();

	AdjustLauout						();
}

void CUIEncyclopediaArticleWnd::AdjustLauout()
{
	m_UIText->SetWndPos					(Fvector2().set(m_UIText->GetWndPos().x, m_UIImage->GetWndPos().y + m_UIImage->GetHeight()));
	SetHeight							(m_UIImage->GetWndPos().y + m_UIImage->GetHeight()+m_UIText->GetHeight());
}

void CUIEncyclopediaArticleWnd::SetArticle(LPCSTR article)
{
	CEncyclopediaArticle				A;
	A.Load								(article);
	SetArticle							(&A);
}
