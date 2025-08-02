#include "StdAfx.h"
#include "UINewsItemWnd.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../game_news.h"
#include "../date_time.h"
#include "UIInventoryUtilities.h"
#include "../../xrUI/UIHelper.h"

CUINewsItemWnd::CUINewsItemWnd()
{
	m_legacyMode = false;
}

CUINewsItemWnd::~CUINewsItemWnd()
{}

void CUINewsItemWnd::Init(CUIXml& uiXml, LPCSTR start_from)
{
	CUIXmlInit::InitWindow		(uiXml,start_from,0,this);

	XML_NODE* stored_root		= uiXml.GetLocalRoot();
	XML_NODE* node				= uiXml.NavigateToNode( start_from, 0 );
	uiXml.SetLocalRoot			( node );

	m_UIImage					= UIHelper::CreateStatic( uiXml, "image", this );

	if (uiXml.NavigateToNode("caption_static"))
		m_UICaption = UIHelper::CreateStatic(uiXml, "caption_static", this);
	else
		m_legacyMode = true;

	m_UIText					= UIHelper::CreateStatic( uiXml, uiXml.NavigateToNode("text_static") ? "text_static" : "text_cont", this);
	m_UIDate					= UIHelper::CreateStatic( uiXml, uiXml.NavigateToNode("date_static") ? "date_static" : "date_text_cont", this);

	uiXml.SetLocalRoot( stored_root );
}

void CUINewsItemWnd::Setup			(GAME_NEWS_DATA& news_data)
{
	shared_str time_str				= InventoryUtilities::GetTimeAndDateAsString( news_data.receive_time, m_legacyMode );
	u32    sz  = (time_str.size() + 5) * sizeof(char);
	xr_string   str = time_str.c_str();
	if (!m_legacyMode)
		str += " -";
	m_UIDate->SetText(str.c_str());
	m_UIDate->AdjustWidthToText();

	if (m_UICaption)
	{
		m_UICaption->SetTextST(news_data.news_caption.c_str());
		Fvector2 pos = m_UICaption->GetWndPos();
		pos.x = m_UIDate->GetWndPos().x + m_UIDate->GetWndSize().x + 5.0f;
		m_UICaption->SetWndPos(pos);
		m_UICaption->SetWidth(_min(m_UIText->GetWidth() - m_UIDate->GetWidth() - 5.0f, m_UICaption->GetWidth()));
	}

	m_UIText->SetTextST				(news_data.news_text.c_str());
	m_UIText->AdjustHeightToText	();
	float h1						= m_UIText->GetWndPos().y + m_UIText->GetHeight() + 6.0f;

	m_UIImage->InitTexture			(news_data.texture_name.c_str());
	
	Frect emptyRect = Frect().set(0.f, 0.f, 0.f, 0.f);
	if (!news_data.tex_rect.cmp(emptyRect))
	{
		Frect texture_rect;
		texture_rect.lt.set(news_data.tex_rect.x1, news_data.tex_rect.y1);
		texture_rect.rb.set(news_data.tex_rect.x2, news_data.tex_rect.y2);
		texture_rect.rb.add(texture_rect.lt);
		m_UIImage->SetTextureRect(texture_rect);
	}
	float h3						= m_UIImage->GetWndPos().y + m_UIImage->GetHeight();
	h1								= _max(h1,h3);
	SetHeight						(h1);
}
