#include "StdAfx.h"
#include "UIEncyclopediaArticleWnd.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../encyclopedia_article.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UICursor.h"
#include "../../xrEngine/string_table.h"
#include "../../xrEngine/xr_input.h"
#include "../../Layers/xrRender/xrRender_console.h"

CUIEncyclopediaArticleWnd::CUIEncyclopediaArticleWnd()
{
	m_Article		= nullptr;
	m_UIImage		= nullptr;
	m_UIModel		= nullptr;
	m_UIText		= nullptr;
	m_bUsedModel	= false;
}

CUIEncyclopediaArticleWnd::~CUIEncyclopediaArticleWnd()
{
}

void CUIEncyclopediaArticleWnd::Init(const char* xml_name, const char* start_from)
{
	CUIXml uiXml;
	uiXml.Load(CONFIG_PATH, UI_PATH, xml_name);

	CUIXmlInit					xml_init;

	string512					str;

	strcpy_s					(str,sizeof(str),start_from);
	xml_init.InitWindow			(uiXml,str,0,this);

	xr_strconcat				(str,start_from,":image");
	m_UIImage					= new CUIStatic();
	m_UIImage->SetAutoDelete	(true);
	xml_init.InitStatic			(uiXml,str,0,m_UIImage);
	AttachChild					(m_UIImage);

	xr_strconcat				(str,start_from,":model");
	if (uiXml.NavigateToNode(str))
	{
		m_UIModel = new CUI3dStatic();
		m_UIModel->SetAutoDelete(true);
		xml_init.InitStatic(uiXml, str, 0, m_UIModel);
		AttachChild(m_UIModel);
	}
	xr_strconcat				(str,start_from,":text_cont");
	m_UIText					= new CUIStatic();
	m_UIText->SetAutoDelete		(true);
	xml_init.InitStatic			(uiXml,str,0,m_UIText);
	AttachChild					(m_UIText);
}

void CUIEncyclopediaArticleWnd::SetArticle(CEncyclopediaArticle* article)
{
	if (article->data()->model.GetVisual())
	{
		// draw only one, model of image
		m_UIModel->SetShader			(article->data()->model.GetShader());
		m_UIModel->SetVisual			(article->data()->model.GetVisual());
		Fvector xyz						= article->data()->model.GetXYZ();
		m_UIModel->SetXYZ				(xyz);
		m_UIModel->SetScaleFactor		(article->data()->model.GetScaleFactor());

		float img_x						= (GetWidth() - m_UIModel->GetWidth()) / 2.0f;
		img_x							= std::max(0.0f, img_x);
		m_UIModel->SetWndPos			(Fvector2().set(img_x, m_UIModel->GetWndPos().y));
		m_bUsedModel					= true;
	}
	else
	{
		if (article->data()->image.GetShader() && article->data()->image.GetShader()->inited())
		{
			m_UIImage->SetShader		(article->data()->image.GetShader());
			m_UIImage->SetTextureRect	(article->data()->image.GetStaticItem()->GetTextureRect());
			m_UIImage->SetWndSize		(article->data()->image.GetWndSize());
			m_UIImage->SetWidth			(m_UIImage->GetWidth() * UI().get_current_kx());

			float img_x					= (GetWidth() - m_UIImage->GetWidth()) / 2.0f;
			img_x						= std::max(0.0f, img_x);
			m_UIImage->SetWndPos		(Fvector2().set(img_x, m_UIImage->GetWndPos().y));
			m_bUsedModel				= false;
		}
	}
	m_UIText->SetTextST					(article->data()->text.c_str());
	m_UIText->AdjustHeightToText		();

	AdjustLauout						();
}

void CUIEncyclopediaArticleWnd::AdjustLauout()
{
	CUIStatic* pic_or_model = m_bUsedModel ? m_UIModel : m_UIImage;
	m_UIText->SetWndPos					(Fvector2().set(m_UIText->GetWndPos().x, pic_or_model->GetWndPos().y + pic_or_model->GetHeight()));
	SetHeight							(pic_or_model->GetWndPos().y + pic_or_model->GetHeight() + m_UIText->GetHeight());
}

void CUIEncyclopediaArticleWnd::SetArticle(const char* article)
{
	CEncyclopediaArticle				A;
	A.Load								(article);
	SetArticle							(&A);
}

extern ENGINE_API float devfloat1;
bool CUIEncyclopediaArticleWnd::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	if (m_UIModel && m_UIModel->CursorOverWindow() && pInput->LeftMouseButtonPressed())
	{
		// need to fix input invertion on 180 degs rotate
		Fvector xyz = m_UIModel->GetXYZ();
		Fvector2 delta_pos = GetUICursor().GetCursorPositionDelta();
		xyz.x += delta_pos.y / 150.f/*devfloat1*/; // seems like value dependences by game screen resolution, will try another solution
		xyz.y += delta_pos.x / 150.f/*devfloat1*/;
		m_UIModel->SetXYZ(xyz);
	}
	return inherited::OnMouseAction(x, y, mouse_action);
}
