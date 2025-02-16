#include "stdafx.h"
#include "UIBtnHint.h"
#include "UIFrameLineWnd.h"
#include "UIXmlInit.h"

CUIButtonHint*		g_btnHint = nullptr; 
CUIButtonHint*		g_statHint = nullptr;

CUIButtonHint::CUIButtonHint	()
:m_ownerWnd(nullptr),m_enabledOnFrame(false)
{
//	Device.seqRender.Add		(this, REG_PRIORITY_LOW-1000);

	CUIXmlInit					xml_init;
	CUIXml						uiXml;
	uiXml.Load					(CONFIG_PATH, UI_PATH, "hint_item.xml");

	xml_init.InitWindow			(uiXml,"button_hint",0,this);
	
	m_border					= new CUIFrameLineWnd(); m_border->SetAutoDelete(true);
	AttachChild					(m_border);
	xml_init.InitFrameLine		(uiXml,"button_hint:frame_line",0,m_border);

	m_text						= new CUIStatic(); m_text->SetAutoDelete(true);
	AttachChild					(m_text);
	xml_init.InitStatic			(uiXml,"button_hint:description",0,m_text);


}

CUIButtonHint::~CUIButtonHint	()
{
//	Device.seqRender.Remove		(this);
}

void CUIButtonHint::OnRender	()
{
	if(m_enabledOnFrame)
	{
		m_text->Update		();
		m_border->Update	();
		m_border->SetTextureColor	(color_rgba(255,255,255,color_get_A(m_text->GetTextColor())));
		Draw				();
		m_enabledOnFrame	= false;
	}
}

void CUIButtonHint::SetHintText	(CUIWindow* w, LPCSTR text)
{
	m_ownerWnd					= w;
	m_text->SetText				(text);
	m_text->AdjustWidthToText	();
	m_text->ResetColorAnimation	();

	float hh = 			_max(m_text->GetWidth()+30.0f, 80.0f);
	SetWidth			(hh);
	m_border->SetWidth		(hh);
}
