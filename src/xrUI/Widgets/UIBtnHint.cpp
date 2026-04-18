#include "stdafx.h"
#include "UIBtnHint.h"
#include "UIFrameWindow.h"
#include "UIStatic.h"
#include "UIXmlInit.h"
#include "UIFrameLineWnd.h"

UI_API CUIButtonHint* g_btnHint = nullptr; 
UI_API CUIButtonHint* g_statHint = nullptr;

CUIButtonHint::CUIButtonHint	()
:m_ownerWnd(nullptr),m_enabledOnFrame(false), m_border(nullptr)
{
//	Device.seqRender.Add		(this, REG_PRIORITY_LOW-1000);

	CUIXmlInit					xml_init;
	CUIXml						uiXml;
	uiXml.Load					(CONFIG_PATH, UI_PATH, "hint_item.xml");

	if (uiXml.NavigateToNode("button_hint:texture")) // COP
		xml_init.InitFrameWindow	(uiXml,"button_hint",0,this);
	else // CS
	{
		xml_init.InitWindow(uiXml, "button_hint", 0, this);

		m_border = new CUIFrameLineWnd();
		m_border->SetAutoDelete(true);
		AttachChild(m_border);
		xml_init.InitFrameLine(uiXml, "button_hint:frame_line", 0, m_border);
	}
	m_text						= new CUIStatic();
	m_text->SetAutoDelete		(true);
	AttachChild					(m_text);
	xml_init.InitStatic		(uiXml,"button_hint:description",0,m_text);
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

        const u32 color = color_rgba(255, 255, 255, color_get_A(m_text->GetTextColor()));

        if (m_border)
            m_border->SetTextureColor(color);
        else
            SetTextureColor(color);

		Draw				();
		m_enabledOnFrame	= false;
	}
}

void CUIButtonHint::SetHintText	(CUIWindow* w, const char* text)
{
	m_ownerWnd					= w;
	m_text->SetTextST			(text);
    if (m_border)
    {
        m_text->AdjustWidthToText();
        const float hh = std::max(m_text->GetWidth()+30.0f, 80.0f);
        SetWidth(hh);
        m_border->SetWidth(hh); // XXX: CUIFrameLineWnd ignores this. Fix
    }
    else
    {
        m_text->AdjustHeightToText();

        const Fvector2 new_size
        {
            GetWndSize().x,
            m_text->GetWndSize().y + 20.0f
        };

        SetWndSize(new_size);
    }
	m_text->ResetColorAnimation	();
}
