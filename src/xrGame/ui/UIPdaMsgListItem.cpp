#include "StdAfx.h"
#include "UIPdaMsgListItem.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrUI/UIXmlInit.h"

void CUIPdaMsgListItem::SetFont(CGameFont* pFont)
{
	if (&UITimeText)
		UITimeText.SetFont		(pFont);
	if (UICaptionText)
		UICaptionText->SetFont	(pFont);
	UIMsgText.SetFont		(pFont);
}

void CUIPdaMsgListItem::InitPdaMsgListItem(const Fvector2& size)
{
	UICaptionText = nullptr;
	inherited::SetWndSize	(size);

	CUIXml					uiXml;
	uiXml.Load				(CONFIG_PATH, UI_PATH,"maingame_pda_msg.xml");

	CUIXmlInit				xml_init;
	AttachChild				(&UIIcon);
	xml_init.InitStatic		(uiXml, "icon_static", 0, &UIIcon);

	if (uiXml.NavigateToNode("time_static"))
	{
		AttachChild(&UITimeText);
		xml_init.InitTextWnd(uiXml, "time_static", 0, &UITimeText);
	}

	if (uiXml.NavigateToNode("caption_static"))
	{
		UICaptionText = new CUITextWnd();
		AttachChild(UICaptionText);
		xml_init.InitTextWnd(uiXml, "caption_static", 0, UICaptionText);
	}
	AttachChild				(&UIMsgText);
	xml_init.InitTextWnd	(uiXml, uiXml.NavigateToNode("msg_static") ? "msg_static" : "text_static", 0, &UIMsgText);
}
