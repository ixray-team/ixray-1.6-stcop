#include "StdAfx.h"
#include "UIVoteStatusWnd.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/Widgets/UIStatic.h"

void UIVoteStatusWnd::InitFromXML(CUIXml& xml_doc)
{
	m_str_message	= new CUIStatic(); m_str_message->SetAutoDelete(true);	 AttachChild	(m_str_message);
	m_hint			= new CUIStatic(); m_hint->SetAutoDelete(true);			AttachChild	(m_hint);
	m_time_message	= new CUIStatic();m_time_message->SetAutoDelete(true);	AttachChild		(m_time_message);

	CUIXmlInit::InitFrameWindow	(xml_doc, "vote_wnd",						0,	this);
	CUIXmlInit::InitStatic		(xml_doc, "vote_wnd:static_str_message",	0, m_str_message);
	CUIXmlInit::InitStatic		(xml_doc, "vote_wnd:static_hint",			0, m_hint);
	CUIXmlInit::InitStatic		(xml_doc, "vote_wnd:static_time_message",	0, m_time_message);
}

void UIVoteStatusWnd::SetVoteTimeResultMsg(const char* s)
{
	m_time_message->SetText	(s);
}

void UIVoteStatusWnd::SetVoteMsg(const char* s)
{
	m_str_message->SetText	(s);
}
