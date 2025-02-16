#include "StdAfx.h"
#include "UIListBoxItem.h"
#include "UIScrollView.h"
#include "../xrCore/object_broker.h"
#include "UIStatic.h"

CUIListBoxItem::CUIListBoxItem(void)
:m_text(NULL),tag(u32(-1))
{
	txt_color			= 0xffffffff;
	txt_color_s			= 0xffffffff;

	SetTextureColor		(0x00000000);

	m_text			= AddTextField("", 10.0f);

	m_dwLastClickTime		= 0;
	m_dwLastClickFrame		= 0;
}

void CUIListBoxItem::SetTAG(u32 value)
{
	tag = value;
}

u32 CUIListBoxItem::GetTAG()
{
	return tag;
}

void CUIListBoxItem::Draw()
{
	u32 CurColor = GetTextColor();
	u32 ResColor = (IsEnabled() ? 0xff000000 : 0x80000000) | (CurColor & 0x00ffffff);
	SetTextColor(ResColor);

	if(m_bSelected)
		DrawElements();

	CUIWindow::Draw	();
}

void CUIListBoxItem::OnFocusReceive()
{
	inherited::OnFocusReceive();
	GetMessageTarget()->SendMessage(this, LIST_ITEM_FOCUS_RECEIVED);
}

void CUIListBoxItem::InitDefault()
{
	InitTexture("ui_listline", "hud\\default");
}

void CUIListBoxItem::SetFont(CGameFont* F)
{
	m_text->SetFont(F);
}

CGameFont* CUIListBoxItem::GetFont()
{
	return (m_text)?m_text->GetFont():NULL;
}

#define DOUBLE_CLICK_TIME 250

bool CUIListBoxItem::OnMouseDown(int mouse_btn)
{
	if (mouse_btn==MOUSE_1)
	{
		smart_cast<CUIScrollView*>(GetParent()->GetParent())->SetSelected(this);
		GetMessageTarget()->SendMessage(this, LIST_ITEM_SELECT, &tag);

		//skyloader: db click for list item
		u32 dwCurTime		= Device.dwTimeContinual;

		if((m_dwLastClickFrame!=Device.dwFrame) && (dwCurTime-m_dwLastClickTime < DOUBLE_CLICK_TIME) )
			GetMessageTarget()->SendMessage(this, LIST_ITEM_DB_CLICKED, &tag);

		GetMessageTarget()->SendMessage(this, LIST_ITEM_CLICKED, &tag);

		m_dwLastClickTime	= dwCurTime;
		m_dwLastClickFrame	= Device.dwFrame;
		return true;
	}else
		return false;
}

void CUIListBoxItem::SetSelected(bool b)
{
	CUISelectable::SetSelected(b);
	u32 col;
	if (b)
		col = txt_color_s;	
	else
		col = txt_color;

	SetTextColor(col);
}

void CUIListBoxItem::SetTextColor(u32 color, u32 color_s)
{
	txt_color = color;
	txt_color_s = color_s;
	SetTextColor(color);
}

void CUIListBoxItem::SetTextColor(u32 color)
{
	m_text->SetTextColor(color);
}

u32 CUIListBoxItem::GetTextColor()
{
	return (m_text)?m_text->GetTextColor():0xffffffff;
}

float CUIListBoxItem::FieldsLength() const
{
	if(m_ChildWndList.empty())
		return 0.0f;

	float len = 0.0f;

	CUIWindow* w	= m_ChildWndList.back();
	len				+= w->GetWndPos().x + w->GetWidth();

	return len;
}

CUIStatic* CUIListBoxItem::AddIconField(float width)
{
	CUIStatic* st			= new CUIStatic();
	st->SetAutoDelete		(true);
	st->SetWndPos			(Fvector2().set(FieldsLength(),0.0f));
	st->SetWndSize			(Fvector2().set(width, GetHeight()));
	AttachChild				(st);
	return					st;
}

CUITextWnd* CUIListBoxItem::AddTextField(LPCSTR txt, float width)
{
	CUITextWnd* st			= new CUITextWnd();
	st->SetAutoDelete		(true);
	st->SetWndPos			(Fvector2().set(FieldsLength(),0.0f));
	st->SetWndSize			(Fvector2().set(width, GetHeight()));

	AttachChild				(st);

	st->SetFont				(GetFont());
	st->SetTextColor		(GetTextColor());
	st->SetText				(txt);	
	//st->SetVTextAlignment	(m_text->TextItemControl().GetVTextAlignment());	//skyloader: incorrect position

	return							st;
}

void CUIListBoxItem::SetData(void* data)
{
	pData = data;
}

void* CUIListBoxItem::GetData()
{
	return pData;
}

void CUIListBoxItem::SetText(LPCSTR txt)
{
	m_text->SetText(txt);
}

LPCSTR CUIListBoxItem::GetText()							
{
	return m_text->GetText();
}

void CUIListBoxItem::SetTextAlignment(ETextAlignment alignment)
{
	m_text->TextItemControl().SetTextAlignment(alignment);
}

ETextAlignment CUIListBoxItem::GetTextAlignment()
{
	return m_text->TextItemControl().GetTextAlignment();
}
