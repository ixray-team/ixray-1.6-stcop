#include "stdafx.h"
#include "UIPropertiesBox.h"
#include "../level.h"
#include "UIListBoxItem.h"
#include "UIXmlInit.h"
#include "../HUDManager.h"

#define OFFSET_X (5.0f)
#define OFFSET_Y (5.0f)
#define ITEM_HEIGHT (GetFont()->CurrentHeight()+2.0f)

CUIPropertiesBox::CUIPropertiesBox()
{
	m_UIListWnd.SetFont					(UI().Font().pFontArial14);
	m_UIListWnd.SetImmediateSelection	(true);
}

CUIPropertiesBox::~CUIPropertiesBox()
{}


void CUIPropertiesBox::InitPropertiesBox(Fvector2 pos, Fvector2 size)
{
	inherited::SetWndPos		(pos);
	inherited::SetWndSize		(size);

	AttachChild				(&m_UIListWnd);

	CUIXml					xml_doc;

	string128		INVENTORY_XML;
	xr_sprintf		(INVENTORY_XML, "inventory_new_%d.xml", ui_hud_type);

	xml_doc.Load			(CONFIG_PATH, UI_PATH, INVENTORY_XML);

	LPCSTR t = xml_doc.Read	("properties_box:texture", 0, "");
	R_ASSERT				(t);
	InitTexture				(t);

	CUIXmlInit::InitListBox	(xml_doc, "properties_box:list", 0, &m_UIListWnd);

	m_UIListWnd.SetWndPos	(Fvector2().set(OFFSET_X, OFFSET_Y));
	m_UIListWnd.SetWndSize	(Fvector2().set(size.x-OFFSET_X*2, size.y-OFFSET_Y*2) );
}

void CUIPropertiesBox::SendMessage(CUIWindow *pWnd, s16 msg, void *pData)
{
	if(pWnd == &m_UIListWnd)
	{
		if(msg == LIST_ITEM_CLICKED)
		{
			GetMessageTarget()->SendMessage	(this, PROPERTY_CLICKED);
			Hide							();
		}
	}
	inherited::SendMessage	(pWnd, msg, pData);
}

bool CUIPropertiesBox::AddItem(LPCSTR  str, void* pData, u32 tag_value)
{
	CUIListBoxItem* itm		= m_UIListWnd.AddTextItem(str);
	itm->SetTAG				(tag_value);
	itm->SetData			(pData);

	return true;
}
void CUIPropertiesBox::RemoveItemByTAG(u32 tag)
{
	m_UIListWnd.RemoveWindow(m_UIListWnd.GetItemByTAG(tag));
}

void CUIPropertiesBox::RemoveAll()
{
	m_UIListWnd.Clear();
}

void CUIPropertiesBox::Show(const Frect& parent_rect, const Fvector2& point)
{
	Fvector2						prop_pos;
	Fvector2 prop_size				= GetWndSize();

	if(point.x-prop_size.x > parent_rect.x1 && point.y+prop_size.y < parent_rect.y2)
	{
		prop_pos.set				(point.x-prop_size.x, point.y);
	}else
	if(point.x-prop_size.x > parent_rect.x1 && point.y-prop_size.y > parent_rect.y1)
	{
		prop_pos.set				(point.x-prop_size.x, point.y-prop_size.y);
	}else
	if(point.x+prop_size.x < parent_rect.x2 && point.y-prop_size.y > parent_rect.y1)
	{
		prop_pos.set				(point.x, point.y-prop_size.y);
	}else
		prop_pos.set				(point.x, point.y);

	SetWndPos						(prop_pos);

	inherited::Show					(true);
	inherited::Enable				(true);

	ResetAll						();

	GetParent()->SetCapture			(this, true);
	m_pOrignMouseCapturer			= this;
	m_UIListWnd.Reset();
}

void CUIPropertiesBox::Hide()
{
	CUIWindow::Show(false);
	CUIWindow::Enable(false);

	m_pMouseCapturer = NULL;
	
	if(GetParent()->GetMouseCapturer() == this)
		GetParent()->SetCapture(this, false);
}

bool CUIPropertiesBox::OnMouseAction(float x, float y, EUIMessages mouse_action)
{
	bool cursor_on_box;


	if(x>=0 && x<GetWidth() && y>=0 && y<GetHeight())
		cursor_on_box = true;
	else
		cursor_on_box = false;


	if ( mouse_action == WINDOW_LBUTTON_DOWN && !cursor_on_box )
	{
		Hide();
		return true;
	}

	return inherited::OnMouseAction(x, y, mouse_action);
}

void CUIPropertiesBox::AutoUpdateSize()
{
	Fvector2 sz				= GetWndSize();
	sz.y					= m_UIListWnd.GetItemHeight()*m_UIListWnd.GetSize()+ m_UIListWnd.GetVertIndent();
	sz.x					= _max(20.0f,float((m_UIListWnd.GetLongestLength()+m_UIListWnd.GetHorizIndent()) + 2));
	SetWndSize				(sz);

	m_UIListWnd.SetWndSize	(GetWndSize());
	m_UIListWnd.UpdateChildrenLenght();
}

CUIListBoxItem* CUIPropertiesBox::GetClickedItem()
{
	return m_UIListWnd.GetSelectedItem();
}
void CUIPropertiesBox::Update()
{
	inherited::Update();
}
void CUIPropertiesBox::Draw()
{
	inherited::Draw();
}

bool CUIPropertiesBox::OnKeyboardAction(int dik, EUIMessages keyboard_action)
{
	if(keyboard_action==WINDOW_KEY_PRESSED)
	{
		if (is_binded(kQUIT, dik))
			Hide();
	}
	return true;
}

