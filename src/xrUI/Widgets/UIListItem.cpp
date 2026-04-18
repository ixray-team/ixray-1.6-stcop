#include "stdafx.h"

#include "UIListItem.h"

CUIListItem::CUIListItem()
{
    m_eButtonState = BUTTON_NORMAL;

    m_pData = NULL;

    m_iIndex = -1;
    m_iValue = 0;
    m_bHighlightText = false;
    m_iGroupID = -1;
    SetAutoDelete(true);
    m_pTextControl->SetTextAlignment(CGameFont::alLeft);
}

CUIListItem::~CUIListItem()
{
}

void CUIListItem::InitListItem(Fvector2 pos, Fvector2 size)
{
    inherited::SetWndPos(pos);
    inherited::SetWndSize(size);
}

void CUIListItem::InitTexture(const char* tex_name)
{
    CUIButton::InitTexture(tex_name);
    SetTextX(m_UIStaticItem.GetTextureRect().width());
}

bool CUIListItem::IsHighlightText()
{
    return CursorOverWindow();
}

void CUIListItem::SetSelected(bool b)
{
    CUISelectable::SetSelected(b);

    MarkSelected(b);
}
