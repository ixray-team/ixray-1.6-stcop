#include "stdafx.h"
#include "../pch_script.h"

#include "UIListBox.h"
#include "UIListBoxItem.h"
#include "UIListBoxItemEx.h"
#include "UIListBoxItemMsgChain.h"
#include "UISpinText.h"
#include "UIMapInfo.h"
#include "UIComboBox.h"
#include "../../xrEngine/device.h"

using namespace luabind;


struct CUIListBoxItemWrapper : public CUIListBoxItem, public luabind::wrap_base 
{
	CUIListBoxItemWrapper():CUIListBoxItem(){}
};

struct CUIListBoxItemExWrapper : public CUIListBoxItemEx, public luabind::wrap_base
{
	CUIListBoxItemExWrapper():CUIListBoxItemEx(){}
};

struct CUIListBoxItemMsgChainWrapper : public CUIListBoxItemMsgChain, public luabind::wrap_base
{
	CUIListBoxItemMsgChainWrapper() : CUIListBoxItemMsgChain() {}
};

bool xrRender_test_hw_script()
{
	return true /*!!Device.m_pRender->Render_test_hw()*/;
}

void add_existing_item_script(CUIListBox* self, CUIListBoxItem* item)
{
	self->AddExistingItem(item);
}

#pragma optimize("s",on)
void CUIListBox::script_register(lua_State *L)
{

	module(L)
	[

		class_<CUIListBox, CUIScrollView>("CUIListBox")
		.def(							constructor<>())
		.def("ShowSelectedItem",		&CUIListBox::Show)
		.def("RemoveAll",				&CUIListBox::Clear)
		.def("GetSize",					&CUIListBox::GetSize)
		.def("GetSelectedItem",			&CUIListBox::GetSelectedItem)
		.def("GetSelectedIndex",		&CUIListBox::GetSelectedIDX)		

		.def("GetItemByTag",			&CUIListBox::GetItemByTAG)
		.def("GetItemByText",			&CUIListBox::GetItemByText)			
		.def("GetItem",					&CUIListBox::GetItemByIDX)		
		.def("RemoveItem",				&CUIListBox::RemoveItem)
		.def("Remove",					&CUIListBox::Remove)
		.def("AddTextItem",				&CUIListBox::AddTextItem)

		.def("SetSelectionTexture",				&CUIListBox::SetSelectionTexture)
		.def("SetItemHeight",				&CUIListBox::SetItemHeight)
		.def("GetItemHeight",				&CUIListBox::GetItemHeight)
		.def("GetLongestLength",				&CUIListBox::GetLongestLength)
		.def("SetSelected",				&CUIListBox::SetSelectedIDX)
		.def("SetSelectedTag",				&CUIListBox::SetSelectedTAG)
		.def("SetSelectedText",				&CUIListBox::SetSelectedText)
		.def("SetImmediateSelection",				&CUIListBox::SetImmediateSelection)
		.def("SetTextColor",				&CUIListBox::SetTextColor)
		.def("SetTextColorS",				&CUIListBox::SetTextColorS)
		.def("GetTextColor",				&CUIListBox::GetTextColor)
		.def("SetTextAlignment",			&CUIListBox::SetTextAlignment)
		.def("GetTextAlignment",			&CUIListBox::GetTextAlignment)

		.def("AddItem",         &add_existing_item_script, adopt<2>()),

		class_<CUIListBoxItem, CUIFrameLineWnd, CUIListBoxItemWrapper>("CUIListBoxItem")
		.def(							constructor<>())
		.def("GetTextItem",             &CUIListBoxItem::GetTextItem)
		.def("AddTextField",            &CUIListBoxItem::AddTextField)
		.def("AddIconField",            &CUIListBoxItem::AddIconField)
		.def("SetTextColors",			(void(CUIListBoxItem::*)(u32,u32))&CUIListBoxItem::SetTextColor)
		.def("SetTextColor",			(void(CUIListBoxItem::*)(u32))&CUIListBoxItem::SetTextColor)
		.def("SetTextAlignment",	&CUIListBoxItem::SetTextAlignment)
		.def("GetTextAlignment",	&CUIListBoxItem::GetTextAlignment),

		class_<CUIListBoxItemEx, CUIListBoxItem, CUIListBoxItemExWrapper>("CUIListBoxItemEx")
		.def(							constructor<>())
		.def("SetSelectionColor",		&CUIListBoxItemEx::SetSelectionColor),
		
		def("xrRender_test_hw",			&xrRender_test_hw_script)
	];
}