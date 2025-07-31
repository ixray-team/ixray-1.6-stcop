#include "stdafx.h"

#include <luabind/luabind.hpp>
#include <luabind/adopt_policy.hpp>

#include "UIListBox.h"
#include "UIListBoxItem.h"
#include "UIListBoxItemMsgChain.h"
#include "UISpinText.h"
#include "UIComboBox.h"

using namespace luabind;


struct CUIListBoxItemWrapper : public CUIListBoxItem, public luabind::wrap_base 
{
	CUIListBoxItemWrapper(float h):CUIListBoxItem(h){}
};

struct CUIListBoxItemMsgChainWrapper : public CUIListBoxItemMsgChain, public luabind::wrap_base
{
	CUIListBoxItemMsgChainWrapper(float h) : CUIListBoxItemMsgChain(h) {}
};


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
		.def("SetSelectedIndex",		&CUIListBox::SetSelectedIDX)
		.def("SetItemHeight", 			&CUIListBox::SetItemHeight)
		.def("GetItemHeight", 			&CUIListBox::GetItemHeight)
		.def("GetItemByIndex",			&CUIListBox::GetItemByIDX)		
		.def("GetItem",					&CUIListBox::GetItem)		
		.def("RemoveItem",				&CUIListBox::RemoveWindow)
		.def("AddTextItem",				&CUIListBox::AddTextItem)
		.def("AddExistingItem",         &CUIListBox::AddExistingItem, adopt<2>()),

		class_<CUIListBoxItem, CUIFrameLineWnd, CUIListBoxItemWrapper>("CUIListBoxItem")
		.def(							constructor<float>())
		.def("GetTextItem",             &CUIListBoxItem::GetTextItem)
		.def("AddTextField",            &CUIListBoxItem::AddTextField)
		.def("AddIconField",            &CUIListBoxItem::AddIconField)
		.def("SetTextColor",			&CUIListBoxItem::SetTextColor),

		class_<CUIListBoxItemMsgChain, CUIListBoxItem, CUIListBoxItemMsgChainWrapper>("CUIListBoxItemMsgChain")
		.def(							constructor<float>())
	];
}