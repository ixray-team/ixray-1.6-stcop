#include "stdafx.h"

#ifndef IXRAY_NO_LUA
#include <luabind/luabind.hpp>
#include <luabind/adopt_policy.hpp>
#endif

#include "UIListBox.h"
#include "UIListBoxItem.h"
#include "UIListBoxItemMsgChain.h"
#include "UISpinText.h"
#include "UIComboBox.h"

#ifndef IXRAY_NO_LUA
using namespace luabind;
#endif

struct CUIListBoxItemWrapper : public CUIListBoxItem
#ifndef IXRAY_NO_LUA
	, public luabind::wrap_base
#endif
{
	CUIListBoxItemWrapper(float h):CUIListBoxItem(h){}
};

struct CUIListBoxItemMsgChainWrapper : public CUIListBoxItemMsgChain
#ifndef IXRAY_NO_LUA
	, public luabind::wrap_base
#endif
{
	CUIListBoxItemMsgChainWrapper(float h) : CUIListBoxItemMsgChain(h) {}
};

#ifndef IXRAY_NO_LUA
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
#endif