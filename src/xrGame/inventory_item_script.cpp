#include "stdafx.h"
#include "pch_script.h"
#include "inventory_item.h"

using namespace luabind;

#pragma optimize("s",on)
void CInventoryItem::script_register(lua_State *L)
{
	module(L)
		[
			class_<CInventoryItem>("CInventoryItem")
			//.def(constructor<>())
			.enum_("EIIFlags")
			[
				value("FdropManual", int(EIIFlags::FdropManual)),
				value("FCanTake", int(EIIFlags::FCanTake)),
				value("FCanTrade", int(EIIFlags::FCanTrade)),
				value("Fbelt", int(EIIFlags::Fbelt)),
				value("Fruck", int(EIIFlags::Fruck)),
				value("FRuckDefault", int(EIIFlags::FRuckDefault)),
				value("FUsingCondition", int(EIIFlags::FUsingCondition)),
				value("FAllowSprint", int(EIIFlags::FAllowSprint)),
				value("Fuseful_for_NPC", int(EIIFlags::Fuseful_for_NPC)),
				value("FInInterpolation", int(EIIFlags::FInInterpolation)),
				value("FInInterpolate", int(EIIFlags::FInInterpolate)),
				value("FIsQuestItem", int(EIIFlags::FIsQuestItem)),
				value("FIsHelperItem", int(EIIFlags::FIsHelperItem)),
				value("FCanStack", int(EIIFlags::FCanStack))
					// TODO: St4lker0k765: Port this?
					//				value("FWeaponDisabled", int(EIIFlags::FWeaponDisabled))
			]
			.def("NameItem", &CInventoryItem::NameItem)
			.def("NameShort", &CInventoryItem::NameShort)
			.def("ItemDescription", &CInventoryItem::ItemDescription)
			.def("Useful", &CInventoryItem::Useful)
			.def("IsUsingCondition", &CInventoryItem::IsUsingCondition)
			.def("CanStack", &CInventoryItem::CanStack)
			.def("HandDependence", &CInventoryItem::HandDependence)
			.def("ActivateItem", &CInventoryItem::ActivateItem)
			.def("DeactivateItem", &CInventoryItem::DeactivateItem)
			.def("GetDropManual", &CInventoryItem::GetDropManual)
			.def("SetDropManual", &CInventoryItem::SetDropManual)
			.def("IsQuestItem", &CInventoryItem::IsQuestItem)
			.def("Cost", &CInventoryItem::Cost)
			.def("Weight", &CInventoryItem::Weight)
			.def("SetWeight", &CInventoryItem::setWeight)
			.def("GetIconName", &CInventoryItem::GetIconName)
			.def("GetCondition", &CInventoryItem::GetCondition)
			.def("SetCondition", &CInventoryItem::SetCondition)
			.def("CanTake", &CInventoryItem::CanTake)
			.def("has_any_upgrades", &CInventoryItem::has_any_upgrades)
			.def("verify_upgrade", &CInventoryItem::verify_upgrade)
			.def("install_upgrade", &CInventoryItem::install_upgrade)
			.def("pre_install_upgrade", &CInventoryItem::pre_install_upgrade)
			.def_readwrite("m_flags", &CInventoryItem::m_flags)
		];
}