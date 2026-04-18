////////////////////////////////////////////////////////////////////////////
//	Module 		: UIInvUpgradeInfo.h
//	Created 	: 21.11.2007
//  Modified 	: 13.03.2009
//	Author		: Evgeniy Sokolov, Prishchepa Sergey
//	Description : inventory upgrade UI info window class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/xrUIXmlParser.h"

namespace inventory::upgrade
{
	class Upgrade;
}

class CUIStatic;
class CUIFrameWindow;
class UIInvUpgPropertiesWnd;
class CInventoryItem;

class UIInvUpgradeInfo final : public CUIWindow
{
private:
	using inherited = CUIWindow;
	using Upgrade_type = inventory::upgrade::Upgrade;

public:
	UIInvUpgradeInfo();
	virtual ~UIInvUpgradeInfo() = default;

	void init_from_xml(const char* xml_name);
	bool init_upgrade(Upgrade_type* upgr, CInventoryItem* inv_item);
	bool is_upgrade() { return (m_upgrade != NULL); }
	IC Upgrade_type const* get_upgrade() const { return m_upgrade; }

	virtual void Draw();

	virtual CUIWindow* ui_cast_window() { return this; }

protected:
	Upgrade_type* m_upgrade = nullptr;
	CUIFrameWindow* m_background = nullptr;

	UIInvUpgPropertiesWnd* m_properties_wnd = nullptr;

	CUIStatic* m_name = nullptr;
	CUIStatic* m_cost = nullptr;
	CUIStatic* m_desc = nullptr;
	CUIStatic* m_prereq = nullptr;
	bool m_legacy_mode = false;

}; // class UIInvUpgradeInfo
