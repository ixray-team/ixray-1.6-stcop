////////////////////////////////////////////////////////////////////////////
//	Module 		: UIInventoryUpgradeWnd.h
//	Created 	: 06.10.2007
//  Modified 	: 13.03.2009
//	Author		: Evgeniy Sokolov, Prishchepa Sergey
//	Description : inventory upgrade UI window class
////////////////////////////////////////////////////////////////////////////

#ifndef UI_INVENTORY_UPGRADE_WND_H_INCLUDED
#define UI_INVENTORY_UPGRADE_WND_H_INCLUDED

#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UI3dStatic.h"
#include "UIInvUpgrade.h"
#include "../../xrUI/ui_defs.h"

extern const char* g_inventory_upgrade_xml;
#define MAX_UI_UPGRADE_CELLS 25

namespace inventory::upgrade
{
	class Manager;
	class Upgrade;
	class Property;
}

class UIUpgrade;
class CInventoryItem;
class CUIItemInfo;
class CUIFrameLineWnd;
class CUI3tButton;
class CUICellItem;

class CUIInventoryUpgradeWnd final : public CUIWindow
{
	friend class CUIActorMenu;

	using inherited = CUIWindow;

	using Upgrade_type = inventory::upgrade::Upgrade;
	using Property_type = inventory::upgrade::Property;
	using UI_Upgrades_type = xr_vector<UIUpgrade*>;

	struct Scheme
	{
		shared_str name;
		UI_Upgrades_type cells = {};

		Scheme() = default;
		virtual ~Scheme();
	};
	using SCHEMES = xr_vector<Scheme*>;

public:
	CUIInventoryUpgradeWnd() = default;
	virtual	~CUIInventoryUpgradeWnd();

	void Init();
	void InitInventory(CUICellItem* item, bool can_upgrade);

	IC CInventoryItem const* get_inventory() const { return m_inv_item; }
	IC const char* get_cell_texture(UIUpgrade::ViewState state) const { return m_cell_textures[state].c_str(); }
	IC const char* get_point_texture(UIUpgrade::ViewState state) const { return m_point_textures[state].c_str(); }
	Fvector2 get_scheme_position() const { return m_scheme_wnd->GetWndPos(); }
	Fvector2 get_item_position() const { return m_item->GetWndPos(); }

	virtual void Show(bool status) override;
	virtual void Update() override;
	virtual void Reset() override;
	void UpdateAllUpgrades();

	bool DBClickOnUIUpgrade(Upgrade_type const* upgr);
	void AskUsing(const char* text, const char* upgrade_name);
	void OnMesBoxYes();

	void HighlightHierarchy(shared_str const& upgrade_id);
	void ResetHighlight();
	void set_info_cur_upgrade(Upgrade_type* upgrade);
	UIUpgrade* FindUIUpgrade(Upgrade_type const* upgr);

	virtual CUIWindow* ui_cast_window() { return this; }
    virtual bool OnMouseAction(float x, float y, EUIMessages mouse_action);
	// Controller UI
	bool SelectorMove(eUIDirection4 dir);
	void SetActiveForController(bool status);
	bool CanApplySelectedUpgrade();
	void ApplySelectedUpgrade();
	void SetInfoVisible(bool status);
protected:
	void SetUpgradeSelected(UIUpgrade* pUpgrade);

private:
	void LoadCellsBacks(CUIXml& uiXml);
	void LoadCellStates(const char* state_str, const char* texture_name, const char* texture_name2, u32 color);
	UIUpgrade::ViewState SelectCellState(const char* state_str);
	void SetCellState(UIUpgrade::ViewState state, const char* texture_name, const char* texture_name2, u32 color);
	bool VerirfyCells();

	void LoadSchemes(CUIXml& uiXml);
	void SetCurScheme(const shared_str& id);
	bool install_item(CInventoryItem& inv_item, bool can_upgrade);
	bool CheckEnableDisassembleButton(CInventoryItem& inv_item);
public:
	CUI3tButton* m_btn_repair = nullptr;
	CUI3tButton* m_btn_disassemble = nullptr;
	void DeInitInventory();

protected:
	CUIStatic* m_background = nullptr;
	CUI3dStatic* m_item = nullptr;
	CUIItemInfo* m_item_info = nullptr;
	CUIWindow* m_back = nullptr;
	CInventoryItem* m_inv_item = nullptr;

	shared_str m_cell_textures[UIUpgrade::STATE_COUNT];
	shared_str m_point_textures[UIUpgrade::STATE_COUNT];
	shared_str m_border_texture;
	shared_str m_ink_texture;

	SCHEMES	m_schemes = {};
	Scheme* m_current_scheme = nullptr;
	const char* m_cur_upgrade_id;
	CUIWindow* m_scheme_wnd = nullptr;

	CUIFrameWindow* m_selectorFrame = nullptr;
	bool m_selector_shown = false;
	UIUpgrade* m_selectedUpgrade = nullptr;

public:
	ui_shader* m_WeaponIconsShader = nullptr;
	ui_shader* m_OutfitIconsShader = nullptr;

}; // class CUIInventoryUpgradeWnd

#endif // UI_INVENTORY_UPGRADE_WND_H_INCLUDED
