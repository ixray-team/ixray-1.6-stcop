////////////////////////////////////////////////////////////////////////////
//	Module 		: UIInvUpgrade.h
//	Created 	: 08.11.2007
//  Modified 	: 13.03.2009
//	Author		: Evgeniy Sokolov, Prishchepa Sergey
//	Description : inventory upgrade UI class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "../../xrUI/UIXmlInit.h"

namespace inventory::upgrade
{
	class Upgrade;
	class Property;
}

class CUIStatic;
class CUIInventoryUpgradeWnd;
class CInventoryItem;
class CUIUpgradePoint;

class UIUpgrade final : public CUIWindow
{
private:
	using Upgrade_type = inventory::upgrade::Upgrade;
	using Property_type = inventory::upgrade::Property;
	using inherited = CUIWindow;

public:
	enum ButtonState
	{ 
		BUTTON_FREE = 0,
		BUTTON_PRESSED,
		BUTTON_DPRESSED,
		BUTTON_FOCUSED
	};

public:
	enum ViewState
	{
		STATE_ENABLED = 0,
		STATE_FOCUSED,
		STATE_TOUCHED, 
		STATE_SELECTED,
		STATE_UNKNOWN,
		
		STATE_DISABLED_PARENT,
		STATE_DISABLED_GROUP,
		STATE_DISABLED_PREC_MONEY,
		STATE_DISABLED_PREC_QUEST,
		STATE_DISABLED_FOCUSED,

		STATE_COUNT
	};

	enum Layer
	{
		LAYER_ITEM = 0,
		LAYER_COLOR,
		LAYER_BORDER,
		LAYER_INK,
		LAYER_POINT,
		LAYER_COUNT
	};

public:
	Fvector2 offset;

private:
	CUIInventoryUpgradeWnd*	m_parent_wnd = nullptr;

	CUIStatic* m_item = nullptr;
	CUIStatic* m_color = nullptr;
	shared_str m_upgrade_id = nullptr;

protected:
	Ivector2 m_scheme_index;

	ButtonState m_button_state;

	ViewState m_state;
	ViewState m_prev_state;

	bool m_state_lock = false;

public:
	UIUpgrade(CUIInventoryUpgradeWnd* parent_wnd, bool cellBorder);
	virtual ~UIUpgrade();

	void init_upgrade(const char* upgrade_id, CInventoryItem& item);

	void load_from_xml(CUIXml& ui_xml, int i_column, int i_cell, Frect const* t_cell_border, Frect const& t_cell_item);
	void set_texture(Layer layer, const char* texture);
			
	virtual	void Draw() override;
	virtual	void Update() override;
	virtual	void Reset() override;
	bool CanBeApplied() const { return m_state == STATE_ENABLED || m_state == STATE_FOCUSED || m_state == STATE_TOUCHED; }

	void update_upgrade_state();
	bool OverrideFreeButtonState(const UIUpgrade::Upgrade_type* my_upgrade, const UIUpgrade::Upgrade_type* active_upgrade, ViewState& new_state);
	void update_mask();
	void update_item(CInventoryItem* inv_item);
				 
	virtual bool OnMouseAction(float x, float y, EUIMessages mouse_action) override;
	virtual void OnFocusReceive() override;
	virtual void OnFocusLost() override;
	void OnClick();
	virtual bool OnDbClick() override;
	void OnRClick();
	void SetSelected(bool status);

	void on_over_window();
				 
	void highlight_relation(bool enable);

	IC ButtonState get_button_state() const { return m_button_state; }
	void set_button_state(ButtonState state) { m_button_state = state; }
	IC Ivector2 const& get_scheme_index() const { return m_scheme_index; }

	Upgrade_type* get_upgrade();
	CUIInventoryUpgradeWnd* get_upgrade_window() { return m_parent_wnd; }
	void attach_point(CUIUpgradePoint* point);

	virtual CUIWindow* ui_cast_window() { return this; }

public:			
	CUIUpgradePoint* m_point = nullptr;
    CUIStatic* m_border = nullptr;
    CUIStatic* m_ink = nullptr;
};

class CUIUpgradePoint final : public CUIStatic
{
private:
	typedef CUIStatic inherited;
	UIUpgrade* m_parent_upgrade = nullptr;

public:
	CUIUpgradePoint(UIUpgrade* upgr);
	virtual	~CUIUpgradePoint() = default;

	void load_from_xml(CUIXml& ui_xml, int i_cell);
	virtual bool OnMouseAction(float x, float y, EUIMessages mouse_action) override;
	virtual void OnFocusReceive() override;
	virtual void OnFocusLost() override;

	virtual CUIWindow* ui_cast_window() { return this; }
	virtual CUIStatic* ui_cast_static() { return this; }
};
