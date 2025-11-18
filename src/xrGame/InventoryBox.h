#pragma once
#include "inventory_space.h"
#include "GameObject.h"
#include "../xrScripts/script_export_space.h"

class CInventoryBox final : public CGameObject
{
	using inherited = CGameObject;

	bool m_in_use = false;
	bool m_can_take = true;
	bool m_closed = false;

public:
	xr_vector<u16> m_items = {};

public:
	CInventoryBox() = default;
	virtual	~CInventoryBox() = default;

	virtual void OnEvent(NET_Packet& P, u16 type) override;
	virtual BOOL net_Spawn(CSE_Abstract* DC) override;
	virtual void net_Destroy() override;
	virtual void net_Relcase(CObject* O) override;
	void AddAvailableItems(TIItemContainer& items_container) const;
	IC bool IsEmpty() const { return m_items.empty(); }
	virtual	void UpdateCL() override;

	IC void	set_in_use(bool status) { m_in_use = status; }
	IC bool	in_use() const { return m_in_use; }

	void set_can_take(bool status);
	IC bool can_take() const { return m_can_take; }

	void set_closed(bool status, LPCSTR reason);
	IC bool closed() const { return m_closed; }

	virtual CInventoryBox* cast_inventory_box() override { return this; }
	virtual CGameObject* cast_game_object() override { return this; }

protected:
	void SE_update_status();
	DECLARE_SCRIPT_REGISTER_FUNCTION

};
