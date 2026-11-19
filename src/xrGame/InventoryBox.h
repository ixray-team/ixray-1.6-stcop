#pragma once
#include "inventory_space.h"
#include "GameObject.h"
#include "../xrScripts/script_export_space.h"

XREFLECT(LuaExport, Bases="CGameObject", Constructible)
class CInventoryBox : public CGameObject
{
	using inherited = CGameObject;

	bool m_in_use = false;
	bool m_can_take = true;
	bool m_closed = false;

public:
	xr_vector<ALife::_OBJECT_ID> m_items = {};

public:
	CInventoryBox() = default;
	virtual ~CInventoryBox() override = default;

	virtual void OnEvent(NET_Packet& P, u16 type) override;
	virtual bool net_Spawn(CSE_Abstract* DC) override;
	virtual void net_Destroy() override;
	virtual void net_Relcase(CObject* O) override;
	void AddAvailableItems(TIItemContainer& items_container) const;
	IC bool IsEmpty() const { return m_items.empty(); }
	virtual void UpdateCL() override;

	IC void	set_in_use(bool status) { m_in_use = status; }
	IC bool	in_use() const { return m_in_use; }

	XPROPERTY(LuaCallable)
	void set_can_take(bool status);
	XPROPERTY(LuaCallable)
	IC bool can_take() const { return m_can_take; }

	XPROPERTY(LuaCallable)
	void set_closed(bool status, const char* reason);
	IC bool closed() const { return m_closed; }

	virtual CInventoryBox* cast_inventory_box() override { return this; }
	virtual CGameObject* cast_game_object() override { return this; }

protected:
	void SE_update_status();
};

XREFLECT(LuaExport, Bases="CGameObject", Name="CInventoryBox", Constructible)
class CTradeStorageBox final : public CInventoryBox
{
	using inherited = CInventoryBox;

	xr_hash_set<shared_str> m_ItemFilter;

public:
	virtual CTradeStorageBox* cast_trade_storage_box() override { return this; }
	virtual void Load(LPCSTR section) override;
	IC u32 GetFilterSize() { return m_ItemFilter.size(); }
	IC bool CanStoreItem(shared_str item) { return !m_ItemFilter.contains(item); }

	XPROPERTY(LuaCallable, Name="can_take")
	IC bool script_can_take() const { return can_take(); }
	XPROPERTY(LuaCallable, Name="set_can_take")
	IC void script_set_can_take(bool status) { set_can_take(status); }
	XPROPERTY(LuaCallable, Name="set_closed")
	IC void script_set_closed(bool status, const char* reason) { set_closed(status, reason); }
};
