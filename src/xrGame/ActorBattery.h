#pragma once

#include "inventory_item_object.h"

class CBattery : public CInventoryItemObject
{
	using inherited = CInventoryItemObject;

private:
	float m_fTorchCost;

public:
	CBattery();
	virtual ~CBattery() = default;

	virtual void Load(LPCSTR section) override;

	virtual void net_Export(NET_Packet& P) override;
	virtual void net_Import(NET_Packet& P) override;

public:
	bool TryMakeTorchWork();

	bool IsEnought4Torch();
};