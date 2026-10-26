#pragma once

#include "inventory_item_object.h"

class CMPPlayersBag :
	public	CInventoryItemObject
{
public:
					CMPPlayersBag(void);
	virtual ~CMPPlayersBag(void) override;
	virtual bool	NeedToDestroyObject	() const override;

	virtual void OnEvent(NET_Packet& P, u16 type) override;
protected:
private:
};