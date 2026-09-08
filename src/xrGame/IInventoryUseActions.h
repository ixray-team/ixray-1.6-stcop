#pragma once

class CUIPropertiesBox;
struct UseActionContext;

struct UseActionContext
{
	CInventory* inventory = nullptr;
	CInventoryOwner* owner = nullptr;
	PIItem item = nullptr;
};

class IInventoryUseActions
{
public:
	virtual ~IInventoryUseActions() = default;

	virtual bool FillUseActions(
		CUIPropertiesBox* box,
		const UseActionContext& context
	) = 0;
};