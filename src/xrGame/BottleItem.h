///////////////////////////////////////////////////////////////
// BottleItem.h
// BottleItem - бутылка с напитком, которую можно разбить
///////////////////////////////////////////////////////////////

#pragma once

#include "FoodItem.h"
#include "Descs/BottleItemDesc.h"
#include "../xrScripts/script_export_space.h"

class CBottleItem final : public CFoodItem
{
	using inherited = CFoodItem;
public:
	CBottleItem() = default;
	virtual	~CBottleItem();

	virtual void Load(const char* section) override;
	virtual void OnEvent(NET_Packet& P, u16 type) override;
	virtual	void Hit(SHit* pHDS) override;
	void BreakToPieces();

	const SBottleItemDesc& BottleDesc() const { VERIFY(CurrentBottleDesc); return *CurrentBottleDesc; }

protected:
	const SBottleItemDesc* CurrentBottleDesc = nullptr;
	ref_sound sndBreaking = {};
	DECLARE_SCRIPT_REGISTER_FUNCTION
};