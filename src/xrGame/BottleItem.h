///////////////////////////////////////////////////////////////
// BottleItem.h
// BottleItem - бутылка с напитком, которую можно разбить
///////////////////////////////////////////////////////////////

#pragma once

#include "FoodItem.h"
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

protected:
	//партиклы разбивания бутылки
	shared_str m_sBreakParticles;
	ref_sound sndBreaking = {};
	DECLARE_SCRIPT_REGISTER_FUNCTION
};