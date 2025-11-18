//////////////////////////////////////////////////////////////////////
// ExplosiveItem.h: класс для вещи которая взрывается под 
//					действием различных хитов (канистры,
//					балоны с газом и т.д.)
//////////////////////////////////////////////////////////////////////

#pragma once

#include "Explosive.h"
#include "inventory_item_object.h"
#include "DelayedActionFuse.h"
class CExplosiveItem final :
	public CInventoryItemObject,
	public CDelayedActionFuse,
	public CExplosive
{
private:
	using inherited = CInventoryItemObject;

public:
	CExplosiveItem() = default;
	virtual ~CExplosiveItem() = default;

	virtual void Load(LPCSTR section) override;
	virtual BOOL net_Spawn(CSE_Abstract* DC) override { return CInventoryItemObject::net_Spawn(DC); }
	virtual void net_Destroy() override;
	virtual void net_Export(NET_Packet& P) override { CInventoryItemObject::net_Export(P); }
	virtual void net_Import(NET_Packet& P) override { CInventoryItemObject::net_Import(P); }
	virtual void net_Relcase(CObject* O) override;

	virtual void GetRayExplosionSourcePos(Fvector& pos) override;
	virtual void ActivateExplosionBox(const Fvector& size, Fvector& in_out_pos) override;
	virtual void OnEvent(NET_Packet& P, u16 type) override;
	virtual	void Hit(SHit* pHDS) override;
	virtual void shedule_Update(u32 dt) override;
	virtual bool shedule_Needed() override;
	
	virtual void UpdateCL() override;
	virtual void renderable_Render() override;
	virtual void ChangeCondition(float fDeltaCondition) override { CInventoryItem::ChangeCondition(fDeltaCondition); };
	virtual void StartTimerEffects() override;

	virtual CGameObject* cast_game_object() override { return this; }
	virtual CInventoryItem* cast_inventory_item() override { return this; }
	virtual CExplosive* cast_explosive() override { return this; }
	virtual IDamageSource* cast_IDamageSource() override { return CExplosive::cast_IDamageSource(); }
};