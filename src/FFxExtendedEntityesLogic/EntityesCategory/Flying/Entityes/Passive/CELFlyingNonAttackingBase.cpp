#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/CELFlyingNonAttackingBase.h"
#include "xrGame/ai/monsters/basemonster/base_monster.h"
// Связывает общую логику неатакующих летунов с необходимыми службами и состоянием владельца.
CELFlyingNonAttackingBase::CELFlyingNonAttackingBase(xr_unique_ptr<IELAdapter> OwnerAdapter, const SELPerceptionProfile& OwnerProfile) : CELFlying(std::move(OwnerAdapter), OwnerProfile) {}
// Завершает общую логику неатакующих летунов и освобождает связанные ресурсы.
CELFlyingNonAttackingBase::~CELFlyingNonAttackingBase()
{
	ClearBehavior();
}
// Обновляет общую логику неатакующих летунов и обрабатывает текущий результат выполнения.
EELControl CELFlyingNonAttackingBase::Update(float DeltaTime)
{
	if (!HasAttached || !Adapter->CanControl())
	{
		return CELFlying::Update(DeltaTime);
	}
	TargetIdentifier = u16(-1);
	return BehaviorManager.UpdateFlight(DeltaTime);
}
