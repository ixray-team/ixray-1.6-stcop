#include "FFxExtendedEntityesLogic/stdafx.h"
#include "FFxExtendedEntityesLogic/EntityesCategory/Flying/Entityes/Passive/Entityes/Crows/CELCrowEntity.h"
#include "FFxExtendedEntityesLogic/EntryPoint.h"

// Связывает нативную сущность новой вороны с необходимыми службами и состоянием владельца.
CELCrowEntity::CELCrowEntity() : CrowAnimation(*this) {}
// Завершает нативную сущность новой вороны и освобождает связанные ресурсы.
CELCrowEntity::~CELCrowEntity()
{
	ExtendedEntityes::Detach(*this);
	CrowAnimation.Detach();
}
// Восстанавливает нативную сущность и подключает её состояние после спавна.
bool CELCrowEntity::net_Spawn(CSE_Abstract* ServerEntity)
{
	CrowAnimation.Detach();
	if (!CELFlyingEntityBase::net_Spawn(ServerEntity))
	{
		return false;
	}
	CrowAnimation.Attach();
	return true;
}
// Освобождает ресурсы сущности перед штатным удалением из уровня.
void CELCrowEntity::net_Destroy()
{
	CrowAnimation.Detach();
	CELFlyingEntityBase::net_Destroy();
}
// Завершает управление живой сущностью и запускает штатную обработку смерти.
void CELCrowEntity::Die(CObject* Killer)
{
	CrowAnimation.Detach();
	CELFlyingEntityBase::Die(Killer);
}
// Обновляет кадр нативной сущности и её расширенной логики.
void CELCrowEntity::UpdateCL()
{
	CELFlyingEntityBase::UpdateCL();
	if (g_Alive())
	{
		CrowAnimation.Update(Device.fTimeDelta);
	}
}
// Сохраняет или восстанавливает состояние через штатный интерфейс сериализации.
void CELCrowEntity::Serialize(ISaveObject& Object)
{
	CELFlyingEntityBase::Serialize(Object);
	ExtendedEntityes::Serialize(*this, Object);
}
