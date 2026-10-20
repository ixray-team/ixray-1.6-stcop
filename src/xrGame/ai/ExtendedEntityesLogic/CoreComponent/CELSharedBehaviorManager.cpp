#include "stdafx.h"
#include "ai/ExtendedEntityesLogic/CoreComponent/CELSharedBehaviorManager.h"

// Завершает переключение и проверку схем поведения и освобождает связанные ресурсы.
CELSharedBehaviorManager::~CELSharedBehaviorManager()
{
	Clear();
}

// Переключает схему с согласованными вызовами выхода и входа.
void CELSharedBehaviorManager::Select(IELBehavior* Behavior)
{
	if (Behavior == ActiveBehavior)
	{
		return;
	}
	Clear();
	Validation = SELBehaviorValidation();
	ActiveBehavior = Behavior;
	if (ActiveBehavior)
	{
		ActiveBehavior->Enter();
	}
}

// Обновляет схему, проверяет её состояние и передаёт сбой восстановлению.
void CELSharedBehaviorManager::Update(float DeltaTime)
{
	if (!ActiveBehavior)
	{
		return;
	}
	ActiveBehavior->Update(DeltaTime);
	Validation = ActiveBehavior->Validate();
	if (Validation.State == EELBehaviorState::Blocked || Validation.State == EELBehaviorState::NoProgress || Validation.State == EELBehaviorState::InvalidTarget)
	{
		const EELRecovery Recovery = ActiveBehavior->Recover(Validation);
		if (Recovery == EELRecovery::Restart)
		{
			ActiveBehavior->Exit();
			ActiveBehavior->Enter();
		}
		else if (Recovery == EELRecovery::Release)
		{
			Clear();
		}
	}
}

// Сбрасывает накопленные данные и незавершённую работу подсистемы.
void CELSharedBehaviorManager::Clear()
{
	if (ActiveBehavior)
	{
		ActiveBehavior->Exit();
		ActiveBehavior = nullptr;
	}
}
