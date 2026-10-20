#include "stdafx.h"
#include "ai/ExtendedEntityesLogic/Navigation/CELNavigationBase.h"
#include "ai/ExtendedEntityesLogic/EntryPoint.h"
#include "ai/ExtendedEntityesLogic/Navigation/CELSharedNavigationScheduler.h"

// Завершает общий обмен заявками и результатами навигации и освобождает связанные ресурсы.
CELNavigationBase::~CELNavigationBase()
{
	Cancel();
}

// Помещает заявку в очередь с проверкой допустимости и доступного места.
bool CELNavigationBase::Submit(SELNavigationRequest Request)
{
	if (HasPendingRequest)
	{
		return PendingIdentity == Request.Identity;
	}
	PendingIdentity = Request.Identity;
	HasPendingRequest = ExtendedEntityes::Navigation().Submit(std::move(Request));
	return HasPendingRequest;
}

// Забирает готовый результат либо продолжает подготовку маршрута.
bool CELNavigationBase::Poll(SELNavigationResult& Result)
{
	if (!HasPendingRequest || !ExtendedEntityes::Navigation().Take(PendingIdentity, Result))
	{
		return false;
	}
	HasPendingRequest = false;
	return true;
}

// Отменяет принадлежащую владельцу заявку и очищает промежуточный результат.
void CELNavigationBase::Cancel()
{
	if (HasPendingRequest)
	{
		ExtendedEntityes::Navigation().Cancel(PendingIdentity);
		HasPendingRequest = false;
	}
}
