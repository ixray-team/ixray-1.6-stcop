#include "stdafx.h"
#include "EntityManager.h"

XRCORE_API CECSManager* GECSManager;

CECSManager::~CECSManager()
{
	DestroyAll();
}

void CECSManager::DestroyAll()
{
	xrSRWLockGuard guard(RWMutex, false);
	for (auto& [_, Storage] : ComponentStorages)
	{
		Storage->DestroyAll();
		xr_delete(Storage);
	}
	ComponentStorages.clear();
}

void CECSManager::DestroyAllForOwner(IECSOwner* Owner)
{
	xrSRWLockGuard guard(RWMutex, true);
	for (auto& [_, Storage] : ComponentStorages)
	{
		Storage->Destroy(Owner);
	}
}