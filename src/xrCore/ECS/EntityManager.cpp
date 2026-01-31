#include "stdafx.h"
#include "EntityManager.h"

XRCORE_API CECSManager* GECSManager;

CECSManager::~CECSManager()
{
	DestroyAll();
}

void CECSManager::DestroyAll()
{
	for (auto& [_, Storage] : ComponentStorages)
	{
		Storage->DestroyAll();
		xr_delete(Storage);
	}
	ComponentStorages.clear();
}

void CECSManager::DestroyAllForOwner(void* Owner)
{
	for (auto& [_, Storage] : ComponentStorages)
	{
		Storage->Destroy(Owner);
	}
}