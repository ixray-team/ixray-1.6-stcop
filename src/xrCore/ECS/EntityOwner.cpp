#include "stdafx.h"
#include "EntityOwner.h"

IECSOwner::IECSOwner()
{
}

IECSOwner::~IECSOwner()
{
	GECSManager->DestroyAllForOwner(this);
}