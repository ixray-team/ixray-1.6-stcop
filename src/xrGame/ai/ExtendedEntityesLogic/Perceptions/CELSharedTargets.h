#pragma once
#include "CELSharedMemory.h"
class IELAdapter;

class CELSharedTargets
{
public:
	static void Clear();
	static void RemoveOwner(u16 OwnerIdentifier);
	static const SELObservation* Select(const CELSharedMemory& Memory, IELAdapter& Adapter, u16 CurrentTarget);
};
