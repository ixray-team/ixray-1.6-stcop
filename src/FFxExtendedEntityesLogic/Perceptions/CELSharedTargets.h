#pragma once
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedMemory.h"
class IELAdapter;
class IELLogic;
class CEntityAlive;
struct SELDebugRecord;

class CELSharedTargets
{
public:
	static void Clear();
	static void RecordHit(CEntityAlive& Target, u16 AttackerIdentifier, float PreviousHealth);
	static void FillDebug(SELDebugRecord& Record, u16 OwnerIdentifier);
	static u32 ThreatPriority(const SELObservation& Observation, IELAdapter& Adapter);
	static void RemoveOwner(u16 OwnerIdentifier);
	static const SELObservation* Select(const CELSharedMemory& Memory, IELAdapter& Adapter, u16 CurrentTarget, IELLogic* Logic = nullptr);
};
