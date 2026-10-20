#include "stdafx.h"
#include "CELDebugOnlinePopulation.h"
#include "../CoreComponent/CELNativeEntityes.h"
#include "ai/monsters/basemonster/base_monster.h"
#include "Level.h"

void CELDebugOnlinePopulation::Update(u32 WorldGeneration)
{
	if (Generation == WorldGeneration && s32(Device.dwTimeGlobal - NextUpdateAt) < 0)
	{
		return;
	}
	Generation = WorldGeneration;
	NextUpdateAt = Device.dwTimeGlobal + 250u;
	FlyingCount = OtherCount = 0;
	if (!g_pGameLevel)
	{
		return;
	}
	for (u32 Index = 0; Index < Level().Objects.o_count(); ++Index)
	{
		CBaseMonster* Monster = smart_cast<CBaseMonster*>(Level().Objects.o_get_by_iterator(Index));
		if (!Monster || Monster->getDestroy() || !Monster->g_Alive())
		{
			continue;
		}
		if (ExtendedEntityesClasses::IsFlying(Monster->CLS_ID))
		{
			++FlyingCount;
		}
		else
		{
			++OtherCount;
		}
	}
}
