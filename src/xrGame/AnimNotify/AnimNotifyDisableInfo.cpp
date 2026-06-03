#include "stdafx.h"
#include "AnimNotifyDisableInfo.h"

#include "alife_registry_container.h" // without this not compiles

#include "ai_space.h"
#include "alife_simulator.h"
#include "InfoPortionDefs.h"
#include "Level.h"

void CAnimNotifyDisableInfo::Construct(const CInifile& ini, const char* sect)
{
    Info = ini.r_string(sect, "Info");
}

void CAnimNotifyDisableInfo::Execute(IRenderVisual* visual, u16 bone_id)
{
    auto known_info = ai().get_alife()->registry().get<CInfoPortionRegistry>().object(0, true);
    VERIFY(known_info);
    known_info->RemoveInfo(Info);
}
