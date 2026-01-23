#include "StdAfx.h"
#include "AnimNotifyGiveInfo.h"

#include "ai_space.h"
#include "alife_registry_container.h" // without this not compiles
#include "alife_simulator.h"
#include "InfoPortionDefs.h"
#include "Level.h"

void CAnimNotifyGiveInfo::Construct(const CInifile& ini, const char* sect)
{
    Info = ini.r_string(sect, "Info");
}

void CAnimNotifyGiveInfo::Execute(IRenderVisual* visual, u16 bone_id)
{
    auto known_info = ai().get_alife()->registry().get<CInfoPortionRegistry>().object(0, true);
    VERIFY(known_info);
    known_info->AddInfo(Info, Level().GetGameTime());
}
