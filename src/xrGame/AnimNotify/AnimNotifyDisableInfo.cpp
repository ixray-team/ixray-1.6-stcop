#include "stdafx.h"
#include "AnimNotifyDisableInfo.h"

#include "alife_registry_container.h" // without this not compiles

#include "ai_space.h"
#include "alife_simulator.h"
#include "InfoPortionDefs.h"
#include "Level.h"

void CAnimNotifyDisableInfo::Construct(const CInifile& ini, LPCSTR sect)
{
    Info = ini.r_string(sect, "Info");
}

void CAnimNotifyDisableInfo::Execute(IRenderVisual* visual, u16 bone_id)
{
    KNOWN_INFO_VECTOR *known_info = ai().get_alife()->registry().get<CInfoPortionRegistry>().object(0, true);
    VERIFY(known_info);
    if (auto It = std::find_if(known_info->begin(), known_info->end(), CFindByIDPred(Info));
        It != known_info->end())
    {
        known_info->erase(It);
    }
}
