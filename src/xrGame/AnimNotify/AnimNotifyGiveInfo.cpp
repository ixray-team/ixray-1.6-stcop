#include "stdafx.h"
#include "AnimNotifyGiveInfo.h"

#include "ai_space.h"
#include "alife_registry_container.h" // without this not compiles
#include "alife_registry_container_composition.h"
//#include "alife_registry_container_space.h"
#include "alife_simulator.h"
#include "InfoPortionDefs.h"
#include "Level.h"

void CAnimNotifyGiveInfo::Construct(CInifile ini, LPCSTR sect)
{
    Info = ini.r_string(sect, "Info");
}

void CAnimNotifyGiveInfo::Execute(IRenderVisual* visual, u16 bone_id)
{
    KNOWN_INFO_VECTOR *known_info = ai().get_alife()->registry(info_portions).object(0, true);
    VERIFY(known_info);
    if (std::find_if(known_info->begin(), known_info->end(), CFindByIDPred(Info)) == known_info->end())
    {
        known_info->push_back(INFO_DATA(Info, Level().GetGameTime()));
    }
}
