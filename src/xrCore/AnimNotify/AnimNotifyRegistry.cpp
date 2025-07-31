#include "stdafx.h"
#include "AnimNotifyRegistry.h"

#include "AnimNotify.h"
#include "magic_enum/magic_enum.hpp"

CAnimNotifyRegistry::CAnimNotifyRegistry()
{
    xr_set<xr_string> files;
    string_path path;
    FS.update_path(path, "$game_config$", "misc\\anim_notify");
    FS.get_all_files_in_dir(files, path);
    for (auto& str : files)
    {
        if (!str.ends_with(".ltx"))
        {
            continue;
        }
        CInifile ini = CInifile(str.c_str());
        for (const auto& section : ini.sections())
        {
            VERIFY(IAnimNotifyHandler::IsValid());
            auto type_str = ini.r_string(section->Name, "type");
            auto type = magic_enum::enum_cast<EAnimNotifyType>(type_str);
            R_ASSERT4(type.has_value(), "Invalid notify type", section->Name.c_str(), type_str);
            auto NewNotify = IAnimNotifyHandler::Get().ConstructNotify(type.value());
            NewNotify->Construct(ini, section->Name.c_str());
            R_ASSERT3(!map.contains(section->Name), "Find duplicated anim notify section", section->Name.c_str());
            map[section->Name] = NewNotify;
        }
    }
}

CAnimNotifyRegistry& CAnimNotifyRegistry::GetInstance()
{
    static CAnimNotifyRegistry instance;
    return instance;
}

bool CAnimNotifyRegistry::contains(const shared_str& name)
{
    return map.contains(name);
}

IAnimNotify* CAnimNotifyRegistry::get(const shared_str& name)
{
    return map[name];
}
