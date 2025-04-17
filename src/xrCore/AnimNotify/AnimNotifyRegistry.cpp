#include "stdafx.h"
#include "AnimNotifyRegistry.h"

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
        for (auto& section : ini.sections())
        {
            R_ASSERT3(!map.contains(section->Name), "Find duplicated anim notify section", section->Name.c_str());
            map[section->Name] = {};
            auto& info = map[section->Name];
            if (section->line_exist("GiveInfo"))
            {
                info.GiveInfo = ini.r_string(section->Name, "GiveInfo");
            }
            if (section->line_exist("DisableInfo"))
            {
                info.GiveInfo = ini.r_string(section->Name, "DisableInfo");
            }
            if (section->line_exist("Functor"))
            {
                info.GiveInfo = ini.r_string(section->Name, "Functor");
            }
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

const CAnimNotifyRegistry::SAnimNotifyInfo& CAnimNotifyRegistry::get(const shared_str& name)
{
    return map[name];
}
