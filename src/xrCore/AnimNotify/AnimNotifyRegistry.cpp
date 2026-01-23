#include "stdafx.h"
#include "AnimNotifyRegistry.h"

#include "AnimNotify.h"
#include "magic_enum/magic_enum.hpp"

CAnimNotifyRegistry::CAnimNotifyRegistry()
{
    xr_set<xr_string> files;
    string_path path;
    FS.update_path(path, _game_config_, "misc\\anim_notify");
    FS.get_all_files_in_dir(files, path);
    for (auto& str : files)
    {
        if (!str.ends_with(".ltx"))
        {
            continue;
        }
        CInifile ini = CInifile(str.c_str());
        CInifile::Root& sections = ini.sections();
        for (CInifile::Sect& sect : sections)
        {
            VERIFY(IAnimNotifyHandler::IsValid());
            shared_str& sect_name = sect.Name;
            LPCSTR type_str = ini.r_string(sect_name, "type");
            auto type = magic_enum::enum_cast<EAnimNotifyType>(type_str);
            R_ASSERT4(type.has_value(), "Invalid notify type", sect_name.c_str(), type_str);
            auto NewNotify = IAnimNotifyHandler::Get().ConstructNotify(type.value());
            NewNotify->Construct(ini, sect_name.c_str());
            R_ASSERT3(!map.contains(sect_name), "Find duplicated anim notify section", sect_name.c_str());
            map[sect_name] = NewNotify;
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
