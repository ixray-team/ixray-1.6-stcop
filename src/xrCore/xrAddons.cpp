#include "stdafx.h"
#include "xrAddons.h"

#include "EngineExternal.h"
#include <FormatParsers/YAML/xr_yaml_reader.h>
#include <filesystem>

XRCORE_API CAddonManager* g_pAddonsManager = nullptr;

void CAddonManager::Initialize()
{
    FS.IsAddonPhase = true;

    CEngineExternal engineExternal; // Hack
    currentPlatform = engineExternal.GetCurrentPlatformName();

    CollectAddons();
    ResolveDependencies();
    MountAddons();

    FS.IsAddonPhase = false;
}

void CAddonManager::CollectAddons()
{
    Addons.clear();

    const xr_string AddonsRoot = FS.get_path("$arch_dir_addons$")->m_Path;
    if (!std::filesystem::exists(AddonsRoot.c_str()))
    {
        Msg("! Addons directory not found: %s", AddonsRoot.c_str());
        return;
    }

    auto options = std::filesystem::directory_options::follow_directory_symlink;
    for (auto& Entry : std::filesystem::recursive_directory_iterator(AddonsRoot.c_str(), options))
    {
        if (!Entry.is_regular_file())
        {
            continue;
        }

        if (Entry.path().filename() == "addon.init")
        {
            ReadMetaInfo(Entry.path().string().c_str());
        }
    }
}

void CAddonManager::ReadMetaInfo(const xr_string& InitFile)
{
    AddonInfo Addon;

    std::filesystem::path InitPath(InitFile.c_str());
    std::filesystem::path RootDir = InitPath.parent_path();
    Addon.EntryDir = RootDir.string().c_str();
    Addon.AddonName = RootDir.filename().string().c_str();
    Addon.Platforms.insert(currentPlatform.c_str());

    CYaml Yaml(RootDir.string().c_str(), "addon.init");

    if (Yaml.IsValid())
    {
        const YAML::Node& Root = Yaml.GetRoot();

        xr_string Name = Yaml.GetStringRoot(Root, "name", RootDir.filename().string().c_str());
        Addon.AddonName = Name.c_str();

        YAML::Node platforms = Root["platform"];
        if (platforms)
        {
            Addon.Platforms.clear();

            if (platforms.IsSequence())
            {
                for (const auto& platform : platforms)
                {
                    if (platform.IsScalar())
                    {
                        Addon.Platforms.insert(platform.as<std::string>().c_str());
                    }
                }
            }
            else if (platforms.IsScalar())
            {
                Addon.Platforms.insert(platforms.as<std::string>().c_str());
            }
        }

        // Script
        xr_string Script = Yaml.GetStringRoot(Root, "script", "");
        if (!Script.empty())
        {
            xr_string S = Script.c_str();
            if (S.ends_with(".script"))
            {
                S.erase(S.length() - 7);
            }
            Addon.ScriptInit = S.c_str();
        }

        YAML::Node DependsNode = Root["depends"];
        if (DependsNode)
        {
            if (DependsNode.IsSequence())
            {
                for (const auto& Dep : DependsNode)
                {
                    if (Dep.IsScalar())
                    {
                        Addon.Dependencies.push_back(Dep.as<std::string>().c_str());
                    }
                }
            }
            else if (DependsNode.IsScalar())
            {
                Addon.Dependencies.push_back(DependsNode.as<std::string>().c_str());
            }
        }
    }

    if (Addon.Platforms.contains(currentPlatform.c_str()))
    {
        Addons.push_back(Addon);
        Msg("Addon found: %s (%s)", Addon.AddonName.c_str(), Addon.EntryDir.c_str());
    }
    else
    {
        Msg("# Add-on `%s` belongs to another platform", Addon.AddonName.c_str());
    }
}

void CAddonManager::ResolveDependencies()
{
    xr_vector<AddonInfo> Sorted;
    xr_vector<AddonInfo> Work = Addons;

    while (!Work.empty())
    {
        bool Progress = false;

        for (auto It = Work.begin(); It != Work.end(); )
        {
            AddonInfo& Addon = *It;
            bool CanLoad = true;

            for (auto& Dep : Addon.Dependencies)
            {
                if (Dep.empty())
                {
                    continue;
                }

                bool Found = false;
                for (auto& Loaded : Sorted)
                {
                    if (!Loaded.AddonName.size() == 0 &&
                        stricmp(*Loaded.AddonName, Dep.c_str()) == 0)
                    {
                        Found = true;
                        break;
                    }
                }

                if (!Found)
                {
                    CanLoad = false;
                    break;
                }
            }

            if (CanLoad)
            {
                Sorted.push_back(Addon);
                It = Work.erase(It);
                Progress = true;
            }
            else
            {
                ++It;
            }
        }

        if (!Progress)
        {
            for (auto& Addon : Work)
            {
                if (!Addon.AddonName.size() == 0)
                {
                    Msg("! Addon '%s' skipped: missing dependencies", *Addon.AddonName);
                }
            }
            break;
        }
    }

    Addons.swap(Sorted);
    Msg("Resolved %zu addons", Addons.size());
}

void CAddonManager::MountAddons()
{
    for (auto& Addon : Addons)
    {
        Msg("Mounting addon: %s", Addon.AddonName.c_str());

        std::filesystem::path AddonPath(Addon.EntryDir.c_str());

        if (std::filesystem::is_directory(AddonPath))
        {
            xr_stack_string_path addon_path = Addon.EntryDir.c_str();
            addon_path.append("\\");
            FS.rescan_path(addon_path.c_str(), true);
        }
        else if (AddonPath.extension() == ".db" && std::filesystem::is_regular_file(AddonPath))
        {
            CLocatorAPI::archive A;
            A.path = AddonPath.string().c_str();
            FS.LoadArchive(A);
        }
        else
        {
            Msg("! Unknown addon format: %s", Addon.EntryDir.c_str());
        }
    }
}

bool CAddonManager::CanApply(xr_string& TempPath, CLocatorAPI::file& Desc)
{
    const xr_string DataPath = FS.get_path(_game_data_)->m_Path;

    if (std::filesystem::is_directory(TempPath.c_str()) || TempPath.ends_with("addon.init"))
    {
        return false;
    }

    for (auto& Addon : Addons)
    {
        xr_string AddonRoot = Addon.EntryDir.c_str();
        xr_strlwr(AddonRoot);

        if (TempPath.starts_with(AddonRoot))
        {
            Desc.wrap = xr_strdup(TempPath.c_str());

            xr_path TempOutPath = DataPath;
            xr_string FilePath = TempPath.substr(AddonRoot.length());

            if (FilePath.starts_with('\\'))
            {
                FilePath = FilePath.substr(1);
            }

            TempOutPath.append(FilePath.c_str());
            TempPath = TempOutPath.xstring();
            return true;
        }
    }

    return false;
}