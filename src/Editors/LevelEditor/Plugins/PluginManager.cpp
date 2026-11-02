#include "stdafx.h"
#include "PluginManager.h"
#include "../Packages/PackageStore.h"

// Subsystems
#include "PluginLua.h"
#include "PluginPython.h"

CPluginsManagers* GPluginManager = nullptr;
CPluginsManagers& CPluginsManagers::Instance()
{
	if (GPluginManager == nullptr)
	{
		GPluginManager = new CPluginsManagers;
		GPluginManager->Init();
	}

	return *GPluginManager;
}

CPluginsManagers::~CPluginsManagers()
{
	for (IPluginBase* Plug : Plugins)
	{
		xr_delete(Plug);
	}
}

CPluginsManagers::CPluginsManagers()
{
	string_path Root;
	FS.update_path(Root, "$fs_root$", "plugins");

	if (!std::filesystem::exists(Root))
	{
		std::filesystem::create_directory(Root);
	}
}

void CPluginsManagers::Reinit()
{
	for (IPluginBase* Plug : Plugins)
	{
		xr_delete(Plug);
	}
	Plugins.clear();

	Init();
}

void CPluginsManagers::Init()
{
	string_path SdkRoot;
	FS.update_path(SdkRoot, "$fs_root$", "");
	xr_string RecoveryError;
	const bool HasRecovered = SdkPackages::PackageStore(SdkRoot).Recover(RecoveryError);
	if (!HasRecovered)
	{
		Msg("! [Packages] %s Packaged plugins will be skipped.", RecoveryError.c_str());
	}
	string_path Root;
	FS.update_path(Root, "$fs_root$", "plugins");

	const xr_path ManagedPluginsRoot = SdkPackages::Join(Root, "packages");
	std::error_code Code;
	for (xr_dir_recursive_iter Iter(Root, Code), End; !Code && Iter != End; Iter.increment(Code))
	{
		const xr_path FileIter = Iter->path();
		if (!HasRecovered && SdkPackages::Details::SamePath(FileIter, ManagedPluginsRoot))
		{
			Iter.disable_recursion_pending();
			continue;
		}
		const xr_path Ext = FileIter.extension();

		IPluginBase* Plug = nullptr;

		if (Ext.xstring().ends_with("lua"))
		{
			Plug = Plugins.emplace_back(new CPluginLua);
		}
		else if (Ext.xstring().ends_with("py"))
		{
			Plug = Plugins.emplace_back(new CPluginPython);
		}

		if (Plug == nullptr)
		{
			continue;
		}

		Plug->Path = FileIter.xstring();
		Plug->Desc = Plug->ReadDesc();
		Plug->Name = FileIter.xfilename().substr(0, FileIter.xfilename().find('.'));
	}
	if (Code)
	{
		Msg("! [Plugins] Cannot enumerate plugins: %s", Code.message().c_str());
	}
}
