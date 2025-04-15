#include "stdafx.h"
#include "PluginManager.h"

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
	string_path Root;
	FS.update_path(Root, "$fs_root$", "plugins");

	for (const xr_path& FileIter : xr_dir_recursive_iter{ Root })
	{
		xr_path Ext = FileIter.extension();

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
			continue;

		Plug->Path = FileIter.xstring();
		Plug->Desc = Plug->ReadDesc();
		Plug->Name = FileIter.xfilename().substr(0, FileIter.xfilename().find('.'));
	}
}